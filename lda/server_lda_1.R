if (!exists("%||%", mode = "function", inherits = TRUE)) {
  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }
}

register_lda_module <- function(input, output, session, rv) {
  # Module LDA piloté par lda.py / wordcloud_lda.py (sans dépendance à lda.R).
  trouver_python_lda <- function() {
    candidats <- c(Sys.which("python3"), Sys.which("python"))
    candidats <- candidats[nzchar(candidats)]
    if (!length(candidats)) stop("Aucun interpréteur Python trouvé (python3/python).")
    candidats[[1]]
  }

  executer_commande_python <- function(python_exec, args, etiquette) {
    logs <- suppressWarnings(system2(python_exec, args = args, stdout = TRUE, stderr = TRUE))
    statut <- as.integer(attr(logs, "status") %||% 0L)
    if (!identical(statut, 0L)) {
      stop(
        paste0(
          etiquette,
          " a échoué (code ", statut, ").\nLogs Python:\n",
          paste(logs, collapse = "\n")
        )
      )
    }
    logs
  }

  construire_stopwords_fr_quanteda <- function(activer) {
    if (!isTRUE(activer)) return(character(0))
    if (!requireNamespace("quanteda", quietly = TRUE)) {
      stop("Le package quanteda est requis pour retirer les stopwords.")
    }
    unique(quanteda::stopwords(language = "fr", source = "snowball"))
  }

  lire_corpus_brut <- function() {
    datapath <- tryCatch(input$fichier_corpus$datapath, error = function(e) NULL)
    if (is.null(datapath) || !nzchar(datapath) || !file.exists(datapath)) {
      return("")
    }
    lignes <- readLines(datapath, warn = FALSE, encoding = "UTF-8")
    paste(lignes, collapse = "\n")
  }

  convertir_resultat_python <- function(res_lda_json, res_wc_json) {
    topics <- res_lda_json$topics
    unites <- res_lda_json$unites

    top_terms <- data.frame(topic = character(0), term = character(0), prob = numeric(0), stringsAsFactors = FALSE)
    for (t in topics) {
      if (is.null(t$mots) || !length(t$mots)) next
      poids <- vapply(t$mots, function(m) as.numeric(m$poids %||% 0), numeric(1))
      somme <- sum(poids)
      prob <- if (somme > 0) poids / somme else poids
      df_t <- data.frame(
        topic = paste0("Topic_", t$topic),
        term = vapply(t$mots, function(m) as.character(m$mot %||% ""), character(1)),
        prob = prob,
        stringsAsFactors = FALSE
      )
      top_terms <- rbind(top_terms, df_t)
    }

    # Matrice topic-terme (pour les graphes/tableaux existants)
    termes_uniques <- unique(top_terms$term)
    topics_uniques <- unique(top_terms$topic)
    mat <- matrix(0, nrow = length(topics_uniques), ncol = length(termes_uniques))
    rownames(mat) <- topics_uniques
    colnames(mat) <- termes_uniques
    if (nrow(top_terms) > 0) {
      for (i in seq_len(nrow(top_terms))) {
        mat[top_terms$topic[i], top_terms$term[i]] <- as.numeric(top_terms$prob[i])
      }
    }

    # Distribution topics / unités
    doc_topics <- data.frame(doc_id = character(0), stringsAsFactors = FALSE)
    if (!is.null(unites) && length(unites) > 0) {
      if (is.data.frame(unites)) {
        unites_df <- unites
      } else {
        unites_df <- as.data.frame(unites, stringsAsFactors = FALSE)
      }

      if ("identifiant" %in% names(unites_df)) {
        doc_topics <- data.frame(doc_id = as.character(unites_df$identifiant), stringsAsFactors = FALSE)
      }

      if ("distribution_topics" %in% names(unites_df)) {
        dists <- unites_df$distribution_topics
        if (is.list(dists) && length(dists) > 0) {
          max_k <- max(vapply(dists, length, integer(1)))
          for (k in seq_len(max_k)) {
            colk <- vapply(dists, function(x) as.numeric(x[[k]] %||% NA_real_), numeric(1))
            doc_topics[[paste0("Topic_", k)]] <- colk
          }
        }
      }
    }

    # Segments textuels
    doc_texts <- data.frame(doc_id = character(0), segment_texte = character(0), stringsAsFactors = FALSE)
    if (!is.null(unites) && length(unites) > 0) {
      if (is.data.frame(unites)) {
        unites_df <- unites
      } else {
        unites_df <- as.data.frame(unites, stringsAsFactors = FALSE)
      }
      if (all(c("identifiant", "texte") %in% names(unites_df))) {
        doc_texts <- data.frame(
          doc_id = as.character(unites_df$identifiant),
          segment_texte = as.character(unites_df$texte),
          stringsAsFactors = FALSE
        )
      }
    }

    wordcloud_images <- character(0)
    if (!is.null(res_wc_json$fichiers) && length(res_wc_json$fichiers) > 0) {
      fichiers_df <- as.data.frame(res_wc_json$fichiers, stringsAsFactors = FALSE)
      if ("image" %in% names(fichiers_df)) {
        wordcloud_images <- as.character(fichiers_df$image)
      }
    }

    list(
      top_terms = top_terms,
      topic_term_matrix = mat,
      doc_topics = doc_topics,
      doc_texts = doc_texts,
      wordcloud_images = wordcloud_images,
      meta = res_lda_json$meta %||% list(),
      brut = res_lda_json
    )
  }

  executer_test_lda <- function(k, n_terms, segment_size = NULL, segmenter_sur_ponctuation_forte = NULL, mode_unite = NULL) {
    tryCatch({
      corpus <- lire_corpus_brut()
      if (!nzchar(trimws(corpus))) {
        rv$lda_erreur <- "Aucun texte disponible pour LDA. Importez d'abord un fichier corpus."
        rv$lda_statut <- "Test LDA impossible."
        showNotification(rv$lda_erreur, type = "warning")
        return(invisible(NULL))
      }

      mode_unite_lda <- as.character(mode_unite %||% input$lda_mode_unite_dyn %||% input$lda_mode_unite %||% "segment")
      if (!mode_unite_lda %in% c("document", "segment")) {
        mode_unite_lda <- "segment"
      }

      python_exec <- trouver_python_lda()
      if (exists("installer_packages_python_lda", mode = "function", inherits = TRUE)) {
        ok_install <- installer_packages_python_lda()
        if (!isTRUE(ok_install)) {
          stop("Dépendances Python LDA indisponibles. Installez scikit-learn, wordcloud et matplotlib (reticulate::py_install).")
        }
      }
      script_lda <- if (exists("LDA_PY_SCRIPT", inherits = TRUE)) get("LDA_PY_SCRIPT", inherits = TRUE) else normalizePath("lda/lda.py", mustWork = TRUE)
      script_wc <- if (exists("LDA_WORDCLOUD_PY_SCRIPT", inherits = TRUE)) get("LDA_WORDCLOUD_PY_SCRIPT", inherits = TRUE) else normalizePath("lda/wordcloud_lda.py", mustWork = TRUE)

      prefixe <- format(Sys.time(), "%Y%m%d_%H%M%S")
      entree <- tempfile(pattern = paste0("entree_lda_app_", prefixe, "_"), tmpdir = "lda", fileext = ".json")
      sortie_lda <- tempfile(pattern = paste0("sortie_lda_app_", prefixe, "_"), tmpdir = "lda", fileext = ".json")
      sortie_wc <- tempfile(pattern = paste0("sortie_wc_app_", prefixe, "_"), tmpdir = "lda", fileext = ".json")
      dossier_images <- file.path("lda", paste0("wordclouds_app_", prefixe))
      dir.create(dossier_images, recursive = TRUE, showWarnings = FALSE)

      stopwords_fr <- construire_stopwords_fr_quanteda(isTRUE(input$lda_retirer_stopwords))
      categories <- if (isTRUE(input$lda_filtrage_morpho)) {
        as.list(tolower(as.character(input$lda_pos_keep %||% c("NOM", "VER", "ADJ"))))
      } else {
        list()
      }

      longueur_min_segment <- max(10L, as.integer(segment_size %||% input$lda_segment_size_dyn %||% input$lda_segment_size %||% 40) * 4L)

      payload <- list(
        corpus_texte = corpus,
        mode_unite = mode_unite_lda,
        longueur_min_segment = as.integer(longueur_min_segment),
        nb_topics = as.integer(k %||% 4),
        nb_mots_par_topic = as.integer(n_terms %||% 8),
        random_state = 42,
        stopwords_personnalises = as.list(stopwords_fr),
        categories_morpho = categories,
        chemin_lexique_fr = normalizePath(file.path("dictionnaires", "lexique_fr.csv"), mustWork = FALSE)
      )

      jsonlite::write_json(payload, entree, auto_unbox = TRUE, pretty = TRUE)

      logs_lda <- executer_commande_python(
        python_exec,
        args = c(script_lda, "--input", entree, "--output", sortie_lda),
        etiquette = "lda.py"
      )
      if (!file.exists(sortie_lda)) {
        stop(paste0("lda.py n'a pas produit de sortie JSON.\nLogs Python:\n", paste(logs_lda, collapse = "\n")))
      }
      res_lda <- jsonlite::fromJSON(sortie_lda, simplifyVector = FALSE)
      if (!isTRUE(res_lda$succes)) {
        stop(as.character(res_lda$erreur %||% "Erreur inconnue dans lda.py"))
      }

      logs_wc <- executer_commande_python(
        python_exec,
        args = c(script_wc, "--input", sortie_lda, "--output", sortie_wc, "--output-dir", dossier_images, "--prefix", "nuage_lda"),
        etiquette = "wordcloud_lda.py"
      )

      res_wc <- list(succes = FALSE, fichiers = list())
      if (file.exists(sortie_wc)) {
        res_wc <- jsonlite::fromJSON(sortie_wc, simplifyVector = FALSE)
      }

      conv <- convertir_resultat_python(res_lda, res_wc)

      rv$lda_resultat <- list(
        top_terms = conv$top_terms,
        topic_term_matrix = conv$topic_term_matrix,
        doc_topics = conv$doc_topics,
        wordcloud_images = conv$wordcloud_images,
        meta = conv$meta,
        logs_lda = logs_lda,
        logs_wordcloud = logs_wc
      )
      rv$lda_doc_texts <- conv$doc_texts
      rv$lda_erreur <- NULL
      rv$lda_statut <- paste0("LDA Python exécuté (mode=", mode_unite_lda, ", k=", as.integer(k %||% 4), ", unités=", nrow(conv$doc_texts), ").")

      updateRadioButtons(session, "lda_mode_unite_dyn", selected = mode_unite_lda)
      updateNumericInput(session, "lda_k_dyn", value = as.integer(k %||% 4))
      updateNumericInput(session, "lda_n_terms_dyn", value = as.integer(n_terms %||% 8))
      updateNumericInput(session, "lda_segment_size_dyn", value = as.integer(segment_size %||% input$lda_segment_size_dyn %||% 40))
      showNotification("LDA Python terminé.", type = "message")
      invisible(NULL)
    }, error = function(e) {
      rv$lda_erreur <- conditionMessage(e)
      rv$lda_statut <- "Échec du LDA Python."
      showNotification(paste("Erreur LDA:", conditionMessage(e)), type = "error", duration = 8)
      invisible(NULL)
    })
  }


  capturer_parametres_lda <- function() {
    list(
      lda_mode_unite = isolate(input$lda_mode_unite_dyn %||% input$lda_mode_unite %||% "segment"),
      lda_k = isolate(input$lda_k_dyn %||% input$lda_k %||% 4),
      lda_n_terms = isolate(input$lda_n_terms_dyn %||% input$lda_n_terms %||% 8),
      lda_retirer_stopwords = isolate(input$lda_retirer_stopwords %||% FALSE),
      lda_filtrage_morpho = isolate(input$lda_filtrage_morpho %||% FALSE),
      lda_pos_keep = isolate(input$lda_pos_keep %||% c("NOM", "VER", "ADJ")),
      lda_segment_size = isolate(input$lda_segment_size_dyn %||% input$lda_segment_size %||% 40),
      lda_segmenter_sur_ponctuation_forte = isolate(
        input$lda_segmenter_sur_ponctuation_forte_dyn %||% input$lda_segmenter_sur_ponctuation_forte %||% TRUE
      ),
      lda_segmenter_sur_ponctuation_forte_dyn = isolate(
        input$lda_segmenter_sur_ponctuation_forte_dyn %||% input$lda_segmenter_sur_ponctuation_forte %||% TRUE
      )
    )
  }

  ouvrir_modal_parametres_lda <- function() {
    defaults <- capturer_parametres_lda()

    showModal(modalDialog(
      title = "Paramètres LDA",
      easyClose = TRUE,
      size = "m",
      ui_form_parametres_lda(defaults = defaults),
      footer = tagList(
        modalButton("Fermer"),
        actionButton("lancer_lda", "Lancer le test LDA", class = "btn-primary")
      )
    ))
  }

  observeEvent(input$ouvrir_param_lda, {
    ouvrir_modal_parametres_lda()
  })

  observeEvent(input$lancer_lda, {
    removeModal()
    tryCatch({
      executer_test_lda(
      k = as.integer(input$lda_k %||% input$lda_k_dyn %||% 4),
      n_terms = as.integer(input$lda_n_terms %||% input$lda_n_terms_dyn %||% 8),
      segment_size = as.integer(input$lda_segment_size %||% input$lda_segment_size_dyn %||% 40),
      segmenter_sur_ponctuation_forte = isTRUE(
        input$lda_segmenter_sur_ponctuation_forte %||% input$lda_segmenter_sur_ponctuation_forte_dyn
      ),
      mode_unite = as.character(input$lda_mode_unite %||% input$lda_mode_unite_dyn %||% "segment")
      )
    }, error = function(e) {
      rv$lda_erreur <- conditionMessage(e)
      rv$lda_statut <- "Échec du LDA Python."
      showNotification(paste("Erreur LDA:", conditionMessage(e)), type = "error", duration = 8)
    })
  })

  observeEvent(input$lancer_lda_dyn, {
    tryCatch({
      executer_test_lda(
      k = as.integer(input$lda_k_dyn %||% input$lda_k %||% 4),
      n_terms = as.integer(input$lda_n_terms_dyn %||% input$lda_n_terms %||% 8),
      segment_size = as.integer(input$lda_segment_size_dyn %||% input$lda_segment_size %||% 40),
      segmenter_sur_ponctuation_forte = isTRUE(
        input$lda_segmenter_sur_ponctuation_forte_dyn %||% input$lda_segmenter_sur_ponctuation_forte
      ),
      mode_unite = as.character(input$lda_mode_unite_dyn %||% input$lda_mode_unite %||% "segment")
      )
    }, error = function(e) {
      rv$lda_erreur <- conditionMessage(e)
      rv$lda_statut <- "Échec du LDA Python."
      showNotification(paste("Erreur LDA:", conditionMessage(e)), type = "error", duration = 8)
    })
  })

  output$ui_lda_statut <- renderUI({
    if (!is.null(rv$lda_erreur) && nzchar(rv$lda_erreur)) {
      return(tags$div(style = "color:#b30000; font-weight:600;", paste("Statut:", rv$lda_statut, "-", rv$lda_erreur)))
    }
    tags$div(style = "color:#1e5aa8; font-weight:600;", paste("Statut:", rv$lda_statut))
  })

  output$table_lda_top_terms <- renderTable({
    req(rv$lda_resultat, rv$lda_resultat$top_terms)
    termes <- rv$lda_resultat$top_terms
    if ("prob" %in% names(termes)) {
      termes$prob <- signif(as.numeric(termes$prob), 6)
    }
    head(termes, 100)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$table_lda_segments_textes <- renderTable({
    req(rv$lda_doc_texts)
    segments_ref <- rv$lda_doc_texts
    segments_ref$doc_id <- as.character(segments_ref$doc_id)
    segments_ref$segment_texte <- as.character(segments_ref$segment_texte)
    segments_ref
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$table_lda_doc_topics <- renderTable({
    req(rv$lda_resultat, rv$lda_resultat$doc_topics)
    doc_topics <- rv$lda_resultat$doc_topics
    topic_cols <- grep("^\\d+$|^Topic_", names(doc_topics), value = TRUE)
    if (!length(topic_cols)) {
      topic_cols <- setdiff(names(doc_topics), "doc_id")
    }
    if (length(topic_cols)) {
      mat <- as.matrix(doc_topics[, topic_cols, drop = FALSE])
      dominant_idx <- max.col(mat, ties.method = "first")
      doc_topics$topic_dominant <- topic_cols[dominant_idx]
      doc_topics$prob_topic_dominant <- round(mat[cbind(seq_len(nrow(mat)), dominant_idx)], 4)
    }
    head(doc_topics, 100)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$plot_lda_top_terms <- renderPlot({
    req(rv$lda_resultat, rv$lda_resultat$top_terms)
    termes <- rv$lda_resultat$top_terms
    req(is.data.frame(termes), nrow(termes) > 0, "prob" %in% names(termes))

    termes$topic <- as.factor(termes$topic)
    termes$term <- stats::reorder(termes$term, termes$prob)

    ggplot2::ggplot(termes, ggplot2::aes(x = term, y = prob, fill = topic)) +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::coord_flip() +
      ggplot2::facet_wrap(~topic, scales = "free_y") +
      ggplot2::labs(
        title = "Top mots par thème (LDA)",
        x = "Mot",
        y = "Probabilité du mot dans le thème"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  })

  if (exists("register_lda_wordcloud_outputs", mode = "function", inherits = TRUE)) {
    register_lda_wordcloud_outputs(output = output, rv = rv)
  }

  output$plot_lda_topics_bubble <- plotly::renderPlotly({
    req(rv$lda_resultat, rv$lda_resultat$topic_term_matrix, rv$lda_resultat$doc_topics)
    mat_topics <- rv$lda_resultat$topic_term_matrix
    req(nrow(mat_topics) >= 2, ncol(mat_topics) >= 2)

    coords <- stats::prcomp(mat_topics, center = TRUE, scale. = TRUE)$x[, 1:2, drop = FALSE]
    docs <- rv$lda_resultat$doc_topics
    topic_cols <- colnames(docs)[colnames(docs) != "doc_id"]
    if (length(topic_cols) == nrow(coords)) {
      rownames(coords) <- topic_cols
    }
    prevalence <- colMeans(as.matrix(docs[, topic_cols, drop = FALSE]), na.rm = TRUE)

    df_plot <- data.frame(
      topic = rownames(coords),
      x = coords[, 1],
      y = coords[, 2],
      prevalence = as.numeric(prevalence[rownames(coords)]),
      stringsAsFactors = FALSE
    )
    df_plot$prevalence[is.na(df_plot$prevalence)] <- 0

    pad_ratio <- 0.20

    x_span <- diff(range(df_plot$x, na.rm = TRUE))
    y_span <- diff(range(df_plot$y, na.rm = TRUE))
    x_pad <- ifelse(is.finite(x_span) && x_span > 0, x_span * pad_ratio, 0.5)
    y_pad <- ifelse(is.finite(y_span) && y_span > 0, y_span * pad_ratio, 0.5)
    x_limits <- c(min(df_plot$x, na.rm = TRUE) - x_pad, max(df_plot$x, na.rm = TRUE) + x_pad)
    y_limits <- c(min(df_plot$y, na.rm = TRUE) - y_pad, max(df_plot$y, na.rm = TRUE) + y_pad)

    bubble_size_max <- 32

    p <- ggplot2::ggplot(df_plot, ggplot2::aes(
      x = x, y = y, size = prevalence, label = topic, color = topic,
      text = paste0(
        "Topic: ", topic,
        "<br>Prévalence: ", round(prevalence, 4),
        "<br>Axe 1: ", round(x, 4),
        "<br>Axe 2: ", round(y, 4)
      )
    )) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::geom_text(vjust = -0.9, show.legend = FALSE) +
      ggplot2::scale_size_area(max_size = bubble_size_max) +
      ggplot2::coord_cartesian(xlim = x_limits, ylim = y_limits, clip = "off") +
      ggplot2::labs(
        title = "Projection des topics (bulle)",
        subtitle = "Position: projection PCA des distributions de mots ; taille: prévalence moyenne dans les documents (zoom/drag interactif disponible)",
        x = "Axe 1",
        y = "Axe 2",
        size = "Prévalence"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(plot.margin = grid::unit(c(8, 20, 8, 8), "pt"))

    plotly::ggplotly(p, tooltip = "text") %>%
      plotly::config(displaylogo = FALSE)
  })

  output$table_lda_segments <- renderTable({
    req(rv$lda_resultat, rv$lda_resultat$doc_topics, rv$lda_doc_texts)
    doc_topics <- rv$lda_resultat$doc_topics
    topic_cols <- setdiff(names(doc_topics), "doc_id")
    req(length(topic_cols) > 0)

    mat <- as.matrix(doc_topics[, topic_cols, drop = FALSE])
    dominant_idx <- max.col(mat, ties.method = "first")
    seg <- data.frame(
      doc_id = doc_topics$doc_id,
      topic_dominant = topic_cols[dominant_idx],
      prob_topic_dominant = round(mat[cbind(seq_len(nrow(mat)), dominant_idx)], 4),
      stringsAsFactors = FALSE
    )
    seg <- merge(seg, rv$lda_doc_texts, by = "doc_id", all.x = TRUE)
    seg <- seg[order(seg$prob_topic_dominant, decreasing = TRUE), , drop = FALSE]
    head(seg, 100)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")
}
