register_lda_module <- function(input, output, session, rv) {
  executer_test_lda <- function(k, n_terms) {
    lire_textes_corpus_lda <- function(segment_size) {
      datapath <- tryCatch(input$fichier_corpus$datapath, error = function(e) NULL)
      if (is.null(datapath) || !nzchar(datapath) || !file.exists(datapath)) {
        return(character(0))
      }

      if (!exists("preparer_textes_lda_depuis_fichier", mode = "function", inherits = TRUE)) {
        stop("Fonction de préparation LDA introuvable (preparer_textes_lda_depuis_fichier).")
      }

      preparer_textes_lda_depuis_fichier(
        chemin_fichier = datapath,
        segment_size = segment_size
      )
    }

    segment_size_lda <- as.integer(input$lda_segment_size_dyn %||% input$lda_segment_size %||% 40)
    textes_lda <- lire_textes_corpus_lda(segment_size = segment_size_lda)
    if (is.null(textes_lda) || !length(textes_lda)) {
      rv$lda_erreur <- "Aucun texte disponible pour LDA. Importez d'abord un fichier corpus."
      rv$lda_statut <- "Test LDA impossible."
      showNotification(rv$lda_erreur, type = "warning")
      return(invisible(NULL))
    }

    if (is.null(names(textes_lda)) || length(names(textes_lda)) != length(textes_lda) || any(!nzchar(names(textes_lda)))) {
      names(textes_lda) <- paste0("doc_", seq_along(textes_lda))
    }

    res_lda <- tryCatch(
      {
        args_lda <- list(
          textes = as.character(textes_lda),
          k = as.integer(k %||% 4),
          n_terms = as.integer(n_terms %||% 8),
          min_termfreq = 1L,
          remove_numbers = FALSE,
          remove_punct = TRUE,
          remove_symbols = TRUE,
          retirer_stopwords = isTRUE(input$lda_retirer_stopwords),
          stopwords_sup = character(0),
          filtrage_morpho = isTRUE(input$lda_filtrage_morpho),
          pos_lexique_a_conserver = as.character(input$lda_pos_keep %||% c("NOM", "VER", "ADJ"))
        )

        do.call(lancer_test_lda, args_lda)
      },
      error = function(e) e
    )

    if (inherits(res_lda, "error")) {
      rv$lda_resultat <- NULL
      rv$lda_erreur <- conditionMessage(res_lda)
      rv$lda_statut <- "Échec du test LDA."
      showNotification(paste("Erreur LDA:", rv$lda_erreur), type = "error", duration = 8)
      return(invisible(NULL))
    }

    rv$lda_resultat <- res_lda
    rv$lda_erreur <- NULL
    rv$lda_doc_texts <- data.frame(
      doc_id = names(textes_lda),
      segment_texte = as.character(textes_lda),
      stringsAsFactors = FALSE
    )
    rv$lda_statut <- paste0(
      "LDA exécuté avec succès (k=", as.integer(k %||% 4),
      ", segments=", length(textes_lda),
      ", taille_segment=", as.integer(segment_size_lda),
      ", termes=", ncol(res_lda$dtm), ")."
    )
    updateNumericInput(session, "lda_k_dyn", value = as.integer(k %||% 4))
    updateNumericInput(session, "lda_n_terms_dyn", value = as.integer(n_terms %||% 8))
    updateNumericInput(session, "lda_segment_size_dyn", value = as.integer(segment_size_lda %||% 40))
    showNotification("Test LDA terminé.", type = "message")
  }

  capturer_parametres_lda <- function() {
    list(
      lda_k = isolate(input$lda_k_dyn %||% input$lda_k %||% 4),
      lda_n_terms = isolate(input$lda_n_terms_dyn %||% input$lda_n_terms %||% 8),
      lda_retirer_stopwords = isolate(input$lda_retirer_stopwords %||% FALSE),
      lda_filtrage_morpho = isolate(input$lda_filtrage_morpho %||% FALSE),
      lda_pos_keep = isolate(input$lda_pos_keep %||% c("NOM", "VER", "ADJ")),
      lda_segment_size = isolate(input$lda_segment_size_dyn %||% input$lda_segment_size %||% 40)
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
    executer_test_lda(
      k = as.integer(input$lda_k %||% input$lda_k_dyn %||% 4),
      n_terms = as.integer(input$lda_n_terms %||% input$lda_n_terms_dyn %||% 8)
    )
  })

  observeEvent(input$lancer_lda_dyn, {
    executer_test_lda(
      k = as.integer(input$lda_k_dyn %||% input$lda_k %||% 4),
      n_terms = as.integer(input$lda_n_terms_dyn %||% input$lda_n_terms %||% 8)
    )
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

  output$table_lda_doc_topics <- renderTable({
    req(rv$lda_resultat, rv$lda_resultat$doc_topics, rv$lda_doc_texts)
    doc_topics <- rv$lda_resultat$doc_topics
    segments_ref <- rv$lda_doc_texts
    seg_lookup <- setNames(as.character(segments_ref$segment_texte), as.character(segments_ref$doc_id))
    seg <- unname(seg_lookup[as.character(doc_topics$doc_id)])
    if (anyNA(seg)) {
      idx_text <- suppressWarnings(as.integer(gsub("^text", "", as.character(doc_topics$doc_id))))
      idx_ok <- !is.na(idx_text) & idx_text >= 1 & idx_text <= nrow(segments_ref)
      seg[idx_ok & is.na(seg)] <- as.character(segments_ref$segment_texte[idx_text[idx_ok & is.na(seg)]])
    }
    doc_topics$doc_id <- ifelse(!is.na(seg) & nzchar(seg), seg, as.character(doc_topics$doc_id))
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

  output$plot_lda_doc_topics_heatmap <- plotly::renderPlotly({
    req(rv$lda_resultat, rv$lda_resultat$doc_topics, rv$lda_doc_texts)
    doc_topics <- rv$lda_resultat$doc_topics
    topic_cols <- grep("^\\d+$|^Topic_", names(doc_topics), value = TRUE)
    if (!length(topic_cols)) topic_cols <- setdiff(names(doc_topics), "doc_id")
    req(length(topic_cols) > 0)

    segments_ref <- rv$lda_doc_texts
    seg_lookup <- setNames(as.character(segments_ref$segment_texte), as.character(segments_ref$doc_id))
    y_labels <- unname(seg_lookup[as.character(doc_topics$doc_id)])
    if (anyNA(y_labels)) {
      idx_text <- suppressWarnings(as.integer(gsub("^text", "", as.character(doc_topics$doc_id))))
      idx_ok <- !is.na(idx_text) & idx_text >= 1 & idx_text <= nrow(segments_ref)
      y_labels[idx_ok & is.na(y_labels)] <- as.character(segments_ref$segment_texte[idx_text[idx_ok & is.na(y_labels)]])
    }
    y_labels[is.na(y_labels) | !nzchar(y_labels)] <- as.character(doc_topics$doc_id[is.na(y_labels) | !nzchar(y_labels)])
    y_labels <- substr(y_labels, 1, 90)

    z <- as.matrix(doc_topics[, topic_cols, drop = FALSE])
    z <- round(z, 4)

    plotly::plot_ly(
      x = topic_cols,
      y = y_labels,
      z = z,
      type = "heatmap",
      colorscale = "Blues",
      hovertemplate = paste0(
        "Document: %{y}<br>",
        "Topic: %{x}<br>",
        "Probabilité: %{z}<extra></extra>"
      )
    ) %>%
      plotly::layout(
        title = "Distribution topics / documents (interactive)",
        xaxis = list(title = "Topics"),
        yaxis = list(title = "Segments de texte", automargin = TRUE)
      ) %>%
      plotly::config(displaylogo = FALSE)
  })

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
