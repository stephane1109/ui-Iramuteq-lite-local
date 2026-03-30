# Outils d'affichage des nuages de mots LDA
# ------------------------------------------------------------

lda_topic_names <- function(mat_topics) {
  topic_names <- rownames(mat_topics)
  if (is.null(topic_names) || length(topic_names) != nrow(mat_topics) || any(!nzchar(topic_names))) {
    topic_names <- paste0("Topic_", seq_len(nrow(mat_topics)))
  }
  topic_names
}

ui_lda_wordclouds_blocks <- function(mat_topics) {
  topic_names <- lda_topic_names(mat_topics)

  shiny::tagList(
    lapply(seq_len(nrow(mat_topics)), function(i) {
      output_id <- paste0("plot_lda_wordcloud_topic_", i)
      shiny::tags$div(
        style = "margin-bottom: 16px; border: 1px solid #eee; border-radius: 8px; padding: 10px;",
        shiny::tags$h5(topic_names[i], style = "margin-top: 0;"),
        shiny::plotOutput(output_id, height = "260px")
      )
    })
  )
}

render_lda_wordcloud_topic <- function(mat_local, idx) {
  if (!requireNamespace("wordcloud", quietly = TRUE)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Package 'wordcloud' manquant.")
    return(invisible(NULL))
  }

  probs <- as.numeric(mat_local[idx, ])
  terms <- colnames(mat_local)
  ord <- order(probs, decreasing = TRUE)
  top_n <- min(length(ord), 60L)
  keep <- ord[seq_len(top_n)]
  words <- as.character(terms[keep])
  freq <- as.numeric(probs[keep])

  ok <- !is.na(words) & nzchar(trimws(words)) & is.finite(freq) & !is.na(freq) & freq > 0
  words <- words[ok]
  freq <- freq[ok]
  shiny::validate(shiny::need(length(words) > 0, "Aucun mot exploitable pour ce topic."))

  couleurs <- if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    grDevices::colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(max(1L, min(8L, length(words))))
  } else {
    grDevices::rainbow(max(1L, min(8L, length(words))))
  }

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par), add = TRUE)
  graphics::par(mar = c(1, 1, 2, 1))
  set.seed(1234 + idx)
  suppressWarnings(wordcloud::wordcloud(
    words = words,
    freq = freq,
    scale = c(4.2, 0.8),
    min.freq = 0,
    max.words = length(words),
    random.order = FALSE,
    rot.per = 0.08,
    colors = couleurs
  ))
}

register_lda_wordcloud_outputs <- function(output, rv) {
  output$ui_lda_wordclouds <- shiny::renderUI({
    shiny::req(rv$lda_resultat)

    images_python <- rv$lda_resultat$wordcloud_images %||% character(0)
    images_python <- as.character(images_python)
    images_python <- images_python[file.exists(images_python)]

    if (length(images_python) > 0) {
      blocs <- lapply(seq_along(images_python), function(i) {
        chemin <- normalizePath(images_python[[i]], winslash = "/", mustWork = FALSE)
        uri <- paste0("file:///", chemin)
        shiny::tags$div(
          style = "margin-bottom: 16px; border: 1px solid #eee; border-radius: 8px; padding: 10px;",
          shiny::tags$h5(paste0("Topic_", i), style = "margin-top: 0;"),
          shiny::tags$img(src = uri, style = "max-width: 100%; height: auto;"),
          shiny::tags$br(),
          shiny::tags$a(href = uri, target = "_blank", "Ouvrir l'image")
        )
      })
      return(shiny::tagList(blocs))
    }

    # Fallback historique: rendu R si aucune image Python n'est disponible.
    shiny::req(rv$lda_resultat$topic_term_matrix)
    mat_topics <- rv$lda_resultat$topic_term_matrix
    shiny::req(nrow(mat_topics) > 0, ncol(mat_topics) > 0)
    ui_lda_wordclouds_blocks(mat_topics)
  })

  shiny::observeEvent(rv$lda_resultat, {
    shiny::req(rv$lda_resultat, rv$lda_resultat$topic_term_matrix)

    images_python <- rv$lda_resultat$wordcloud_images %||% character(0)
    images_python <- as.character(images_python)
    images_python <- images_python[file.exists(images_python)]
    if (length(images_python) > 0) {
      return(invisible(NULL))
    }

    mat_topics <- rv$lda_resultat$topic_term_matrix
    shiny::req(nrow(mat_topics) > 0, ncol(mat_topics) > 0)

    for (i in seq_len(nrow(mat_topics))) {
      local({
        idx <- i
        output_id <- paste0("plot_lda_wordcloud_topic_", idx)
        output[[output_id]] <- shiny::renderPlot({
          shiny::req(rv$lda_resultat, rv$lda_resultat$topic_term_matrix)
          mat_local <- rv$lda_resultat$topic_term_matrix
          shiny::validate(shiny::need(nrow(mat_local) >= idx, "Topic non disponible."))
          render_lda_wordcloud_topic(mat_local = mat_local, idx = idx)
        })
      })
    }
  }, ignoreInit = FALSE)
}
