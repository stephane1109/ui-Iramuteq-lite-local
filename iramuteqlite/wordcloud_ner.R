# Rôle du fichier: tracer le nuage de mots NER.

tracer_wordcloud_ner <- function(df_ner_trouves) {
  if (!requireNamespace("wordcloud", quietly = TRUE) || !requireNamespace("RColorBrewer", quietly = TRUE)) {
    plot.new()
    title("Nuage NER indisponible")
    text(0.5, 0.5, "Packages manquants: wordcloud / RColorBrewer")
    return(invisible(NULL))
  }

  if (is.null(df_ner_trouves) || !is.data.frame(df_ner_trouves) || !nrow(df_ner_trouves)) {
    plot.new()
    title("Nuage NER indisponible")
    text(0.5, 0.5, "Aucune entité NER détectée.")
    return(invisible(NULL))
  }

  mots <- as.character(df_ner_trouves$text)
  freqs <- suppressWarnings(as.numeric(df_ner_trouves$freq))
  ok <- nzchar(trimws(mots)) & is.finite(freqs) & !is.na(freqs) & freqs > 0
  mots <- mots[ok]
  freqs <- freqs[ok]

  if (!length(mots)) {
    plot.new()
    title("Nuage NER indisponible")
    text(0.5, 0.5, "Aucune entité NER détectée.")
    return(invisible(NULL))
  }

  suppressWarnings(wordcloud::wordcloud(
    words = mots,
    freq = freqs,
    min.freq = 1,
    max.words = min(length(mots), 100L),
    random.order = FALSE,
    rot.per = 0.1,
    scale = c(5, 0.8),
    colors = RColorBrewer::brewer.pal(8, "Dark2")
  ))

  invisible(NULL)
}
