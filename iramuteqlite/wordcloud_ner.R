# Rôle du fichier: extraire des occurrences NER à partir d'un corpus texte
# et générer un nuage de mots (fréquence par entité).

extraire_ner_corpus <- function(texte, ner_df) {
  texte <- if (is.null(texte)) "" else as.character(texte)
  if (!nzchar(trimws(texte))) {
    return(data.frame(text = character(0), label = character(0), freq = integer(0), stringsAsFactors = FALSE))
  }

  if (is.null(ner_df) || !is.data.frame(ner_df) || !all(c("text", "label") %in% names(ner_df)) || nrow(ner_df) == 0) {
    return(data.frame(text = character(0), label = character(0), freq = integer(0), stringsAsFactors = FALSE))
  }

  entites <- trimws(as.character(ner_df$text))
  labels <- toupper(trimws(as.character(ner_df$label)))
  idx_valide <- nzchar(entites) & nzchar(labels)
  entites <- entites[idx_valide]
  labels <- labels[idx_valide]

  if (!length(entites)) {
    return(data.frame(text = character(0), label = character(0), freq = integer(0), stringsAsFactors = FALSE))
  }

  # Déduplication insensible à la casse pour éviter les doublons de règles.
  idx_uniques <- !duplicated(tolower(entites))
  entites <- entites[idx_uniques]
  labels <- labels[idx_uniques]

  freq <- vapply(seq_along(entites), function(i) {
    motif <- gsub("([][{}()+*^$|\\?.])", "\\\\\\1", entites[[i]], perl = TRUE)
    regex <- paste0("(?i)(?<![[:alnum:]_])", motif, "(?![[:alnum:]_])")
    m <- gregexpr(regex, texte, perl = TRUE)[[1]]
    if (length(m) == 1 && identical(m[[1]], -1L)) return(0L)
    as.integer(length(m))
  }, integer(1))

  out <- data.frame(
    text = entites,
    label = labels,
    freq = as.integer(freq),
    stringsAsFactors = FALSE
  )

  out <- out[out$freq > 0L, , drop = FALSE]
  if (!nrow(out)) return(out)

  out[order(-out$freq, out$text), , drop = FALSE]
}

tracer_wordcloud_ner <- function(df_ner_trouves) {
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
