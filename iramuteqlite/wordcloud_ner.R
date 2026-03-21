# Rôle du fichier: extraire des occurrences NER à partir d'un corpus texte
# et générer un nuage de mots (fréquence par entité).

# Essaie d'utiliser spaCy (script Python) pour une détection NER fiable.
spacy_ner_disponible <- function(script_path = file.path("spacy", "ner_spacy.py")) {
  python_bin <- Sys.which("python3")
  if (!nzchar(python_bin)) python_bin <- Sys.which("python")
  nzchar(python_bin) && file.exists(script_path)
}

detecter_ner_spacy <- function(texte, model = "fr_core_news_lg", script_path = file.path("spacy", "ner_spacy.py")) {
  texte <- if (is.null(texte)) "" else as.character(texte)
  if (!nzchar(trimws(texte))) {
    return(data.frame(text = character(0), label = character(0), freq = integer(0), stringsAsFactors = FALSE))
  }

  python_bin <- Sys.which("python3")
  if (!nzchar(python_bin)) python_bin <- Sys.which("python")
  if (!spacy_ner_disponible(script_path = script_path)) {
    return(NULL)
  }

  in_txt <- tempfile(pattern = "ner_spacy_in_", fileext = ".txt")
  out_csv <- tempfile(pattern = "ner_spacy_out_", fileext = ".csv")
  on.exit(unlink(c(in_txt, out_csv), force = TRUE), add = TRUE)

  writeLines(enc2utf8(texte), in_txt, useBytes = TRUE)

  cmd_out <- suppressWarnings(system2(
    python_bin,
    args = c(
      script_path,
      "--input-txt", in_txt,
      "--output-csv", out_csv,
      "--model", model
    ),
    stdout = TRUE,
    stderr = TRUE
  ))
  status <- attr(cmd_out, "status")
  if (is.null(status)) status <- 0L
  if (!identical(as.integer(status), 0L) || !file.exists(out_csv)) {
    return(NULL)
  }

  parsed <- tryCatch(
    utils::read.csv(out_csv, stringsAsFactors = FALSE, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  if (!is.data.frame(parsed) || !all(c("text", "label", "freq") %in% names(parsed))) {
    return(NULL)
  }

  parsed$text <- trimws(as.character(parsed$text))
  parsed$label <- toupper(trimws(as.character(parsed$label)))
  parsed$freq <- suppressWarnings(as.integer(parsed$freq))
  parsed <- parsed[nzchar(parsed$text) & nzchar(parsed$label) & is.finite(parsed$freq) & !is.na(parsed$freq) & parsed$freq > 0L, , drop = FALSE]
  if (!nrow(parsed)) {
    return(data.frame(text = character(0), label = character(0), freq = integer(0), stringsAsFactors = FALSE))
  }

  parsed <- parsed[order(-parsed$freq, parsed$text), c("text", "label", "freq"), drop = FALSE]
  rownames(parsed) <- NULL
  parsed
}

# Détection automatique d'entités: NER via spaCy uniquement.
detecter_ner_automatique <- function(texte, label_defaut = "MISC", min_chars = 2L, min_freq = 2L, max_entities = 200L) {
  texte <- if (is.null(texte)) "" else as.character(texte)
  if (!nzchar(trimws(texte))) {
    return(data.frame(text = character(0), label = character(0), freq = integer(0), stringsAsFactors = FALSE))
  }

  # NER = spaCy uniquement. Sans spaCy disponible, on retourne vide.
  spacy_df <- detecter_ner_spacy(texte)
  if (is.data.frame(spacy_df)) {
    return(spacy_df)
  }
  data.frame(text = character(0), label = character(0), freq = integer(0), stringsAsFactors = FALSE)
}

fusionner_annotations_ner <- function(df_existant, df_auto) {
  if (is.null(df_existant) || !is.data.frame(df_existant)) {
    df_existant <- data.frame(text = character(0), label = character(0), stringsAsFactors = FALSE)
  }
  if (!all(c("text", "label") %in% names(df_existant))) {
    df_existant <- data.frame(text = character(0), label = character(0), stringsAsFactors = FALSE)
  }

  if (is.null(df_auto) || !is.data.frame(df_auto) || !nrow(df_auto) || !"text" %in% names(df_auto)) {
    return(df_existant)
  }

  out <- df_existant[, c("text", "label"), drop = FALSE]
  for (i in seq_len(nrow(df_auto))) {
    txt <- trimws(as.character(df_auto$text[[i]]))
    if (!nzchar(txt)) next
    key <- tolower(txt)
    idx <- match(key, tolower(out$text))
    if (is.na(idx)) {
      out <- rbind(out, data.frame(text = txt, label = "MISC", stringsAsFactors = FALSE))
    }
  }

  out <- out[order(tolower(out$text)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

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
