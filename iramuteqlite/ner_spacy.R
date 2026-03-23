# Rôle du fichier: logique NER (détection spaCy, diagnostics, fusion/extraction).

# Rôle du fichier: extraire des occurrences NER à partir d'un corpus texte
# et générer un nuage de mots (fréquence par entité).

# Essaie d'utiliser spaCy (script Python) pour une détection NER fiable.
spacy_ner_disponible <- function(script_path = file.path("spacy", "ner_spacy.py")) {
  python_bin <- Sys.which("python3")
  if (!nzchar(python_bin)) python_bin <- Sys.which("python")
  nzchar(python_bin) && file.exists(script_path)
}

modele_spacy_fr_disponible <- function(models = c("fr_core_news_sm", "fr_core_news_md", "fr_core_news_lg")) {
  for (m in models) {
    if (isTRUE(spacy_modele_disponible(model = m))) return(m)
  }
  NULL
}

diagnostiquer_dependances_ner <- function(model = "fr_core_news_sm") {
  python_bin <- Sys.which("python3")
  if (!nzchar(python_bin)) python_bin <- Sys.which("python")
  has_python <- nzchar(python_bin)

  script_ner <- file.path("spacy", "ner_spacy.py")
  has_script_ner <- file.exists(script_ner)
  modele_actif <- if (has_python && has_script_ner) modele_spacy_fr_disponible(models = c(model, "fr_core_news_sm", "fr_core_news_md", "fr_core_news_lg")) else NULL
  has_model <- !is.null(modele_actif)
  has_wordcloud <- requireNamespace("wordcloud", quietly = TRUE)
  has_brewer <- requireNamespace("RColorBrewer", quietly = TRUE)
  has_reticulate <- requireNamespace("reticulate", quietly = TRUE)
  has_spacyr <- requireNamespace("spacyr", quietly = TRUE)

  list(
    python = has_python,
    script_ner = has_script_ner,
    reticulate = has_reticulate,
    spacyr = has_spacyr,
    spacy_model = has_model,
    spacy_model_name = if (is.null(modele_actif)) "" else as.character(modele_actif),
    wordcloud = has_wordcloud,
    rcolorbrewer = has_brewer,
    ok_ner = has_python && has_script_ner && has_model
  )
}

spacy_modele_disponible <- function(model = "fr_core_news_sm") {
  python_bin <- Sys.which("python3")
  if (!nzchar(python_bin)) python_bin <- Sys.which("python")
  if (!nzchar(python_bin)) return(FALSE)

  script_path <- file.path("spacy", "ner_spacy.py")
  if (!file.exists(script_path)) return(FALSE)

  in_txt <- tempfile(pattern = "spacy_probe_in_", fileext = ".txt")
  out_csv <- tempfile(pattern = "spacy_probe_out_", fileext = ".csv")
  on.exit(unlink(c(in_txt, out_csv), force = TRUE), add = TRUE)
  writeLines("", in_txt, useBytes = TRUE)

  out <- suppressWarnings(system2(
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
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  identical(as.integer(status), 0L) && file.exists(out_csv)
}

installer_package_spacyr_si_necessaire <- function() {
  if (requireNamespace("spacyr", quietly = TRUE)) return(TRUE)

  cran_repo <- trimws(Sys.getenv("R_CRAN_MIRROR", unset = "https://cloud.r-project.org"))
  log_parts <- character(0)

  out_cran <- tryCatch({
    out <- capture.output(utils::install.packages("spacyr", repos = cran_repo), type = "output")
    paste(out, collapse = "\n")
  }, error = function(e) paste("install.packages('spacyr') error:", conditionMessage(e)))
  log_parts <- c(log_parts, out_cran)
  if (requireNamespace("spacyr", quietly = TRUE)) {
    options(iramuteq_spacy_install_last_log = paste(log_parts, collapse = "\n\n"))
    return(TRUE)
  }

  out_remotes <- tryCatch({
    if (!requireNamespace("remotes", quietly = TRUE)) {
      utils::install.packages("remotes", repos = cran_repo)
    }
    out <- capture.output(remotes::install_github("quanteda/spacyr"), type = "output")
    paste(out, collapse = "\n")
  }, error = function(e) paste("remotes::install_github('quanteda/spacyr') error:", conditionMessage(e)))
  log_parts <- c(log_parts, out_remotes)
  options(iramuteq_spacy_install_last_log = paste(log_parts, collapse = "\n\n"))
  requireNamespace("spacyr", quietly = TRUE)
}

installer_spacy_si_necessaire <- function(model = "fr_core_news_sm") {
  if (spacy_modele_disponible(model = model)) return(TRUE)
  if (isTRUE(getOption("iramuteq_spacy_install_attempted", FALSE))) return(FALSE)
  options(iramuteq_spacy_install_attempted = TRUE)
  options(iramuteq_spacy_install_last_log = NULL)

  # Priorité: installation via spacyr (env Python isolé via reticulate).
  installer_package_spacyr_si_necessaire()
  if (requireNamespace("spacyr", quietly = TRUE)) {
    install_args <- list()
    fml <- tryCatch(names(formals(spacyr::spacy_install)), error = function(e) character(0))
    if ("lang_models" %in% fml) install_args$lang_models <- model
    if ("prompt" %in% fml) install_args$prompt <- FALSE
    if ("python_executable" %in% fml) install_args$python_executable <- Sys.which("python3")
    log_spacyr <- tryCatch({
      out <- capture.output(do.call(spacyr::spacy_install, install_args), type = "output")
      paste(out, collapse = "\n")
    }, error = function(e) paste("spacyr::spacy_install error:", conditionMessage(e)))
    log_initialize <- tryCatch({
      out <- capture.output(spacyr::spacy_initialize(model = model), type = "output")
      paste(out, collapse = "\n")
    }, error = function(e) paste("spacyr::spacy_initialize error:", conditionMessage(e)))
    options(iramuteq_spacy_install_last_log = paste(log_spacyr, log_initialize, sep = "\n\n"))
    if (isTRUE(spacy_modele_disponible(model = model))) return(TRUE)
  }
  FALSE
}


detecter_ner_spacy <- function(texte, model = "fr_core_news_sm", script_path = file.path("spacy", "ner_spacy.py")) {
  texte <- if (is.null(texte)) "" else as.character(texte)
  if (!nzchar(trimws(texte))) {
    return(data.frame(text = character(0), label = character(0), freq = integer(0), stringsAsFactors = FALSE))
  }

  python_bin <- Sys.which("python3")
  if (!nzchar(python_bin)) python_bin <- Sys.which("python")
  if (!spacy_ner_disponible(script_path = script_path)) {
    return(NULL)
  }
  if (!spacy_modele_disponible(model = model)) {
    modele_alt <- modele_spacy_fr_disponible()
    if (!is.null(modele_alt)) {
      model <- modele_alt
    } else {
      return(NULL)
    }
  }
  if (!spacy_modele_disponible(model = model)) {
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
