# Préparation dédiée du corpus pour LDA (indépendante de CHD)
# - import du fichier corpus
# - découpage optionnel en segments de longueur cible

importer_textes_bruts_lda <- function(chemin_fichier) {
  if (is.null(chemin_fichier) || !nzchar(chemin_fichier) || !file.exists(chemin_fichier)) {
    return(character(0))
  }

  lignes <- tryCatch(readLines(chemin_fichier, encoding = "UTF-8", warn = FALSE), error = function(e) character(0))
  if (!length(lignes)) return(character(0))

  headers <- grepl("^\\*\\*\\*\\*", lignes)

  if (any(headers)) {
    idx <- which(headers)
    bornes <- c(idx, length(lignes) + 1L)
    textes <- character(0)
    ids <- character(0)

    for (i in seq_along(idx)) {
      debut <- idx[[i]] + 1L
      fin <- bornes[[i + 1L]] - 1L
      contenu <- if (debut <= fin) lignes[debut:fin] else character(0)
      contenu <- trimws(gsub("\\r", "", contenu, fixed = TRUE))
      contenu <- contenu[nzchar(contenu)]
      if (!length(contenu)) next

      textes <- c(textes, paste(contenu, collapse = " "))
      ids <- c(ids, paste0("doc_", i))
    }

    names(textes) <- ids
    return(textes)
  }

  lignes <- trimws(gsub("\\r", "", lignes, fixed = TRUE))
  lignes <- lignes[nzchar(lignes)]
  if (!length(lignes)) return(character(0))

  names(lignes) <- paste0("doc_", seq_along(lignes))
  lignes
}

segmenter_textes_lda <- function(textes, segment_size = 40L) {
  textes <- as.character(textes)
  if (!length(textes)) return(character(0))

  segment_size <- suppressWarnings(as.integer(segment_size))
  if (is.na(segment_size) || segment_size <= 0L) {
    if (is.null(names(textes)) || any(!nzchar(names(textes)))) names(textes) <- paste0("doc_", seq_along(textes))
    return(textes)
  }

  ids_base <- names(textes)
  if (is.null(ids_base) || length(ids_base) != length(textes) || any(!nzchar(ids_base))) {
    ids_base <- paste0("doc_", seq_along(textes))
  }

  out <- character(0)
  out_ids <- character(0)

  for (i in seq_along(textes)) {
    txt <- trimws(textes[[i]])
    if (!nzchar(txt)) next

    mots <- unlist(strsplit(txt, "\\s+", perl = TRUE), use.names = FALSE)
    mots <- mots[nzchar(mots)]
    if (!length(mots)) next

    starts <- seq.int(1L, length(mots), by = segment_size)
    for (j in seq_along(starts)) {
      s <- starts[[j]]
      e <- min(length(mots), s + segment_size - 1L)
      seg <- paste(mots[s:e], collapse = " ")
      if (!nzchar(seg)) next
      out <- c(out, seg)
      out_ids <- c(out_ids, paste0(ids_base[[i]], "_seg", j))
    }
  }

  names(out) <- out_ids
  out
}

preparer_textes_lda_depuis_fichier <- function(chemin_fichier, segment_size = 40L) {
  textes <- importer_textes_bruts_lda(chemin_fichier)
  if (!length(textes)) return(character(0))
  segmenter_textes_lda(textes, segment_size = segment_size)
}
