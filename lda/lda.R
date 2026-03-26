# Outils LDA (Latent Dirichlet Allocation)
# ------------------------------------------------------------
# Ce module fournit un "test LDA" simple et réutilisable sur un corpus texte.
# Il peut être appelé depuis des scripts batch, puis branché dans l'UI Shiny.

preparer_dtm_lda <- function(textes,
                             langue = "fr",
                             min_termfreq = 1,
                             remove_numbers = TRUE,
                             remove_punct = TRUE,
                             remove_symbols = TRUE,
                             retirer_stopwords = FALSE,
                             stopwords_sup = character(),
                             filtrage_morpho = FALSE,
                             pos_lexique_a_conserver = c("NOM", "VER", "ADJ"),
                             lexique_path = file.path(getwd(), "dictionnaires", "lexique_fr.csv")) {
  if (!requireNamespace("quanteda", quietly = TRUE)) {
    stop("Le package 'quanteda' est requis pour préparer la matrice terme-document.")
  }

  if (is.null(textes) || !length(textes)) {
    stop("Aucun texte fourni pour le test LDA.")
  }

  textes <- as.character(textes)
  if (is.null(names(textes)) || length(names(textes)) != length(textes) || any(!nzchar(names(textes)))) {
    names(textes) <- paste0("doc_", seq_along(textes))
  }

  textes <- textes[!is.na(textes)]
  textes <- trimws(textes)
  textes <- textes[nzchar(textes)]

  if (!length(textes)) {
    stop("Les textes fournis sont vides après nettoyage.")
  }

  corpus <- quanteda::corpus(textes)
  toks <- quanteda::tokens(
    corpus,
    remove_numbers = remove_numbers,
    remove_punct = remove_punct,
    remove_symbols = remove_symbols
  )

  stopwords_final <- unique(as.character(stopwords_sup))
  stopwords_final <- stopwords_final[nzchar(trimws(stopwords_final))]

  if (isTRUE(retirer_stopwords)) {
    stopwords_final <- unique(c(quanteda::stopwords(langue), stopwords_final))
  }

  if (length(stopwords_final)) {
    toks <- quanteda::tokens_remove(toks, pattern = stopwords_final)
  }

  dtm <- quanteda::dfm(toks)
  dtm <- quanteda::dfm_trim(dtm, min_termfreq = min_termfreq)

  if (isTRUE(filtrage_morpho)) {
    if (!file.exists(lexique_path)) {
      stop("Filtrage morphosyntaxique demandé, mais lexique_fr.csv est introuvable.")
    }

    lexique_df <- tryCatch(
      utils::read.csv2(lexique_path, stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (is.null(lexique_df) || !all(c("c_mot", "c_lemme", "c_morpho") %in% names(lexique_df))) {
      stop("Le lexique_fr.csv ne contient pas les colonnes attendues (c_mot, c_lemme, c_morpho).")
    }

    pos_keep <- toupper(trimws(as.character(pos_lexique_a_conserver)))
    pos_keep <- pos_keep[nzchar(pos_keep)]
    if (!length(pos_keep)) {
      stop("Filtrage morphosyntaxique activé mais aucune catégorie POS sélectionnée.")
    }

    lexique_df$c_morpho <- toupper(trimws(as.character(lexique_df$c_morpho)))
    lexique_df$c_mot <- tolower(trimws(as.character(lexique_df$c_mot)))
    lexique_df$c_lemme <- tolower(trimws(as.character(lexique_df$c_lemme)))
    lexique_filtre <- lexique_df[lexique_df$c_morpho %in% pos_keep, , drop = FALSE]

    vocab_ok <- unique(c(lexique_filtre$c_mot, lexique_filtre$c_lemme))
    vocab_ok <- vocab_ok[nzchar(vocab_ok)]
    if (!length(vocab_ok)) {
      stop("Aucun terme conservé après filtrage morphosyntaxique.")
    }

    dtm <- quanteda::dfm_select(dtm, pattern = vocab_ok, selection = "keep")
  }

  if (quanteda::nfeat(dtm) == 0) {
    stop("La matrice DTM est vide après filtrage (min_termfreq trop élevé ?).")
  }

  quanteda::convert(dtm, to = "topicmodels")
}

lancer_test_lda <- function(textes,
                            k = 4,
                            seed = 1234,
                            iter = 1000,
                            burnin = 250,
                            thin = 100,
                            alpha = 50 / k,
                            eta = 0.1,
                            n_terms = 10,
                            ...) {
  if (!requireNamespace("topicmodels", quietly = TRUE)) {
    stop("Le package 'topicmodels' est requis pour exécuter le test LDA.")
  }

  dtm <- preparer_dtm_lda(textes = textes, ...)

  controle <- list(
    seed = seed,
    iter = iter,
    burnin = burnin,
    thin = thin,
    alpha = alpha,
    delta = eta,
    verbose = 0
  )

  modele <- topicmodels::LDA(
    x = dtm,
    k = as.integer(k),
    method = "Gibbs",
    control = controle
  )

  posterior_lda <- topicmodels::posterior(modele)
  topic_term_mat <- posterior_lda$terms

  top_par_topic <- lapply(seq_len(nrow(topic_term_mat)), function(i) {
    probs <- topic_term_mat[i, ]
    ord <- order(probs, decreasing = TRUE)
    idx <- ord[seq_len(min(n_terms, length(ord)))]
    data.frame(
      topic = paste0("Topic_", i),
      rank = seq_along(idx),
      term = colnames(topic_term_mat)[idx],
      prob = as.numeric(probs[idx]),
      stringsAsFactors = FALSE
    )
  })
  termes_df <- do.call(rbind, top_par_topic)

  doc_topics <- as.data.frame(posterior_lda$topics)
  doc_topics$doc_id <- rownames(doc_topics)

  list(
    modele = modele,
    top_terms = termes_df,
    doc_topics = doc_topics,
    dtm = dtm,
    topic_term_matrix = topic_term_mat
  )
}
