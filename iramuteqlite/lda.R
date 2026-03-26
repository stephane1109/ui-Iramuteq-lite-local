# Outils LDA (Latent Dirichlet Allocation)
# ------------------------------------------------------------
# Ce module fournit un "test LDA" simple et réutilisable sur un corpus texte.
# Il peut être appelé depuis des scripts batch, puis branché dans l'UI Shiny.

preparer_dtm_lda <- function(textes,
                             langue = "fr",
                             min_termfreq = 5,
                             remove_numbers = TRUE,
                             remove_punct = TRUE,
                             remove_symbols = TRUE,
                             stopwords_sup = character()) {
  if (!requireNamespace("quanteda", quietly = TRUE)) {
    stop("Le package 'quanteda' est requis pour préparer la matrice terme-document.")
  }

  if (is.null(textes) || !length(textes)) {
    stop("Aucun texte fourni pour le test LDA.")
  }

  textes <- as.character(textes)
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

  stopwords_final <- unique(c(quanteda::stopwords(langue), stopwords_sup))
  toks <- quanteda::tokens_remove(toks, pattern = stopwords_final)

  dtm <- quanteda::dfm(toks)
  dtm <- quanteda::dfm_trim(dtm, min_termfreq = min_termfreq)

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

  termes <- topicmodels::terms(modele, n_terms)
  termes_df <- data.frame(
    topic = rep(colnames(termes), each = nrow(termes)),
    rank = rep(seq_len(nrow(termes)), times = ncol(termes)),
    term = as.vector(termes),
    stringsAsFactors = FALSE
  )

  doc_topics <- as.data.frame(topicmodels::posterior(modele)$topics)
  doc_topics$doc_id <- rownames(doc_topics)

  list(
    modele = modele,
    top_terms = termes_df,
    doc_topics = doc_topics,
    dtm = dtm
  )
}
