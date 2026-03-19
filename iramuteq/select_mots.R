# Rôle du fichier: select_mots.R gère la préparation/peuplement de la sélection des termes pour la similitude.

get_simi_terms_choices <- function(dfm_obj) {
  if (is.null(dfm_obj) || quanteda::ndoc(dfm_obj) < 1 || quanteda::nfeat(dfm_obj) < 1) {
    return(list(choices = stats::setNames(character(0), character(0)), ordered_terms = character(0)))
  }

  mat_dfm <- as.matrix(dfm_obj)
  mat_bin <- ifelse(mat_dfm > 0, 1, 0)
  freq <- colSums(mat_bin)
  if (!length(freq)) {
    return(list(choices = stats::setNames(character(0), character(0)), ordered_terms = character(0)))
  }

  ord <- order(freq, decreasing = TRUE)
  terms <- names(freq)[ord]
  labels <- paste0(terms, " (", as.integer(freq[ord]), ")")
  list(choices = stats::setNames(terms, labels), ordered_terms = terms)
}

peupler_termes_similitudes <- function(input,
                                       session,
                                       dfm_obj,
                                       preselect_top = TRUE,
                                       current_selection = NULL) {
  termes <- get_simi_terms_choices(dfm_obj)
  n_top <- suppressWarnings(as.integer(input$simi_top_terms))
  if (length(n_top) != 1L || !is.finite(n_top) || is.na(n_top) || n_top < 1) n_top <- 40L

  if (is.null(current_selection)) current_selection <- character(0)
  current_selection <- as.character(current_selection)
  selected <- if (length(current_selection) > 0) {
    current_selection
  } else if (isTRUE(preselect_top)) {
    head(termes$ordered_terms, n_top)
  } else {
    character(0)
  }
  selected <- intersect(as.character(selected), termes$ordered_terms)

  updateSelectizeInput(
    session = session,
    inputId = "simi_terms_selected",
    choices = termes$choices,
    selected = selected,
    server = TRUE
  )
}
