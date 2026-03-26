ui_form_parametres_lda <- function(defaults = NULL) {
  valeur_defaut <- function(id, fallback) {
    if (is.null(defaults) || is.null(defaults[[id]]) || (length(defaults[[id]]) == 1 && is.na(defaults[[id]]))) {
      return(fallback)
    }
    defaults[[id]]
  }

  tagList(
    tags$p(
      "Version très simple: choisissez seulement le nombre de thèmes.",
      style = "color:#444; margin-bottom:10px;"
    ),
    numericInput("lda_k", "Nombre de thèmes (topics)", value = valeur_defaut("lda_k", 4), min = 2, step = 1),
    tags$p(
      "k = nombre de thèmes latents recherchés dans le corpus (plus k est grand, plus les thèmes sont fins).",
      style = "color:#c00; font-size:0.9em; margin-top:-8px; margin-bottom:10px;"
    ),
    numericInput("lda_n_terms", "Mots affichés par thème", value = valeur_defaut("lda_n_terms", 8), min = 3, step = 1),
    numericInput("lda_segment_size", "Taille des segments LDA (en mots)", value = valeur_defaut("lda_segment_size", 40), min = 5, step = 1),
    tags$p(
      "n_terms = nombre de mots les plus représentatifs affichés pour chaque thème.",
      style = "color:#c00; font-size:0.9em; margin-top:-8px; margin-bottom:10px;"
    ),
    checkboxInput("lda_retirer_stopwords", "Retirer les stopwords", value = valeur_defaut("lda_retirer_stopwords", FALSE)),
    tags$p(
      "Stopwords = mots-outils fréquents (quanteda::stopwords(langue) + liste additionnelle) retirés avant le modèle.",
      style = "color:#c00; font-size:0.9em; margin-top:-8px; margin-bottom:10px;"
    ),
    checkboxInput("lda_filtrage_morpho", "Filtrer par catégories morphosyntaxiques", value = valeur_defaut("lda_filtrage_morpho", FALSE)),
    tags$p(
      "Filtrage morpho = conservation des termes dont c_morpho du lexique_fr.csv appartient aux catégories sélectionnées.",
      style = "color:#c00; font-size:0.9em; margin-top:-8px; margin-bottom:10px;"
    ),
    selectizeInput(
      "lda_pos_keep",
      "Catégories à conserver (utilisées si le filtrage morphosyntaxique est activé)",
      choices = c("NOM", "VER", "ADJ", "ADV", "AUX", "PRE", "PRO", "ART", "CON"),
      selected = valeur_defaut("lda_pos_keep", c("NOM", "VER", "ADJ")),
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    tags$p(
      "Les catégories sont lues dans dictionnaires/lexique_fr.csv (colonnes c_mot/c_lemme/c_morpho).",
      style = "color:#c00; font-size:0.9em; margin-top:-8px; margin-bottom:10px;"
    )
  )
}

ui_controles_dynamiques_lda <- function(defaults = NULL) {
  valeur_defaut <- function(id, fallback) {
    if (is.null(defaults) || is.null(defaults[[id]]) || (length(defaults[[id]]) == 1 && is.na(defaults[[id]]))) {
      return(fallback)
    }
    defaults[[id]]
  }

  tags$div(
    style = "display:flex; gap:12px; flex-wrap:wrap; align-items:flex-end; margin-bottom:10px;",
    tags$div(
      style = "min-width:220px;",
      numericInput("lda_k_dyn", "Nombre de topics (k)", value = valeur_defaut("lda_k_dyn", 4), min = 2, step = 1)
    ),
    tags$div(
      style = "min-width:220px;",
      numericInput("lda_n_terms_dyn", "Mots affichés / topic", value = valeur_defaut("lda_n_terms_dyn", 8), min = 3, step = 1)
    ),
    tags$div(
      style = "min-width:220px;",
      numericInput("lda_segment_size_dyn", "Taille segment LDA", value = valeur_defaut("lda_segment_size_dyn", 40), min = 5, step = 1)
    ),
    actionButton("lancer_lda_dyn", "Appliquer et relancer LDA", class = "btn-primary")
  )
}
