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
    numericInput("lda_n_terms", "Mots affichés par thème", value = valeur_defaut("lda_n_terms", 8), min = 3, step = 1),
    checkboxInput("lda_retirer_stopwords", "Retirer les stopwords", value = valeur_defaut("lda_retirer_stopwords", FALSE)),
    checkboxInput("lda_filtrage_morpho", "Filtrer par catégories morphosyntaxiques", value = valeur_defaut("lda_filtrage_morpho", FALSE)),
    conditionalPanel(
      condition = "input.lda_filtrage_morpho == true",
      selectizeInput(
        "lda_pos_keep",
        "Catégories à conserver",
        choices = c("NOM", "VER", "ADJ", "ADV", "AUX", "PRE", "PRO", "ART", "CON"),
        selected = valeur_defaut("lda_pos_keep", c("NOM", "VER", "ADJ")),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    )
  )
}
