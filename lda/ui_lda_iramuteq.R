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
    tags$small(
      style = "display:block; color:#666; margin-top:-8px; margin-bottom:10px;",
      "k = nombre de thèmes latents recherchés dans le corpus (plus k est grand, plus les thèmes sont fins)."
    ),
    numericInput("lda_n_terms", "Mots affichés par thème", value = valeur_defaut("lda_n_terms", 8), min = 3, step = 1),
    tags$small(
      style = "display:block; color:#666; margin-top:-8px; margin-bottom:10px;",
      "n_terms = nombre de mots les plus représentatifs affichés pour chaque thème."
    ),
    checkboxInput("lda_retirer_stopwords", "Retirer les stopwords", value = valeur_defaut("lda_retirer_stopwords", FALSE)),
    tags$small(
      style = "display:block; color:#666; margin-top:-8px; margin-bottom:10px;",
      "Si activé, les mots très fréquents peu informatifs (le, de, et, ...) sont retirés."
    ),
    checkboxInput("lda_filtrage_morpho", "Filtrer par catégories morphosyntaxiques", value = valeur_defaut("lda_filtrage_morpho", FALSE)),
    tags$small(
      style = "display:block; color:#666; margin-top:-8px; margin-bottom:10px;",
      "Si activé, seuls les mots appartenant aux catégories morphosyntaxiques sélectionnées sont conservés."
    ),
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
