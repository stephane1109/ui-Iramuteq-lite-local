ui_form_parametres_lda <- function(defaults = NULL) {
  valeur_defaut <- function(id, fallback) {
    if (is.null(defaults) || is.null(defaults[[id]]) || (length(defaults[[id]]) == 1 && is.na(defaults[[id]]))) {
      return(fallback)
    }
    defaults[[id]]
  }

  tagList(
    tags$p(
      "Version simplifiée: choisissez le nombre de thèmes et le niveau de nettoyage.",
      style = "color:#444; margin-bottom:10px;"
    ),
    numericInput("lda_k", "Nombre de thèmes (topics)", value = valeur_defaut("lda_k", 4), min = 2, step = 1),
    numericInput("lda_n_terms", "Mots affichés par thème", value = valeur_defaut("lda_n_terms", 8), min = 3, step = 1),
    numericInput("lda_min_termfreq", "Fréquence minimale des mots", value = valeur_defaut("lda_min_termfreq", 5), min = 1, step = 1),
    selectInput(
      "lda_langue",
      "Langue (stopwords)",
      choices = c("Français" = "fr", "Anglais" = "en", "Espagnol" = "es"),
      selected = valeur_defaut("lda_langue", "fr")
    ),
    checkboxInput("lda_remove_numbers", "Retirer les nombres", value = valeur_defaut("lda_remove_numbers", TRUE)),
    checkboxInput("lda_remove_punct", "Retirer la ponctuation", value = valeur_defaut("lda_remove_punct", TRUE)),
    textInput(
      "lda_stopwords_sup",
      "Mots à ignorer en plus (séparés par des virgules)",
      value = valeur_defaut("lda_stopwords_sup", "")
    )
  )
}
