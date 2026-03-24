# ---------------------------------------------------------
# Bootstrap automatique des dépendances R et spaCy
# À placer tout en haut de global.R ou app.R
# ---------------------------------------------------------

options(repos = c(CRAN = "https://cloud.r-project.org"))

required_packages <- c(
  "shiny", "bslib", "htmltools", "quanteda", "wordcloud",
  "RColorBrewer", "igraph", "dplyr", "remotes", "rgexf",
  "Matrix", "factoextra", "FactoMineR", "ggplot2", "plotly",
  "visNetwork", "DT", "jsonlite", "sna", "intergraph",
  "colorspace", "rgl", "reticulate", "spacyr"
)

installer_packages_r <- function(packages) {
  manquants <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]

  if (length(manquants) > 0) {
    message("Installation des packages R manquants : ", paste(manquants, collapse = ", "))
    install.packages(manquants, dependencies = TRUE)
  } else {
    message("Tous les packages R requis sont déjà installés.")
  }
}

est_mac_apple_silicon <- function() {
  if (Sys.info()[["sysname"]] != "Darwin") {
    return(FALSE)
  }

  arch <- tryCatch(system("uname -m", intern = TRUE), error = function(e) "")
  identical(arch, "arm64")
}

initialiser_spacy_si_possible <- function() {
  if (!requireNamespace("spacyr", quietly = TRUE)) {
    stop("Le package 'spacyr' n'est pas disponible après installation.")
  }

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("Le package 'reticulate' n'est pas disponible après installation.")
  }

  modeles_a_tester <- c("fr_core_news_lg", "fr_core_news_md", "fr_core_news_sm")

  for (modele in modeles_a_tester) {
    ok <- tryCatch({
      spacyr::spacy_initialize(model = modele)
      TRUE
    }, error = function(e) {
      FALSE
    })

    if (ok) {
      message("Modèle spaCy chargé avec succès : ", modele)
      return(modele)
    }
  }

  message("Aucun modèle spaCy FR détecté. Lancement de l'installation automatique...")

  version_spacy <- if (est_mac_apple_silicon()) "apple" else "latest"

  tryCatch({
    spacyr::spacy_install(
      version = version_spacy,
      lang_models = "fr_core_news_sm",
      ask = FALSE,
      force = FALSE
    )
  }, error = function(e) {
    stop("Échec de l'installation automatique de spaCy : ", conditionMessage(e))
  })

  ok <- tryCatch({
    spacyr::spacy_initialize(model = "fr_core_news_sm")
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (!ok) {
    stop("spaCy a été installé, mais le modèle 'fr_core_news_sm' n'a pas pu être initialisé.")
  }

  message("spaCy et le modèle français ont été installés puis initialisés avec succès.")
  return("fr_core_news_sm")
}

# ---------------------------------------------------------
# Exécution du bootstrap
# ---------------------------------------------------------

installer_packages_r(required_packages)

library(shiny)
library(bslib)
library(htmltools)
library(quanteda)
library(wordcloud)
library(RColorBrewer)
library(igraph)
library(dplyr)
library(remotes)
library(rgexf)
library(Matrix)
library(factoextra)
library(FactoMineR)
library(ggplot2)
library(plotly)
library(visNetwork)
library(DT)
library(jsonlite)
library(sna)
library(intergraph)
library(colorspace)
library(rgl)
library(reticulate)
library(spacyr)

modele_spacy_charge <- initialiser_spacy_si_possible()

message("Bootstrap terminé. Modèle spaCy actif : ", modele_spacy_charge)
