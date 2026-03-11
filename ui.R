# Rôle du fichier: ui.R porte une partie du pipeline d'analyse IRaMuTeQ-lite.
# ui.R

library(shiny)
library(bslib)
library(htmltools)


if (!exists("ui_options_iramuteq", mode = "function", inherits = TRUE)) {
  app_dir <- tryCatch(shiny::getShinyOption("appDir"), error = function(e) NULL)
  if (is.null(app_dir) || !nzchar(app_dir)) app_dir <- getwd()
  chemin_options_iramuteq <- file.path(app_dir, "iramuteqlite", "ui_options_iramuteq.R")

  if (file.exists(chemin_options_iramuteq)) {
    source(chemin_options_iramuteq, encoding = "UTF-8", local = TRUE)
  }
}


if (!exists("ui_resultats_chd_iramuteq", mode = "function", inherits = TRUE)) {
  app_dir <- tryCatch(shiny::getShinyOption("appDir"), error = function(e) NULL)
  if (is.null(app_dir) || !nzchar(app_dir)) app_dir <- getwd()
  chemin_affichage_iramuteq <- file.path(app_dir, "iramuteqlite", "affichage_iramuteq.R")

  if (file.exists(chemin_affichage_iramuteq)) {
    source(chemin_affichage_iramuteq, encoding = "UTF-8", local = TRUE)
  }
}

if (!exists("ui_explorateur_iramuteq", mode = "function", inherits = TRUE)) {
  app_dir <- tryCatch(shiny::getShinyOption("appDir"), error = function(e) NULL)
  if (is.null(app_dir) || !nzchar(app_dir)) app_dir <- getwd()
  chemin_explorateur_iramuteq <- file.path(app_dir, "iramuteqlite", "ui_explorateur_iramuteq.R")

  if (file.exists(chemin_explorateur_iramuteq)) {
    source(chemin_explorateur_iramuteq, encoding = "UTF-8", local = TRUE)
  }
}

if (!exists("ui_aide_huggingface", mode = "function")) {
  if (file.exists("help/help.md")) {
    ui_aide_huggingface <- function() {
      tagList(
        tags$h2("Aide"),
        includeMarkdown("help/help.md")
      )
    }
  } else {
    ui_aide_huggingface <- function() {
      tagList(
        tags$h2("Aide"),
        tags$p("Le fichier help/help.md est introuvable. Vérifie le dossier d'aide du projet.")
      )
    }
  }
}

ui_form_parametres_analyse <- function() {
  lexique_path <- file.path(getwd(), "dictionnaires", "lexique_fr.csv")
  morpho_choices <- c(
    "NOM", "NOM_SUP", "VER", "VER_SUP", "AUX",
    "ADJ", "ADJ_SUP", "ADJ_DEM", "ADJ_IND", "ADJ_INT", "ADJ_NUM", "ADJ_POS",
    "ADV", "ADV_SUP", "PRE", "CON", "ONO",
    "PRO_PER", "PRO_POS", "PRO_DEM", "PRO_IND", "PRO_REL",
    "ART_DEF", "ART_IND"
  )

  if (file.exists(lexique_path)) {
    lexique_df <- tryCatch(
      utils::read.csv2(lexique_path, stringsAsFactors = FALSE),
      error = function(e) NULL
    )

    if (is.data.frame(lexique_df) && "c_morpho" %in% names(lexique_df)) {
      morpho_from_lexique <- unique(toupper(trimws(as.character(lexique_df$c_morpho))))
      morpho_from_lexique <- morpho_from_lexique[nzchar(morpho_from_lexique)]
      if (length(morpho_from_lexique) > 0) {
        morpho_choices <- sort(unique(morpho_from_lexique))
      }
    }
  }

  morpho_choices_labels <- stats::setNames(morpho_choices, morpho_choices)
  if ("VER_SUP" %in% names(morpho_choices_labels)) {
    morpho_choices_labels[["VER_SUP"]] <- "VER_SUP (verbe supplémentaire)"
  }

  tagList(
    radioButtons(
      "modele_chd",
      "Méthode Iramuteq-like",
      choices = c("IRaMuTeQ-lite" = "iramuteq"),
      selected = "iramuteq",
      inline = FALSE
    ),

    tags$div(class = "sidebar-section-title", "Paramètres généraux CHD"),
    numericInput("segment_size", "segment_size", value = 40, min = 5, step = 1),
    numericInput("min_docfreq", "Fréquence minimale des termes (min_docfreq)", value = 3, min = 1, step = 1),
    numericInput("max_p", "max_p (p-value)", value = 0.05, min = 0, max = 1, step = 0.01),
    checkboxInput("filtrer_affichage_pvalue", "Filtrer l'affichage des résultats par p-value (p ≤ max_p)", value = TRUE),

    conditionalPanel(
      condition = "input.modele_chd == 'iramuteq'",
      ui_options_iramuteq()
    ),

    tags$div(class = "sidebar-section-title", "Dictionnaire"),
    radioButtons(
      "source_dictionnaire",
      "Source de lemmatisation",
      choices = c("Lexique (fr)" = "lexique_fr"),
      selected = "lexique_fr",
      inline = FALSE
    ),
    conditionalPanel(
      condition = "input.source_dictionnaire == 'lexique_fr'",
      checkboxInput("lexique_utiliser_lemmes", "Lemmatisation via les lemmes de lexique_fr (forme → c_lemme)", value = TRUE)
    ),
    checkboxInput("expression_utiliser_dictionnaire", "Utiliser le dictionnaire d'expression (dic_mot → dic_norm)", value = FALSE),

    tags$div(class = "sidebar-section-title", "Nettoyage"),
    checkboxInput("nettoyage_caracteres", "Nettoyage caractères (regex)", value = FALSE),
    tags$p(
      "[^a-zA-Z0-9àÀâÂäÄáÁåÅãéÉèÈêÊëËìÌîÎïÏíÍóÓòÒôÔöÖõÕøØùÙûÛüÜúÚçÇßœŒ’ñÑ\\.:,;!\\?']",
      style = "color: #c00; font-size: 0.9em; margin-top: 4px; margin-bottom: 8px;"
    ),
    tags$p(
      "Les caractères présents dans la liste entre crochets sont conservés ; tous les autres (ex. @ # & / emoji) sont remplacés par des espaces.",
      style = "color: #c00; font-size: 0.9em; margin-top: 4px; margin-bottom: 8px;"
    ),
    checkboxInput("supprimer_ponctuation", "Supprimer la ponctuation", value = FALSE),
    tags$p(
      "Supprime la ponctuation à la tokenisation quanteda (remove_punct), par ex. . , ; : ! ? ' ’ \" - ( ) [ ] …",
      style = "color: #c00; font-size: 0.9em; margin-top: 4px; margin-bottom: 8px;"
    ),
    checkboxInput("supprimer_chiffres", "Supprimer les chiffres (0-9)", value = FALSE),
    checkboxInput("supprimer_apostrophes", "Traiter les élisions FR (c'est→est, m'écrire→écrire)", value = FALSE),
    checkboxInput("remplacer_tirets_espaces", "Remplacer les tirets (-) par des espaces", value = FALSE),
    checkboxInput("retirer_stopwords", "Retirer les stopwords (liste française quanteda)", value = FALSE),
    checkboxInput("filtrage_morpho", "Filtrage morphosyntaxique", value = FALSE),
    conditionalPanel(
      condition = "input.filtrage_morpho == true && input.source_dictionnaire == 'lexique_fr'",
      selectizeInput(
        "pos_lexique_a_conserver",
        "Catégories c_morpho à conserver (lexique_fr)",
        choices = morpho_choices_labels,
        selected = c("NOM", "VER", "ADJ"),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    ),

    tags$div(class = "sidebar-section-title", "Paramètres AFC"),
    checkboxInput("afc_reduire_chevauchement", "Réduire les chevauchements des mots (AFC)", value = FALSE),
    radioButtons(
      "afc_taille_mots",
      "Taille des mots (AFC termes)",
      choices = c("Fréquence" = "frequency", "Chi2" = "chi2"),
      selected = "frequency",
      inline = FALSE
    ),
    numericInput("top_n", "Top N mots par classe (nuages)", value = 20, min = 5, step = 1)
  )
}

if (!exists("REGEX_CARACTERES_A_SUPPRIMER", inherits = TRUE)) {
  app_dir <- tryCatch(shiny::getShinyOption("appDir"), error = function(e) NULL)
  if (is.null(app_dir) || !nzchar(app_dir)) app_dir <- getwd()
  chemin_nettoyage <- file.path(app_dir, "iramuteqlite", "nettoyage_iramuteq.R")

  if (file.exists(chemin_nettoyage)) {
    source(chemin_nettoyage, encoding = "UTF-8", local = TRUE)
  }
}

if (!exists("REGEX_CARACTERES_A_SUPPRIMER", inherits = TRUE)) {
  REGEX_CARACTERES_AUTORISES <- "a-zA-Z0-9àÀâÂäÄáÁåÅãéÉèÈêÊëËìÌîÎïÏíÍóÓòÒôÔöÖõÕøØùÙûÛüÜúÚçÇßœŒ’ñÑ\\.:,;!\\?'"
  REGEX_CARACTERES_A_SUPPRIMER <- paste0("[^", REGEX_CARACTERES_AUTORISES, "]")
}

ui <- page_navbar(
  id = "nav_principal",
  title = tags$div(
    style = "color: #ffffff;",
    tags$div(style = "font-weight: 700;", "IRaMuTeQ-Lite"),
    tags$small(
      style = "display:block; font-size: 0.75rem; line-height: 1.35; font-weight: 400; color: #ffffff; margin-top: 2px;",
      "Tentative de reproduction de la CHD du logiciel IRaMuTeQ (IRaMuTeQ - Pierre Ratinaud - LERASS)",
      tags$br(),
      "Pour d'autres scripts/appli, vous pouvez consulter mon site : www.codeandcortex.fr",
      tags$br(),
      "version beta 0.1 - 11-03-2026"
    )
  ),
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  fillable = FALSE,
  sidebar = sidebar(
    width = 320,
    open = "always",
    title = "Fichier",
    actionButton("menu_importer_fichier_sidebar", "Importer le fichier texte", class = "btn-primary"),
    tags$div(style = "margin-top: 10px;"),
    tags$strong("Nom du fichier :"),
    tags$div(textOutput("nom_fichier_selectionne"), style = "margin-bottom: 12px;"),
    downloadButton("dl_zip", "Télécharger les résultats")
  ),

  tags$head(
    tags$style(HTML(" 
      #shiny-modal .modal-dialog { max-width: 760px; margin-top: 6vh; }
      #shiny-modal .modal-body { max-height: 70vh; overflow-y: auto; }
      .sidebar-section-title { font-weight:700; color:#1e5aa8; margin-top:10px; }
      .navbar .container-fluid { display: flex; flex-direction: column; align-items: flex-start; }
      .navbar .navbar-brand { color: #ffffff !important; margin-right: 0; white-space: normal; }
      .navbar .navbar-brand small { color: #ffffff !important; }
      .navbar .navbar-nav { margin-top: 0.5rem; }
    "))
  ),

  nav_panel(
    "Analyse",
    value = "analyse",
    fluidRow(
      column(
        width = 12,
        actionButton("ouvrir_parametres", "Paramétrer l'analyse", class = "btn-secondary"),
        tags$br(), tags$br(),
        tags$h3("Statut"), textOutput("statut"),
        tags$h3("Journal"), tags$pre(style = "white-space: pre-wrap;", textOutput("logs")),
        tags$h3("Analyse du corpus"), uiOutput("ui_table_stats_corpus"),
        tags$div(style = "width: 600px;", plotOutput("plot_stats_zipf", height = "600px", width = "600px")),
        tags$h3("Répartition des classes"), tableOutput("table_classes")
      )
    )
  ),

  nav_panel("Corpus", value = "corpus", tags$h3("Corpus importé"), uiOutput("ui_corpus_preview")),
  nav_panel("Résultats CHD", value = "resultats_chd", ui_resultats_chd_iramuteq()),
  nav_panel(
    "AFC", value = "afc",
    tags$h3("AFC"), uiOutput("ui_afc_statut"), uiOutput("ui_afc_erreurs"),
    tags$h4("AFC des classes"), plotOutput("plot_afc_classes", height = "620px"),
    tags$h4("AFC des termes"),
    tags$div(style = "display:flex; gap:8px; align-items:center; margin-bottom:8px;",
      actionButton("afc_zoom_in", "Zoom +"),
      actionButton("afc_zoom_out", "Zoom -"),
      actionButton("afc_zoom_reset", "Réinitialiser le zoom AFC termes"),
      tags$small("Astuce: cliquer-glisser sur le graphique pour zoomer.")
    ),
    plotOutput("plot_afc", height = "720px", brush = brushOpts(id = "afc_brush", resetOnNew = TRUE)),
    tags$h4("Table des mots projetés"), uiOutput("ui_table_afc_mots_par_classe"),
    tags$h4("AFC des variables étoilées"), plotOutput("plot_afc_vars", height = "720px"),
    tags$h4("Table des modalités projetées"), tableOutput("table_afc_vars"),
    tags$h4("Valeurs propres"), tableOutput("table_afc_eig")
  ),

  nav_panel("Aide", value = "aide", ui_aide_huggingface())
)
