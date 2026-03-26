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

if (!exists("ui_form_parametres_similitudes", mode = "function", inherits = TRUE) ||
    !exists("ui_panel_similitudes_iramuteq", mode = "function", inherits = TRUE)) {
  app_dir <- tryCatch(shiny::getShinyOption("appDir"), error = function(e) NULL)
  if (is.null(app_dir) || !nzchar(app_dir)) app_dir <- getwd()
  chemin_ui_simi_iramuteq <- file.path(app_dir, "iramuteqlite", "ui_simi_iramuteq.R")

  if (file.exists(chemin_ui_simi_iramuteq)) {
    source(chemin_ui_simi_iramuteq, encoding = "UTF-8", local = TRUE)
  }
}

if (!exists("ui_form_parametres_lda", mode = "function", inherits = TRUE) ||
    !exists("ui_controles_dynamiques_lda", mode = "function", inherits = TRUE) ||
    !exists("ui_panel_lda_iramuteq", mode = "function", inherits = TRUE)) {
  app_dir <- tryCatch(shiny::getShinyOption("appDir"), error = function(e) NULL)
  if (is.null(app_dir) || !nzchar(app_dir)) app_dir <- getwd()
  chemin_ui_lda_iramuteq <- file.path(app_dir, "lda", "ui_lda_iramuteq.R")

  if (file.exists(chemin_ui_lda_iramuteq)) {
    source(chemin_ui_lda_iramuteq, encoding = "UTF-8", local = TRUE)
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

if (!exists("ui_aide_morpho", mode = "function")) {
  if (file.exists("help/pos_lexique.md")) {
    ui_aide_morpho <- function() {
      tagList(
        tags$h2("Aide morpho"),
        includeMarkdown("help/pos_lexique.md")
      )
    }
  } else {
    ui_aide_morpho <- function() {
      tagList(
        tags$h2("Aide morpho"),
        tags$p("Le fichier help/pos_lexique.md est introuvable. Vérifie le dossier d'aide du projet.")
      )
    }
  }
}

ui_form_parametres_analyse <- function(defaults = NULL) {
  valeur_defaut <- function(id, fallback) {
    if (is.null(defaults) || is.null(defaults[[id]]) || (length(defaults[[id]]) == 1 && is.na(defaults[[id]]))) {
      return(fallback)
    }
    defaults[[id]]
  }
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
  morpho_choices_labels[["AUTRE_FORME"]] <- "AUTRE_FORME (forme non reconnue : type vide)"

  tagList(
    radioButtons(
      "modele_chd",
      "Méthode Iramuteq-lite",
      choices = c("IRaMuTeQ-lite" = "iramuteq"),
      selected = valeur_defaut("modele_chd", "iramuteq"),
      inline = FALSE
    ),

    tags$div(class = "sidebar-section-title", "Paramètres CHD"),
    numericInput("segment_size", "segment_size", value = valeur_defaut("segment_size", 40), min = 5, step = 1),
    checkboxInput(
      "segmenter_sur_ponctuation_forte",
      "Tenir compte de la ponctuation forte (. ! ?) dans le découpage",
      value = valeur_defaut("segmenter_sur_ponctuation_forte", TRUE)
    ),
    tags$p(
      "Si activé, le découpage recherche la meilleure frontière autour de segment_size avec priorité . ! ?, puis ; :, puis , puis espace ; un retour à la ligne clôture aussi le segment.",
      style = "color: #c00; font-size: 0.9em; margin-top: 4px; margin-bottom: 8px;"
    ),
    numericInput("min_docfreq", "Fréquence minimale des termes (min_docfreq)", value = 3, min = 1, step = 1),
    numericInput("max_p", "max_p (p-value)", value = 0.05, min = 0, max = 1, step = 0.1),
    checkboxInput("filtrer_affichage_pvalue", "Filtrer l'affichage des résultats par p-value (p ≤ max_p)", value = TRUE),

    conditionalPanel(
      condition = "input.modele_chd == 'iramuteq'",
      ui_options_iramuteq(defaults = defaults)
    ),

    tags$div(class = "sidebar-section-title", "Dictionnaire"),
    radioButtons(
      "source_dictionnaire",
      "Source de lemmatisation",
      choices = c(
        "Lexique (fr)" = "lexique_fr"
      ),
      selected = valeur_defaut("source_dictionnaire", "lexique_fr"),
      inline = FALSE
    ),
    tags$p("spaCy est réservé à la détection NER (pas au filtrage POS CHD).", style = "font-size: 0.9em; color: #555; margin-top: -6px;"),
    conditionalPanel(
      condition = "input.source_dictionnaire == 'lexique_fr'",
      checkboxInput("lexique_utiliser_lemmes", "Lemmatisation via les lemmes de lexique_fr (forme → c_lemme)", value = valeur_defaut("lexique_utiliser_lemmes", TRUE))
    ),
    checkboxInput("expression_utiliser_dictionnaire", "Utiliser le dictionnaire d'expression (dic_mot → dic_norm)", value = valeur_defaut("expression_utiliser_dictionnaire", FALSE)),
    tags$p("Upload du dictionnaire dans l'onglet Annotation expressions.", style = "font-size: 0.9em; color: #555;"),

    tags$div(class = "sidebar-section-title", "Nettoyage"),
    checkboxInput("nettoyage_caracteres", "Nettoyage caractères (regex)", value = valeur_defaut("nettoyage_caracteres", TRUE)),
    tags$p(
      "[^a-zA-Z0-9àÀâÂäÄáÁåÅãéÉèÈêÊëËìÌîÎïÏíÍóÓòÒôÔöÖõÕøØùÙûÛüÜúÚçÇßœŒ’ñÑ_\\.:,;!\\?']",
      style = "color: #c00; font-size: 0.9em; margin-top: 4px; margin-bottom: 8px;"
    ),
    tags$p(
      "Les caractères présents dans la liste entre crochets sont conservés (dont _ pour les expressions normalisées) ; tous les autres (ex. @ # & / emoji) sont remplacés par des espaces.",
      style = "color: #c00; font-size: 0.9em; margin-top: 4px; margin-bottom: 8px;"
    ),
    checkboxInput("supprimer_ponctuation", "Supprimer la ponctuation", value = valeur_defaut("supprimer_ponctuation", FALSE)),
    tags$p(
      "Supprime la ponctuation à la tokenisation quanteda (remove_punct), par ex. . , ; : ! ? ' ’ \" - ( ) [ ] …",
      style = "color: #c00; font-size: 0.9em; margin-top: 4px; margin-bottom: 8px;"
    ),
    checkboxInput("supprimer_chiffres", "Supprimer les chiffres (0-9)", value = valeur_defaut("supprimer_chiffres", FALSE)),
    checkboxInput("supprimer_apostrophes", "Traiter les élisions FR (c'est→est, m'écrire→écrire)", value = valeur_defaut("supprimer_apostrophes", TRUE)),
    checkboxInput("remplacer_tirets_espaces", "Remplacer les tirets (-) par des espaces", value = valeur_defaut("remplacer_tirets_espaces", FALSE)),
    checkboxInput("retirer_stopwords", "Retirer les stopwords (liste française quanteda)", value = valeur_defaut("retirer_stopwords", FALSE)),
    checkboxInput("filtrage_morpho", "Filtrage morphosyntaxique", value = valeur_defaut("filtrage_morpho", FALSE)),
    conditionalPanel(
      condition = "input.filtrage_morpho == true && input.source_dictionnaire == 'lexique_fr'",
      selectizeInput(
        "pos_lexique_a_conserver",
        "Catégories c_morpho à conserver (lexique_fr)",
        choices = morpho_choices_labels,
        selected = valeur_defaut("pos_lexique_a_conserver", c("NOM", "VER", "ADJ")),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      checkboxInput(
        "morpho_exclure_etre_verbe",
        "Filtrer le terme \"être\" dans la catégorie VERB (également reconnu comme NOM dans lexique_fr)",
        value = valeur_defaut("morpho_exclure_etre_verbe", FALSE)
      ),
      checkboxInput(
        "morpho_conserver_hors_lexique",
        "Conserver les formes non reconnues par le lexique (AUTRE_FORME)",
        value = valeur_defaut("morpho_conserver_hors_lexique", TRUE)
      )
    ),
    tags$div(class = "sidebar-section-title", "Paramètres AFC"),
    checkboxInput("afc_reduire_chevauchement", "Réduire les chevauchements des mots (AFC)", value = valeur_defaut("afc_reduire_chevauchement", TRUE)),
    radioButtons(
      "afc_taille_mots",
      "Taille des mots (AFC termes)",
      choices = c("Fréquence" = "frequency", "Chi2" = "chi2"),
      selected = valeur_defaut("afc_taille_mots", "frequency"),
      inline = FALSE
    ),
    numericInput("top_n", "Top N mots par classe (nuages)", value = valeur_defaut("top_n", 20), min = 5, step = 1)
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
    title = "Résultats",
    actionButton("menu_importer_fichier_sidebar", "Importer un fichier texte", class = "btn-primary"),
    tags$div(style = "margin-top:8px;"),
    uiOutput("nom_fichier_corpus_sidebar"),
    tags$hr(style = "margin-top: 10px; margin-bottom: 10px;"),
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
      span.highlight { background-color: yellow; color: inherit; font-weight: inherit; padding: 0; border-radius: 0; }
      .annotation-action-btn {
        min-width: 190px !important;
        height: 38px !important;
        display: inline-flex;
        align-items: center;
        justify-content: center;
      }
      #simi_terms_selected + .selectize-control .selectize-input > div {
        display: inline-flex;
        align-items: center;
        min-height: 28px;
        padding-right: 2px;
      }
      #simi_terms_selected + .selectize-control .selectize-input > div .remove {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        min-width: 20px;
        min-height: 20px;
        margin-left: 6px;
        border-radius: 50%;
        font-size: 16px;
        line-height: 1;
      }
      #simi_terms_selected + .selectize-control .selectize-input > div .remove:hover {
        background: rgba(0, 0, 0, 0.08);
      }
    ")),
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function () {
        var docEl = document.documentElement;
        var bouton = document.getElementById('btn_plein_ecran');
        var estDansIframe = window.self !== window.top;

        function entrerPleinePage() {
          if (document.fullscreenElement) return Promise.resolve(true);
          if (!document.fullscreenEnabled || !docEl.requestFullscreen) {
            return Promise.resolve(false);
          }
          return docEl.requestFullscreen().then(function () { return true; }).catch(function () { return false; });
        }

        if (bouton) {
          bouton.addEventListener('click', function () {
            entrerPleinePage().then(function (ok) {
              if (!ok && estDansIframe) {
                console.warn('Plein écran refusé en contexte embarqué (iframe/viewer).');
              }
            });
          });
        }

        document.addEventListener('click', function (event) {
          var item = event.target && event.target.closest('#simi_terms_selected + .selectize-control .selectize-input > div');
          if (!item) return;
          if (event.target.classList && event.target.classList.contains('remove')) return;

          var champ = document.getElementById('simi_terms_selected');
          if (!champ || !champ.selectize) return;

          var val = item.getAttribute('data-value');
          if (!val) return;

          event.preventDefault();
          event.stopPropagation();
          champ.selectize.removeItem(val, true);
        }, true);

      });
    "))
  ),

  nav_panel(
    "Analyse", value = "analyse",
    tags$h3("Analyse du corpus"),
    uiOutput("ui_table_stats_corpus"),
    tags$hr(),
    tags$h4("Journal debug"),
    tags$div(
      style = "max-height: 320px; overflow-y: auto;",
      verbatimTextOutput("logs", placeholder = TRUE)
    ),
    tags$hr(),
    tags$h4("Loi de Zipf"),
    tags$div(
      style = "width: 800px; max-width: 100%;",
      plotOutput("plot_stats_zipf", height = "800px", width = "800px")
    )
  ),
  nav_panel("Corpus", value = "corpus", tags$h3("Corpus importé"), uiOutput("ui_corpus_preview")),
  nav_panel(
    "Annotation", value = "annotation_expressions",
    tags$h3("Annotation du corpus => add_expression_fr.csv"),
    fileInput("annotation_import_csv", "Charger un dictionnaire d'expression (.csv)", accept = c(".csv")),
    tags$p("Vous pouvez réimporter un fichier d'expressions déjà annoté (il doit impérativement être nommé : add_expression_fr.csv)."),
    tags$p("Sélectionnez un extrait dans la zone ci-dessous puis ajoutez-le à votre propre dictionnaire (dic_mot, dic_norm, dic_morpho) qui sera fusionné avec le dictionnaire d'expressions IRAMUTEQ."),
    tags$p("Vous devez surligner, copier/coller les expressions pour les ajouter au dictionnaire."),
    textAreaInput("annotation_corpus_text", NULL, value = "", rows = 14, width = "100%"),
    tags$h4("Prévisualisation annotée"),
    uiOutput("annotation_corpus_colore"),
    tags$div(
      style = "display:flex; gap:8px; align-items:flex-end; flex-wrap:wrap;",
      tags$div(style = "flex:1; min-width:220px;", textInput("annotation_selection", "Texte sélectionné (dic_mot)", value = "")),
      tags$div(style = "flex:1; min-width:220px;", textInput("annotation_norm", "Normalisation (dic_norm)", value = "")),
      tags$div(style = "flex:1; min-width:220px;", textInput("annotation_morpho", "Type morpho (dic_morpho, optionnel)", value = ""))
    ),
    tags$div(style = "display:flex; gap:8px; flex-wrap:wrap; align-items:flex-end; margin-bottom:10px;",
      actionButton("annotation_add_entry", "Ajouter / mettre à jour", class = "annotation-action-btn"),
      textInput("annotation_remove_key", NULL, value = "", placeholder = "dic_mot à supprimer", width = "190px"),
      actionButton("annotation_remove_entry", "Supprimer", class = "annotation-action-btn")
    ),
    downloadButton("dl_expression_csv", "Télécharger add_expression_fr.csv"),
    tags$h4("Dictionnaire d'expressions (session)"),
    tableOutput("table_annotation_dict")
  ),
  nav_panel("CHD", value = "resultats_chd", ui_resultats_chd_iramuteq()),
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
  ui_panel_lda_iramuteq(),
  ui_panel_similitudes_iramuteq(),

  nav_panel("Aide", value = "aide", ui_aide_huggingface()),
  nav_panel("Aide morpho", value = "aide_morpho", ui_aide_morpho())
)
