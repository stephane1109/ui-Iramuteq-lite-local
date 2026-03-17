###############################################################################
#                    Script CHD - version beta 0.2 - 14-03-2026               #
#      A partir d'un corpus texte formaté aux exigences IRAMUTEQ              #
#                            Stéphane Meurisse                                #
#                           wwww.codeandcortex.fr                             #          
#                        DEV EN LOCAL + ANNOTATIONS                           #
###############################################################################

required_packages <- c("shiny", "bslib", "quanteda", "wordcloud", "RColorBrewer", "igraph", "dplyr", "htmltools", "remotes", "irlba", "markdown", "rgexf", "Matrix", "factoextra", "ape", "ggplot2")
installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed_packages)

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

if (!"FactoMineR" %in% installed_packages) {
  remotes::install_github("husson/FactoMineR", dependencies = NA, upgrade = "never")
}

# Augmente la limite d'upload Shiny (défaut ~5 Mo), utile pour les corpus .txt volumineux.
options(shiny.maxRequestSize = 30 * 1024^2)

# En environnement non interactif (containers, services), certains appels graphiques
# sans device explicite peuvent tenter d'écrire "Rplots.pdf" dans le répertoire courant.
# Si ce répertoire n'est pas inscriptible, cela provoque l'erreur:
# "cannot open file 'Rplots.pdf'".
# On force donc un device PDF de repli dans tempdir(), toujours inscriptible.
if (!interactive()) {
  options(device = function(...) {
    grDevices::pdf(file = file.path(tempdir(), "Rplots.pdf"), ...)
  })
}

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

surligner_segment_afc <- function(segment, terme) {
  segment <- ifelse(is.na(segment), "", as.character(segment))
  terme <- ifelse(is.na(terme), "", as.character(terme))
  if (!nzchar(trimws(segment)) || !nzchar(trimws(terme))) {
    return(htmltools::htmlEscape(segment))
  }

  motifs <- preparer_motifs_surlignage_nfd(terme, taille_lot = 1)
  if (!length(motifs)) return(htmltools::htmlEscape(segment))

  segment_hl <- surligner_vecteur_html_unicode(
    segment,
    motifs,
    "<span style='background-color: yellow;'>",
    "</span>"
  )

  echapper_segments_en_preservant_surlignage(
    segment_hl,
    "<span style='background-color: yellow;'>",
    "</span>"
  )
}

generer_table_html_afc_mots <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(tags$p("Aucun mot disponible pour cette classe."))
  }
  
  htmltools::div(
    style = "max-height: 420px; overflow-y: auto;",
    tags$table(
      style = "width: 100%; border-collapse: collapse;",
      tags$thead(
        tags$tr(lapply(names(df), function(col) tags$th(style = "text-align:left; padding:6px; border-bottom:1px solid #ddd;", col)))
      ),
      tags$tbody(
        lapply(seq_len(nrow(df)), function(i) {
          terme_ligne <- if ("Terme" %in% names(df)) df$Terme[[i]] else ""
          tags$tr(
            lapply(names(df), function(col) {
              val <- df[[col]][[i]]
              contenu <- ifelse(is.na(val), "", as.character(val))
              if (identical(col, "Segment_texte")) {
                contenu <- htmltools::HTML(surligner_segment_afc(contenu, terme_ligne))
              }
              tags$td(style = "padding:6px; border-bottom:1px solid #f0f0f0; vertical-align: top;", contenu)
            })
          )
        })
      )
    )
  )
}

source("iramuteqlite/nettoyage_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/concordancier-iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/afc_helpers_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/afc_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/ui_chd_stats_mode_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/ui_options_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/ui_explorateur_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/affichage_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/wordcloud_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/simi.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/simi_graph.R", encoding = "UTF-8", local = TRUE)
source("ui.R", encoding = "UTF-8", local = TRUE)

source("iramuteqlite/chd_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/dendrogramme_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/iramuteq_bars.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/stats_chd.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/popup.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/chd_engine_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/server_outputs_status_iramuteq.R", encoding = "UTF-8", local = TRUE)
source("iramuteqlite/server_events_lancer_iramuteq.R", encoding = "UTF-8", local = TRUE)

# Compatibilité défensive: certains chemins historiques utilisent encore des appels
# non qualifiés (docvars/docnames). On expose des wrappers explicites pour éviter
# les erreurs de résolution si ces symboles ne sont pas attachés dans la session.
if (!exists("docvars", mode = "function", inherits = FALSE)) {
  docvars <- function(...) quanteda::docvars(...)
}
if (!exists("docnames", mode = "function", inherits = FALSE)) {
  docnames <- function(...) quanteda::docnames(...)
}

server <- function(input, output, session) {
  
  est_texte_non_vide <- function(x) {
    is.character(x) && length(x) > 0 && !is.na(x[[1]]) && nzchar(x[[1]])
  }
  
  zone_trace_disponible <- function(output_id, min_width = 120, min_height = 120) {
    largeur <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", output_id, "_width")]]))
    hauteur <- suppressWarnings(as.numeric(session$clientData[[paste0("output_", output_id, "_height")]]))
    
    is.finite(largeur) && is.finite(hauteur) && largeur >= min_width && hauteur >= min_height
  }

  creer_zip_depuis_dossier <- function(dossier_source, fichier_zip) {
    if (!dir.exists(dossier_source)) {
      stop("Dossier d'exports introuvable.")
    }
    
    ancien_wd <- getwd()
    on.exit(setwd(ancien_wd), add = TRUE)
    setwd(dirname(dossier_source))
    
    if (file.exists(fichier_zip)) unlink(fichier_zip)
    utils::zip(zipfile = fichier_zip, files = basename(dossier_source))
    
    if (!file.exists(fichier_zip)) {
      stop("Impossible de créer l'archive ZIP.")
    }
  }
  
  rv <- reactiveValues(
    logs = "",
    statut = "En attente.",
    progression = 0,
    
    base_dir = NULL,
    export_dir = NULL,
    segments_file = NULL,
    stats_file = NULL,
    html_file = NULL,
    zip_file = NULL,
    
    res = NULL,
    res_chd = NULL,
    dfm_chd = NULL,
    dfm = NULL,
    filtered_corpus = NULL,
    res_stats_df = NULL,
    clusters = NULL,
    max_n_groups = NULL,
    max_n_groups_chd = NULL,
    
    res_type = "simple",
    
    exports_prefix = paste0("exports_", session$token),
    wordcloud_refresh_token = 0,
    
    lexique_fr_df = NULL,
    expression_fr_df = NULL,
    expression_annotations_df = data.frame(dic_mot = character(0), dic_norm = character(0), dic_morpho = character(0), stringsAsFactors = FALSE),
    expressions_actives_df = NULL,
    utiliser_add_expression = FALSE,
    textes_indexation = NULL,
    
    afc_obj = NULL,
    afc_erreur = NULL,
    
    afc_vars_obj = NULL,
    afc_vars_erreur = NULL,
    
    afc_dir = NULL,
    afc_table_mots = NULL,
    afc_table_vars = NULL,
    afc_plot_classes = NULL,
    afc_plot_termes = NULL,
    afc_plot_vars = NULL,
    afc_zoom_terms = NULL,
    
    explor_assets = NULL,
    stats_corpus_df = NULL,
    stats_zipf_df = NULL,
    min_docfreq_applique = 3L,
    
    simi_graph = NULL,
    simi_layout = NULL,
    simi_vertex_freq = NULL,
    simi_method = "cooc",
    simi_seuil_applique = NA_real_,
    simi_communities = NULL,
    simi_zoom = 1,
    simi_terms_used = 0L,
    simi_terms_total = 0L,
    simi_top_terms_requested = 40L,
    
    parametres_analyse = list()
  )
  
  app_dir <- tryCatch(shiny::getShinyOption("appDir"), error = function(e) NULL)
  if (is.null(app_dir) || !nzchar(app_dir)) app_dir <- getwd()
  
  sauvegarder_add_expression <- function(df) {
    if (is.null(df) || !is.data.frame(df)) return(invisible(NULL))
    if (!"dic_morpho" %in% names(df)) df$dic_morpho <- ""
    df$dic_morpho[is.na(df$dic_morpho)] <- ""
    path_out <- file.path(app_dir, "dictionnaires", "add_expression_fr.csv")
    tryCatch({
      utils::write.csv2(df, path_out, row.names = FALSE, fileEncoding = "UTF-8", na = "")
      invisible(path_out)
    }, error = function(e) invisible(NULL))
  }
  
  normaliser_add_expression_df <- function(df) {
    if (is.null(df) || !is.data.frame(df)) return(NULL)
    noms <- names(df)
    noms <- gsub("^\ufeff", "", noms, perl = TRUE)
    names(df) <- noms
    if (!all(c("dic_mot", "dic_norm") %in% names(df))) return(NULL)
    if (!"dic_morpho" %in% names(df)) df$dic_morpho <- ""
    df <- df[, c("dic_mot", "dic_norm", "dic_morpho"), drop = FALSE]
    df$dic_mot[is.na(df$dic_mot)] <- ""
    df$dic_norm[is.na(df$dic_norm)] <- ""
    df$dic_morpho[is.na(df$dic_morpho)] <- ""
    df$dic_mot <- tolower(trimws(as.character(df$dic_mot)))
    df$dic_norm <- tolower(trimws(as.character(df$dic_norm)))
    df$dic_morpho <- trimws(as.character(df$dic_morpho))
    df <- df[nzchar(df$dic_mot) & nzchar(df$dic_norm), , drop = FALSE]
    df <- df[!duplicated(df$dic_mot), , drop = FALSE]
    df
  }
  
  lire_add_expression_depuis_upload <- function(path_in) {
    lecteurs <- list(
      function() utils::read.csv2(path_in, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = character()),
      function() utils::read.csv(path_in, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = character(), sep = ","),
      function() utils::read.csv(path_in, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = character(), sep = ";"),
      function() utils::read.delim(path_in, stringsAsFactors = FALSE, encoding = "UTF-8", na.strings = character(), sep = "\t")
    )
    
    for (lecteur in lecteurs) {
      df <- tryCatch(lecteur(), error = function(e) NULL)
      df_norm <- normaliser_add_expression_df(df)
      if (!is.null(df_norm)) return(df_norm)
    }
    
    NULL
  }
  
  reinitialiser_add_expression_travail <- function() {
    path_in <- file.path(app_dir, "dictionnaires", "add_expression_fr.csv")
    if (file.exists(path_in)) {
      tryCatch(unlink(path_in), error = function(e) invisible(NULL))
    }
    invisible(NULL)
  }
  
  reinitialiser_add_expression_travail()
  
  
  if (exists("register_outputs_status", mode = "function", inherits = TRUE)) {
    register_outputs_status(input, output, session, rv)
  } else {
    output$statut <- renderText({ rv$statut })
    output$logs <- renderText({ rv$logs })
  }
  
  if (exists("server_explorateur_iramuteq", mode = "function", inherits = TRUE)) {
    server_explorateur_iramuteq(
      id = "explorer",
      rv = rv,
      nom_corpus_reactif = reactive({
        if (!is.null(input$fichier_corpus$name) && nzchar(input$fichier_corpus$name)) {
          return(as.character(input$fichier_corpus$name))
        }
        NULL
      })
    )
  }
  
  
  output$ui_afc_statut <- renderUI({
    if (est_texte_non_vide(rv$afc_erreur)) {
      return(tags$p("AFC : erreur (voir ci-dessous)."))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      return(tags$p("AFC non calculée. Lance une analyse pour calculer l'AFC classes × termes."))
    }
    ncl <- nrow(rv$afc_obj$table)
    nt <- ncol(rv$afc_obj$table)
    tags$p(paste0("AFC calculée sur ", ncl, " classes et ", nt, " termes (table Classes × Termes)."))
  })
  
  output$ui_afc_erreurs <- renderUI({
    messages <- Filter(
      est_texte_non_vide,
      list(
        rv$afc_erreur,
        rv$afc_vars_erreur
      )
    )
    
    if (length(messages) == 0) {
      return(NULL)
    }
    
    tags$div(
      style = "display: flex; flex-direction: column; gap: 8px; margin-bottom: 12px;",
      lapply(messages, function(msg) {
        tags$div(
          style = "border: 1px solid #f5c2c7; background: #f8d7da; color: #842029; border-radius: 4px; padding: 10px; white-space: pre-wrap;",
          msg
        )
      })
    )
  })
  
  
  ids_parametres_analyse <- c(
    "modele_chd", "segment_size", "segmenter_sur_ponctuation_forte", "min_docfreq", "max_p", "filtrer_affichage_pvalue",
    "k_iramuteq", "iramuteq_max_formes", "iramuteq_mincl_mode", "iramuteq_mincl", "iramuteq_classif_mode",
    "iramuteq_rst1", "iramuteq_rst2", "iramuteq_svd_method", "iramuteq_stats_mode",
    "source_dictionnaire", "lexique_utiliser_lemmes", "expression_utiliser_dictionnaire",
    "nettoyage_caracteres", "supprimer_ponctuation", "supprimer_chiffres", "supprimer_apostrophes",
    "remplacer_tirets_espaces", "retirer_stopwords", "filtrage_morpho", "pos_lexique_a_conserver",
    "morpho_conserver_hors_lexique", "afc_reduire_chevauchement", "afc_taille_mots", "top_n"
  )
  
  capturer_parametres_analyse <- function() {
    valeurs <- lapply(ids_parametres_analyse, function(id) isolate(input[[id]]))
    names(valeurs) <- ids_parametres_analyse
    valeurs <- valeurs[!vapply(valeurs, is.null, logical(1))]
    
    if (length(valeurs)) {
      rv$parametres_analyse <- modifyList(rv$parametres_analyse, valeurs, keep.null = FALSE)
    }
    
    rv$parametres_analyse
  }
  
  output$nom_fichier_selectionne <- renderText({
    if (!is.null(input$fichier_corpus$name) && nzchar(input$fichier_corpus$name)) {
      return(as.character(input$fichier_corpus$name))
    }
    "Aucun fichier choisi"
  })
  
  capturer_parametres_analyse <- function() {
    # Capture défensive des paramètres UI sans dépendre de modifyList.
    defaults <- list(
      modele_chd = "iramuteq",
      segment_size = 40,
      segmenter_sur_ponctuation_forte = FALSE,
      iramuteq_max_formes = 20000,
      iramuteq_mincl_mode = "auto",
      iramuteq_mincl = 5,
      iramuteq_classif_mode = "simple",
      iramuteq_rst1 = 12,
      iramuteq_rst2 = 14,
      iramuteq_svd_method = "irlba",
      source_dictionnaire = "lexique_fr",
      expression_utiliser_dictionnaire = FALSE,
      utiliser_stopwords = FALSE,
      min_docfreq = 3,
      max_p = 0.05,
      afc_taille_mots = "frequency",
      top_n = 20
    )
    
    entrees <- reactiveValuesToList(input)
    out <- defaults
    for (nm in intersect(names(defaults), names(entrees))) {
      val <- entrees[[nm]]
      if (!is.null(val)) out[[nm]] <- val
    }
    
    rv$parametres_analyse <- out
    out
  }
  
  ouvrir_modal_parametres <- function() {
    defaults <- capturer_parametres_analyse()
    
    showModal(modalDialog(
      title = "Paramétrages de l'analyse",
      easyClose = TRUE,
      size = "m",
      ui_form_parametres_analyse(defaults = defaults),
      footer = tagList(
        modalButton("Fermer"),
        actionButton("lancer", "Lancer l'analyse", class = "btn-primary")
      )
    ))
  }
  
  ouvrir_modal_parametres_similitudes <- function() {
    showModal(modalDialog(
      title = "Paramètres de l'analyse de similitudes",
      easyClose = TRUE,
      size = "m",
      ui_form_parametres_similitudes(),
      footer = tagList(
        modalButton("Fermer"),
        actionButton("lancer_simi", "Lancer l'analyse de similitudes", class = "btn-primary")
      )
    ))
  }
  
  journaliser_evenement <- function(message) {
    if (exists("ajouter_log", mode = "function", inherits = TRUE)) {
      ajouter_log(rv, message)
      return(invisible(NULL))
    }
    
    msg <- as.character(message)
    msg <- msg[!is.na(msg)]
    msg <- msg[nzchar(msg)]
    if (!length(msg)) return(invisible(NULL))
    msg <- paste(msg, collapse = " ")
    
    horodatage <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    msg_horodate <- paste0("[", horodatage, "] ", msg)
    
    precedent <- rv$logs
    if (is.null(precedent) || !length(precedent) || all(is.na(precedent)) || !any(nzchar(precedent))) {
      rv$logs <- msg_horodate
    } else {
      precedent <- precedent[!is.na(precedent)]
      precedent <- precedent[nzchar(precedent)]
      rv$logs <- paste(c(precedent, msg_horodate), collapse = "\n")
    }
    
    message("[IRaMuTeQ-lite] ", msg_horodate)
    flush.console()
    
    invisible(NULL)
  }
  
  observeEvent(input$nav_principal, {
    if (input$nav_principal %in% c("chd", "resultats_chd")) {
      ouvrir_modal_parametres()
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ouvrir_param_chd_depuis_nav, {
    ouvrir_modal_parametres()
  }, ignoreInit = TRUE)
  
  observeEvent(input$nav_principal, {
    if (isTRUE(identical(input$nav_principal, "similitudes"))) {
      ouvrir_modal_parametres_similitudes()
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$ouvrir_param_simi, {
    ouvrir_modal_parametres_similitudes()
  })
  
  observeEvent(input$simi_zoom_in, {
    rv$simi_zoom <- min(3, rv$simi_zoom + 0.2)
  })
  
  observeEvent(input$simi_zoom_out, {
    rv$simi_zoom <- max(0.6, rv$simi_zoom - 0.2)
  })
  
  observeEvent(input$simi_zoom_reset, {
    rv$simi_zoom <- 1
  })
  
  observeEvent(input$lancer_simi, {
    removeModal()
    
    if (is.null(rv$dfm) || quanteda::ndoc(rv$dfm) < 2 || quanteda::nfeat(rv$dfm) < 2) {
      showNotification("Analyse CHD/AFC non disponible: lancez d'abord l'analyse principale pour construire le graphe de similitude.", type = "warning")
      return(invisible(NULL))
    }
    
    res_simi <- tryCatch(
      construire_graphe_similitudes(
        dfm_obj = rv$dfm,
        method = input$simi_method,
        seuil = input$simi_seuil,
        max_tree = isTRUE(input$simi_max_tree),
        top_terms = input$simi_top_terms,
        layout_type = input$simi_layout,
        communities = isTRUE(input$simi_communities),
        community_method = input$simi_community_method
      ),
      error = function(e) e
    )
    
    if (inherits(res_simi, "error")) {
      showNotification(paste0("Erreur analyse similitudes: ", res_simi$message), type = "error")
      journaliser_evenement(paste0("Erreur analyse similitudes: ", res_simi$message))
      return(invisible(NULL))
    }
    
    rv$simi_graph <- res_simi$graph
    rv$simi_layout <- res_simi$layout
    rv$simi_vertex_freq <- res_simi$vertex_freq
    rv$simi_method <- res_simi$method
    rv$simi_seuil_applique <- res_simi$seuil
    rv$simi_communities <- res_simi$communities
    rv$simi_terms_used <- res_simi$n_terms_used
    rv$simi_terms_total <- res_simi$n_terms_total
    rv$simi_top_terms_requested <- res_simi$top_terms_requested
    
    rv$statut <- paste0(
      "Graphe de similitudes généré — méthode: ", rv$simi_method,
      ", sommets: ", igraph::vcount(rv$simi_graph),
      ", arêtes: ", igraph::ecount(rv$simi_graph)
    )
    journaliser_evenement(rv$statut)
    if (igraph::ecount(rv$simi_graph) == 0) {
      showNotification("Aucune arête après filtrage. Diminuez le seuil ou augmentez le nombre de termes.", type = "warning")
    } else {
      showNotification("Graphe de similitudes généré.", type = "message")
    }
  })
  
  output$ui_simi_statut <- renderUI({
    seuil_label <- if (is.null(input$simi_seuil) || is.na(input$simi_seuil)) "aucun" else as.character(input$simi_seuil)
    n_vertices <- if (!is.null(rv$simi_graph) && inherits(rv$simi_graph, "igraph")) igraph::vcount(rv$simi_graph) else 0
    n_edges <- if (!is.null(rv$simi_graph) && inherits(rv$simi_graph, "igraph")) igraph::ecount(rv$simi_graph) else 0
    
    tags$div(
      style = "border:1px solid #d9e2ef; background:#f8fbff; border-radius:6px; padding:12px;",
      tags$strong("Configuration actuelle"),
      tags$ul(
        tags$li(paste0("Méthode: ", if (is.null(input$simi_method)) "cooc" else input$simi_method)),
        tags$li(paste0("Seuil: ", seuil_label)),
        tags$li(paste0("Top termes: ", if (is.null(input$simi_top_terms)) 40 else input$simi_top_terms)),
        tags$li(paste0("Layout: ", if (is.null(input$simi_layout)) "frutch" else input$simi_layout)),
        tags$li(paste0("Arbre max: ", if (isTRUE(input$simi_max_tree)) "oui" else "non")),
        tags$li(paste0("Labels des arêtes: ", if (isTRUE(input$simi_edge_labels)) "oui" else "non")),
        tags$li(paste0("Communautés: ", if (isTRUE(input$simi_communities)) "oui" else "non")),
        tags$li(paste0("Méthode communautés: ", if (is.null(input$simi_community_method)) "edge_betweenness" else input$simi_community_method)),
        tags$li(paste0("Halo: ", if (isTRUE(input$simi_halo)) "oui" else "non")),
        tags$li(paste0("Mots conservés: ", rv$simi_terms_used, " / ", rv$simi_terms_total,
                       " (top demandé = ", rv$simi_top_terms_requested, ")")),
        tags$li(paste0("Zoom affichage: x", format(round(rv$simi_zoom, 2), nsmall = 2))),
        tags$li(paste0("Graphe courant: ", n_vertices, " sommets / ", n_edges, " arêtes"))
      )
    )
  })
  
  observeEvent(input$menu_importer_fichier_sidebar, {
    showModal(modalDialog(
      title = "Importer un fichier corpus",
      easyClose = TRUE,
      size = "s",
      fileInput("fichier_corpus", "Choisir un fichier .txt", accept = c(".txt")),
      footer = modalButton("Fermer")
    ))
  })
  
  observeEvent(input$fichier_corpus, {
    req(input$fichier_corpus$datapath)
    lignes_auto <- tryCatch(readLines(input$fichier_corpus$datapath, encoding = "UTF-8", warn = FALSE), error = function(e) character(0))
    if (length(lignes_auto) > 0) {
      updateTextAreaInput(session, "annotation_corpus_text", value = paste(lignes_auto, collapse = "\n"))
    }
    removeModal()
  }, ignoreInit = TRUE)
  
  output$ui_corpus_preview <- renderUI({
    fichier <- input$fichier_corpus
    if (is.null(fichier) || is.null(fichier$datapath) || !file.exists(fichier$datapath)) {
      return(tags$p("Aucun corpus importé pour le moment."))
    }
    
    lignes <- tryCatch(
      readLines(fichier$datapath, encoding = "UTF-8", warn = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(lignes) || length(lignes) == 0) {
      return(tags$p("Le corpus importé est vide ou illisible."))
    }
    
    max_lignes <- 250
    extrait <- lignes[seq_len(min(length(lignes), max_lignes))]
    texte <- paste(extrait, collapse = "\n")
    
    if (length(lignes) > max_lignes) {
      texte <- paste0(
        texte,
        "\n\n… Aperçu limité aux ", max_lignes,
        " premières lignes (", length(lignes), " lignes au total)."
      )
    }
    
    tags$div(
      tags$p(
        style = "margin-bottom: 8px;",
        paste0("Fichier : ", fichier$name)
      ),
      tags$pre(
        style = "white-space: pre-wrap; max-height: 70vh; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background: #fafafa;",
        texte
      )
    )
  })
  
  .annotation_regex_escape <- function(x) {
    x <- as.character(x)
    if (!nzchar(x)) return("")
    meta <- c("\\", ".", "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?")
    chars <- strsplit(x, "", fixed = TRUE)[[1]]
    chars <- vapply(chars, function(ch) if (ch %in% meta) paste0("\\", ch) else ch, character(1))
    paste(chars, collapse = "")
  }
  
  surligner_occurrences_annotation <- function(texte, terme) {
    texte <- as.character(texte)
    terme <- trimws(as.character(terme))
    
    if (!nzchar(texte)) {
      return(tags$em("Aucun corpus chargé dans la zone d'annotation."))
    }
    if (!nzchar(terme)) {
      return(tags$div(
        style = "white-space: pre-wrap; max-height: 320px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background: #fafafa; border-radius: 6px;",
        htmltools::htmlEscape(texte)
      ))
    }
    
    motif <- .annotation_regex_escape(terme)
    locs <- gregexpr(motif, texte, ignore.case = TRUE, perl = TRUE)[[1]]
    
    if (length(locs) == 1L && identical(locs, -1L)) {
      return(tags$em("Aucune occurrence trouvée pour la sélection actuelle."))
    }
    
    lens <- attr(locs, "match.length")
    morceaux <- vector("list", length(locs) * 2 + 1)
    pos <- 1L
    k <- 1L
    
    for (i in seq_along(locs)) {
      deb <- locs[[i]]
      fin <- deb + lens[[i]] - 1L
      if (deb > pos) {
        morceaux[[k]] <- htmltools::htmlEscape(substr(texte, pos, deb - 1L))
        k <- k + 1L
      }
      morceaux[[k]] <- tags$span(class = "highlight", htmltools::htmlEscape(substr(texte, deb, fin)))
      k <- k + 1L
      pos <- fin + 1L
    }
    
    if (pos <= nchar(texte)) {
      morceaux[[k]] <- htmltools::htmlEscape(substr(texte, pos, nchar(texte)))
    }
    
    tags$div(
      style = "white-space: pre-wrap; max-height: 320px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background: #fafafa; border-radius: 6px;",
      do.call(tagList, morceaux)
    )
  }
  
  output$ui_annotation_highlight_preview <- renderUI({
    surligner_occurrences_annotation(input$annotation_corpus_text, input$annotation_selection)
  })
  
  observeEvent(input$annotation_selection, {
    txt <- trimws(as.character(input$annotation_selection))
    if (!nzchar(txt)) return(invisible(NULL))
    norm <- tolower(txt)
    norm <- gsub("[’`´ʼʹ]", "'", norm, perl = TRUE)
    norm <- gsub("\\s+", "_", norm, perl = TRUE)
    norm <- gsub("[^[:alnum:]_àâäáãåéèêëíìîïóòôöõúùûüçœñ]", "", norm, perl = TRUE)
    updateTextInput(session, "annotation_norm", value = norm)
  }, ignoreInit = TRUE)
  
  output$annotation_corpus_colore <- renderUI({
    texte <- if (is.null(input$annotation_corpus_text)) "" else as.character(input$annotation_corpus_text)
    if (!nzchar(trimws(texte))) {
      return(tags$div(
        style = "white-space: pre-wrap; max-height: 280px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background: #fafafa;",
        "Aucun texte à annoter pour le moment."
      ))
    }
    
    expr_df <- rv$expression_annotations_df
    if (is.null(expr_df) || !is.data.frame(expr_df) || nrow(expr_df) == 0 || !"dic_mot" %in% names(expr_df)) {
      return(tags$div(
        style = "white-space: pre-wrap; max-height: 280px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background: #fafafa;",
        htmltools::HTML(htmltools::htmlEscape(texte))
      ))
    }
    
    motifs <- tolower(trimws(as.character(expr_df$dic_mot)))
    motifs <- motifs[nzchar(motifs)]
    motifs <- unique(motifs)
    motifs <- motifs[order(nchar(motifs), decreasing = TRUE)]
    
    construire_motif_regex <- function(motif) {
      motif <- gsub("[’`´ʼʹ]", "'", motif, perl = TRUE)
      chars <- strsplit(motif, "", fixed = TRUE)[[1]]
      out <- character(length(chars))
      for (k in seq_along(chars)) {
        ch <- chars[[k]]
        if (ch == "'") {
          out[[k]] <- "['’`´ʼʹ]"
        } else {
          special <- c("\\", "^", "$", ".", "|", "?", "*", "+", "(", ")", "[", "]", "{", "}")
          out[[k]] <- if (ch %in% special) paste0("\\", ch) else ch
        }
      }
      paste0(out, collapse = "")
    }
    
    texte_hl <- texte
    for (motif in motifs) {
      motif_regex <- construire_motif_regex(motif)
      regex <- paste0("(?i)(?<![[:alnum:]_])(", motif_regex, ")(?![[:alnum:]_])")
      texte_hl <- gsub(regex, "<span class='highlight'>\\1</span>", texte_hl, perl = TRUE)
    }
    
    texte_safe <- echapper_segments_en_preservant_surlignage(
      texte_hl,
      "<span class='highlight'>",
      "</span>"
    )
    
    tags$div(
      style = "white-space: pre-wrap; max-height: 280px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background: #fafafa;",
      htmltools::HTML(texte_safe)
    )
  })
  
  observeEvent(input$annotation_add_entry, {
    dic_mot <- tolower(trimws(as.character(input$annotation_selection)))
    dic_norm <- tolower(trimws(as.character(input$annotation_norm)))
    dic_morpho <- trimws(as.character(input$annotation_morpho))
    dic_mot <- gsub("[’`´ʼʹ]", "'", dic_mot, perl = TRUE)
    dic_norm <- gsub("[’`´ʼʹ]", "'", dic_norm, perl = TRUE)
    if (!nzchar(dic_mot) || !nzchar(dic_norm)) {
      showNotification("dic_mot et dic_norm sont obligatoires.", type = "error")
      return(invisible(NULL))
    }
    df <- rv$expression_annotations_df
    if (is.null(df) || !is.data.frame(df)) {
      df <- data.frame(dic_mot = character(0), dic_norm = character(0), dic_morpho = character(0), stringsAsFactors = FALSE)
    }
    idx <- match(dic_mot, df$dic_mot)
    if (!is.na(idx)) {
      df$dic_norm[idx] <- dic_norm
      df$dic_morpho[idx] <- dic_morpho
    } else {
      df <- rbind(df, data.frame(dic_mot = dic_mot, dic_norm = dic_norm, dic_morpho = dic_morpho, stringsAsFactors = FALSE))
    }
    rv$expression_annotations_df <- df[order(df$dic_mot), , drop = FALSE]
    sauvegarder_add_expression(rv$expression_annotations_df)
    showNotification("Entrée dictionnaire enregistrée (session).", type = "message")
  })
  
  observeEvent(input$annotation_remove_entry, {
    key <- tolower(trimws(as.character(input$annotation_remove_key)))
    if (!nzchar(key)) return(invisible(NULL))
    df <- rv$expression_annotations_df
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(invisible(NULL))
    rv$expression_annotations_df <- df[df$dic_mot != key, , drop = FALSE]
    sauvegarder_add_expression(rv$expression_annotations_df)
    showNotification("Entrée supprimée (si existante).", type = "message")
  })
  
  observeEvent(input$annotation_import_csv, {
    f <- input$annotation_import_csv
    if (is.null(f) || is.null(f$datapath) || !file.exists(f$datapath)) return(invisible(NULL))
    
    tryCatch({
      df <- lire_add_expression_depuis_upload(f$datapath)
      if (is.null(df) || nrow(df) == 0) {
        showNotification("CSV invalide/vide : colonnes attendues dic_mot et dic_norm (séparateur ; ou , accepté).", type = "warning")
        return(invisible(NULL))
      }
      rv$expression_annotations_df <- df
      rv$utiliser_add_expression <- TRUE
      sauvegarder_add_expression(rv$expression_annotations_df)
      showNotification(paste0("Dictionnaire importé depuis ", f$name, " (", nrow(df), " entrées) et activé pour l'analyse."), type = "message")
      invisible(NULL)
    }, error = function(e) {
      showNotification(paste0("Erreur pendant l'import du dictionnaire : ", conditionMessage(e)), type = "error")
      invisible(NULL)
    })
  })
  
  output$table_annotation_dict <- renderTable({
    df <- rv$expression_annotations_df
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
      return(data.frame(info = "Aucune entrée annotée.", stringsAsFactors = FALSE))
    }
    df
  }, striped = TRUE, bordered = TRUE, hover = TRUE, spacing = "xs", width = "100%")
  
  output$dl_expression_csv <- downloadHandler(
    filename = function() "add_expression_fr.csv",
    content = function(file) {
      df <- rv$expression_annotations_df
      if (is.null(df) || !is.data.frame(df)) {
        df <- data.frame(dic_mot = character(0), dic_norm = character(0), dic_morpho = character(0), stringsAsFactors = FALSE)
      }
      if (!"dic_morpho" %in% names(df)) df$dic_morpho <- ""
      df$dic_morpho[is.na(df$dic_morpho)] <- ""
      utils::write.csv2(df, file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
    }
  )
  
  
  output$ui_table_stats_corpus <- renderUI({
    req(rv$stats_corpus_df)
    
    definitions <- c(
      "Nom du corpus" = "Nom du fichier corpus importé.",
      "Nombre de textes" = "Nombre d'unités de texte détectées dans le corpus.",
      "Nombre de mots dans le corpus" = "Total des occurrences de mots (tokens).",
      "Nombre de formes" = "Nombre de formes lexicales distinctes (types), différent des hapax.",
      "Nombre de segments de texte" = "Nombre de segments après découpage pour l'analyse.",
      "Nombre d'Hapax" = "Nombre de formes apparaissant une seule fois dans le corpus.",
      "Loi de Zpif" = "Indicateur de conformité approximative à la loi de Zipf."
    )
    
    lignes <- lapply(seq_len(nrow(rv$stats_corpus_df)), function(i) {
      metrique <- as.character(rv$stats_corpus_df$Metrique[i])
      valeur <- as.character(rv$stats_corpus_df$Valeur[i])
      definition <- unname(definitions[[metrique]])
      if (is.null(definition) || !nzchar(definition)) definition <- ""
      
      tags$tr(
        tags$td(
          tags$div(metrique),
          if (nzchar(definition)) tags$div(
            style = "font-size: 0.85em; color: #c62828; margin-top: 2px;",
            definition
          )
        ),
        tags$td(valeur)
      )
    })
    
    tags$table(
      class = "table table-striped table-condensed",
      tags$thead(
        tags$tr(
          tags$th("Metrique"),
          tags$th("Valeur")
        )
      ),
      tags$tbody(lignes)
    )
  })
  
  
  output$plot_stats_zipf <- renderPlot({
    req(zone_trace_disponible("plot_stats_zipf", min_width = 180, min_height = 180))
    req(rv$stats_zipf_df)
    df <- rv$stats_zipf_df
    if (is.null(df) || nrow(df) < 2) {
      plot.new()
      text(0.5, 0.5, "Données insuffisantes pour tracer la loi de Zpif.", cex = 1.1)
      return(invisible(NULL))
    }
    
    x_lim <- range(df$log_rang, na.rm = TRUE)
    y_lim <- range(c(df$log_frequence, df$log_pred), na.rm = TRUE)
    
    plot(
      x = df$log_rang,
      y = df$log_frequence,
      pch = 16,
      cex = 0.8,
      col = grDevices::adjustcolor("#2C7FB8", alpha.f = 0.7),
      xlab = "log(rang)",
      ylab = "log(fréquence)",
      main = "Loi de Zpif",
      xlim = x_lim,
      ylim = y_lim,
      asp = 1
    )
    grid(col = "#E6E6E6", lty = "dotted")
    
    ord <- order(df$log_rang)
    lines(df$log_rang[ord], df$log_pred[ord], col = "#D7301F", lwd = 2.5)
    
    legend(
      "topright",
      legend = c("Données", "Régression log-log"),
      col = c("#2C7FB8", "#D7301F"),
      pch = c(16, NA),
      lty = c(NA, 1),
      lwd = c(NA, 2),
      bty = "n"
    )
  })
  
  output$ui_chd_statut <- renderUI({
    if (length(packages_manquants) > 0) {
      return(tags$div(
        style = "border: 1px solid #f5c2c7; background: #f8d7da; color: #842029; border-radius: 4px; padding: 10px; margin-bottom: 10px;",
        tags$strong("Dépendances manquantes : "),
        paste(packages_manquants, collapse = ", ")
      ))
    }
    
    if (is.null(rv$res)) {
      return(tags$p("CHD non disponible. Lance une analyse."))
    }
    
    nb_classes <- NA_integer_
    if (!is.null(rv$clusters)) nb_classes <- length(rv$clusters)
    
    if (identical(rv$res_type, "iramuteq")) {
      return(tags$p(paste0("CHD disponible (moteur IRaMuTeQ-like) - classes détectées : ", nb_classes, ".")))
    }
    
    tags$p(paste0("CHD disponible (moteur IRaMuTeQ-like) - classes détectées : ", nb_classes, "."))
  })
  
  output$table_classes <- renderTable({
    classes <- integer(0)
    
    if (!is.null(rv$filtered_corpus) && quanteda::ndoc(rv$filtered_corpus) > 0) {
      classes_docs <- quanteda::docvars(rv$filtered_corpus, "Classes")
      classes <- suppressWarnings(as.integer(classes_docs))
    }
    
    if (!length(classes) && !is.null(rv$res_stats_df) && is.data.frame(rv$res_stats_df) && nrow(rv$res_stats_df) > 0) {
      col_classe <- intersect(c("Classe", "classe", "cluster", "Class"), names(rv$res_stats_df))
      if (length(col_classe) > 0) {
        classes <- suppressWarnings(as.integer(rv$res_stats_df[[col_classe[[1]]]]))
      }
    }
    
    classes <- classes[is.finite(classes) & classes > 0]
    if (!length(classes)) {
      return(data.frame(Message = "Aucune classe valide détectée.", stringsAsFactors = FALSE))
    }
    
    tab <- table(classes)
    tab <- tab[order(as.integer(names(tab)))]
    pct <- round(100 * as.numeric(tab) / sum(tab), 1)
    data.frame(
      Classe = paste0("Classe ", names(tab)),
      Effectif = as.integer(tab),
      Pourcentage = paste0(format(pct, nsmall = 1), " %"),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)
  
  output$plot_simi <- renderPlot({
    req(zone_trace_disponible("plot_simi", min_width = 240, min_height = 220))
    info_txt <- paste0(
      "Méthode: ", rv$simi_method,
      " | Mots conservés: ", rv$simi_terms_used, "/", rv$simi_terms_total,
      " (top demandé=", rv$simi_top_terms_requested, ")"
    )
    
    tracer_graphe_similitudes(
      g = rv$simi_graph,
      layout = rv$simi_layout,
      edge_labels = isTRUE(input$simi_edge_labels),
      main = "Graphe de similitude",
      communities = rv$simi_communities,
      halo = isTRUE(input$simi_halo),
      zoom = rv$simi_zoom,
      info_text = info_txt
    )
  })
  
  output$plot_chd_iramuteq_dendro <- renderPlot({
    req(zone_trace_disponible("plot_chd_iramuteq_dendro", min_width = 200, min_height = 180))
    if (is.null(rv$res) && is.null(rv$res_chd)) {
      plot.new()
      text(0.5, 0.5, "Dendrogramme CHD indisponible. Lance une analyse.", cex = 1.1)
      return(invisible(NULL))
    }

    style_dendro <- input$chd_dendro_style %||% "iramuteq_bars"
    package_par_style <- c(
      factoextra = "factoextra",
      iramuteq_bars = NA_character_
    )
    pkg <- unname(package_par_style[[style_dendro]])
    pkg_requis <- is.character(pkg) && length(pkg) == 1 && !is.na(pkg) && nzchar(pkg)
    if (isTRUE(pkg_requis) && !requireNamespace(pkg, quietly = TRUE)) {
      plot.new()
      text(
        0.5,
        0.5,
        paste0("Le package '", pkg, "' est requis pour ce style de dendrogramme.\nInstallez-le puis relancez l'analyse."),
        cex = 1.0
      )
      return(invisible(NULL))
    }

    tracer_dendrogramme_iramuteq_ui(
      rv = rv,
      top_n_terms = 4,
      orientation = "horizontal",
      style_affichage = style_dendro
    )
  })
  
  register_events_lancer(input, output, session, rv)
  
  observeEvent(input$lancer, {
    capturer_parametres_analyse()
    rv$afc_zoom_terms <- NULL
  }, ignoreInit = TRUE)
  
  output$plot_afc_classes <- renderPlot({
    req(zone_trace_disponible("plot_afc_classes", min_width = 220, min_height = 220))
    if (est_texte_non_vide(rv$afc_erreur)) {
      plot.new()
      text(0.5, 0.5, "AFC indisponible (erreur).", cex = 1.1)
      return(invisible(NULL))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      plot.new()
      text(0.5, 0.5, "AFC non disponible. Lance une analyse.", cex = 1.1)
      return(invisible(NULL))
    }
    tracer_afc_classes_seules(rv$afc_obj, axes = c(1, 2), cex_labels = 1.05)
  })
  
  output$ui_tables_stats_chd_iramuteq <- renderUI({
    req(rv$res_stats_df)
    
    col_classes <- intersect(c("cluster", "classe", "Classe", "Class"), names(rv$res_stats_df))
    req(length(col_classes) > 0)
    classes <- unique(as.character(rv$res_stats_df[[col_classes[[1]]]]))
    classes <- classes[!is.na(classes) & nzchar(classes)]
    classes <- sort(classes)
    req(length(classes) > 0)
    
    panneaux <- lapply(classes, function(cl) {
      output_id <- paste0("table_stats_chd_iramuteq_cl_", cl)
      
      output[[output_id]] <- renderTable({
        extraire_stats_chd_classe(
          rv$res_stats_df,
          classe = cl,
          n_max = NULL,
          show_negative = FALSE,
          max_p = 1,
          seuil_p_significativite = input$max_p,
          style = "iramuteq_clone"
        )
      }, rownames = FALSE, sanitize.text.function = function(x) x)

      enregistrer_popup_forme_stats_chd(
        input = input,
        rv = rv,
        classe = cl,
        output_id = output_id,
        seuil_p_significativite = function() input$max_p
      )
      
      tabPanel(
        title = paste0("Classe ", cl),
        tableOutput(output_id)
      )
    })
    
    do.call(tabsetPanel, c(id = "tabs_stats_chd_iramuteq", panneaux))
  })
  
  .calculer_limites_afc_termes <- function(obj, axes = c(1, 2)) {
    if (is.null(obj) || is.null(obj$rowcoord) || is.null(obj$colcoord) || is.null(obj$termes_stats)) return(NULL)
    
    ax1 <- axes[1]
    ax2 <- axes[2]
    st <- obj$termes_stats
    st <- st[!is.na(st$Terme) & nzchar(st$Terme), , drop = FALSE]
    st <- st[st$Terme %in% rownames(obj$colcoord), , drop = FALSE]
    if (nrow(st) < 2) return(NULL)
    
    xy_m <- obj$colcoord[st$Terme, , drop = FALSE]
    x_m <- xy_m[, ax1]
    y_m <- xy_m[, ax2]
    x_c <- obj$rowcoord[, ax1]
    y_c <- obj$rowcoord[, ax2]
    
    lim <- calculer_lim_sym(c(x_m, x_c), c(y_m, y_c))
    list(x = lim, y = lim)
  }
  
  .zoomer_limites <- function(lims, facteur = 1) {
    if (!is.list(lims) || is.null(lims$x) || is.null(lims$y)) return(lims)
    
    x <- suppressWarnings(as.numeric(lims$x))
    y <- suppressWarnings(as.numeric(lims$y))
    if (length(x) != 2 || any(!is.finite(x)) || length(y) != 2 || any(!is.finite(y))) return(lims)
    
    cx <- mean(x)
    cy <- mean(y)
    hx <- abs(diff(range(x))) / 2
    hy <- abs(diff(range(y))) / 2
    if (!is.finite(hx) || hx == 0 || !is.finite(hy) || hy == 0) return(lims)
    
    facteur <- as.numeric(facteur)
    if (!is.finite(facteur) || facteur <= 0) facteur <- 1
    
    list(
      x = c(cx - hx * facteur, cx + hx * facteur),
      y = c(cy - hy * facteur, cy + hy * facteur)
    )
  }
  
  output$plot_afc <- renderPlot({
    req(zone_trace_disponible("plot_afc", min_width = 220, min_height = 220))
    if (est_texte_non_vide(rv$afc_erreur)) {
      plot.new()
      text(0.5, 0.5, "AFC indisponible (erreur).", cex = 1.1)
      return(invisible(NULL))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      plot.new()
      text(0.5, 0.5, "AFC non disponible. Lance une analyse.", cex = 1.1)
      return(invisible(NULL))
    }
    
    activer_repel <- TRUE
    if (!is.null(input$afc_reduire_chevauchement)) activer_repel <- isTRUE(input$afc_reduire_chevauchement)
    
    taille_sel <- "frequency"
    taille_sel_input <- as.character(input$afc_taille_mots)
    if (length(taille_sel_input) > 0 && !is.na(taille_sel_input[[1]]) && nzchar(taille_sel_input[[1]])) {
      taille_sel <- taille_sel_input[[1]]
    }
    if (!taille_sel %in% c("frequency", "chi2")) taille_sel <- "frequency"
    
    top_termes <- 120
    if (!is.null(input$afc_top_termes) && is.finite(input$afc_top_termes)) top_termes <- as.integer(input$afc_top_termes)
    
    limites_base <- .calculer_limites_afc_termes(rv$afc_obj, axes = c(1, 2))
    
    xlim_zoom <- NULL
    ylim_zoom <- NULL
    if (is.list(rv$afc_zoom_terms)) {
      x_vals <- suppressWarnings(as.numeric(rv$afc_zoom_terms$x))
      y_vals <- suppressWarnings(as.numeric(rv$afc_zoom_terms$y))
      if (length(x_vals) == 2 && all(is.finite(x_vals))) xlim_zoom <- sort(x_vals)
      if (length(y_vals) == 2 && all(is.finite(y_vals))) ylim_zoom <- sort(y_vals)
    }
    
    if (is.null(xlim_zoom) && is.list(limites_base)) xlim_zoom <- limites_base$x
    if (is.null(ylim_zoom) && is.list(limites_base)) ylim_zoom <- limites_base$y
    
    tracer_afc_classes_termes(
      rv$afc_obj,
      axes = c(1, 2),
      top_termes = top_termes,
      taille_sel = taille_sel,
      activer_repel = activer_repel,
      xlim_zoom = xlim_zoom,
      ylim_zoom = ylim_zoom
    )
  })
  
  observeEvent(input$afc_brush, {
    b <- input$afc_brush
    if (is.null(b)) return()
    if (!all(c("xmin", "xmax", "ymin", "ymax") %in% names(b))) return()
    vals <- suppressWarnings(as.numeric(c(b$xmin, b$xmax, b$ymin, b$ymax)))
    if (length(vals) != 4 || any(!is.finite(vals))) return()
    rv$afc_zoom_terms <- list(x = sort(vals[1:2]), y = sort(vals[3:4]))
  })
  
  observeEvent(input$afc_zoom_in, {
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) return()
    limites_base <- .calculer_limites_afc_termes(rv$afc_obj, axes = c(1, 2))
    limites_courantes <- rv$afc_zoom_terms
    if (!is.list(limites_courantes) || is.null(limites_courantes$x) || is.null(limites_courantes$y)) {
      limites_courantes <- limites_base
    }
    rv$afc_zoom_terms <- .zoomer_limites(limites_courantes, facteur = 0.75)
  })
  
  observeEvent(input$afc_zoom_out, {
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) return()
    limites_base <- .calculer_limites_afc_termes(rv$afc_obj, axes = c(1, 2))
    limites_courantes <- rv$afc_zoom_terms
    if (!is.list(limites_courantes) || is.null(limites_courantes$x) || is.null(limites_courantes$y)) {
      limites_courantes <- limites_base
    }
    lim_zoom <- .zoomer_limites(limites_courantes, facteur = 1.33)
    if (is.list(limites_base)) {
      lim_zoom$x <- c(
        max(min(lim_zoom$x), min(limites_base$x)),
        min(max(lim_zoom$x), max(limites_base$x))
      )
      lim_zoom$y <- c(
        max(min(lim_zoom$y), min(limites_base$y)),
        min(max(lim_zoom$y), max(limites_base$y))
      )
    }
    rv$afc_zoom_terms <- lim_zoom
  })
  
  observeEvent(input$afc_zoom_reset, {
    rv$afc_zoom_terms <- NULL
  })
  
  output$ui_table_afc_mots_par_classe <- renderUI({
    if (is.null(rv$afc_table_mots)) {
      output$table_afc_mots_message <- renderTable({
        data.frame(Message = "AFC mots : non disponible.", stringsAsFactors = FALSE)
      }, rownames = FALSE)
      return(tableOutput("table_afc_mots_message"))
    }
    
    df <- rv$afc_table_mots
    colonnes <- intersect(c("Terme", "Classe_max", "frequency", "chi2", "p_value", "Segment_texte"), names(df))
    df <- df[, colonnes, drop = FALSE]
    if ("p_value" %in% names(df)) {
      df$p_value <- ifelse(
        is.na(df$p_value),
        NA_character_,
        formatC(df$p_value, format = "f", digits = 6)
      )
    }
    
    classes <- unique(as.character(df$Classe_max))
    classes <- classes[!is.na(classes) & nzchar(classes)]
    classes <- sort(classes)
    
    if (length(classes) == 0) {
      output$table_afc_mots_message <- renderTable({
        data.frame(Message = "AFC mots : aucune classe disponible.", stringsAsFactors = FALSE)
      }, rownames = FALSE)
      return(tableOutput("table_afc_mots_message"))
    }
    
    ui_tables <- lapply(seq_along(classes), function(i) {
      cl <- classes[[i]]
      id <- paste0("table_afc_mots_", i)
      
      output[[id]] <- renderUI({
        sous_df <- df[df$Classe_max == cl, , drop = FALSE]
        colonnes <- intersect(c("Terme", "frequency", "chi2", "p_value", "Segment_texte"), names(sous_df))
        sous_df <- sous_df[, colonnes, drop = FALSE]
        
        if ("p_value" %in% names(sous_df)) {
          sous_df$p_value <- ifelse(
            is.na(sous_df$p_value),
            NA_character_,
            formatC(sous_df$p_value, format = "f", digits = 6)
          )
        }
        
        if ("chi2" %in% names(sous_df)) {
          sous_df <- sous_df[order(-sous_df$chi2), , drop = FALSE]
          sous_df$chi2 <- ifelse(
            is.na(sous_df$chi2),
            NA_character_,
            formatC(sous_df$chi2, format = "f", digits = 6)
          )
        }
        
        sous_df <- head(sous_df, 100)
        generer_table_html_afc_mots(sous_df)
      })
      
      tagList(
        tags$h5(cl),
        uiOutput(id)
      )
    })
    
    do.call(tagList, ui_tables)
  })
  
  output$plot_afc_vars <- renderPlot({
    req(zone_trace_disponible("plot_afc_vars", min_width = 220, min_height = 220))
    if (est_texte_non_vide(rv$afc_vars_erreur)) {
      plot.new()
      text(0.5, 0.5, "AFC variables étoilées indisponible (erreur).", cex = 1.1)
      return(invisible(NULL))
    }
    if (is.null(rv$afc_vars_obj) || is.null(rv$afc_vars_obj$ca)) {
      plot.new()
      text(0.5, 0.5, "AFC variables étoilées non disponible. Lance une analyse.", cex = 1.1)
      return(invisible(NULL))
    }
    
    activer_repel <- TRUE
    if (!is.null(input$afc_reduire_chevauchement)) activer_repel <- isTRUE(input$afc_reduire_chevauchement)
    
    top_mod <- 120
    if (!is.null(input$afc_top_modalites) && is.finite(input$afc_top_modalites)) top_mod <- as.integer(input$afc_top_modalites)
    
    tracer_afc_variables_etoilees(rv$afc_vars_obj, axes = c(1, 2), top_modalites = top_mod, activer_repel = activer_repel)
  })
  
  output$table_afc_vars <- renderTable({
    if (is.null(rv$afc_table_vars)) {
      return(data.frame(Message = "AFC variables étoilées : non disponible.", stringsAsFactors = FALSE))
    }
    df <- rv$afc_table_vars
    colonnes <- intersect(c("Modalite", "Classe_max", "frequency", "chi2", "p_value"), names(df))
    df <- df[, colonnes, drop = FALSE]
    if ("p_value" %in% names(df)) {
      p_values <- df$p_value
      df$p_value <- ifelse(
        is.na(p_values),
        NA_character_,
        ifelse(
          p_values > 0.05,
          sprintf("<span style='color:#d97706;font-weight:600;'>%s</span>", formatC(p_values, format = "f", digits = 6)),
          formatC(p_values, format = "f", digits = 6)
        )
      )
    }
    if ("chi2" %in% names(df)) df <- df[order(-df$chi2), , drop = FALSE]
    if ("chi2" %in% names(df)) {
      df$chi2 <- ifelse(
        is.na(df$chi2),
        NA_character_,
        formatC(df$chi2, format = "f", digits = 6)
      )
    }
    head(df, 200)
  }, rownames = FALSE, sanitize.text.function = function(x) x)
  
  output$table_afc_eig <- renderTable({
    if (est_texte_non_vide(rv$afc_erreur)) {
      return(data.frame(Message = "AFC indisponible (erreur).", stringsAsFactors = FALSE))
    }
    if (is.null(rv$afc_obj) || is.null(rv$afc_obj$ca)) {
      return(data.frame(Message = "AFC non disponible.", stringsAsFactors = FALSE))
    }
    eig <- rv$afc_obj$ca$eig
    if (is.null(eig)) return(data.frame(Message = "Valeurs propres indisponibles.", stringsAsFactors = FALSE))
    df <- as.data.frame(eig)
    df$Dim <- rownames(df)
    rownames(df) <- NULL
    df <- df[, c("Dim", names(df)[1], names(df)[2], names(df)[3]), drop = FALSE]
    names(df) <- c("Dim", "Valeur_propre", "Pourcentage_inertie", "Pourcentage_cumule")
    df
  }, rownames = FALSE)
  
  output$dl_segments <- downloadHandler(
    filename = function() "segments_par_classe.txt",
    content = function(file) {
      req(rv$segments_file)
      file.copy(rv$segments_file, file, overwrite = TRUE)
    }
  )
  
  output$dl_stats <- downloadHandler(
    filename = function() "stats_par_classe.csv",
    content = function(file) {
      req(rv$stats_file)
      file.copy(rv$stats_file, file, overwrite = TRUE)
    }
  )
  
  output$dl_html <- downloadHandler(
    filename = function() "segments_par_classe.html",
    content = function(file) {
      req(rv$html_file)
      file.copy(rv$html_file, file, overwrite = TRUE)
    }
  )
  
  output$dl_zip <- downloadHandler(
    filename = function() "exports_iramuteq_like.zip",
    content = function(file) {
      req(rv$export_dir)
      zip_tmp <- tempfile(fileext = ".zip")
      creer_zip_depuis_dossier(rv$export_dir, zip_tmp)
      file.copy(zip_tmp, file, overwrite = TRUE)
    }
  )
  
}

app <- shinyApp(ui = ui, server = server)
app
