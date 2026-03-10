# Rôle du fichier: composant explorateur (Phase 2) pour naviguer corpus/analyses.

ui_explorateur_iramuteq <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    full_screen = TRUE,
    bslib::card_header(tags$strong("Explorateur")),
    bslib::card_body(
      tags$div(
        style = "max-height: 360px; overflow-y: auto; margin-bottom: 10px;",
        uiOutput(ns("tree"))
      ),
      selectInput(ns("target"), "Accès rapide", choices = c()),
      actionButton(ns("open"), "Ouvrir", class = "btn-primary btn-sm")
    )
  )
}

server_explorateur_iramuteq <- function(id, rv, nom_corpus_reactif) {
  moduleServer(id, function(input, output, session) {

    output$tree <- renderUI({
      nom_corpus <- nom_corpus_reactif()
      if (is.null(nom_corpus) || !nzchar(nom_corpus)) {
        nom_corpus <- "Aucun corpus importé"
      }

      assets <- rv$explor_assets
      wc_df <- if (!is.null(assets) && !is.null(assets$wordclouds)) assets$wordclouds else data.frame()
      n_wc <- if (is.data.frame(wc_df)) nrow(wc_df) else 0

      tags$ul(
        style = "padding-left: 16px; margin-bottom: 0;",
        tags$li(tags$strong("Corpus"), tags$ul(tags$li(nom_corpus))),
        tags$li(
          tags$strong("Analyses"),
          tags$ul(
            tags$li("CHD"),
            tags$li(sprintf("Nuages de mots (%s)", n_wc)),
            tags$li("AFC"),
            tags$li("Concordancier")
          )
        )
      )
    })

    observe({
      choices <- c(
        "Analyse" = "analyse",
        "Corpus" = "corpus",
        "AFC" = "afc",
        "Résultats CHD > Dendrogramme" = "res_dendro",
        "Résultats CHD > Stats CHD" = "res_stats",
        "Résultats CHD > Concordancier" = "res_concord",
        "Résultats CHD > Nuage de mots" = "res_wc"
      )
      updateSelectInput(session, "target", choices = choices, selected = "analyse")
    })

    observeEvent(input$open, {
      cible <- input$target
      session_root <- session$rootScope

      if (identical(cible, "analyse")) {
        bslib::nav_select("nav_principal", selected = "analyse", session = session_root)
      } else if (identical(cible, "corpus")) {
        bslib::nav_select("nav_principal", selected = "corpus", session = session_root)
      } else if (identical(cible, "afc")) {
        bslib::nav_select("nav_principal", selected = "afc", session = session_root)
      } else if (cible %in% c("res_dendro", "res_stats", "res_concord", "res_wc")) {
        bslib::nav_select("nav_principal", selected = "resultats_chd", session = session_root)

        sous_onglet <- switch(
          cible,
          "res_dendro" = "Dendrogramme",
          "res_stats" = "Stats CHD",
          "res_concord" = "Concordancier IRaMuTeQ-lite",
          "res_wc" = "Nuage de mots"
        )
        updateTabsetPanel(session_root, "tabs_resultats_chd_iramuteq", selected = sous_onglet)
      }
    })
  })
}
