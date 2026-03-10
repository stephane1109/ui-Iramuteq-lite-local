# Rôle du fichier: composant explorateur (Phase 2) pour naviguer corpus/analyses.

ui_explorateur_iramuteq <- function(id) {
  ns <- shiny::NS(id)

  bslib::card(
    full_screen = FALSE,
    bslib::card_header(tags$strong("Explorateur")),
    bslib::card_body(
      uiOutput(ns("tree")),
      tags$hr(style = "margin: 8px 0;"),
      selectInput(ns("target"), "Accès rapide", choices = c()),
      actionButton(ns("open"), "Ouvrir", class = "btn-primary btn-sm")
    )
  )
}

server_explorateur_iramuteq <- function(id, rv, nom_corpus_reactif) {
  moduleServer(id, function(input, output, session) {
    output$tree <- renderUI({
      nom_corpus <- nom_corpus_reactif()
      if (is.null(nom_corpus) || !nzchar(nom_corpus)) nom_corpus <- "Aucun corpus"

      tags$ul(
        style = "padding-left: 18px;",
        tags$li(sprintf("corpus : %s", nom_corpus)),
        tags$li("chd"),
        tags$li("wordcloud"),
        tags$li("afc")
      )
    })

    observe({
      updateSelectInput(
        session,
        "target",
        choices = c(
          "Fichier" = "fichier",
          "Analyse" = "analyse",
          "Corpus" = "corpus",
          "CHD" = "res_dendro",
          "Wordcloud" = "res_wc",
          "AFC" = "afc"
        ),
        selected = "analyse"
      )
    })

    observeEvent(input$open, {
      cible <- input$target
      session_root <- session$rootScope

      if (cible %in% c("fichier", "analyse", "corpus", "afc")) {
        bslib::nav_select("nav_principal", selected = cible, session = session_root)
      } else if (cible %in% c("res_dendro", "res_wc")) {
        bslib::nav_select("nav_principal", selected = "resultats_chd", session = session_root)
        updateTabsetPanel(
          session_root,
          "tabs_resultats_chd_iramuteq",
          selected = if (identical(cible, "res_dendro")) "Dendrogramme" else "Nuage de mots"
        )
      }
    })
  })
}
