# Interface Shiny complète pour piloter une analyse LDA via Python.
# Le script gère :
# - chargement d'un corpus texte,
# - choix du mode d'unité d'analyse (document / segment),
# - paramétrage du modèle LDA,
# - affichage des résultats,
# - génération et visualisation des nuages de mots.

library(shiny)
library(jsonlite)
library(DT)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' Recherche un interpréteur Python local.
#' @return Chemin de l'exécutable python.
trouver_python <- function() {
  candidats <- c(Sys.which("python3"), Sys.which("python"))
  candidats <- candidats[nzchar(candidats)]
  if (length(candidats) == 0) {
    stop("Aucun interpréteur Python n'a été trouvé (python3/python).")
  }
  return(candidats[[1]])
}

#' Lit un corpus texte UTF-8 depuis un fichier.
#' @param chemin_fichier Chemin complet du fichier source.
#' @return Texte brut unique.
charger_corpus <- function(chemin_fichier) {
  lignes <- readLines(chemin_fichier, warn = FALSE, encoding = "UTF-8")
  texte <- paste(lignes, collapse = "\n")
  return(texte)
}

#' Exécute un script Python de manière robuste et renvoie l'erreur éventuelle.
#' @param python_exec Exécutable Python.
#' @param script_path Chemin du script Python.
#' @param args Vecteur d'arguments.
executer_script_python <- function(python_exec, script_path, args) {
  sortie <- character(0)
  erreur <- character(0)

  code <- tryCatch(
    {
      status <- system2(
        command = python_exec,
        args = c(script_path, args),
        stdout = TRUE,
        stderr = TRUE
      )
      sortie <<- status
      as.integer(attr(status, "status") %||% 0L)
    },
    error = function(e) {
      erreur <<- conditionMessage(e)
      1L
    }
  )

  list(
    code = code,
    logs = c(sortie, erreur)
  )
}

#' Construit le tableau des mots par topic pour affichage.
#' @param resultat_lda Liste JSON issue de lda.py
#' @return data.frame exploitable par DT
construire_table_topics <- function(resultat_lda) {
  topics <- resultat_lda$topics
  if (is.null(topics) || length(topics) == 0) {
    return(data.frame())
  }

  lignes <- lapply(topics, function(t) {
    mots <- vapply(t$mots, function(m) {
      paste0(m$mot, " (", sprintf("%.2f", as.numeric(m$poids)), ")")
    }, FUN.VALUE = character(1))

    data.frame(
      Topic = t$topic,
      `Mots pondérés` = paste(mots, collapse = " | "),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, lignes)
}

ui <- fluidPage(
  titlePanel("Analyse LDA (R/Shiny + Python)"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "corpus_file",
        label = "Charger un corpus texte",
        multiple = FALSE,
        accept = c("text/plain", ".txt")
      ),
      tags$small("Chaque document est supposé commencer par le marqueur ****."),
      hr(),
      radioButtons(
        inputId = "mode_unite",
        label = "Unité d'analyse",
        choices = c(
          "Document (bloc commençant par ****)" = "document",
          "Segment de texte (ponctuation simple)" = "segment"
        ),
        selected = "document"
      ),
      numericInput(
        inputId = "longueur_min_segment",
        label = "Longueur minimale des segments (caractères)",
        value = 50,
        min = 5,
        max = 1000,
        step = 5
      ),
      numericInput(
        inputId = "nb_topics",
        label = "Nombre de topics",
        value = 5,
        min = 1,
        max = 50,
        step = 1
      ),
      numericInput(
        inputId = "nb_mots_par_topic",
        label = "Nombre de mots affichés par topic",
        value = 10,
        min = 3,
        max = 50,
        step = 1
      ),
      actionButton("lancer", "Lancer l'analyse LDA", class = "btn-primary")
    ),
    mainPanel(
      h4("État du traitement"),
      verbatimTextOutput("message_statut"),
      hr(),
      h4("Résumé"),
      tableOutput("table_meta"),
      hr(),
      h4("Mots pondérés par topic"),
      DTOutput("table_topics"),
      hr(),
      h4("Nuages de mots LDA"),
      uiOutput("nuages_ui")
    )
  )
)

server <- function(input, output, session) {
  resultats <- reactiveVal(NULL)
  resultats_nuages <- reactiveVal(NULL)
  message_erreur <- reactiveVal("En attente d'exécution.")

  observeEvent(input$lancer, {
    req(input$corpus_file)

    message_erreur("Traitement en cours...")
    resultats(NULL)
    resultats_nuages(NULL)

    tryCatch({
      texte_corpus <- charger_corpus(input$corpus_file$datapath)
      if (!nzchar(trimws(texte_corpus))) {
        stop("Le corpus est vide.")
      }

      python_exec <- trouver_python()
      script_lda <- normalizePath("lda/lda.py", mustWork = TRUE)
      script_wordcloud <- normalizePath("lda/wordcloud_lda.py", mustWork = TRUE)

      prefixe_fichier <- format(Sys.time(), "%Y%m%d_%H%M%S")
      fichier_entree <- tempfile(pattern = paste0("entree_lda_", prefixe_fichier, "_"), tmpdir = "lda", fileext = ".json")
      fichier_sortie_lda <- tempfile(pattern = paste0("sortie_lda_", prefixe_fichier, "_"), tmpdir = "lda", fileext = ".json")
      fichier_sortie_nuages <- tempfile(pattern = paste0("sortie_nuages_", prefixe_fichier, "_"), tmpdir = "lda", fileext = ".json")
      dossier_images <- file.path("lda", paste0("wordclouds_", prefixe_fichier))
      dir.create(dossier_images, recursive = TRUE, showWarnings = FALSE)

      donnees_entree <- list(
        corpus_texte = texte_corpus,
        mode_unite = input$mode_unite,
        longueur_min_segment = as.integer(input$longueur_min_segment),
        nb_topics = as.integer(input$nb_topics),
        nb_mots_par_topic = as.integer(input$nb_mots_par_topic),
        random_state = 42
      )

      write_json(donnees_entree, path = fichier_entree, auto_unbox = TRUE, pretty = TRUE)

      retour_lda <- executer_script_python(
        python_exec = python_exec,
        script_path = script_lda,
        args = c("--input", fichier_entree, "--output", fichier_sortie_lda)
      )

      if (!file.exists(fichier_sortie_lda)) {
        stop("Le script lda.py n'a pas généré de fichier de sortie.")
      }

      resultat_lda <- fromJSON(fichier_sortie_lda, simplifyVector = TRUE)
      if (!isTRUE(resultat_lda$succes)) {
        details <- resultat_lda$erreur %||% "Erreur inconnue dans lda.py"
        stop(details)
      }

      resultats(resultat_lda)

      retour_nuages <- executer_script_python(
        python_exec = python_exec,
        script_path = script_wordcloud,
        args = c(
          "--input", fichier_sortie_lda,
          "--output", fichier_sortie_nuages,
          "--output-dir", dossier_images,
          "--prefix", "nuage_lda"
        )
      )

      if (!file.exists(fichier_sortie_nuages)) {
        stop("Le script wordcloud_lda.py n'a pas généré de fichier de sortie.")
      }

      resultat_nuages <- fromJSON(fichier_sortie_nuages, simplifyVector = TRUE)
      if (!isTRUE(resultat_nuages$succes)) {
        warning_msg <- resultat_nuages$erreur %||% "Échec de génération des nuages de mots."
        message_erreur(paste(
          "Analyse LDA terminée, mais génération des nuages incomplète :",
          warning_msg
        ))
      } else {
        resultats_nuages(resultat_nuages)
      }

      logs_lda <- paste(retour_lda$logs, collapse = "\n")
      logs_nuages <- paste(retour_nuages$logs, collapse = "\n")

      message_erreur(paste(
        "Analyse terminée avec succès.",
        if (nzchar(logs_lda)) paste("\n[Logs lda.py]\n", logs_lda) else "",
        if (nzchar(logs_nuages)) paste("\n[Logs wordcloud_lda.py]\n", logs_nuages) else ""
      ))
    }, error = function(e) {
      message_erreur(paste("Erreur:", conditionMessage(e)))
    })
  })

  output$message_statut <- renderText({
    message_erreur()
  })

  output$table_meta <- renderTable({
    res <- resultats()
    req(res)

    as.data.frame(res$meta, check.names = FALSE)
  }, rownames = FALSE)

  output$table_topics <- renderDT({
    res <- resultats()
    req(res)

    table_topics <- construire_table_topics(res)
    datatable(table_topics, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$nuages_ui <- renderUI({
    nuages <- resultats_nuages()

    if (is.null(nuages) || !isTRUE(nuages$succes) || length(nuages$fichiers) == 0) {
      return(tags$p("Aucun nuage de mots disponible."))
    }

    # Gestion robuste : nuages$fichiers peut être data.frame ou liste selon jsonlite.
    fichiers_df <- as.data.frame(nuages$fichiers, stringsAsFactors = FALSE)

    blocs <- lapply(seq_len(nrow(fichiers_df)), function(i) {
      chemin_image <- fichiers_df$image[i]
      topic_num <- fichiers_df$topic[i]

      if (!file.exists(chemin_image)) {
        return(tags$div(
          tags$h5(sprintf("Topic %s", topic_num)),
          tags$p("Image introuvable sur disque :"),
          tags$code(chemin_image)
        ))
      }

      uri <- paste0("file:///", gsub("\\\\", "/", normalizePath(chemin_image, winslash = "/", mustWork = FALSE)))

      tags$div(
        style = "margin-bottom: 24px;",
        tags$h5(sprintf("Topic %s", topic_num)),
        tags$img(src = uri, style = "max-width: 100%; height: auto; border: 1px solid #ddd;"),
        tags$br(),
        tags$a(href = uri, target = "_blank", "Ouvrir l'image dans un nouvel onglet")
      )
    })

    do.call(tagList, blocs)
  })
}

shinyApp(ui = ui, server = server)
