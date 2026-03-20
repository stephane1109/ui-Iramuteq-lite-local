# Rôle du fichier: ui_simi_iramuteq.R regroupe l'UI spécifique à l'analyse de similitudes.

ui_form_parametres_similitudes <- function() {
  tagList(
    tags$p(
      "Paramétrez l'analyse de similitudes (Vergès). ",
      "Ces options prépareront la matrice et l'affichage du graphe de similitude."
    ),
    selectInput(
      "simi_method",
      "Méthode de calcul",
      choices = c(
        "Cooccurrence" = "cooc",
        "Jaccard" = "jaccard",
        "Binomiale" = "binom"
      ),
      selected = "cooc"
    ),
    numericInput(
      "simi_seuil",
      "Seuil minimal des arêtes (laisser vide pour aucun seuil)",
      value = NA,
      min = 0,
      step = 1
    ),
    numericInput(
      "simi_top_terms",
      "Nombre de termes à conserver (plus fréquents)",
      value = 100,
      min = 5,
      step = 1
    ),
    selectizeInput(
      "simi_terms_selected",
      "Termes à analyser (triés par fréquence)",
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = list(
        placeholder = "Sélectionnez un ou plusieurs termes",
        plugins = list("remove_button")
      )
    ),
    checkboxInput(
      "simi_max_tree",
      "Limiter au graphe couvrant maximal (arbre de poids max)",
      value = TRUE
    ),
    selectInput(
      "simi_layout",
      "Type de layout",
      choices = c(
        "Fruchterman-Reingold" = "frutch",
        "Kamada-Kawai" = "kawa",
        "Circulaire" = "circle",
        "Aléatoire" = "random",
        "Spirale" = "spirale"
      ),
      selected = "frutch"
    ),
    selectInput(
      "simi_view_mode",
      "Type d'affichage du graphe",
      choices = c(
        "Interactif (visNetwork)" = "interactive",
        "Statique (igraph avec halo)" = "igraph"
      ),
      selected = "interactive"
    ),
    checkboxInput(
      "simi_edge_labels",
      "Afficher les labels des arêtes",
      value = FALSE
    ),
    checkboxInput(
      "simi_edge_width_by_index",
      "Largeur des arêtes proportionnelle à l'indice",
      value = TRUE
    ),
    checkboxInput(
      "simi_vertex_text_by_freq",
      "Taille du texte des sommets proportionnelle aux fréquences",
      value = FALSE
    ),
    checkboxInput(
      "simi_communities",
      "Communautés",
      value = FALSE
    ),
    conditionalPanel(
      condition = "input.simi_communities == true",
      selectInput(
        "simi_community_method",
        "Méthode de communautés",
        choices = c(
          "edge.betweenness.community" = "edge_betweenness",
          "fastgreedy.community" = "fast_greedy",
          "label.propagation.community" = "label_propagation",
          "leading.eigenvector.community" = "leading_eigen",
          "multilevel.community" = "multilevel",
          "walktrap.community" = "walktrap"
        ),
        selected = "edge_betweenness"
      ),
      checkboxInput(
        "simi_halo",
        "Halo",
        value = FALSE
      )
    )
  )
}

ui_panel_similitudes_iramuteq <- function() {
  nav_panel(
    "Analyse similitudes", value = "similitudes",
    tags$h3("Analyse de similitudes (Vergès)"),
    tags$p("Ouvrez la boîte de dialogue pour configurer les paramètres de l'analyse."),
    tags$p(
      style = "background:#f8fbff; border:1px solid #d9e2ef; border-radius:6px; padding:10px;",
      "Fonctionnement: le graphe est construit à partir du DFM de l'analyse principale. ",
      "On conserve les N mots les plus fréquents (Top termes), puis on calcule les liens selon l'indice choisi ",
      "(cooccurrence / jaccard / binom). Le seuil supprime les arêtes trop faibles."
    ),
    tags$div(
      style = "display:flex; gap:8px; flex-wrap:wrap; margin-bottom:12px; align-items:center;",
      actionButton("ouvrir_param_simi", "Paramétrer l'analyse de similitudes", class = "btn-primary"),
      actionButton("simi_zoom_in", "Zoom +"),
      actionButton("simi_zoom_out", "Zoom -"),
      actionButton("simi_zoom_reset", "Réinitialiser zoom")
    ),
    uiOutput("ui_simi_statut"),
    tags$div(style = "max-width: 1400px;", uiOutput("plot_simi_container"))
  )
}
