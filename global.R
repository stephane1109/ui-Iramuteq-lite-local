required_packages <- c(
  "shiny", "bslib", "htmltools", "quanteda", "wordcloud", "RColorBrewer",
  "igraph", "dplyr", "remotes", "rgexf", "Matrix", "factoextra", "FactoMineR",
  "ggplot2", "plotly", "visNetwork", "DT", "jsonlite", "sna", "intergraph",
  "shinyFiles",
  "colorspace", "rgl", "reticulate", "topicmodels"
)

cran_repo <- trimws(Sys.getenv("R_CRAN_MIRROR", unset = "https://cloud.r-project.org"))
if (is.null(getOption("repos")) || identical(getOption("repos")[["CRAN"]], "@CRAN")) {
  options(repos = c(CRAN = cran_repo))
}

configurer_librairie_utilisateur <- function() {
  lib_cible <- Sys.getenv("IRAMUTEQ_R_LIBS_USER", unset = "")
  if (!nzchar(lib_cible)) {
    lib_cible <- file.path(path.expand("~"), ".local", "share", "iramuteq-lite", "R", "library")
  }

  if (!dir.exists(lib_cible)) {
    dir.create(lib_cible, recursive = TRUE, showWarnings = FALSE)
  }

  if (dir.exists(lib_cible)) {
    .libPaths(unique(c(lib_cible, .libPaths())))
  }
  invisible(.libPaths())
}

installer_packages_manquants <- function(packages, repo = cran_repo) {
  packages_absents <- setdiff(packages, rownames(installed.packages()))
  if (!length(packages_absents)) return(character(0))

  tryCatch(
    install.packages(
      packages_absents,
      repos = repo,
      dependencies = TRUE,
      lib = .libPaths()[1]
    ),
    error = function(e) {
      message("Installation des packages manquants impossible: ", conditionMessage(e))
    }
  )
  setdiff(packages, rownames(installed.packages()))
}

charger_packages_requis <- function(packages, repo = cran_repo) {
  packages_absents <- installer_packages_manquants(packages, repo = repo)
  for (pkg in packages) {
    if (pkg %in% packages_absents) next
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE))
  }
  packages_absents
}

configurer_librairie_utilisateur()
packages_manquants <- charger_packages_requis(required_packages, repo = cran_repo)
installed_packages <- rownames(installed.packages())

if (!"FactoMineR" %in% installed_packages && "remotes" %in% installed_packages) {
  tryCatch(
    remotes::install_github(
      "husson/FactoMineR",
      dependencies = NA,
      upgrade = "never",
      lib = .libPaths()[1]
    ),
    error = function(e) message("Installation GitHub de FactoMineR impossible: ", conditionMessage(e))
  )
}

if (length(packages_manquants) > 0) {
  message(
    "Packages non installés automatiquement: ",
    paste(packages_manquants, collapse = ", "),
    ". Utilise installer_environnement_application() pour relancer."
  )
}

# Dépendances Python requises pour le pipeline LDA.
packages_python_lda <- c(
  sklearn = "scikit-learn",
  wordcloud = "wordcloud",
  matplotlib = "matplotlib"
)

trouver_python_global <- function() {
  candidats <- c(Sys.which("python3"), Sys.which("python"))
  candidats <- candidats[nzchar(candidats)]
  if (!length(candidats)) return("")
  candidats[[1]]
}

module_python_disponible <- function(python_exec, module_name) {
  if (!nzchar(python_exec)) return(FALSE)
  out <- suppressWarnings(system2(python_exec, args = c("-c", sprintf("import %s", module_name)), stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (is.null(status)) status <- 0L
  as.integer(status) == 0L
}

installer_packages_python_lda <- function() {
  python_exec <- trouver_python_global()
  if (!nzchar(python_exec)) {
    message("Python non trouvé: installation auto des dépendances LDA ignorée.")
    return(invisible(FALSE))
  }

  manquants <- names(packages_python_lda)[!vapply(names(packages_python_lda), function(m) module_python_disponible(python_exec, m), logical(1))]
  if (!length(manquants)) return(invisible(TRUE))

  paquets <- unname(unlist(packages_python_lda[manquants]))
  message("Installation des dépendances Python LDA: ", paste(paquets, collapse = ", "))

  logs <- suppressWarnings(system2(python_exec, args = c("-m", "pip", "install", "--user", paquets), stdout = TRUE, stderr = TRUE))
  echec <- names(packages_python_lda)[!vapply(names(packages_python_lda), function(m) module_python_disponible(python_exec, m), logical(1))]
  if (length(echec)) {
    message("Échec installation dépendances Python LDA: ", paste(unname(unlist(packages_python_lda[echec])), collapse = ", "))
    message(paste(logs, collapse = "\n"))
    return(invisible(FALSE))
  }

  invisible(TRUE)
}

# Tentative d'installation auto des dépendances Python LDA au démarrage.
tryCatch(
  installer_packages_python_lda(),
  error = function(e) message("Installation Python LDA ignorée: ", conditionMessage(e))
)

installer_environnement_application <- function(envname = NULL) {
  configurer_librairie_utilisateur()
  cran_repo <- trimws(Sys.getenv("R_CRAN_MIRROR", unset = "https://cloud.r-project.org"))
  if (is.null(getOption("repos")) || identical(getOption("repos")[["CRAN"]], "@CRAN")) {
    options(repos = c(CRAN = cran_repo))
  }

  missing_packages <- setdiff(required_packages, rownames(installed.packages()))
  ok_r <- TRUE
  if (length(missing_packages) > 0) {
    ok_r <- tryCatch({
      install.packages(
        missing_packages,
        repos = cran_repo,
        dependencies = TRUE,
        lib = .libPaths()[1]
      )
      TRUE
    }, error = function(e) {
      message("installer_environnement_application/install.packages error: ", conditionMessage(e))
      FALSE
    })
  }

  invisible(list(ok_r = ok_r, envname = envname))
}

IRAMUTEQ_GLOBAL_INIT_DONE <- TRUE
