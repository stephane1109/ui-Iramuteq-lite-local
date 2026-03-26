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
