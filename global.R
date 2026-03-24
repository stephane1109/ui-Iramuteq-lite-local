required_packages <- c(
  "shiny", "bslib", "htmltools", "quanteda", "wordcloud", "RColorBrewer",
  "igraph", "dplyr", "remotes", "rgexf", "Matrix", "factoextra", "FactoMineR",
  "ggplot2", "plotly", "visNetwork", "DT", "jsonlite", "sna", "intergraph",
  "colorspace", "rgl"
)

installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed_packages)
packages_manquants <- missing_packages

cran_repo <- trimws(Sys.getenv("R_CRAN_MIRROR", unset = "https://cloud.r-project.org"))
if (is.null(getOption("repos")) || identical(getOption("repos")[["CRAN"]], "@CRAN")) {
  options(repos = c(CRAN = cran_repo))
}

if (length(missing_packages) > 0) {
  tryCatch(
    install.packages(missing_packages, repos = cran_repo, dependencies = TRUE),
    error = function(e) message("Installation des packages manquants impossible: ", conditionMessage(e))
  )
  installed_packages <- rownames(installed.packages())
  packages_manquants <- setdiff(required_packages, installed_packages)
}

charger_packages_requis <- function(packages) {
  for (pkg in packages) {
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    )
  }
}

charger_packages_requis(required_packages)
installed_packages <- rownames(installed.packages())

if (!"FactoMineR" %in% installed_packages) {
  remotes::install_github("husson/FactoMineR", dependencies = NA, upgrade = "never")
}

IRAMUTEQ_GLOBAL_INIT_DONE <- TRUE
