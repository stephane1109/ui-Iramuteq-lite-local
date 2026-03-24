required_packages <- c(
  "shiny", "bslib", "htmltools", "quanteda", "wordcloud", "RColorBrewer",
  "igraph", "dplyr", "remotes", "rgexf", "Matrix", "factoextra", "FactoMineR",
  "ggplot2", "plotly", "visNetwork", "DT", "jsonlite", "sna", "intergraph",
  "colorspace", "rgl", "reticulate", "spacyr"
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

assurer_env_spacy <- function(
  envname = trimws(Sys.getenv("IRAMUTEQ_SPACY_ENV", unset = "iramuteq-spacy")),
  model = trimws(Sys.getenv("IRAMUTEQ_SPACY_MODEL", unset = "fr_core_news_sm"))
) {
  if (!requireNamespace("reticulate", quietly = TRUE)) return(invisible(FALSE))
  if (!nzchar(envname)) envname <- "iramuteq-spacy"
  if (!nzchar(model)) model <- "fr_core_news_sm"

  py_sys <- Sys.which("python3")
  if (!nzchar(py_sys)) py_sys <- Sys.which("python")

  log_parts <- character(0)
  ok <- tryCatch({
    env_root <- reticulate::virtualenv_root()
    env_path <- file.path(env_root, envname)
    py_env <- file.path(env_path, "bin", "python")

    if (!file.exists(py_env)) {
      out_create <- capture.output(
        reticulate::virtualenv_create(envname = envname, python = if (nzchar(py_sys)) py_sys else NULL),
        type = "output"
      )
      log_parts <<- c(log_parts, paste(out_create, collapse = "\n"))
    }

    reticulate::use_virtualenv(envname, required = TRUE)
    Sys.setenv(RETICULATE_PYTHON = py_env)
    options(iramuteq_python_bin = py_env)

    if (!reticulate::py_module_available("spacy")) {
      out_install <- capture.output(reticulate::py_install("spacy", envname = envname, pip = TRUE), type = "output")
      log_parts <<- c(log_parts, paste(out_install, collapse = "\n"))
    }

    probe_model <- paste0(
      "import importlib.util,sys;sys.exit(0 if importlib.util.find_spec('",
      model,
      "') else 1)"
    )
    probe <- suppressWarnings(system2(py_env, c("-c", probe_model), stdout = TRUE, stderr = TRUE))
    status_probe <- attr(probe, "status")
    if (is.null(status_probe)) status_probe <- 0L
    if (!identical(as.integer(status_probe), 0L)) {
      out_model <- suppressWarnings(system2(py_env, c("-m", "spacy", "download", model), stdout = TRUE, stderr = TRUE))
      log_parts <<- c(log_parts, paste(out_model, collapse = "\n"))
    }

    probe2 <- suppressWarnings(system2(py_env, c("-c", probe_model), stdout = TRUE, stderr = TRUE))
    status_probe2 <- attr(probe2, "status")
    if (is.null(status_probe2)) status_probe2 <- 0L
    identical(as.integer(status_probe2), 0L)
  }, error = function(e) {
    log_parts <<- c(log_parts, paste("assurer_env_spacy error:", conditionMessage(e)))
    FALSE
  })

  options(iramuteq_spacy_env_last_log = paste(log_parts, collapse = "\n\n"))
  invisible(ok)
}

# Bootstrap d'un environnement Python dédié à spaCy pour exécution packagée/compilée.
bootstrap_spacy <- tolower(trimws(Sys.getenv("IRAMUTEQ_BOOTSTRAP_SPACY", unset = "1"))) %in% c("1", "true", "yes", "y", "on")
if (bootstrap_spacy) {
  assurer_env_spacy()
}

IRAMUTEQ_GLOBAL_INIT_DONE <- TRUE
