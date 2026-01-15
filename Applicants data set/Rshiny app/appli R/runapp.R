# ============================================================
# calculetteRWA - run_app.R (ROBUSTE / STRUCTURE A PLAT)
# Lancement Shiny portable Windows (R-4.5.1)
# Arbo: calculetteRWA/
#   run_app.R, app.R, 00_config.R ... 07_*.R
#   R/, library/, tmp/, logs/
# ============================================================

# --------- util sans %||% ----------
`%or%` <- function(a, b) if (!is.null(a) && length(a) && nzchar(as.character(a))) a else b

# --------- récupérer dossier du script (robuste) ----------
get_script_dir <- function() {
  # Rscript --file=...
  ca <- commandArgs(trailingOnly = FALSE)
  farg <- grep("^--file=", ca, value = TRUE)
  if (length(farg) == 1) {
    p <- sub("^--file=", "", farg)
    return(normalizePath(dirname(p), winslash = "/", mustWork = FALSE))
  }
  # source(".../run_app.R")
  for (i in rev(seq_along(sys.frames()))) {
    of <- tryCatch(sys.frames()[[i]]$ofile, error = function(e) NULL)
    if (!is.null(of) && nzchar(of)) return(normalizePath(dirname(of), winslash = "/", mustWork = FALSE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

# --------- log ----------
log_line <- function(path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  cat(txt, "\n", file = path, append = TRUE)
}

# --------- main ----------
APP_HOME <- get_script_dir()

# logs/tmp
dir.create(file.path(APP_HOME, "logs"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(APP_HOME, "tmp"),  recursive = TRUE, showWarnings = FALSE)

LOG_FILE <- file.path(APP_HOME, "logs", "startup.log")
log_line(LOG_FILE, "=== START calculetteRWA ===")
log_line(LOG_FILE, "APP_HOME =", APP_HOME)
log_line(LOG_FILE, "getwd()  =", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
log_line(LOG_FILE, "R        =", R.version.string)

# Vérifier fichiers attendus en structure à plat
must_files <- c(
  "app.R",
  "00_config.R","01_helpers.R","02_io_retour4c.R","03_calc_rwa.R",
  "04_module1_retour4c_server.R","05_module2_anomalies_server.R",
  "06_module3_impacts_server.R","07_module4_calcul_server.R"
)
missing_files <- must_files[!file.exists(file.path(APP_HOME, must_files))]
if (length(missing_files) > 0) {
  log_line(LOG_FILE, "ERROR fichiers manquants:", paste(missing_files, collapse = ", "))
  stop(
    paste0(
      "Structure racine incomplète.\nFichiers manquants:\n- ",
      paste(missing_files, collapse = "\n- "),
      "\n\n=> Ils doivent être dans le dossier racine calculetteRWA.\n",
      "Log: ", LOG_FILE
    ),
    call. = FALSE
  )
}

# Fix TEMP local (évite temp Windows saturé / réseau)
TMP <- file.path(APP_HOME, "tmp")
Sys.setenv(TMPDIR = TMP, TEMP = TMP, TMP = TMP)
log_line(LOG_FILE, "TMPDIR =", TMP)

# Force libs offline : calculetteRWA/library puis calculetteRWA/R/library
APP_LIB  <- file.path(APP_HOME, "library")
BASE_LIB <- file.path(APP_HOME, "R", "library")

if (!dir.exists(APP_LIB))  dir.create(APP_LIB, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(BASE_LIB)) log_line(LOG_FILE, "WARN: R/library introuvable:", BASE_LIB)

Sys.setenv(R_LIBS_USER = "")
.libPaths(c(APP_LIB, BASE_LIB))
log_line(LOG_FILE, ".libPaths =", paste(.libPaths(), collapse = " | "))

# Vérifier packages
pkgs <- c("shiny","bslib","shinyjs","data.table","DT","ggplot2","readxl","openxlsx")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  log_line(LOG_FILE, "ERROR packages manquants:", paste(missing_pkgs, collapse = ", "))
  stop(
    paste0(
      "Packages manquants dans calculetteRWA/library:\n- ",
      paste(missing_pkgs, collapse = "\n- "),
      "\n\n=> Installer/copier ces packages (avec dépendances) dans calculetteRWA/library.\n",
      "Log: ", LOG_FILE
    ),
    call. = FALSE
  )
}

# Options utiles
options(shiny.maxRequestSize = 500 * 1024^2)
options(calculetteRWA.home = APP_HOME)

# Lancer Shiny sur le dossier racine (app.R est à la racine)
log_line(LOG_FILE, "Launching shiny::runApp(appDir=APP_HOME)")
tryCatch({
  shiny::runApp(
    appDir = APP_HOME,
    launch.browser = TRUE,
    host = "127.0.0.1",
    port = 0
  )
}, error = function(e) {
  log_line(LOG_FILE, "FATAL:", conditionMessage(e))
  stop(paste0("Erreur lancement Shiny: ", conditionMessage(e), "\nLog: ", LOG_FILE), call. = FALSE)
})

log_line(LOG_FILE, "=== STOP calculetteRWA ===")