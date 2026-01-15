# ============================================================
# calculetteRWA - run_app.R (ROBUSTE + DIAGNOSTIC)
# Objectif : lancer Shiny portable (sans dépendre du getwd)
# ============================================================

# ---------- petite utilité (sans %||%) ----------
`%or%` <- function(a, b) if (!is.null(a) && length(a) && nzchar(as.character(a))) a else b

# ---------- récupérer le dossier du script (robuste) ----------
get_script_dir <- function() {
  # Cas Rscript --file=...
  ca <- commandArgs(trailingOnly = FALSE)
  farg <- grep("^--file=", ca, value = TRUE)
  if (length(farg) == 1) {
    p <- sub("^--file=", "", farg)
    return(normalizePath(dirname(p), winslash = "/", mustWork = FALSE))
  }

  # Cas source(".../run_app.R") : ofile dans sys.frames
  for (i in rev(seq_along(sys.frames()))) {
    of <- tryCatch(sys.frames()[[i]]$ofile, error = function(e) NULL)
    if (!is.null(of) && nzchar(of)) return(normalizePath(dirname(of), winslash = "/", mustWork = FALSE))
  }

  # Fallback
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

# ---------- logger ----------
log_line <- function(path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  cat(txt, "\n", file = path, append = TRUE)
}

# ---------- chercher la racine calculetteRWA ----------
find_app_root <- function(start_dir) {
  d <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)

  for (k in 0:6) {  # remonte jusqu'à 6 niveaux
    cand <- d
    if (file.exists(file.path(cand, "app", "app.R"))) return(cand)
    d <- normalizePath(file.path(d, ".."), winslash = "/", mustWork = FALSE)
  }
  ""
}

# ---------- main ----------
SCRIPT_DIR <- get_script_dir()
APP_HOME <- find_app_root(SCRIPT_DIR)

# Si introuvable, on essaie avec le WD
if (!nzchar(APP_HOME)) APP_HOME <- find_app_root(getwd())

# Préparer logs même si racine introuvable (meilleur diagnostic)
LOG_FILE <- file.path(SCRIPT_DIR, "logs", "startup.log")
log_line(LOG_FILE, "=== START calculetteRWA ===")
log_line(LOG_FILE, "SCRIPT_DIR =", SCRIPT_DIR)
log_line(LOG_FILE, "getwd()    =", normalizePath(getwd(), winslash="/", mustWork=FALSE))
log_line(LOG_FILE, "APP_HOME   =", APP_HOME %or% "<NON TROUVE>")

if (!nzchar(APP_HOME)) {
  stop(paste0(
    "Impossible de trouver la racine de l'app.\n",
    "Attendu: un dossier contenant app/app.R.\n",
    "Démarré depuis: ", SCRIPT_DIR, "\n",
    "Voir log: ", LOG_FILE
  ), call. = FALSE)
}

# Dossiers runtime
dir.create(file.path(APP_HOME, "tmp"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(APP_HOME, "logs"), recursive = TRUE, showWarnings = FALSE)
LOG_FILE <- file.path(APP_HOME, "logs", "startup.log")  # log final dans app

log_line(LOG_FILE, "APP_HOME =", APP_HOME)
log_line(LOG_FILE, "R.version =", R.version.string)

# Vérifier arborescence attendue
must_exist <- c(
  file.path(APP_HOME, "app", "app.R"),
  file.path(APP_HOME, "app", "R"),
  file.path(APP_HOME, "R", "bin", "R.exe"),
  file.path(APP_HOME, "R", "bin", "Rscript.exe"),
  file.path(APP_HOME, "library")
)

missing <- must_exist[!file.exists(must_exist)]
if (length(missing)) {
  log_line(LOG_FILE, "ERROR: éléments manquants:")
  for (m in missing) log_line(LOG_FILE, " -", m)
  stop(paste0(
    "Arborescence incomplète. Manquant:\n- ",
    paste(missing, collapse = "\n- "),
    "\nVoir log: ", LOG_FILE
  ), call. = FALSE)
}

# Fix TEMP local (évite temp Windows saturé / verrouillé)
TMP <- file.path(APP_HOME, "tmp")
Sys.setenv(TMPDIR = TMP, TEMP = TMP, TMP = TMP)
log_line(LOG_FILE, "TMPDIR set:", TMP)

# Force les libs offline (priorité : APP library puis R/library)
APP_LIB  <- file.path(APP_HOME, "library")
BASE_LIB <- file.path(APP_HOME, "R", "library")

# Eviter collision avec libs user Windows
Sys.setenv(R_LIBS_USER = "")
.libPaths(c(APP_LIB, BASE_LIB))
log_line(LOG_FILE, "libPaths:", paste(.libPaths(), collapse = " | "))

# Vérifier packages requis
pkgs <- c("shiny","bslib","shinyjs","data.table","DT","ggplot2","readxl","openxlsx")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]

if (length(missing_pkgs)) {
  log_line(LOG_FILE, "ERROR: packages manquants:", paste(missing_pkgs, collapse = ", "))
  stop(paste0(
    "Packages manquants dans calculetteRWA/library:\n- ",
    paste(missing_pkgs, collapse = "\n- "),
    "\n\n=> Installer/copier ces packages (et dépendances) dans calculetteRWA/library.\n",
    "Voir log: ", LOG_FILE
  ), call. = FALSE)
}

# Options Shiny : tolérance fichiers (Excel), debug
options(shiny.maxRequestSize = 500 * 1024^2)  # 500MB
options(calculetteRWA.home = APP_HOME)

# Lancement Shiny (port auto libre)
log_line(LOG_FILE, "Launching shiny::runApp(appDir=app)")
tryCatch({
  shiny::runApp(
    appDir = file.path(APP_HOME, "app"),
    launch.browser = TRUE,
    host = "127.0.0.1",
    port = 0
  )
}, error = function(e) {
  log_line(LOG_FILE, "FATAL:", conditionMessage(e))
  stop(paste0("Erreur au lancement Shiny: ", conditionMessage(e), "\nVoir log: ", LOG_FILE), call. = FALSE)
})

log_line(LOG_FILE, "=== STOP calculetteRWA ===")