# ============================================================
# calculetteRWA - run_app.R (ROBUSTE)
# Objectif: lancement Shiny portable Windows (R-4.5.1)
# Sans .bat / sans .exe "maison"
# ============================================================

# ---- util: get script dir (robuste en Rscript / source) ----
get_script_dir <- function() {
  # Cas 1: Rscript --file=...
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd, value = TRUE)
  if (length(file_arg) == 1) {
    p <- sub("^--file=", "", file_arg)
    return(normalizePath(dirname(p), winslash = "/", mustWork = FALSE))
  }

  # Cas 2: source(".../run_app.R")
  # On tente d'extraire via sys.frames
  for (i in rev(seq_along(sys.frames()))) {
    of <- tryCatch(sys.frames()[[i]]$ofile, error = function(e) NULL)
    if (!is.null(of) && nzchar(of)) {
      return(normalizePath(dirname(of), winslash = "/", mustWork = FALSE))
    }
  }

  # Cas 3: fallback = dossier courant
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

# ---- util: log ----
log_line <- function(..., file) {
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  cat(txt, "\n", file = file, append = TRUE)
}

# ---- 1) Résoudre le dossier racine calculetteRWA ----
SCRIPT_DIR <- get_script_dir()

# Si run_app.R est à la racine: SCRIPT_DIR = calculetteRWA
# Sinon on tente de remonter
candidate_root <- SCRIPT_DIR
if (!file.exists(file.path(candidate_root, "app", "app.R"))) {
  candidate_root2 <- normalizePath(file.path(SCRIPT_DIR, ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(candidate_root2, "app", "app.R"))) candidate_root <- candidate_root2
}

APP_HOME <- normalizePath(candidate_root, winslash = "/", mustWork = FALSE)

# ---- 2) Préparer logs et tmp ----
dir.create(file.path(APP_HOME, "logs"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(APP_HOME, "tmp"),  showWarnings = FALSE, recursive = TRUE)

LOG_FILE <- file.path(APP_HOME, "logs", "startup.log")
log_line("=== START calculetteRWA ===", file = LOG_FILE)
log_line("SCRIPT_DIR =", SCRIPT_DIR, file = LOG_FILE)
log_line("APP_HOME   =", APP_HOME, file = LOG_FILE)
log_line("R.version  =", R.version.string, file = LOG_FILE)

# ---- 3) Vérifications arborescence ----
must_exist <- c(
  file.path(APP_HOME, "app", "app.R"),
  file.path(APP_HOME, "app", "R"),
  file.path(APP_HOME, "R", "bin", "R.exe"),
  file.path(APP_HOME, "R", "bin", "Rscript.exe"),
  file.path(APP_HOME, "library")
)
missing <- must_exist[!file.exists(must_exist)]
if (length(missing) > 0) {
  log_line("ERROR: fichiers/dossiers manquants:", file = LOG_FILE)
  for (m in missing) log_line("  -", m, file = LOG_FILE)

  stop(
    paste0(
      "Arborescence incomplète. Éléments manquants:\n- ",
      paste(missing, collapse = "\n- "),
      "\n\nCorriger l'arborescence puis relancer.\n(voir logs: ", LOG_FILE, ")"
    ),
    call. = FALSE
  )
}

# ---- 4) Fix TEMP local (évite locks réseau / manque espace dans temp Windows) ----
TMP <- file.path(APP_HOME, "tmp")
Sys.setenv(TMPDIR = TMP, TEMP = TMP, TMP = TMP)
log_line("TMPDIR set to", TMP, file = LOG_FILE)

# ---- 5) Fix librairies: priorité à calculetteRWA/library puis R/library ----
APP_LIB  <- file.path(APP_HOME, "library")
BASE_LIB <- file.path(APP_HOME, "R", "library")

# Désactive le R libs user si besoin (évite collisions)
Sys.setenv(R_LIBS_USER = "")

.libPaths(c(APP_LIB, BASE_LIB, .libPaths()))
log_line("libPaths =", paste(.libPaths(), collapse = " | "), file = LOG_FILE)

# ---- 6) Vérifier packages requis (sans installer) ----
pkgs_required <- c("shiny","bslib","shinyjs","data.table","DT","ggplot2","readxl","openxlsx")
missing_pkgs <- pkgs_required[!vapply(pkgs_required, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]

if (length(missing_pkgs) > 0) {
  log_line("ERROR: packages manquants:", paste(missing_pkgs, collapse = ", "), file = LOG_FILE)
  stop(
    paste0(
      "Packages manquants dans calculetteRWA/library :\n- ",
      paste(missing_pkgs, collapse = "\n- "),
      "\n\n=> Copier/installer ces packages OFFLINE dans calculetteRWA/library.\n",
      "(voir logs: ", LOG_FILE, ")"
    ),
    call. = FALSE
  )
}

# ---- 7) Options Shiny (robuste) ----
# maxRequestSize: utile pour Excel anomalies un peu gros (fileInput)
options(shiny.maxRequestSize = 500 * 1024^2)  # 500 MB
options(datatable.print.class = TRUE)

# Aide à la perf data.table
try(data.table::setDTthreads(0), silent = TRUE)  # auto

# ---- 8) Exporter APP_HOME à app/app.R via option ----
options(calculetteRWA.home = APP_HOME)

# ---- 9) Lancer runApp (le plus robuste) ----
APP_DIR <- file.path(APP_HOME, "app")
log_line("Launching shiny::runApp on", APP_DIR, file = LOG_FILE)

# runApp gère mieux le contexte que source(app.R)
shiny::runApp(
  appDir = APP_DIR,
  launch.browser = TRUE,
  host = "127.0.0.1",
  port = 0  # port libre
)

log_line("=== STOP calculetteRWA ===", file = LOG_FILE)