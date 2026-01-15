# ============================================================
# calculetteRWA - run_app.R (ROBUSTE / APP DANS app/)
# Fix principal: NE JAMAIS utiliser port=0 (évite 127.0.0.1:0)
# ============================================================

`%or%` <- function(a, b) if (!is.null(a) && length(a) && nzchar(as.character(a))) a else b

get_script_dir <- function() {
  ca <- commandArgs(trailingOnly = FALSE)
  farg <- grep("^--file=", ca, value = TRUE)
  if (length(farg) == 1) {
    p <- sub("^--file=", "", farg)
    return(normalizePath(dirname(p), winslash = "/", mustWork = FALSE))
  }
  for (i in rev(seq_along(sys.frames()))) {
    of <- tryCatch(sys.frames()[[i]]$ofile, error = function(e) NULL)
    if (!is.null(of) && nzchar(of)) return(normalizePath(dirname(of), winslash = "/", mustWork = FALSE))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

log_line <- function(path, ...) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  txt <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
  cat(txt, "\n", file = path, append = TRUE)
}

find_root <- function(start_dir) {
  d <- normalizePath(start_dir, winslash = "/", mustWork = FALSE)
  for (k in 0:8) {
    if (file.exists(file.path(d, "app", "app.R"))) return(d)
    d <- normalizePath(file.path(d, ".."), winslash = "/", mustWork = FALSE)
  }
  ""
}

SCRIPT_DIR <- get_script_dir()
APP_HOME <- find_root(SCRIPT_DIR)
if (!nzchar(APP_HOME)) APP_HOME <- find_root(getwd())

# Préparer log
if (!nzchar(APP_HOME)) APP_HOME <- SCRIPT_DIR
dir.create(file.path(APP_HOME, "logs"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(APP_HOME, "tmp"), recursive = TRUE, showWarnings = FALSE)
LOG_FILE <- file.path(APP_HOME, "logs", "startup.log")

log_line(LOG_FILE, "=== START calculetteRWA ===")
log_line(LOG_FILE, "SCRIPT_DIR =", SCRIPT_DIR)
log_line(LOG_FILE, "APP_HOME   =", APP_HOME)
log_line(LOG_FILE, "getwd()    =", normalizePath(getwd(), winslash = "/", mustWork = FALSE))
log_line(LOG_FILE, "R.version  =", R.version.string)

# Vérifier arborescence attendue
must_exist <- c(
  file.path(APP_HOME, "app", "app.R"),
  file.path(APP_HOME, "app", "R"),
  file.path(APP_HOME, "R", "bin", "R.exe"),
  file.path(APP_HOME, "R", "bin", "Rscript.exe")
)
missing <- must_exist[!file.exists(must_exist)]
if (length(missing)) {
  for (m in missing) log_line(LOG_FILE, "MISSING:", m)
  stop("Arborescence invalide. Voir logs/startup.log", call. = FALSE)
}

# TEMP local
TMP <- file.path(APP_HOME, "tmp")
Sys.setenv(TMPDIR = TMP, TEMP = TMP, TMP = TMP)
log_line(LOG_FILE, "TMPDIR =", TMP)

# Libs: priorité aux libs du kit
APP_LIB  <- file.path(APP_HOME, "library")      # packages applicatifs offline (si vous l’avez)
BASE_LIB <- file.path(APP_HOME, "R", "library") # libs R copiées

# Ne pas polluer avec libs user
Sys.setenv(R_LIBS_USER = "")
if (dir.exists(APP_LIB)) {
  .libPaths(c(APP_LIB, BASE_LIB))
} else {
  .libPaths(c(BASE_LIB))
}
log_line(LOG_FILE, ".libPaths =", paste(.libPaths(), collapse = " | "))

# Vérifier shiny + httpuv
pkgs <- c("shiny", "httpuv")
missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)]
if (length(missing_pkgs)) {
  log_line(LOG_FILE, "MISSING PACKAGES:", paste(missing_pkgs, collapse = ", "))
  stop(paste0("Packages manquants: ", paste(missing_pkgs, collapse = ", "),
              " (voir logs/startup.log)"), call. = FALSE)
}

# Taille upload (excel etc.)
options(shiny.maxRequestSize = 500 * 1024^2)
options(calculetteRWA.home = APP_HOME)

# ✅ Choix d'un port sûr (évite 127.0.0.1:0 et ERR_UNSAFE_PORT)
port <- httpuv::randomPort()
if (is.na(port) || port < 1024) port <- 3838
log_line(LOG_FILE, "PORT =", port)

# Lancement
log_line(LOG_FILE, "Launching shiny::runApp(appDir=APP_HOME/app)")
tryCatch({
  shiny::runApp(
    appDir = file.path(APP_HOME, "app"),
    host = "127.0.0.1",
    port = port,
    launch.browser = function(url) {
      # garantit l'ouverture de la bonne URL
      utils::browseURL(url)
    }
  )
}, error = function(e) {
  log_line(LOG_FILE, "FATAL:", conditionMessage(e))
  stop(conditionMessage(e), call. = FALSE)
})

log_line(LOG_FILE, "=== STOP calculetteRWA ===")