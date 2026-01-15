# ============================================================
# run_app.R - Lancement de l'application calculetteRWA
# Sans .bat, sans .exe - Compatible Portable Windows R-4.5.1
# ============================================================

# 1) Déterminer le dossier racine
APP_HOME <- normalizePath(dirname(sys.frame(1)$ofile %||% "."), winslash = "/", mustWork = TRUE)

# 2) Préparer dossiers runtime
dir.create(file.path(APP_HOME, "tmp"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(APP_HOME, "logs"), showWarnings = FALSE, recursive = TRUE)

# 3) Temp local (évite temp Windows bloquant / réseau)
Sys.setenv(TMPDIR = file.path(APP_HOME, "tmp"))
Sys.setenv(TEMP   = file.path(APP_HOME, "tmp"))
Sys.setenv(TMP    = file.path(APP_HOME, "tmp"))

# 4) Forcer la librairie offline de l'app (packages)
#    On veut que .libPaths() pointe d'abord vers calculetteRWA/library
app_lib <- file.path(APP_HOME, "library")
base_lib <- file.path(APP_HOME, "R", "library")
.libPaths(c(app_lib, base_lib, .libPaths()))

# 5) Log minimal
log_file <- file.path(APP_HOME, "logs", "startup.log")
cat("=== calculetteRWA startup ===\n", file = log_file)
cat("APP_HOME=", APP_HOME, "\n", file = log_file, append = TRUE)
cat("LIBS=", paste(.libPaths(), collapse = " | "), "\n", file = log_file, append = TRUE)

# 6) Lancer l'app
app_path <- file.path(APP_HOME, "app", "app.R")
if (!file.exists(app_path)) stop("app/app.R introuvable: ", app_path)

source(app_path, local = TRUE)
