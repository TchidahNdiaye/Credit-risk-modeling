# tools/healthcheck.R
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: R.exe -f tools/healthcheck.R --args <APP_ROOT>")

root <- normalizePath(args[1], winslash="/", mustWork=TRUE)
lib_app <- file.path(root, "library")
lib_r   <- file.path(root, "R", "library")
app_dir <- file.path(root, "app")
app_file <- file.path(app_dir, "app.R")

paths <- c()
if (dir.exists(lib_app)) paths <- c(paths, lib_app)
if (dir.exists(lib_r))   paths <- c(paths, lib_r)
.libPaths(paths)

pkgs <- c("shiny","bslib","shinyjs","data.table","DT","ggplot2","readxl","openxlsx")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly=TRUE)]
if (length(missing)) stop("Missing packages: ", paste(missing, collapse=", "))

if (!file.exists(app_file)) stop("Missing app.R: ", app_file)

cat("OK: libs=", paste(.libPaths(), collapse=" | "), "\n")
cat("OK: packages available\n")

tryCatch({
  source(app_file, local = TRUE)
  cat("OK: app.R loaded without errors\n")
}, error=function(e) {
  stop("Error in app.R: ", conditionMessage(e))
})

cat("Healthcheck complete.\n")
