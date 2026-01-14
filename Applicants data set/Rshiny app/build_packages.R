# tools/build_packages.R
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1) stop("Usage: R.exe -f tools/build_packages.R --args <APP_ROOT>")

app_root <- normalizePath(args[1], winslash = "/", mustWork = TRUE)

lib_dir <- file.path(app_root, "library")  # librairie applicative dédiée
dir.create(lib_dir, recursive = TRUE, showWarnings = FALSE)

.libPaths(c(lib_dir, .libPaths()))

pkgs <- c("shiny","bslib","shinyjs","data.table","DT","ggplot2","readxl","openxlsx")

message("Installing packages into: ", lib_dir)
install.packages(pkgs, lib = lib_dir, dependencies = TRUE)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Packages manquants: ", paste(missing, collapse=", "))

message("OK - packages installed in: ", lib_dir)
