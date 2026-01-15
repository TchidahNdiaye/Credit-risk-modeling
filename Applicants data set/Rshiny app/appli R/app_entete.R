# ============================================================
# app/app.R - calculetteRWA
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(data.table)
  library(DT)
  library(ggplot2)
  library(readxl)
  library(openxlsx)
})

# ---- Résolution robuste des chemins ----
ROOT <- getOption("calculetteRWA.home", default = NA_character_)
if (is.na(ROOT) || !nzchar(ROOT) || !file.exists(ROOT)) {
  # fallback: quand runApp(appDir="app") est utilisé, getwd() == dossier app
  # donc ROOT = parent
  ROOT <- normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE)
}

APP_DIR <- normalizePath(file.path(ROOT, "app"), winslash = "/", mustWork = FALSE)
SRC_DIR <- file.path(APP_DIR, "R")

# ---- Vérif scripts ----
if (!file.exists(SRC_DIR)) stop("Dossier scripts introuvable: ", SRC_DIR, call. = FALSE)

# ---- Source traitements (ordre stable) ----
source(file.path(SRC_DIR, "00_config.R"), local = TRUE)
source(file.path(SRC_DIR, "01_helpers.R"), local = TRUE)
source(file.path(SRC_DIR, "02_io_retour4c.R"), local = TRUE)
source(file.path(SRC_DIR, "03_calc_rwa.R"), local = TRUE)
source(file.path(SRC_DIR, "04_module1_retour4c_server.R"), local = TRUE)
source(file.path(SRC_DIR, "05_module2_anomalies_server.R"), local = TRUE)
source(file.path(SRC_DIR, "06_module3_impacts_server.R"), local = TRUE)
source(file.path(SRC_DIR, "07_module4_calcul_server.R"), local = TRUE)

# ---- server / ui (comme ta version) ----
# server <- function(...) { ... }
# ui <- page_navbar(...) { ... }
# shinyApp(ui, server)