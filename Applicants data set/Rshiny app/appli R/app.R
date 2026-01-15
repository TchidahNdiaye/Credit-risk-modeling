# ============================================================
# calculetteRWA - app/app.R (ROBUSTE)
# Scripts dans app/R/00..07
# ============================================================

`%or%` <- function(a, b) if (!is.null(a) && length(a) && nzchar(as.character(a))) a else b

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

# Root défini par run_app.R
ROOT <- getOption("calculetteRWA.home", default = "")

# fallback si l'app est lancée à la main via runApp()
if (!nzchar(ROOT) || !dir.exists(ROOT)) {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  # si wd == .../app, root = parent
  ROOT <- if (basename(wd) == "app") normalizePath(file.path(wd, ".."), winslash="/", mustWork = FALSE) else wd
}

APP_DIR <- normalizePath(file.path(ROOT, "app"), winslash = "/", mustWork = FALSE)
SRC_DIR <- file.path(APP_DIR, "R")

if (!dir.exists(SRC_DIR)) {
  stop(paste0("Dossier scripts introuvable: ", SRC_DIR, "\nROOT=", ROOT), call. = FALSE)
}

safe_source <- function(fname) {
  f <- file.path(SRC_DIR, fname)
  if (!file.exists(f)) stop("Script introuvable: ", f, call. = FALSE)
  source(f, local = TRUE, keep.source = TRUE)
}

# ============================================================
# ----------------- TRAITEMENTS (AVANT UI) --------------------
# ============================================================

safe_source("00_config.R")
safe_source("01_helpers.R")
safe_source("02_io_retour4c.R")
safe_source("03_calc_rwa.R")
safe_source("04_module1_retour4c_server.R")
safe_source("05_module2_anomalies_server.R")
safe_source("06_module3_impacts_server.R")
safe_source("07_module4_calcul_server.R")

# ============================================================
# ------------------------- SERVER ----------------------------
# ============================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    retour_path = "",
    retour_cols = character(),
    retour_preview = NULL,
    retour_ready = FALSE,

    anomalies_dt = NULL,
    extract_dt = NULL,
    impacts_dt = NULL,

    syn_gs = NULL,
    syn_prtf = NULL,

    logs = list(retour = "", ano = "", impact = "", calc = "")
  )

  # Les fonctions doivent être définies dans vos scripts 04..07
  module1_server(input, output, rv)
  module2_server(input, output, rv)
  module3_server(input, output, rv)
  module4_server(input, output, rv)
}

# ============================================================
# --------------------------- UI (EN BAS) ---------------------
# ============================================================

ui <- page_navbar(
  title = "calculetteRWA – RWA Crédit (CRR3 / Bâle IV)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  useShinyjs(),

  # ⚠️ IMPORTANT: utiliser bslib::nav_panel (pas tabPanel)
  nav_panel("Module 1 : Retour4C",
    layout_sidebar(
      sidebar = sidebar(
        tags$b("Gros fichiers : Chemin direct (recommandé)"),
        textInput("retour_path", "Chemin direct Retour4C (.csv/.txt)", placeholder = "\\\\serveur\\share\\Retour4C.csv"),
        hr(),
        fileInput("retour_file", "Ou via fileInput (petits fichiers)", accept = c(".csv", ".txt")),
        actionButton("btn_preview", "Afficher preview (5 lignes)", class = "btn-primary"),
        actionButton("btn_prepare", "Charger complet + Préparer", class = "btn-warning"),
        actionButton("btn_synth", "Réaliser synthèses", class = "btn-success"),
        hr(),
        uiOutput("retour_status_ui"),
        width = 360
      ),
      card(card_header("Arrêté / Infos / Preview"),
           uiOutput("retour_infos_ui"),
           DTOutput("retour_preview_dt"),
           hr(),
           DTOutput("retour_desc_dt")),
      card(card_header("Synthèse par GoldenSource"), DTOutput("syn_gs_dt")),
      card(card_header("Synthèse par portefeuille Bâlois"), DTOutput("syn_prtf_dt")),
      card(card_header("Logs / Erreurs"), verbatimTextOutput("retour_log"))
    )
  ),

  nav_panel("Module 2 : Anomalies → Extraction",
    layout_sidebar(
      sidebar = sidebar(
        fileInput("ano_file", "Charger anomalies_dq.xlsx", accept = c(".xlsx")),
        actionButton("btn_preview_ano", "Afficher preview anomalies (5 lignes)", class = "btn-primary"),
        hr(),
        actionButton("btn_extract", "Extraire dans Retour4C", class = "btn-success"),
        hr(),
        downloadButton("dl_extract", "Télécharger ead_rwa_ano_dq.xlsx"),
        width = 330
      ),
      card(card_header("Preview anomalies"), DTOutput("ano_preview_dt")),
      card(card_header("Extraction (5 premières + 5 dernières)"), DTOutput("extract_head_dt"), DTOutput("extract_tail_dt")),
      card(card_header("Logs / Erreurs"), verbatimTextOutput("ano_log"))
    )
  ),

  nav_panel("Module 3 : ImpactsRWA",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("impact_method", "Méthode de recalcul",
                     choices = c("Standard (SA-CR)" = "SA", "IRBA" = "IRB"),
                     selected = "SA"),
        numericInput("haircut", "Haircut sûreté (0–1)", value = 0.20, min = 0, max = 1, step = 0.01),
        actionButton("btn_compute_impacts", "Recalculer + comparer", class = "btn-success"),
        hr(),
        downloadButton("dl_impacts", "Télécharger impacts_rwa.xlsx"),
        width = 330
      ),
      card(card_header("Impacts RWA"), DTOutput("impacts_dt")),
      card(card_header("Synthèse impacts"), DTOutput("impact_syn_dt")),
      card(card_header("Logs / Erreurs"), verbatimTextOutput("impact_log"))
    )
  ),

  nav_panel("Module 4 : Calcul RWA",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("calc_method", "Méthode", choices = c("Standard (SA-CR)" = "SA", "IRBA" = "IRB"), selected = "SA"),
        selectInput("calc_prtf", "PRTF", choices = c("Retail", "Non Retail"), selected = "Retail"),
        selectInput("calc_ssprtf", "SSPRTF", choices = c("Immobilier", "Revolving", "Autres", "Non Retail"), selected = "Immobilier"),
        hr(),
        numericInput("ead_init", "EAD initial", value = 1000000),
        numericInput("ead_corr", "EAD corrigé (optionnel)", value = NA_real_),
        numericInput("rw_init", "RW initial (0.75 ou 75)", value = 0.75),
        numericInput("rw_corr", "RW corrigé (optionnel)", value = NA_real_),
        checkboxInput("surete_flag", "Sûreté", value = FALSE),
        numericInput("hc_calc", "Haircut (0–1)", value = 0.20, min = 0, max = 1, step = 0.01),
        hr(),
        numericInput("pd_init", "PD initial", value = 0.01, min = 0),
        numericInput("pd_corr", "PD corrigé (optionnel)", value = NA_real_, min = 0),
        numericInput("lgd_init", "LGD initial", value = 0.45, min = 0),
        numericInput("lgd_corr", "LGD corrigé (optionnel)", value = NA_real_, min = 0),
        numericInput("m_init", "M initial", value = 2.5, min = 0),
        numericInput("m_corr", "M corrigé (optionnel)", value = NA_real_, min = 0),
        hr(),
        actionButton("btn_calc", "Calculer", class = "btn-success"),
        width = 360
      ),
      card(card_header("Résultats"), DTOutput("calc_table_dt"), plotOutput("calc_plot")),
      card(card_header("Logs / Erreurs"), verbatimTextOutput("calc_log"))
    )
  )
)

shinyApp(ui, server)