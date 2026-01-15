# ============================================================
# calculetteRWA - app.R (ROBUSTE / STRUCTURE A PLAT)
# Tous les scripts 00..07 sont dans le même dossier racine.
# ============================================================

# ---------- util sans %||% ----------
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

# ---------- base dir robuste ----------
# run_app.R définit options(calculetteRWA.home)
BASE_DIR <- getOption("calculetteRWA.home", default = "")
if (!nzchar(BASE_DIR) || !file.exists(BASE_DIR)) {
  # fallback : si l'app est lancée directement via runApp("...") / setwd
  BASE_DIR <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

# ---------- source scripts (avec diagnostic clair) ----------
safe_source <- function(fname) {
  f <- file.path(BASE_DIR, fname)
  if (!file.exists(f)) stop("Script introuvable: ", f, call. = FALSE)
  # keep.source=TRUE pour faciliter debug
  source(f, local = TRUE, keep.source = TRUE)
}

# ============================================================
# ------------------- TRAITEMENTS / MODULES -------------------
# ============================================================

# -- Début chargement scripts --
safe_source("00_config.R")
safe_source("01_helpers.R")
safe_source("02_io_retour4c.R")
safe_source("03_calc_rwa.R")
safe_source("04_module1_retour4c_server.R")
safe_source("05_module2_anomalies_server.R")
safe_source("06_module3_impacts_server.R")
safe_source("07_module4_calcul_server.R")
# -- Fin chargement scripts --

# ============================================================
# ------------------------- SERVER ----------------------------
# ============================================================

server <- function(input, output, session) {

  rv <- reactiveValues(
    # état commun
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

  # ----- début module 1 : Retour4C -----
  module1_server(input, output, rv)
  # ----- fin module 1 -----

  # ----- début module 2 : Anomalies → Extraction -----
  module2_server(input, output, rv)
  # ----- fin module 2 -----

  # ----- début module 3 : ImpactsRWA -----
  module3_server(input, output, rv)
  # ----- fin module 3 -----

  # ----- début module 4 : Calcul RWA -----
  module4_server(input, output, rv)
  # ----- fin module 4 -----
}

# ============================================================
# --------------------------- UI (EN BAS) ---------------------
# ============================================================

ui <- page_navbar(
  title = "calculetteRWA – RWA Crédit (CRR3 / Bâle IV)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  useShinyjs(),

  # ----- Début Module 1 : Retour4C (UI) -----
  nav_panel(
    "Module 1 : Retour4C",
    layout_sidebar(
      sidebar = sidebar(
        tags$b("Gros fichiers: utiliser un chemin direct"),
        textInput("retour_path", "Chemin direct Retour4C (.csv/.txt)", placeholder = "\\\\serveur\\share\\Retour4C.csv"),
        tags$small("Évite fileInput (copie en temp) pour les fichiers de plusieurs Go."),
        hr(),
        fileInput("retour_file", "Ou via fileInput (petits fichiers)", accept = c(".csv",".txt")),
        actionButton("btn_preview", "Afficher preview (5 lignes)", class = "btn-primary"),
        actionButton("btn_prepare", "Préparer (entête + infos)", class = "btn-warning"),
        actionButton("btn_synth", "Réaliser synthèses", class = "btn-success"),
        hr(),
        uiOutput("retour_status_ui"),
        width = 360
      ),
      card(card_header("Arrêté / Infos / Preview"), uiOutput("retour_infos_ui"), DTOutput("retour_preview_dt"), hr(), DTOutput("retour_desc_dt")),
      card(card_header("Synthèse par GoldenSource"), DTOutput("syn_gs_dt")),
      card(card_header("Synthèse par portefeuille Bâlois (PRTF)"), DTOutput("syn_prtf_dt")),
      card(card_header("Logs / Erreurs"), verbatimTextOutput("retour_log"))
    )
  ),
  # ----- Fin Module 1 -----

  # ----- Début Module 2 -----
  nav_panel(
    "Module 2 : Anomalies → Extraction",
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
  # ----- Fin Module 2 -----

  # ----- Début Module 3 -----
  nav_panel(
    "Module 3 : ImpactsRWA",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("impact_method", "Méthode de recalcul", choices = c("Standard (SA-CR)"="SA","IRBA"="IRB"), selected = "SA"),
        numericInput("haircut", "Haircut sûreté (0–1)", value = 0.20, min = 0, max = 1, step = 0.01),
        actionButton("btn_compute_impacts", "Recalculer + comparer", class = "btn-success"),
        hr(),
        downloadButton("dl_impacts", "Télécharger impacts_rwa.xlsx"),
        width = 330
      ),
      card(card_header("Table – Impacts RWA"), DTOutput("impacts_dt")),
      card(card_header("Synthèse impacts (PRTF / SSPRTF)"), DTOutput("impact_syn_dt")),
      card(card_header("Logs / Erreurs"), verbatimTextOutput("impact_log"))
    )
  ),
  # ----- Fin Module 3 -----

  # ----- Début Module 4 -----
  nav_panel(
    "Module 4 : Calcul RWA",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("calc_method", "Méthode", choices = c("Standard (SA-CR)"="SA","IRBA"="IRB"), selected = "SA"),
        selectInput("calc_prtf", "PRTF", choices = c("Retail","Non Retail"), selected = "Retail"),
        selectInput("calc_ssprtf", "SSPRTF", choices = c("Immobilier","Revolving","Autres","Non Retail"), selected = "Immobilier"),
        hr(),
        h5("Paramètres exposition / RW"),
        numericInput("ead_init", "EAD initial", value = 1000000),
        numericInput("ead_corr", "EAD corrigé (optionnel)", value = NA_real_),
        numericInput("rw_init", "RW initial (0.75 ou 75)", value = 0.75),
        numericInput("rw_corr", "RW corrigé (optionnel)", value = NA_real_),
        checkboxInput("surete_flag", "Sûreté", value = FALSE),
        numericInput("hc_calc", "Haircut (0–1)", value = 0.20, min=0, max=1, step=0.01),
        hr(),
        h5("Paramètres IRBA (si IRBA)"),
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
  # ----- Fin Module 4 -----
)

shinyApp(ui, server)