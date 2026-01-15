# ============================================================
# calculetteRWA - app/app.R (ROBUSTE)
# - compatible runApp(appDir=".../app")
# - ne dépend pas de %||% ou sys.frame fragile
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

# ---------- Résolution robuste ROOT ----------
ROOT <- getOption("calculetteRWA.home", default = "")

# Si lancé via runApp(appDir=".../app"), getwd() est souvent ".../app"
if (!nzchar(ROOT) || !file.exists(ROOT)) {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  # Si on est dans .../app : ROOT = parent
  if (basename(wd) == "app") ROOT <- normalizePath(file.path(wd, ".."), winslash = "/", mustWork = FALSE)
  else ROOT <- wd
}

APP_DIR <- normalizePath(file.path(ROOT, "app"), winslash = "/", mustWork = FALSE)
SRC_DIR <- file.path(APP_DIR, "R")

if (!file.exists(file.path(APP_DIR, "R"))) {
  stop(paste0(
    "Dossier scripts introuvable: ", SRC_DIR, "\n",
    "ROOT=", ROOT, "\n",
    "APP_DIR=", APP_DIR, "\n",
    "Lancer via run_app.R (recommandé)."
  ), call. = FALSE)
}

# ============================================================
# ---------------- TRAITEMENTS (source scripts) ---------------
# ============================================================
source(file.path(SRC_DIR, "00_config.R"), local = TRUE)
source(file.path(SRC_DIR, "01_helpers.R"), local = TRUE)
source(file.path(SRC_DIR, "02_io_retour4c.R"), local = TRUE)
source(file.path(SRC_DIR, "03_calc_rwa.R"), local = TRUE)
source(file.path(SRC_DIR, "04_module1_retour4c_server.R"), local = TRUE)
source(file.path(SRC_DIR, "05_module2_anomalies_server.R"), local = TRUE)
source(file.path(SRC_DIR, "06_module3_impacts_server.R"), local = TRUE)
source(file.path(SRC_DIR, "07_module4_calcul_server.R"), local = TRUE)

# ============================================================
# ------------------------ SERVER -----------------------------
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

  # ----- debut module 1 -----
  module1_server(input, output, rv)
  # ----- fin module 1 -----

  # ----- debut module 2 -----
  module2_server(input, output, rv)
  # ----- fin module 2 -----

  # ----- debut module 3 -----
  module3_server(input, output, rv)
  # ----- fin module 3 -----

  # ----- debut module 4 -----
  module4_server(input, output, rv)
  # ----- fin module 4 -----
}

# ============================================================
# -------------------------- UI (EN BAS) ----------------------
# ============================================================

ui <- page_navbar(
  title = "calculetteRWA – RWA Crédit (CRR3 / Bâle IV)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  useShinyjs(),

  # ----- DEBUT MODULE 1 : Retour4C (UI) -----
  nav_panel(
    "Module 1 : Retour4C",
    layout_sidebar(
      sidebar = sidebar(
        tags$b("Mode recommandé gros fichier : Chemin direct"),
        textInput("retour_path", "Chemin direct Retour4C (.csv/.txt)", placeholder = "\\\\serveur\\share\\Retour4C.csv"),
        tags$small("Pour des fichiers de plusieurs Go, éviter fileInput (copie en temp)."),
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
  # ----- FIN MODULE 1 -----

  # ----- DEBUT MODULE 2 -----
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
  # ----- FIN MODULE 2 -----

  # ----- DEBUT MODULE 3 -----
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
  # ----- FIN MODULE 3 -----

  # ----- DEBUT MODULE 4 -----
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
  # ----- FIN MODULE 4 -----
)

shinyApp(ui, server)