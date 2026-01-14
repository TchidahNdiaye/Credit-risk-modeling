# ============================================================
# calculetteRWA - Application RShiny RWA (CRR3 / Bâle IV)
# Compatible Portable Windows R-4.5.1
# Packages requis: shiny, bslib, shinyjs, data.table, DT, ggplot2, readxl, openxlsx
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

# ---------------------------
# Helpers (formatting, safety)
# ---------------------------

fmt_num <- function(x, digits = 2, big_mark = " ", dec_mark = ",") {
  # Format numbers with thousands separator and 2 decimals
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), NA_character_,
         formatC(x, format = "f", digits = digits, big.mark = big_mark, decimal.mark = dec_mark))
}

normalize_id <- function(x) {
  # Robust normalization for contract IDs (Excel often changes types)
  # - convert to character
  # - trim spaces
  # - remove non-printing
  # - keep leading zeros by NOT converting to numeric
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("[\r\n\t]", "", x)
  x <- ifelse(is.na(x), "", x)
  x
}

smart_as_numeric <- function(x) {
  # Try numeric conversion:
  # 1) as.numeric
  # 2) if too many NAs, replace comma decimal -> dot and retry
  if (is.numeric(x)) return(x)
  x0 <- x
  x1 <- suppressWarnings(as.numeric(x0))
  na_rate <- mean(is.na(x1) & !is.na(x0) & nzchar(as.character(x0)))
  if (is.nan(na_rate)) na_rate <- 0
  if (na_rate > 0.25) {
    x2 <- gsub(",", ".", as.character(x0), fixed = FALSE)
    x2 <- gsub(" ", "", x2, fixed = TRUE)
    x2 <- suppressWarnings(as.numeric(x2))
    return(x2)
  }
  x1
}

safe_run <- function(step_name, expr, prereq = NULL) {
  # Safe execution wrapper returning list(ok, value, message)
  tryCatch(
    {
      val <- eval.parent(substitute(expr))
      list(ok = TRUE, value = val, message = paste0("OK: ", step_name))
    },
    error = function(e) {
      # Best-effort call stack summary (R doesn't always provide line numbers)
      calls <- sys.calls()
      call_txt <- paste0(utils::capture.output(print(calls)), collapse = "\n")
      msg <- paste0(
        "ERREUR (", step_name, ")\n",
        "Message: ", conditionMessage(e), "\n\n",
        if (!is.null(prereq)) paste0("Pré-requis / Correction:\n- ", prereq, "\n\n") else "",
        "Pile d'appels (diagnostic):\n", call_txt
      )
      list(ok = FALSE, value = NULL, message = msg)
    }
  )
}

need_cols <- function(dt, cols, context = "Retour4C") {
  miss <- setdiff(cols, names(dt))
  if (length(miss) > 0) {
    stop(paste0(
      context, " - colonnes manquantes: ", paste(miss, collapse = ", "),
      "."
    ), call. = FALSE)
  }
  TRUE
}

asof_label <- function(d) {
  # expects Date vector
  if (length(d) == 0) return(NA_character_)
  d <- d[!is.na(d)]
  if (length(d) == 0) return(NA_character_)
  # show dominant / range
  if (length(unique(d)) == 1) {
    format(unique(d), "%d/%m/%Y")
  } else {
    paste0(format(min(d), "%d/%m/%Y"), " → ", format(max(d), "%d/%m/%Y"))
  }
}

# ---------------------------------
# Core calc functions (SA & IRBA)
# ---------------------------------

# Standard: RWA = EAD * RW (RW may be 0.75 or 75)
compute_rwa_sa <- function(EAD, RW) {
  EAD <- smart_as_numeric(EAD)
  RW  <- smart_as_numeric(RW)
  RWn <- ifelse(RW > 1.5, RW / 100, RW)
  EAD * RWn
}

# IRBA core (CRE31 style)
Ncdf <- stats::pnorm
Ginv <- stats::qnorm

R_corporate <- function(PD) {
  a <- (1 - exp(-50 * PD)) / (1 - exp(-50))
  0.12 * a + 0.24 * (1 - a)
}

R_retail_other <- function(PD) {
  a <- (1 - exp(-35 * PD)) / (1 - exp(-35))
  0.03 * a + 0.16 * (1 - a)
}

R_qrre <- function(PD) rep(0.04, length(PD))

maturity_adjustment <- function(PD, M) {
  b <- (0.11852 - 0.05478 * log(PD))^2
  (1 + (M - 2.5) * b) / (1 - 1.5 * b)
}

K_irb_nondefault <- function(PD, LGD, R, M = 2.5, apply_MA = TRUE) {
  PD  <- pmin(pmax(PD, 1e-6), 0.999999)
  LGD <- pmin(pmax(LGD, 0.0001), 1)
  z <- (Ginv(PD) / sqrt(1 - R)) + sqrt(R / (1 - R)) * Ginv(0.999)
  K <- LGD * (Ncdf(z) - PD)
  if (apply_MA) K <- K * maturity_adjustment(PD, M)
  K
}

K_irb_default_proxy <- function(PD, LGD) {
  PD  <- pmin(pmax(PD, 1e-6), 0.999999)
  LGD <- pmin(pmax(LGD, 0.0001), 1)
  pmax(0, LGD - PD * LGD)
}

map_irb_type <- function(PRTF, SSPRTF) {
  pr <- toupper(trimws(as.character(PRTF)))
  ss <- toupper(trimws(as.character(SSPRTF)))
  if (pr == "RETAIL") {
    if (ss %in% c("REVOLVING", "QRRE")) return("qrre")
    return("retail_other")
  }
  "corporate"
}

compute_rwa_irb <- function(EAD, PD, LGD, M, PRTF, SSPRTF, default_flag = NULL, scaling = 1.0) {
  EAD <- smart_as_numeric(EAD)
  PD  <- smart_as_numeric(PD)
  LGD <- smart_as_numeric(LGD)
  M   <- smart_as_numeric(M)

  PDn  <- ifelse(PD > 1, PD / 100, PD)
  LGDn <- ifelse(LGD > 1, LGD / 100, LGD)

  irb_type <- mapply(map_irb_type, PRTF, SSPRTF, USE.NAMES = FALSE)

  R <- rep(NA_real_, length(PDn))
  apply_MA <- rep(FALSE, length(PDn))

  idx_q <- irb_type == "qrre"
  idx_r <- irb_type == "retail_other"
  idx_c <- irb_type == "corporate"

  R[idx_q] <- R_qrre(PDn[idx_q])
  R[idx_r] <- R_retail_other(PDn[idx_r])
  R[idx_c] <- R_corporate(PDn[idx_c])
  apply_MA[idx_c] <- TRUE

  if (is.null(default_flag)) default_flag <- rep(0L, length(PDn))
  df <- as.integer(default_flag)
  df[is.na(df)] <- 0L

  K <- rep(NA_real_, length(PDn))
  nd <- df == 0L
  dd <- df == 1L

  K[nd] <- K_irb_nondefault(PDn[nd], LGDn[nd], R[nd], M = M[nd], apply_MA = apply_MA[nd])
  K[dd] <- K_irb_default_proxy(PDn[dd], LGDn[dd])

  12.5 * K * EAD * scaling
}

# Apply surety via simple haircut on EAD (app logic)
apply_surety_haircut <- function(EAD, surete, hc) {
  EAD <- smart_as_numeric(EAD)
  s <- smart_as_numeric(surete)
  s <- ifelse(is.na(s), 0, s)
  hc <- smart_as_numeric(hc)
  hc <- ifelse(is.na(hc), 0, hc)
  EAD * ifelse(s >= 1, pmax(0, 1 - hc), 1)
}

# ---------------------------
# UI
# ---------------------------

ui <- page_navbar(
  title = "calculetteRWA – RWA Crédit (CRR3 / Bâle IV)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  useShinyjs(),

  nav_panel(
    "Module 1 : Retour4C",
    layout_sidebar(
      sidebar = sidebar(
        fileInput("retour_file", "Charger Retour4C (.csv ou .txt)", accept = c(".csv", ".txt")),
        actionButton("btn_preview", "Afficher preview (5 lignes)", class = "btn-primary"),
        actionButton("btn_load_full", "Charger complet + Préparer", class = "btn-warning"),
        actionButton("btn_synth", "Réaliser synthèses", class = "btn-success"),
        hr(),
        checkboxInput("keep_in_memory", "Conserver Retour4C en mémoire (recommandé)", TRUE),
        hr(),
        uiOutput("retour_status_ui"),
        width = 330
      ),
      card(
        card_header("Résultats – Arrêté / Infos / Preview"),
        uiOutput("retour_infos_ui"),
        DTOutput("retour_preview_dt"),
        hr(),
        DTOutput("retour_desc_dt")
      ),
      card(
        card_header("Synthèse par GoldenSource"),
        DTOutput("syn_gs_dt")
      ),
      card(
        card_header("Synthèse par portefeuille Bâlois (PRTF)"),
        DTOutput("syn_prtf_dt")
      ),
      card(
        card_header("Logs / Erreurs"),
        verbatimTextOutput("retour_log")
      )
    )
  ),

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
      card(
        card_header("Preview anomalies (5 premières lignes)"),
        DTOutput("ano_preview_dt")
      ),
      card(
        card_header("Extraction (5 premières + 5 dernières)"),
        DTOutput("extract_head_dt"),
        DTOutput("extract_tail_dt")
      ),
      card(
        card_header("Logs / Erreurs"),
        verbatimTextOutput("ano_log")
      )
    )
  ),

  nav_panel(
    "Module 3 : ImpactsRWA",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons(
          "impact_method",
          "Méthode de recalcul",
          choices = c("Standard (SA-CR)" = "SA", "IRBA" = "IRB"),
          selected = "SA"
        ),
        numericInput("haircut", "Haircut sûreté (0–1) – appli", value = 0.20, min = 0, max = 1, step = 0.01),
        checkboxInput("use_floor", "Calculer aussi le floor (72.5% SA) – optionnel", FALSE),
        hr(),
        actionButton("btn_compute_impacts", "Recalculer + comparer", class = "btn-success"),
        hr(),
        downloadButton("dl_impacts", "Télécharger impacts_rwa.xlsx"),
        width = 330
      ),
      card(
        card_header("Table – Impacts RWA (extrait)"),
        DTOutput("impacts_dt")
      ),
      card(
        card_header("Synthèse impacts (PRTF / SSPRTF)"),
        DTOutput("impact_syn_dt")
      ),
      card(
        card_header("Logs / Erreurs"),
        verbatimTextOutput("impact_log")
      )
    )
  ),

  nav_panel(
    "Module 4 : Calcul RWA",
    layout_sidebar(
      sidebar = sidebar(
        radioButtons("calc_method", "Méthode", choices = c("Standard (SA-CR)" = "SA", "IRBA" = "IRB"), selected = "SA"),
        selectInput("calc_prtf", "PRTF", choices = c("Retail", "Non Retail"), selected = "Retail"),
        selectInput("calc_ssprtf", "SSPRTF", choices = c("Immobilier", "Revolving", "Autres", "Non Retail"), selected = "Immobilier"),
        hr(),
        h5("Paramètres exposition / RW"),
        numericInput("ead_init", "EAD initial", value = 1000000),
        numericInput("ead_corr", "EAD corrigé (optionnel)", value = NA_real_),
        numericInput("rw_init", "RW initial (0.75 ou 75)", value = 0.75),
        numericInput("rw_corr", "RW corrigé (optionnel)", value = NA_real_),
        checkboxInput("surete_flag", "Sûreté (Oui/Non)", value = FALSE),
        numericInput("hc_calc", "Haircut sûreté (0–1)", value = 0.20, min = 0, max = 1, step = 0.01),
        hr(),
        h5("Bilan / Hors-bilan (optionnel)"),
        numericInput("ead_b_init", "EAD_B initial", value = NA_real_),
        numericInput("ead_b_corr", "EAD_B corrigé", value = NA_real_),
        numericInput("ead_hb_init", "EAD_HB initial (brut)", value = NA_real_),
        numericInput("ead_hb_corr", "EAD_HB corrigé (brut)", value = NA_real_),
        numericInput("ccf_init", "CCF initial", value = NA_real_),
        numericInput("ccf_corr", "CCF corrigé", value = NA_real_),
        hr(),
        h5("Paramètres IRBA (si IRBA)"),
        numericInput("pd_init", "PD initial (0-1 ou %)", value = 0.01, min = 0),
        numericInput("pd_corr", "PD corrigé (optionnel)", value = NA_real_, min = 0),
        numericInput("lgd_init", "LGD initial (0-1 ou %)", value = 0.45, min = 0),
        numericInput("lgd_corr", "LGD corrigé (optionnel)", value = NA_real_, min = 0),
        numericInput("m_init", "M initial (années)", value = 2.5, min = 0),
        numericInput("m_corr", "M corrigé (optionnel)", value = NA_real_, min = 0),
        hr(),
        actionButton("btn_calc", "Calculer", class = "btn-success"),
        width = 360
      ),
      card(
        card_header("Résultats"),
        DTOutput("calc_table_dt"),
        plotOutput("calc_plot")
      ),
      card(
        card_header("Logs / Erreurs"),
        verbatimTextOutput("calc_log")
      )
    )
  )
)

# ---------------------------
# Server
# ---------------------------

server <- function(input, output, session) {

  rv <- reactiveValues(
    retour_preview = NULL,
    retour_dt = NULL,
    retour_ready = FALSE,
    anomalies_dt = NULL,
    extract_dt = NULL,
    impacts_dt = NULL,
    syn_gs = NULL,
    syn_prtf = NULL,
    logs = list(retour = "", ano = "", impact = "", calc = "")
  )

  # -------------
  # Module 1: Retour4C
  # -------------

  output$retour_status_ui <- renderUI({
    if (is.null(input$retour_file)) {
      tags$div(tags$b("Statut:"), " aucun fichier chargé.")
    } else {
      tags$div(tags$b("Fichier:"), input$retour_file$name)
    }
  })

  output$retour_infos_ui <- renderUI({
    if (is.null(rv$retour_preview) && is.null(rv$retour_dt)) {
      return(tags$div("Chargez un fichier pour afficher les informations."))
    }
    asof_txt <- NA_character_
    if (!is.null(rv$retour_dt) && "ASOF_DT" %in% names(rv$retour_dt)) {
      asof_txt <- asof_label(rv$retour_dt$ASOF_DT)
    } else if (!is.null(rv$retour_preview) && "ASOF_DT" %in% names(rv$retour_preview)) {
      asof_txt <- asof_label(rv$retour_preview$ASOF_DT)
    }
    nobs <- if (!is.null(rv$retour_dt)) nrow(rv$retour_dt) else if (!is.null(rv$retour_preview)) nrow(rv$retour_preview) else NA_integer_
    tags$div(
      tags$b("Arrêté (ASOF_DT): "), asof_txt, tags$br(),
      tags$b("Observations chargées: "), nobs
    )
  })

  output$retour_log <- renderText(rv$logs$retour)

  observeEvent(input$btn_preview, {
    req(input$retour_file)

    res <- safe_run(
      "Retour4C - Preview 5 lignes",
      {
        path <- input$retour_file$datapath
        dt <- data.table::fread(path, nrows = 5, showProgress = FALSE, encoding = "UTF-8")
        setDT(dt)
        # Try parse ASOF_DT if present
        if ("ASOF_DT" %in% names(dt)) {
          dt[, ASOF_DT := as.IDate(ASOF_DT)]
        }
        dt
      },
      prereq = "Fichier Retour4C doit être un .csv ou .txt lisible (séparateur auto)."
    )

    rv$logs$retour <- res$message
    if (res$ok) {
      rv$retour_preview <- res$value
      output$retour_preview_dt <- renderDT({
        datatable(rv$retour_preview, options = list(pageLength = 5, scrollX = TRUE))
      })
    }
  })

  observeEvent(input$btn_load_full, {
    req(input$retour_file)

    start_time <- Sys.time()
    rv$logs$retour <- "Chargement complet en cours...\n"

    withProgress(message = "Chargement Retour4C", value = 0, {
      incProgress(0.05, detail = "Lecture du fichier (fread) ...")

      res <- safe_run(
        "Retour4C - Charger complet + Préparer",
        {
          path <- input$retour_file$datapath
          dt <- data.table::fread(path, showProgress = TRUE, encoding = "UTF-8")
          setDT(dt)

          # Préparation colonnes (best effort)
          # ASOF_DT
          if ("ASOF_DT" %in% names(dt)) {
            dt[, ASOF_DT := as.IDate(ASOF_DT)]
            dt[, ASOF := format(ASOF_DT, "%m/%Y")]
          } else {
            dt[, ASOF := NA_character_]
          }

          # Normalisation ID columns (to help joins later)
          id_cols <- intersect(c("SG_CONTRACT_ID", "SG_CONTR_BCE", "SG_CONTR_3D_ID", "SG_FACILITY_ID", "SG_ID_ENREGISTRMNT"), names(dt))
          for (cc in id_cols) dt[, (cc) := normalize_id(get(cc))]

          # Numeric columns (best effort)
          num_cols <- intersect(c("Encours", "EAD", "RWA", "RW", "PD", "LGD", "M", "CCF", "EAD_B", "EAD_HB"), names(dt))
          for (cc in num_cols) dt[, (cc) := smart_as_numeric(get(cc))]

          # Guarantee some key classification columns exist
          if (!"PRTF" %in% names(dt)) dt[, PRTF := NA_character_]
          if (!"SSPRTF" %in% names(dt)) dt[, SSPRTF := NA_character_]
          if (!"GoldenSource" %in% names(dt)) dt[, GoldenSource := NA_character_]

          dt
        },
        prereq = paste0(
          "Le fichier doit contenir au minimum ASOF_DT (recommandé), EAD et RWA ou EAD+RW pour la partie RWA.",
          " Colonnes utiles: GoldenSource, PRTF, SSPRTF, Encours, EAD, RWA."
        )
      )

      incProgress(0.85, detail = "Préparation terminée.")
      rv$logs$retour <- paste0(rv$logs$retour, "\n", res$message)

      if (res$ok) {
        rv$retour_dt <- res$value
        rv$retour_ready <- TRUE
        incProgress(0.95, detail = "Calcul description globale...")
      } else {
        rv$retour_ready <- FALSE
      }

      incProgress(1, detail = paste0("Terminé (", round(difftime(Sys.time(), start_time, units = "secs"), 1), "s)"))
    })

    # Description globale (Arrêté/Infos)
    output$retour_desc_dt <- renderDT({
      req(rv$retour_ready)
      dt <- rv$retour_dt

      # global description (best effort)
      n <- nrow(dt)
      p <- ncol(dt)
      asof_txt <- if ("ASOF_DT" %in% names(dt)) asof_label(dt$ASOF_DT) else NA_character_

      key_cols <- intersect(c("Encours","EAD","RWA"), names(dt))
      sums <- lapply(key_cols, function(cc) sum(dt[[cc]], na.rm = TRUE))
      names(sums) <- key_cols

      desc <- data.table(
        Champ = c("Nb observations", "Nb colonnes", "Arrêté (ASOF_DT)", paste0("Somme ", key_cols)),
        Valeur = c(
          fmt_num(n, digits = 0),
          fmt_num(p, digits = 0),
          asof_txt,
          vapply(sums, function(v) fmt_num(v, digits = 2), character(1))
        )
      )
      datatable(desc, options = list(dom = "t", scrollX = TRUE))
    })
  })

  # Bouton synthèses (réintroduit)
  observeEvent(input$btn_synth, {
    req(rv$retour_ready)

    res <- safe_run(
      "Retour4C - Synthèses",
      {
        dt <- copy(rv$retour_dt)

        need_cols(dt, c("ASOF","GoldenSource","PRTF"), "Retour4C")
        # Encours/EAD/RWA may not all exist; compute best effort
        if (!"Encours" %in% names(dt)) dt[, Encours := NA_real_]
        if (!"EAD" %in% names(dt)) dt[, EAD := NA_real_]
        if (!"RWA" %in% names(dt)) dt[, RWA := NA_real_]

        # Synthèse GoldenSource
        syn_gs <- dt[, .(
          Nbre_observations = .N,
          Encours_MEUR = sum(Encours, na.rm = TRUE) / 1e6,
          EAD_MEUR     = sum(EAD, na.rm = TRUE) / 1e6,
          RWA_MEUR     = sum(RWA, na.rm = TRUE) / 1e6
        ), by = .(ASOF, GoldenSource)]

        # Total par arrêté
        tot_gs <- syn_gs[, .(
          Nbre_observations = sum(Nbre_observations, na.rm = TRUE),
          Encours_MEUR = sum(Encours_MEUR, na.rm = TRUE),
          EAD_MEUR     = sum(EAD_MEUR, na.rm = TRUE),
          RWA_MEUR     = sum(RWA_MEUR, na.rm = TRUE)
        ), by = .(ASOF)]
        tot_gs[, GoldenSource := "TOTAL"]
        syn_gs <- rbindlist(list(syn_gs, tot_gs), use.names = TRUE, fill = TRUE)
        setorder(syn_gs, ASOF, GoldenSource)

        # Synthèse Portefeuille
        syn_prtf <- dt[, .(
          Nbre_observations = .N,
          Encours_MEUR = sum(Encours, na.rm = TRUE) / 1e6,
          EAD_MEUR     = sum(EAD, na.rm = TRUE) / 1e6,
          RWA_MEUR     = sum(RWA, na.rm = TRUE) / 1e6
        ), by = .(ASOF, PRTF)]

        tot_prtf <- syn_prtf[, .(
          Nbre_observations = sum(Nbre_observations, na.rm = TRUE),
          Encours_MEUR = sum(Encours_MEUR, na.rm = TRUE),
          EAD_MEUR     = sum(EAD_MEUR, na.rm = TRUE),
          RWA_MEUR     = sum(RWA_MEUR, na.rm = TRUE)
        ), by = .(ASOF)]
        tot_prtf[, PRTF := "TOTAL"]
        syn_prtf <- rbindlist(list(syn_prtf, tot_prtf), use.names = TRUE, fill = TRUE)
        setorder(syn_prtf, ASOF, PRTF)

        list(syn_gs = syn_gs, syn_prtf = syn_prtf)
      },
      prereq = "Retour4C doit être chargé complet + préparé. Colonnes recommandées: ASOF_DT, GoldenSource, PRTF, Encours, EAD, RWA."
    )

    rv$logs$retour <- res$message
    if (res$ok) {
      rv$syn_gs <- res$value$syn_gs
      rv$syn_prtf <- res$value$syn_prtf

      output$syn_gs_dt <- renderDT({
        dt <- copy(rv$syn_gs)
        # format for display
        dt[, `:=`(
          Encours_MEUR = fmt_num(Encours_MEUR),
          EAD_MEUR = fmt_num(EAD_MEUR),
          RWA_MEUR = fmt_num(RWA_MEUR),
          Nbre_observations = fmt_num(Nbre_observations, digits = 0)
        )]
        datatable(dt, options = list(pageLength = 20, scrollX = TRUE))
      })

      output$syn_prtf_dt <- renderDT({
        dt <- copy(rv$syn_prtf)
        dt[, `:=`(
          Encours_MEUR = fmt_num(Encours_MEUR),
          EAD_MEUR = fmt_num(EAD_MEUR),
          RWA_MEUR = fmt_num(RWA_MEUR),
          Nbre_observations = fmt_num(Nbre_observations, digits = 0)
        )]
        datatable(dt, options = list(pageLength = 20, scrollX = TRUE))
      })
    }
  })

  # -------------
  # Module 2: Anomalies -> Extraction
  # -------------

  output$ano_log <- renderText(rv$logs$ano)

  observeEvent(input$btn_preview_ano, {
    req(input$ano_file)

    res <- safe_run(
      "Anomalies - Preview",
      {
        dt <- readxl::read_excel(input$ano_file$datapath, n_max = 200)
        dt <- as.data.table(dt)
        if (!"ID_CONTRAT" %in% names(dt)) {
          # tolerate lowercase or variants
          cand <- names(dt)[toupper(names(dt)) == "ID_CONTRAT"]
          if (length(cand) == 1) setnames(dt, cand, "ID_CONTRAT")
        }
        need_cols(dt, c("ID_CONTRAT"), "anomalies_dq.xlsx")
        dt[, ID_CONTRAT := normalize_id(ID_CONTRAT)]
        dt
      },
      prereq = "Le fichier anomalies_dq.xlsx doit contenir une colonne ID_CONTRAT."
    )

    rv$logs$ano <- res$message
    if (res$ok) {
      rv$anomalies_dt <- res$value
      output$ano_preview_dt <- renderDT({
        datatable(head(rv$anomalies_dt, 5), options = list(dom = "t", scrollX = TRUE))
      })
    }
  })

  observeEvent(input$btn_extract, {
    req(rv$retour_ready)
    req(rv$anomalies_dt)

    res <- safe_run(
      "Anomalies → Extraction",
      {
        dt_ret <- rv$retour_dt
        dt_ano <- copy(rv$anomalies_dt)

        need_cols(dt_ano, c("ID_CONTRAT"), "anomalies")
        dt_ano[, ID_CONTRAT := normalize_id(ID_CONTRAT)]
        dt_ano <- dt_ano[nzchar(ID_CONTRAT)]
        if (nrow(dt_ano) == 0) stop("Aucun ID_CONTRAT non vide dans anomalies.", call. = FALSE)

        # Columns to match in Retour4C
        match_cols <- intersect(c("SG_CONTRACT_ID", "SG_CONTR_BCE", "SG_CONTR_3D_ID", "SG_FACILITY_ID", "SG_ID_ENREGISTRMNT"), names(dt_ret))
        if (length(match_cols) == 0) {
          stop("Aucune colonne d'identifiant de contrat trouvée dans Retour4C (SG_CONTRACT_ID, SG_CONTR_BCE, ...).", call. = FALSE)
        }

        # Ensure match columns are normalized
        for (cc in match_cols) dt_ret[, (cc) := normalize_id(get(cc))]

        # Perform joins per column and rbind
        dt_ids <- unique(dt_ano[, .(ID_CONTRAT)])
        out_list <- list()

        for (cc in match_cols) {
          # dynamic join: dt_ret[dt_ids, on=.(cc = ID_CONTRAT)]
          tmp <- dt_ret[dt_ids, on = setNames("ID_CONTRAT", cc), nomatch = 0]
          if (nrow(tmp) > 0) {
            tmp[, matched_on := cc]
            tmp[, ID_CONTRAT := get(cc)]  # matched value
            out_list[[cc]] <- tmp
          }
        }

        out <- rbindlist(out_list, use.names = TRUE, fill = TRUE)
        if (nrow(out) == 0) {
          # Provide diagnostic: sample IDs and sample retour IDs
          diag <- paste0(
            "Aucune correspondance.\n",
            "Vérifier: (1) types (texte), (2) espaces/zeros, (3) colonne ID utilisée.\n",
            "Exemples anomalies IDs: ", paste(head(dt_ids$ID_CONTRAT, 5), collapse = " | "), "\n",
            "Colonnes testées: ", paste(match_cols, collapse = ", ")
          )
          stop(diag, call. = FALSE)
        }

        # Keep requested output columns (best effort)
        keep <- intersect(
          c("ID_CONTRAT","matched_on","ASOF_DT","ASOF","PRTF","SSPRTF","Segment","PRODUCT_ID","Encours","EAD","RWA",
            "SG_CONTRACT_ID","SG_CONTR_BCE","SG_CONTR_3D_ID","SG_FACILITY_ID","SG_ID_ENREGISTRMNT","GoldenSource","RW","PD","LGD","M","CCF"),
          names(out)
        )
        out <- out[, ..keep]

        # De-duplicate: keep one row per ID_CONTRAT per matched_on + ASOF (best effort)
        if ("ASOF" %in% names(out)) {
          setorder(out, ID_CONTRAT, matched_on, ASOF)
        } else {
          setorder(out, ID_CONTRAT, matched_on)
        }

        out
      },
      prereq = "Pré-requis: Retour4C chargé complet; anomalies chargées (preview OK). Retour4C doit contenir une ou plusieurs colonnes d'ID (SG_CONTRACT_ID...)."
    )

    rv$logs$ano <- res$message

    if (res$ok) {
      rv$extract_dt <- res$value

      output$extract_head_dt <- renderDT({
        datatable(head(rv$extract_dt, 5), options = list(dom = "t", scrollX = TRUE))
      })
      output$extract_tail_dt <- renderDT({
        datatable(tail(rv$extract_dt, 5), options = list(dom = "t", scrollX = TRUE))
      })

      # write excel for download
      tmp <- file.path(tempdir(), "ead_rwa_ano_dq.xlsx")
      wb <- createWorkbook()
      addWorksheet(wb, "extract")
      writeDataTable(wb, "extract", rv$extract_dt)
      saveWorkbook(wb, tmp, overwrite = TRUE)

      output$dl_extract <- downloadHandler(
        filename = function() "ead_rwa_ano_dq.xlsx",
        content = function(file) file.copy(tmp, file, overwrite = TRUE)
      )
    }
  })

  # -------------
  # Module 3: ImpactsRWA (recalc & compare)
  # -------------

  output$impact_log <- renderText(rv$logs$impact)

  observeEvent(input$btn_compute_impacts, {
    req(rv$retour_ready)
    req(rv$anomalies_dt)
    req(rv$extract_dt)

    res <- safe_run(
      "ImpactsRWA - Recalcul + comparaison",
      {
        dt_base <- copy(rv$extract_dt)
        dt_ano  <- copy(rv$anomalies_dt)

        # Normalize and keep ID
        need_cols(dt_ano, c("ID_CONTRAT"), "anomalies")
        dt_ano[, ID_CONTRAT := normalize_id(ID_CONTRAT)]
        dt_base[, ID_CONTRAT := normalize_id(ID_CONTRAT)]

        # Merge corrections onto base extract
        setkey(dt_base, ID_CONTRAT)
        setkey(dt_ano, ID_CONTRAT)
        dt <- dt_ano[dt_base]  # left join: keep base rows with anomalies fields

        # Ensure base columns exist
        if (!"EAD" %in% names(dt)) dt[, EAD := NA_real_]
        if (!"RWA" %in% names(dt)) dt[, RWA := NA_real_]
        if (!"RW" %in% names(dt)) {
          # infer RW from RWA/EAD if possible
          dt[, RW := fifelse(!is.na(RWA) & !is.na(EAD) & EAD > 0, RWA / EAD, NA_real_)]
        }

        if (!"PRTF" %in% names(dt)) dt[, PRTF := "Non Retail"]
        if (!"SSPRTF" %in% names(dt)) dt[, SSPRTF := "Non Retail"]

        # Corrections columns (optional): RW_corrige, EAD_corrige, CCF_corrige, Surete, PD_corrige, LGD_corrige, M_corrige
        # tolerate different case
        nm <- names(dt)
        rename_if <- function(old, new) {
          cand <- nm[toupper(nm) == toupper(old)]
          if (length(cand) == 1 && cand != new) setnames(dt, cand, new)
        }
        rename_if("EAD_corrige","EAD_corrige")
        rename_if("RW_corrige","RW_corrige")
        rename_if("CCF_corrige","CCF_corrige")
        rename_if("Surete","Surete")
        rename_if("PD_corrige","PD_corrige")
        rename_if("LGD_corrige","LGD_corrige")
        rename_if("M_corrige","M_corrige")
        rename_if("PortBalois_corrige","PortBalois_corrige")
        rename_if("SSPRTF_corrige","SSPRTF_corrige")

        # Apply corrections with fallback (partial corrections allowed)
        dt[, EAD_base := smart_as_numeric(EAD)]
        dt[, RW_base  := smart_as_numeric(RW)]
        dt[, RWA_base := smart_as_numeric(RWA)]

        if (!"EAD_corrige" %in% names(dt)) dt[, EAD_corrige := NA_real_]
        if (!"RW_corrige" %in% names(dt)) dt[, RW_corrige := NA_real_]
        if (!"Surete" %in% names(dt)) dt[, Surete := 0]
        if (!"CCF_corrige" %in% names(dt)) dt[, CCF_corrige := NA_real_]

        # Update portfolio classification if provided
        if ("PortBalois_corrige" %in% names(dt)) {
          dt[!is.na(PortBalois_corrige) & nzchar(as.character(PortBalois_corrige)), PRTF := as.character(PortBalois_corrige)]
        }
        if ("SSPRTF_corrige" %in% names(dt)) {
          dt[!is.na(SSPRTF_corrige) & nzchar(as.character(SSPRTF_corrige)), SSPRTF := as.character(SSPRTF_corrige)]
        }

        dt[, EAD_used := fifelse(!is.na(EAD_corrige), smart_as_numeric(EAD_corrige), EAD_base)]
        dt[, RW_used  := fifelse(!is.na(RW_corrige),  smart_as_numeric(RW_corrige),  RW_base)]

        # Apply surety haircut (app logic)
        hc <- input$haircut
        dt[, EAD_adj := apply_surety_haircut(EAD_used, Surete, hc)]

        # Compute corrected RWA according to selected method
        method <- input$impact_method

        if (method == "SA") {
          dt[, RWA_calc := compute_rwa_sa(EAD_adj, RW_used)]
        } else {
          # IRBA needs PD/LGD/M (base + optional corrections)
          if (!"PD" %in% names(dt)) dt[, PD := NA_real_]
          if (!"LGD" %in% names(dt)) dt[, LGD := NA_real_]
          if (!"M" %in% names(dt)) dt[, M := 2.5]

          if (!"PD_corrige" %in% names(dt)) dt[, PD_corrige := NA_real_]
          if (!"LGD_corrige" %in% names(dt)) dt[, LGD_corrige := NA_real_]
          if (!"M_corrige" %in% names(dt)) dt[, M_corrige := NA_real_]

          dt[, PD_used  := fifelse(!is.na(PD_corrige),  smart_as_numeric(PD_corrige),  smart_as_numeric(PD))]
          dt[, LGD_used := fifelse(!is.na(LGD_corrige), smart_as_numeric(LGD_corrige), smart_as_numeric(LGD))]
          dt[, M_used   := fifelse(!is.na(M_corrige),   smart_as_numeric(M_corrige),   smart_as_numeric(M))]

          # default flag optional
          default_flag <- if ("DEFAULT_FLAG" %in% names(dt)) dt$DEFAULT_FLAG else NULL

          dt[, RWA_calc := compute_rwa_irb(
            EAD = EAD_adj, PD = PD_used, LGD = LGD_used, M = M_used,
            PRTF = PRTF, SSPRTF = SSPRTF, default_flag = default_flag, scaling = 1.0
          )]
        }

        # Optional floor (72.5% SA) computed on top
        if (isTRUE(input$use_floor)) {
          dt[, RWA_SA_floor := compute_rwa_sa(EAD_adj, RW_used)]
          if (method == "IRB") {
            dt[, RWA_floor := pmax(RWA_calc, 0.725 * RWA_SA_floor)]
          } else {
            dt[, RWA_floor := 0.725 * RWA_SA_floor]
          }
        }

        # Impact vs base RWA from Retour4C extract
        dt[, Delta_RWA := RWA_calc - RWA_base]
        dt[, Delta_RWA_pct := fifelse(RWA_base > 0, Delta_RWA / RWA_base, NA_real_)]

        # Driver (simple)
        dt[, driver := fifelse(!is.na(EAD_corrige) & is.na(RW_corrige), "EAD",
                        fifelse(is.na(EAD_corrige) & !is.na(RW_corrige), "RW",
                          fifelse(!is.na(EAD_corrige) & !is.na(RW_corrige), "EAD+RW",
                            fifelse(Surete >= 1, "Surete", "Autre")
                          )))]

        # Output subset for UI
        keep <- intersect(
          c("ID_CONTRAT","matched_on","ASOF","PRTF","SSPRTF","GoldenSource",
            "EAD_base","RW_base","RWA_base",
            "EAD_used","RW_used","EAD_adj","RWA_calc","Delta_RWA","Delta_RWA_pct","driver"),
          names(dt)
        )
        dt_out <- dt[, ..keep]

        # Synthesis by PRTF/SSPRTF
        syn <- dt[, .(
          Nbre_observations = .N,
          EAD_MEUR = sum(EAD_base, na.rm = TRUE)/1e6,
          RWA_base_MEUR = sum(RWA_base, na.rm = TRUE)/1e6,
          RWA_calc_MEUR = sum(RWA_calc, na.rm = TRUE)/1e6,
          Delta_RWA_MEUR = sum(Delta_RWA, na.rm = TRUE)/1e6
        ), by = .(PRTF, SSPRTF)]
        setorder(syn, PRTF, SSPRTF)

        list(dt = dt, dt_out = dt_out, syn = syn)
      },
      prereq = "Pré-requis: Module 1 (Retour4C) chargé complet; Module 2 extraction réussie; anomalies chargées. Colonnes utiles: EAD/RW/RWA et (IRBA) PD/LGD/M."
    )

    rv$logs$impact <- res$message

    if (res$ok) {
      rv$impacts_dt <- res$value$dt

      output$impacts_dt <- renderDT({
        dt <- copy(res$value$dt_out)
        # format for display
        num_cols <- intersect(c("EAD_base","RW_base","RWA_base","EAD_used","RW_used","EAD_adj","RWA_calc","Delta_RWA","Delta_RWA_pct"), names(dt))
        for (cc in num_cols) dt[, (cc) := fmt_num(get(cc), digits = ifelse(cc == "Delta_RWA_pct", 4, 2))]
        datatable(dt, options = list(pageLength = 20, scrollX = TRUE))
      })

      output$impact_syn_dt <- renderDT({
        syn <- copy(res$value$syn)
        syn[, `:=`(
          Nbre_observations = fmt_num(Nbre_observations, digits = 0),
          EAD_MEUR = fmt_num(EAD_MEUR),
          RWA_base_MEUR = fmt_num(RWA_base_MEUR),
          RWA_calc_MEUR = fmt_num(RWA_calc_MEUR),
          Delta_RWA_MEUR = fmt_num(Delta_RWA_MEUR)
        )]
        datatable(syn, options = list(pageLength = 20, scrollX = TRUE))
      })

      # Export Excel
      tmp <- file.path(tempdir(), "impacts_rwa.xlsx")
      wb <- createWorkbook()
      addWorksheet(wb, "impacts_detail")
      addWorksheet(wb, "impacts_synthese")
      writeDataTable(wb, "impacts_detail", rv$impacts_dt)
      writeDataTable(wb, "impacts_synthese", res$value$syn)
      saveWorkbook(wb, tmp, overwrite = TRUE)

      output$dl_impacts <- downloadHandler(
        filename = function() "impacts_rwa.xlsx",
        content = function(file) file.copy(tmp, file, overwrite = TRUE)
      )
    }
  })

  # -------------
  # Module 4: Calcul RWA (IHM)
  # -------------

  output$calc_log <- renderText(rv$logs$calc)

  observeEvent(input$btn_calc, {
    res <- safe_run(
      "Calcul RWA (IHM)",
      {
        method <- input$calc_method
        prtf <- input$calc_prtf
        ssprtf <- input$calc_ssprtf

        # base inputs
        ead0 <- smart_as_numeric(input$ead_init)
        ead1 <- smart_as_numeric(input$ead_corr)
        rw0  <- smart_as_numeric(input$rw_init)
        rw1  <- smart_as_numeric(input$rw_corr)

        surete <- ifelse(isTRUE(input$surete_flag), 1, 0)
        hc <- smart_as_numeric(input$hc_calc)

        # fallback corrected to initial if NA
        ead_corr_used <- ifelse(is.na(ead1), ead0, ead1)
        rw_corr_used  <- ifelse(is.na(rw1),  rw0,  rw1)

        # apply surety
        ead0_adj <- apply_surety_haircut(ead0, surete, hc)
        ead1_adj <- apply_surety_haircut(ead_corr_used, surete, hc)

        # optional bilan/hb
        # if bilan/hb provided, compute RWA split
        eb0 <- smart_as_numeric(input$ead_b_init)
        eb1 <- smart_as_numeric(input$ead_b_corr)
        ehb0 <- smart_as_numeric(input$ead_hb_init)
        ehb1 <- smart_as_numeric(input$ead_hb_corr)
        ccf0 <- smart_as_numeric(input$ccf_init)
        ccf1 <- smart_as_numeric(input$ccf_corr)

        # IRB params
        pd0 <- smart_as_numeric(input$pd_init)
        pd1 <- smart_as_numeric(input$pd_corr)
        lgd0 <- smart_as_numeric(input$lgd_init)
        lgd1 <- smart_as_numeric(input$lgd_corr)
        m0 <- smart_as_numeric(input$m_init)
        m1 <- smart_as_numeric(input$m_corr)

        pd_used0 <- ifelse(is.na(pd0), NA_real_, pd0)
        pd_used1 <- ifelse(is.na(pd1), pd_used0, pd1)
        lgd_used0 <- ifelse(is.na(lgd0), NA_real_, lgd0)
        lgd_used1 <- ifelse(is.na(lgd1), lgd_used0, lgd1)
        m_used0 <- ifelse(is.na(m0), 2.5, m0)
        m_used1 <- ifelse(is.na(m1), m_used0, m1)

        # compute main RWA
        if (method == "SA") {
          rwa0 <- compute_rwa_sa(ead0_adj, rw0)
          rwa1 <- compute_rwa_sa(ead1_adj, rw_corr_used)
        } else {
          # map SSPRTF values to internal (Immobilier -> retail_other here; Revolving -> qrre)
          prtf_map <- ifelse(prtf == "Retail", "Retail", "Non Retail")
          ss_map <- ifelse(prtf == "Retail",
                           ifelse(ssprtf == "Revolving", "Revolving", "Autres"),
                           "Non Retail")

          rwa0 <- compute_rwa_irb(ead0_adj, pd_used0, lgd_used0, m_used0, prtf_map, ss_map, default_flag = 0L)
          rwa1 <- compute_rwa_irb(ead1_adj, pd_used1, lgd_used1, m_used1, prtf_map, ss_map, default_flag = 0L)
        }

        # compute bilan/hors-bilan RWA if possible (SA logic)
        split <- NULL
        if (!is.na(ehb0) && !is.na(ccf0)) {
          # HB uses EHB*CCF
          ead_hb0 <- apply_surety_haircut(ehb0 * ccf0, surete, hc)
          ead_hb1 <- if (!is.na(ehb1) && !is.na(ccf1)) apply_surety_haircut(ehb1 * ccf1, surete, hc) else ead_hb0
          # Bilan if provided
          if (!is.na(eb0)) {
            ead_b0 <- apply_surety_haircut(eb0, surete, hc)
            ead_b1 <- if (!is.na(eb1)) apply_surety_haircut(eb1, surete, hc) else ead_b0
          } else {
            ead_b0 <- NA_real_; ead_b1 <- NA_real_
          }

          if (method == "SA") {
            rwa_hb0 <- compute_rwa_sa(ead_hb0, rw0)
            rwa_hb1 <- compute_rwa_sa(ead_hb1, rw_corr_used)
            rwa_b0  <- if (!is.na(ead_b0)) compute_rwa_sa(ead_b0, rw0) else NA_real_
            rwa_b1  <- if (!is.na(ead_b1)) compute_rwa_sa(ead_b1, rw_corr_used) else NA_real_
          } else {
            prtf_map <- ifelse(prtf == "Retail", "Retail", "Non Retail")
            ss_map <- ifelse(prtf == "Retail",
                             ifelse(ssprtf == "Revolving", "Revolving", "Autres"),
                             "Non Retail")
            rwa_hb0 <- compute_rwa_irb(ead_hb0, pd_used0, lgd_used0, m_used0, prtf_map, ss_map, default_flag = 0L)
            rwa_hb1 <- compute_rwa_irb(ead_hb1, pd_used1, lgd_used1, m_used1, prtf_map, ss_map, default_flag = 0L)
            rwa_b0  <- if (!is.na(ead_b0)) compute_rwa_irb(ead_b0, pd_used0, lgd_used0, m_used0, prtf_map, ss_map, default_flag = 0L) else NA_real_
            rwa_b1  <- if (!is.na(ead_b1)) compute_rwa_irb(ead_b1, pd_used1, lgd_used1, m_used1, prtf_map, ss_map, default_flag = 0L) else NA_real_
          }

          split <- data.table(
            Bloc = c("Bilan", "Hors bilan", "Total"),
            RWA_initial = c(rwa_b0, rwa_hb0, sum(c(rwa_b0, rwa_hb0), na.rm = TRUE)),
            RWA_corrige = c(rwa_b1, rwa_hb1, sum(c(rwa_b1, rwa_hb1), na.rm = TRUE))
          )
          split[, Delta := RWA_corrige - RWA_initial]
        }

        main <- data.table(
          Indicateur = c("RWA initial", "RWA corrigé", "Delta (corr - init)"),
          Valeur = c(rwa0, rwa1, rwa1 - rwa0)
        )

        list(main = main, split = split)
      },
      prereq = "Pré-requis: EAD initial et RW initial (SA) ou PD/LGD/M (IRBA). Les champs corrigés sont optionnels. Pour bilan/hors-bilan: EAD_HB et CCF."
    )

    rv$logs$calc <- res$message

    if (res$ok) {
      main <- res$value$main
      split <- res$value$split

      output$calc_table_dt <- renderDT({
        # build display table
        m <- copy(main)
        m[, Valeur := fmt_num(Valeur)]
        if (!is.null(split)) {
          s <- copy(split)
          s[, `:=`(
            RWA_initial = fmt_num(RWA_initial),
            RWA_corrige = fmt_num(RWA_corrige),
            Delta = fmt_num(Delta)
          )]
          # show as concatenated view (simple)
          datatable(
            rbindlist(list(
              data.table(Section = "Global", m),
              data.table(Section = "Bilan/HB", Indicateur = s$Bloc, Valeur = s$RWA_initial) # just keep simple
            ), fill = TRUE),
            options = list(pageLength = 20, scrollX = TRUE)
          )
        } else {
          datatable(m, options = list(dom = "t", scrollX = TRUE))
        }
      })

      output$calc_plot <- renderPlot({
        df <- data.table(
          type = c("Initial", "Corrigé"),
          RWA = c(res$value$main$Valeur[1], res$value$main$Valeur[2])
        )
        ggplot(df, aes(x = type, y = RWA)) +
          geom_col() +
          labs(title = "RWA – Initial vs Corrigé", x = NULL, y = "RWA")
      })
    }
  })
}

shinyApp(ui, server)
