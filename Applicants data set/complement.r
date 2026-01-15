

# 1. progress % + texte dans la console Rscript (démarrage)

# 2. progress Shiny sur chaque action + un statut global persistant dans l’IHM



# 1 calculetteRWA/run_app.R — Progress console + log

# ---- PROGRESS (console + log) -----------------------------------------
progress_console <- function(pct, txt, log_file = NULL) {
  pct <- max(0, min(100, as.integer(pct)))
  line <- sprintf("[%-3s%%] %s", pct, txt)
  cat(line, "\n")
  flush.console()
  if (!is.null(log_file)) {
    dir.create(dirname(log_file), recursive = TRUE, showWarnings = FALSE)
    cat(paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", line, "\n"),
        file = log_file, append = TRUE)
  }
}
# ----------------------------------------------------------------------



# 1.2 Remplacer / Ajouter les étapes %

progress_console(0,  "Démarrage calculetteRWA", LOG_FILE)

progress_console(5,  "Détection des chemins (SCRIPT_DIR / APP_HOME)", LOG_FILE)
# ... ton code SCRIPT_DIR / APP_HOME

progress_console(15, "Vérification arborescence (app/app.R, R/bin)", LOG_FILE)
# ... checks

progress_console(25, "Configuration TEMP/TMPDIR", LOG_FILE)
# ... Sys.setenv(TMPDIR=...)

progress_console(35, "Configuration .libPaths (portable)", LOG_FILE)
# ... .libPaths

progress_console(45, "Contrôle packages (shiny, httpuv)", LOG_FILE)
# ... requireNamespace

progress_console(60, "Sélection d'un port disponible", LOG_FILE)
# ... httpuv::randomPort()

progress_console(75, "Lancement serveur Shiny", LOG_FILE)
# ... shiny::runApp(...)

# Optionnel : si tu as un launch.browser custom
progress_console(90, "Ouverture navigateur", LOG_FILE)

# Ne sera affiché qu'après fermeture de l'app
progress_console(100, "Fin (app fermée)", LOG_FILE)



# ----------------------------------------------------------------------



# ----------------------------------------------------------------------
# 2) ✅ calculetteRWA/app/app.R — Statut global persistant dans l’IHM

# 2.1 Ajouter une sortie UI “progress”

hr(),
uiOutput("progress_ui"),


# 2.2 Ajouter le renderUI côté server

# ---- PROGRESS UI (global) ---------------------------------------------
rv$progress_pct  <- 0
rv$progress_text <- "Prêt."

output$progress_ui <- renderUI({
  pct <- rv$progress_pct %||% 0
  txt <- rv$progress_text %||% ""
  tags$div(
    tags$b("Avancement : "), paste0(pct, "%"),
    tags$div(
      style = "width:100%; background:#e9ecef; border-radius:6px; height:10px; margin-top:6px;",
      tags$div(style = paste0("width:", pct, "%; background:#198754; height:10px; border-radius:6px;"))
    ),
    tags$div(style = "margin-top:6px; font-size: 0.9em;", txt)
  )
})
# ----------------------------------------------------------------------

# 2.3 Ajouter une fonction utilitaire set_progress()

set_progress <- function(rv, pct, txt) {
  rv$progress_pct  <- max(0, min(100, as.integer(pct)))
  rv$progress_text <- as.character(txt)
}

# ----------------------------------------------------------------------







# ----------------------------------------------------------------------
# 3) ✅ Progress Shiny sur chaque action (Module 1–4)

withProgress(message = "...", value = 0, {
  set_progress(rv, 5,  "...")
  incProgress(0.05, detail = "...")

  ...
})

# 3.A Module 1 : Retour4C (04_module1_retour4c_server.R)

withProgress(message = "Retour4C : preview", value = 0, {
  set_progress(rv, 5, "Lecture des 5 premières lignes...")
  incProgress(0.2, detail = "Lecture (fread nrows=5)")

  # ton fread nrows=5 ici

  set_progress(rv, 100, "Preview affichée.")
  incProgress(1, detail = "Terminé")
})




# A2) Bouton Charger complet + Préparer (btn_prepare / btn_load_full)

withProgress(message = "Retour4C : chargement complet + préparation", value = 0, {
  set_progress(rv, 5, "Lecture du fichier Retour4C (peut prendre du temps)...")
  incProgress(0.1, detail = "Lecture fread")

  # fread complet

  set_progress(rv, 55, "Préparation des données (ASOF, IDs, numériques)...")
  incProgress(0.5, detail = "Préparation colonnes")

  # parse date, normalize ids, numeric

  set_progress(rv, 85, "Contrôles et finalisation...")
  incProgress(0.3, detail = "Contrôles")

  # checks

  set_progress(rv, 100, "Retour4C prêt pour synthèses.")
  incProgress(0.1, detail = "Terminé")
})



# A3) Bouton Synthèses (btn_synth)

withProgress(message = "Synthèses Retour4C", value = 0, {
  set_progress(rv, 10, "Calcul synthèse GoldenSource...")
  incProgress(0.4, detail = "GoldenSource")

  # calc syn_gs

  set_progress(rv, 65, "Calcul synthèse Portefeuille Bâlois (PRTF)...")
  incProgress(0.4, detail = "Portefeuille Bâlois")

  # calc syn_prtf

  set_progress(rv, 100, "Synthèses terminées.")
  incProgress(0.2, detail = "Terminé")
})



# 3.B Module 2 : Anomalies → Extraction

withProgress(message = "Anomalies : preview", value = 0, {
  set_progress(rv, 10, "Lecture anomalies_dq.xlsx...")
  incProgress(0.6, detail = "readxl::read_excel")

  # read_excel

  set_progress(rv, 100, "Preview anomalies affichée.")
  incProgress(0.4, detail = "Terminé")
})




# B2) Extraction (btn_extract)

withProgress(message = "Extraction : anomalies → Retour4C", value = 0, {
  set_progress(rv, 5, "Préparation des ID_CONTRAT...")
  incProgress(0.15, detail = "Normalisation ID")

  # normalize ids

  set_progress(rv, 30, "Recherche des correspondances dans Retour4C...")
  incProgress(0.45, detail = "Joins sur SG_CONTRACT_ID / ...")

  # joins

  set_progress(rv, 80, "Génération du fichier ead_rwa_ano_dq.xlsx...")
  incProgress(0.3, detail = "Export openxlsx")

  # export

  set_progress(rv, 100, "Extraction terminée.")
  incProgress(0.1, detail = "Terminé")
})



# 3.C Module 3 : ImpactsRWA (06_module3_impacts_server.R)

withProgress(message = "ImpactsRWA : recalcul + comparaison", value = 0, {
  set_progress(rv, 10, "Fusion extraction + corrections anomalies...")
  incProgress(0.25, detail = "Join corrections")

  # merge dt_ano + extract

  set_progress(rv, 45, "Application corrections (RW_corrige, EAD_corrige, Surete...)")
  incProgress(0.25, detail = "Apply corrections")

  # apply corrections

  set_progress(rv, 70, "Recalcul RWA (Standard/IRBA)...")
  incProgress(0.35, detail = "Compute RWA")

  # compute

  set_progress(rv, 90, "Synthèse + export Excel...")
  incProgress(0.15, detail = "Export")

  # export

  set_progress(rv, 100, "ImpactsRWA terminé.")
  incProgress(0.1, detail = "Terminé")
})



# 3.D Module 4 : Calcul RWA (07_module4_calcul_server.R)

withProgress(message = "Calcul RWA", value = 0, {
  set_progress(rv, 20, "Lecture des paramètres...")
  incProgress(0.2, detail = "Inputs")

  # read inputs

  set_progress(rv, 60, "Calcul RWA initial...")
  incProgress(0.4, detail = "Initial")

  # compute initial

  set_progress(rv, 85, "Calcul RWA corrigé + graphique...")
  incProgress(0.3, detail = "Corrigé")

  # compute corrected + plot

  set_progress(rv, 100, "Calcul terminé.")
  incProgress(0.1, detail = "Terminé")
})





# 5) Point important : garder le statut “Prêt”

set_progress(rv, 100, "Terminé.")










# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# ----------------------------------------------------------------------




# A) Remplacer GoldenSource par System (SYSTEM_CODE)

# ============================================================
# [02_io_retour4c.R] - Normalisation des colonnes "référentielles"
# Objectif : créer des colonnes canoniques attendues par l'appli.
# Ici : System basé sur SYSTEM_CODE
# ============================================================

# ---- Canonicalisation : System = SYSTEM_CODE --------------------------
if ("SYSTEM_CODE" %in% names(dt)) {
  dt[, System := as.character(SYSTEM_CODE)]
} else {
  # sécurité : si SYSTEM_CODE absent on garde la colonne pour éviter crash
  dt[, System := NA_character_]
}
# ----------------------------------------------------------------------



# B) Module 1 – Recherche globale “top 5” sur tout Retour4C

# --- Recherche globale sur Retour4C (top 5) ----------------------------
textInput("global_search", "Recherche globale (top 5 sur Retour4C)", value = ""),
actionButton("btn_global_search", "Chercher", class = "btn-info"),
# ----------------------------------------------------------------------


# Dans l’espace “Arrêté/Infos/Preview”, juste après DTOutput("retour_preview_dt"),

DTOutput("retour_search_dt"),




# B2) Niveau server Module 1
# ============================================================
# [04_module1_retour4c_server.R] - Recherche globale (top 5)
# Objectif : rechercher dans toutes les colonnes texte et renvoyer 5 lignes
# ============================================================
observeEvent(input$btn_global_search, {
  req(rv$retour_ready)
  q <- trimws(input$global_search)
  req(nzchar(q))

  withProgress(message = "Recherche globale Retour4C", value = 0, {
    incProgress(0.2, detail = "Préparation...")
    dt <- rv$retour_dt

    incProgress(0.6, detail = "Recherche sur colonnes texte...")
    chr_cols <- names(dt)[vapply(dt, is.character, logical(1))]
    if (length(chr_cols) == 0) chr_cols <- names(dt)

    idx <- rep(FALSE, nrow(dt))
    for (cc in chr_cols) {
      x <- dt[[cc]]
      idx <- idx | (!is.na(x) & grepl(q, x, fixed = TRUE))
      if (sum(idx) >= 5) break
    }

    res <- dt[idx][1:min(5, sum(idx))]
    incProgress(1, detail = "Terminé")

    output$retour_search_dt <- renderDT({
      datatable(res, options = list(pageLength = 5, scrollX = TRUE))
    })
  })
})



# C) Module 2 – Suppression des doublons avec priorité SG_CONTRACT_ID

# ============================================================
# [05_module2_anomalies_server.R] - Déduplication extraction
# Règle : conserver 1 ligne par ID_CONTRAT (et arrêté si dispo),
# priorité absolue : matched_on == "SG_CONTRACT_ID"
# ============================================================

# Priorité (1 = meilleur)
prio <- c("SG_CONTRACT_ID", "SG_CONTR_BCE_ID", "SG_CONTR_3D_ID", "SG_FACILITY_ID", "SG_ID_ENREGISTRMNT")
out[, matched_prio := match(matched_on, prio)]
out[is.na(matched_prio), matched_prio := 999L]

# Dédup clé : ID_CONTRAT + ASOF_DT si présent, sinon ID seulement
if ("ASOF_DT" %in% names(out)) {
  setorder(out, ID_CONTRAT, ASOF_DT, matched_prio)
  out <- out[ , .SD[1], by = .(ID_CONTRAT, ASOF_DT)]
} else {
  setorder(out, ID_CONTRAT, matched_prio)
  out <- out[ , .SD[1], by = .(ID_CONTRAT)]
}

out[, matched_prio := NULL]



# D1) Supprimer le haircut partout
dt[, EAD_adj := EAD_used]  # aucune correction haircut


# D2) MTBAL (méthode par ligne)

# ============================================================
# [02_io_retour4c.R] - Normalisation MTBAL
# MTBAL : 1 = Standard (SA), 3 = IRBA
# ============================================================
if (!"MTBAL" %in% names(dt)) dt[, MTBAL := 1L]
dt[, MTBAL := as.integer(MTBAL)]




# D3) UI Module 3 : supprimer méthode + haircut, ajouter dropdown
# ============================================================
# [Module 3 UI] - Choix d'une seule anomalie (scénario)
# Une seule variable est corrigée à la fois.
# ============================================================
selectInput("dq_type", "Type d’anomalie DQ", choices = c(
  "Sûreté manquante (Collateral non prise en compte)" = "SURETE",
  "Mauvaise classification Retail/NonRetail" = "CLASSIF",
  "Erreur statut défaut/sain" = "DEFAULT",
  "CCF erroné (+/- %)" = "CCF",
  "EAD erroné (+/- %)" = "EAD",
  "PD erroné (+/- %)" = "PD",
  "LGD erroné (+/- %)" = "LGD",
  "Erreur Bilan/HorsBilan (+/- %)" = "BILANHB"
), selected = "EAD"),

uiOutput("dq_inputs_ui"),
actionButton("btn_apply_dq", "Appliquer scénario DQ", class = "btn-success"),
hr(),
actionButton("btn_compute_impacts", "Recalculer + comparer", class = "btn-success"),
downloadButton("dl_impacts", "Télécharger impacts_rwa.xlsx")



# D4) Server Module 3 : champs dynamiques + application correction

# ============================================================
# [06_module3_impacts_server.R] - Inputs dynamiques selon anomalie
# ============================================================
output$dq_inputs_ui <- renderUI({
  switch(input$dq_type,
    "SURETE" = tagList(
      selectInput("surete_init", "Sûreté initiale (Retour4C)", choices = c(0,1), selected = 0),
      selectInput("surete_corr", "Sûreté corrigée", choices = c(0,1), selected = 1)
    ),
    "CLASSIF" = tagList(
      selectInput("prtf_corr", "PRTF corrigé", choices = c("Retail","Non Retail"), selected = "Non Retail")
    ),
    "DEFAULT" = tagList(
      selectInput("stat_corr", "Statut corrigé", choices = c("sain","defaut"), selected = "sain"),
      tags$small("Règle : PD=1 => défaut. Sinon sain. La correction agit via la PD.")
    ),
    # Cas +/- %
    tagList(
      numericInput("delta_pct", "Variation corrigée (+/- %)", value = 0, step = 1),
      tags$small("Formule: Valeur_corrigée = Valeur_Retour4C * (1 - delta_pct/100).")
    )
  )
})



# D4.2 Appliquer le scénario (une seule variable)

# ============================================================
# [06_module3_impacts_server.R] - Application du scénario DQ
# - Une seule variable est corrigée à la fois
# - La base est la valeur Retour4C (et Excel *_corrige si présent)
# ============================================================
observeEvent(input$btn_apply_dq, {
  req(rv$extract_dt)
  dt <- rv$impacts_dt %or% copy(rv$extract_dt)  # selon votre variable de travail

  # Préparer colonnes base
  if (!"EAD" %in% names(dt)) dt[, EAD := NA_real_]
  if (!"PD" %in% names(dt)) dt[, PD := NA_real_]
  if (!"LGD" %in% names(dt)) dt[, LGD := NA_real_]
  if (!"CCF" %in% names(dt)) dt[, CCF := NA_real_]

  dt[, EAD_base := as.numeric(EAD)]
  dt[, PD_base  := as.numeric(PD)]
  dt[, LGD_base := as.numeric(LGD)]
  dt[, CCF_base := as.numeric(CCF)]

  # Appliquer Excel *_corrige en priorité sur base (si présent)
  # (mais une seule variable sera ensuite surchargée par le scénario UI)
  if ("EAD_corrige" %in% names(dt)) dt[, EAD_used := fifelse(!is.na(EAD_corrige), as.numeric(EAD_corrige), EAD_base)] else dt[, EAD_used := EAD_base]
  if ("PD_corrige"  %in% names(dt)) dt[, PD_used  := fifelse(!is.na(PD_corrige),  as.numeric(PD_corrige),  PD_base)]  else dt[, PD_used  := PD_base]
  if ("LGD_corrige" %in% names(dt)) dt[, LGD_used := fifelse(!is.na(LGD_corrige), as.numeric(LGD_corrige), LGD_base)] else dt[, LGD_used := LGD_base]
  if ("CCF_corrige" %in% names(dt)) dt[, CCF_used := fifelse(!is.na(CCF_corrige), as.numeric(CCF_corrige), CCF_base)] else dt[, CCF_used := CCF_base]

  # --- Appliquer le scénario UI (UNE seule variable) ---
  if (input$dq_type %in% c("EAD","PD","LGD","CCF","BILANHB")) {
    k <- 1 - (as.numeric(input$delta_pct)/100)

    if (input$dq_type == "EAD") dt[, EAD_used := EAD_base * k]
    if (input$dq_type == "PD")  dt[, PD_used  := pmin(1, pmax(0, PD_base  * k))]
    if (input$dq_type == "LGD") dt[, LGD_used := pmin(1, pmax(0, LGD_base * k))]
    if (input$dq_type == "CCF") dt[, CCF_used := pmin(1, pmax(0, CCF_base * k))]

    # Bilan/HB : on agit sur EAD_B et EAD_HB si disponibles
    if (input$dq_type == "BILANHB") {
      if ("EAD_B" %in% names(dt))  dt[, EAD_B := as.numeric(EAD_B) * k]
      if ("EAD_HB" %in% names(dt)) dt[, EAD_HB := as.numeric(EAD_HB) * k]
    }
  }

  if (input$dq_type == "CLASSIF") {
    dt[, PRTF := as.character(input$prtf_corr)]
  }

  if (input$dq_type == "DEFAULT") {
    # Statut corrigé agit via PD (PD=1 défaut)
    pd_sain_ref <- dt[PD_base < 1 & !is.na(PD_base), median(PD_base)]
    if (is.na(pd_sain_ref)) pd_sain_ref <- 0.01

    if (input$stat_corr == "defaut") dt[, PD_used := 1]
    if (input$stat_corr == "sain")   dt[, PD_used := fifelse(PD_base < 1 & !is.na(PD_base), PD_base, pd_sain_ref)]
  }

  if (input$dq_type == "SURETE") {
    # On conserve l’info sureté mais on ne haircutt pas (tu veux suppression haircut)
    # ici la sûreté peut impacter RW si vous avez une logique RW dépendant de sûreté.
    dt[, Surete_used := as.integer(input$surete_corr)]
  }

  rv$impacts_dt <- dt
})



# D4.3 Recalcul RWA : méthode via MTBAL

# ============================================================
# [06_module3_impacts_server.R] - Calcul RWA corrigé selon MTBAL
# MTBAL=1 => SA ; MTBAL=3 => IRBA
# ============================================================

# Aucune sûreté/haircut : EAD_adj = EAD_used
dt[, EAD_adj := EAD_used]

idx_sa  <- dt$MTBAL == 1L
idx_irb <- dt$MTBAL == 3L

dt[, RWA_calc := NA_real_]

# SA : RW doit exister ou être dérivé
if (!"RW" %in% names(dt)) dt[, RW := fifelse(!is.na(RWA) & !is.na(EAD) & EAD>0, RWA/EAD, NA_real_)]
dt[, RW_base := as.numeric(RW)]
if ("RW_corrige" %in% names(dt)) dt[, RW_used := fifelse(!is.na(RW_corrige), as.numeric(RW_corrige), RW_base)] else dt[, RW_used := RW_base]

dt[idx_sa, RWA_calc := compute_rwa_sa(EAD_adj, RW_used)]

# IRBA : PD/LGD/M
if (!"M" %in% names(dt)) dt[, M := 2.5]
dt[, M_base := as.numeric(M)]
if ("M_corrige" %in% names(dt)) dt[, M_used := fifelse(!is.na(M_corrige), as.numeric(M_corrige), M_base)] else dt[, M_used := M_base]

default_flag <- as.integer(dt$PD_used == 1)

dt[idx_irb, RWA_calc := compute_rwa_irb(
  EAD = EAD_adj, PD = PD_used, LGD = LGD_used, M = M_used,
  PRTF = PRTF, SSPRTF = SSPRTF, default_flag = default_flag, scaling = 1.0
)]




# E) Module 2 – bouton “extraire tout un SYSTEM_CODE”

hr(),
selectInput("sys_pick", "SYSTEM_CODE (System)", choices = NULL),
actionButton("btn_extract_system", "Extraire toutes les lignes du System", class = "btn-warning"),
downloadButton("dl_sys_xlsx", "Télécharger System (Excel)"),
downloadButton("dl_sys_csv", "Télécharger System (CSV)")


# E2) Niveau server

# ============================================================
# [05_module2_anomalies_server.R] - Extraction complète par SYSTEM_CODE
# ============================================================
observe({
  req(rv$retour_ready)
  dt <- rv$retour_dt
  req("System" %in% names(dt))
  choices <- sort(unique(na.omit(dt$System)))
  updateSelectInput(session, "sys_pick", choices = choices)
})

observeEvent(input$btn_extract_system, {
  req(rv$retour_ready)
  req(input$sys_pick)

  dt <- rv$retour_dt[System == input$sys_pick]

  tmp_xlsx <- file.path(tempdir(), "extract_system.xlsx")
  tmp_csv  <- file.path(tempdir(), "extract_system.csv")

  openxlsx::write.xlsx(dt, tmp_xlsx, overwrite = TRUE)
  data.table::fwrite(dt, tmp_csv)

  output$dl_sys_xlsx <- downloadHandler(
    filename = function() paste0("Retour4C_System_", input$sys_pick, ".xlsx"),
    content = function(file) file.copy(tmp_xlsx, file, overwrite = TRUE)
  )
  output$dl_sys_csv <- downloadHandler(
    filename = function() paste0("Retour4C_System_", input$sys_pick, ".csv"),
    content = function(file) file.copy(tmp_csv, file, overwrite = TRUE)
  )
})



# ============================================================
# ============================================================




# 2.1 02_io_retour4c.R (préparation data) — AVANT tout calcul/synthèse

# ============================================================
# 02_io_retour4c.R - Préparation Encours Bilan/HorsBilan
# Objectif: garantir Encours_B, Encours_HB et Encours cohérents
# ============================================================

# Sécuriser la présence des colonnes
if (!"Encours_B" %in% names(dt))  dt[, Encours_B  := NA_real_]
if (!"Encours_HB" %in% names(dt)) dt[, Encours_HB := NA_real_]

# Convertir en numérique (si besoin)
dt[, Encours_B  := smart_as_numeric(Encours_B)]
dt[, Encours_HB := smart_as_numeric(Encours_HB)]

# Recalcul Encours total si absent (ou si vous voulez forcer cohérence)
if (!"Encours" %in% names(dt)) dt[, Encours := Encours_B + Encours_HB]
# Option: forcer la cohérence systématiquement :
# dt[, Encours := Encours_B + Encours_HB]




# 3) Module 3 (ImpactsRWA) — adaptation “Bilan/HorsBilan” basé sur Encours_B/HB

"BILANHB" = tagList(
  selectInput("bhb_target", "Cible correction", choices = c("Bilan"="B", "HorsBilan"="HB"), selected = "HB"),
  numericInput("delta_pct", "Variation corrigée (+/- %)", value = 0, step = 1),
  tags$small("Formule: Encours_cible_corrigé = Encours_cible_Retour4C * (1 - delta_pct/100). Puis Encours et EAD sont recalculés.")
)


# 3.2 Server Module 3 (06_module3_impacts_server.R) — application du scénario

# ============================================================
# Scénario DQ : Erreur Bilan/HorsBilan
# Correction appliquée sur Encours_B ou Encours_HB
# Puis recalcul Encours et EAD
# ============================================================

if (input$dq_type == "BILANHB") {

  # Sécuriser colonnes
  if (!"Encours_B" %in% names(dt))  dt[, Encours_B := NA_real_]
  if (!"Encours_HB" %in% names(dt)) dt[, Encours_HB := NA_real_]
  if (!"Encours" %in% names(dt))    dt[, Encours := Encours_B + Encours_HB]

  dt[, Encours_B  := as.numeric(Encours_B)]
  dt[, Encours_HB := as.numeric(Encours_HB)]
  dt[, Encours_base := as.numeric(Encours)]
  dt[, EAD_base := as.numeric(EAD)]

  k <- 1 - (as.numeric(input$delta_pct) / 100)

  # Appliquer la variation sur UNE seule composante (B ou HB)
  if (input$bhb_target == "B") {
    dt[, Encours_B := Encours_B * k]
  } else {
    dt[, Encours_HB := Encours_HB * k]
  }

  # Recalcul Encours total
  dt[, Encours := Encours_B + Encours_HB]

  # Recalcul EAD :
  # A) si CCF existe => EAD = Encours_B + CCF*Encours_HB
  # B) sinon => conservation ratio alpha = EAD_base / Encours_base
  if ("CCF_used" %in% names(dt) || "CCF" %in% names(dt)) {
    if (!"CCF_used" %in% names(dt)) dt[, CCF_used := as.numeric(CCF)]
    dt[, CCF_used := pmin(1, pmax(0, as.numeric(CCF_used)))]

    dt[, EAD_used := Encours_B + CCF_used * Encours_HB]

  } else {
    dt[, alpha := fifelse(!is.na(Encours_base) & Encours_base != 0,
                          EAD_base / Encours_base, 1)]
    dt[, EAD_used := alpha * Encours]
    dt[, alpha := NULL]
  }
}




# 5) Compatibilité Excel _corrige pour Bilan/HB

# --- Excel corrections (si présentes) pour Bilan/HB ---
if ("Encours_B_corrige" %in% names(dt)) {
  dt[, Encours_B := fifelse(!is.na(Encours_B_corrige), as.numeric(Encours_B_corrige), Encours_B)]
}
if ("Encours_HB_corrige" %in% names(dt)) {
  dt[, Encours_HB := fifelse(!is.na(Encours_HB_corrige), as.numeric(Encours_HB_corrige), Encours_HB)]
}
# Puis recalcul Encours si une des deux existe
if (("Encours_B_corrige" %in% names(dt)) || ("Encours_HB_corrige" %in% names(dt))) {
  dt[, Encours := Encours_B + Encours_HB]
  # Recalcul EAD via CCF si dispo, sinon ratio (même logique que plus haut)
}




# 1) 02_io_retour4c.R — Préparation Encours_B/HB + System + MTBAL

# --- System (ex-GoldenSource) : colonne canonique ----------------------
if ("SYSTEM_CODE" %in% names(dt)) {
  dt[, System := as.character(SYSTEM_CODE)]
} else {
  dt[, System := NA_character_]
}
# ----------------------------------------------------------------------

# 1.3 Bloc “MTBAL”

# --- Méthode réglementaire : MTBAL (1=SA, 3=IRBA) ----------------------
if (!"MTBAL" %in% names(dt)) dt[, MTBAL := 1L]
dt[, MTBAL := as.integer(MTBAL)]
# ----------------------------------------------------------------------


# 1.4 Bloc Encours_B/HB + Encours

# --- Encours Bilan/Hors-Bilan -----------------------------------------
if (!"Encours_B" %in% names(dt))  dt[, Encours_B := NA_real_]
if (!"Encours_HB" %in% names(dt)) dt[, Encours_HB := NA_real_]

dt[, Encours_B  := smart_as_numeric(Encours_B)]
dt[, Encours_HB := smart_as_numeric(Encours_HB)]

# Encours total (cohérence)
if (!"Encours" %in% names(dt)) dt[, Encours := Encours_B + Encours_HB]
# Option recommandée : forcer cohérence systématique
# dt[, Encours := Encours_B + Encours_HB]
# ----------------------------------------------------------------------



# 1.5 Typage numériques (si pas déjà fait)

# --- Colonnes numériques clés -----------------------------------------
num_cols <- intersect(c("EAD","RW","RWA","CCF"), names(dt))
for (cc in num_cols) dt[, (cc) := smart_as_numeric(get(cc))]
# ----------------------------------------------------------------------



# 3) Module 2 — Déduplication extraction anomalies (priorité SG_CONTRACT_ID)

out <- rbindlist(out_list, use.names = TRUE, fill = TRUE)



# --- DEDUP extraction anomalies : priorité SG_CONTRACT_ID --------------
prio <- c("SG_CONTRACT_ID", "SG_CONTR_BCE_ID", "SG_CONTR_3D_ID", "SG_FACILITY_ID", "SG_ID_ENREGISTRMNT")
out[, matched_prio := match(matched_on, prio)]
out[is.na(matched_prio), matched_prio := 999L]

# Un seul enregistrement par (ID_CONTRAT, ASOF_DT) si ASOF_DT existe
if ("ASOF_DT" %in% names(out)) {
  setorder(out, ID_CONTRAT, ASOF_DT, matched_prio)
  out <- out[, .SD[1], by = .(ID_CONTRAT, ASOF_DT)]
} else {
  setorder(out, ID_CONTRAT, matched_prio)
  out <- out[, .SD[1], by = .(ID_CONTRAT)]
}

out[, matched_prio := NULL]
# ----------------------------------------------------------------------


# 4) Module 3 — Scénario “Bilan/HorsBilan (+/-%)” basé Encours_B/HB, recalcul Encours + EAD (avec CCF)

"BILANHB" = tagList(
  selectInput("bhb_target", "Cible correction", choices = c("Bilan (Encours_B)"="B", "Hors bilan (Encours_HB)"="HB"), selected = "HB"),
  numericInput("delta_pct", "Variation corrigée (+/- %)", value = 0, step = 1),
  tags$small("Formule: Encours_cible_corrigé = Encours_cible_Retour4C * (1 - delta_pct/100). Puis Encours = Encours_B + Encours_HB. EAD = Encours_B + CCF*Encours_HB.")
)






# 4.2 Application scénario (Module 3) : bloc prêt à coller

# ============================================================
# DQ: Erreur Bilan/HorsBilan (+/- %)
# - Une seule composante corrigée: Encours_B OU Encours_HB
# - Recalcul Encours total
# - Recalcul EAD cohérent via CCF:
#   EAD_used = Encours_B + CCF_used * Encours_HB
# ============================================================
if (input$dq_type == "BILANHB") {

  # Sécuriser colonnes attendues
  if (!"Encours_B" %in% names(dt))  dt[, Encours_B := NA_real_]
  if (!"Encours_HB" %in% names(dt)) dt[, Encours_HB := NA_real_]
  if (!"Encours" %in% names(dt))    dt[, Encours := Encours_B + Encours_HB]
  if (!"CCF" %in% names(dt))        dt[, CCF := NA_real_]
  if (!"EAD" %in% names(dt))        dt[, EAD := NA_real_]

  # Bases numériques
  dt[, Encours_B_base  := smart_as_numeric(Encours_B)]
  dt[, Encours_HB_base := smart_as_numeric(Encours_HB)]
  dt[, CCF_base        := smart_as_numeric(CCF)]
  dt[, EAD_base        := smart_as_numeric(EAD)]

  # Appliquer Excel *_corrige si présent (priorité sur la base)
  if ("Encours_B_corrige" %in% names(dt)) {
    dt[, Encours_B_base := fifelse(!is.na(Encours_B_corrige), smart_as_numeric(Encours_B_corrige), Encours_B_base)]
  }
  if ("Encours_HB_corrige" %in% names(dt)) {
    dt[, Encours_HB_base := fifelse(!is.na(Encours_HB_corrige), smart_as_numeric(Encours_HB_corrige), Encours_HB_base)]
  }
  if ("CCF_corrige" %in% names(dt)) {
    dt[, CCF_base := fifelse(!is.na(CCF_corrige), smart_as_numeric(CCF_corrige), CCF_base)]
  }

  # Correction UI en % sur une seule composante
  k <- 1 - (as.numeric(input$delta_pct) / 100)

  dt[, Encours_B_used  := Encours_B_base]
  dt[, Encours_HB_used := Encours_HB_base]

  if (input$bhb_target == "B") {
    dt[, Encours_B_used := Encours_B_base * k]
  } else {
    dt[, Encours_HB_used := Encours_HB_base * k]
  }

  # Recalcul Encours total
  dt[, Encours_used := Encours_B_used + Encours_HB_used]

  # Recalcul EAD via CCF
  dt[, CCF_used := pmin(1, pmax(0, CCF_base))]
  dt[, EAD_used := Encours_B_used + CCF_used * Encours_HB_used]

  # (Optionnel) Remplacer aussi Encours/EAD dans dt pour cohérence affichage
  dt[, Encours := Encours_used]
  dt[, EAD := EAD_used]

  # Nettoyage léger (vous pouvez garder si utile pour debug)
  # dt[, c("Encours_B_base","Encours_HB_base","Encours_B_used","Encours_HB_used","Encours_used") := NULL]
}





# ============================================================
# ============================================================


# 1.3 Modifier dq_inputs_ui pour le cas SURETE — 06_module3_impacts_server.R

"SURETE" = tagList(
  selectInput("surete_init", "Sûreté initiale", choices = c(0,1), selected = 0),
  selectInput("surete_corr", "Sûreté corrigée", choices = c(0,1), selected = 1),
  checkboxInput("rho_custom_on", "Surcharger ρ (taux de couverture)", value = FALSE),
  conditionalPanel(
    condition = "input.rho_custom_on == true",
    numericInput("rho_custom", "ρ (0-1)", value = 0.50, min = 0, max = 1, step = 0.05)
  ),
  tags$small("Standard: RWA_corr = ρEAD×RW_coll + (1-ρ)EAD×RW_orig.  IRBA: LGD_used = LGD_secured_proxy (ou LGD_corrige).")
)


# 1.4 Appliquer la correction SURETE dans btn_apply_dq — 06_module3_impacts_server.R

# ============================================================
# DQ: Sûreté manquante (0/1)
# Objectif:
# - Standard: split couvert/non couvert (rho, RW_coll)
# - IRBA: appliquer LGD secured proxy (ou LGD_corrige si présent)
# ============================================================
if (input$dq_type == "SURETE") {

  # Base columns
  if (!"Surete" %in% names(dt)) dt[, Surete := 0L]
  dt[, Surete_base := as.integer(Surete)]
  dt[is.na(Surete_base), Surete_base := 0L]

  # Valeur corrigée depuis UI
  dt[, Surete_used := as.integer(input$surete_corr)]

  # Récupérer les proxies par ligne (PRTF/SSPRTF)
  # -> retourne rho, RW_coll, LGD_secured
  px <- mapply(proxy_surete_params, dt$PRTF, dt$SSPRTF, SIMPLIFY = FALSE)

  dt[, rho_proxy := vapply(px, `[[`, numeric(1), "rho")]
  dt[, RW_coll_proxy := vapply(px, `[[`, numeric(1), "RW_coll")]
  dt[, LGD_secured_proxy := vapply(px, `[[`, numeric(1), "LGD_secured")]

  # Option: surcharge rho via UI (global)
  if (isTRUE(input$rho_custom_on)) {
    dt[, rho_used := as.numeric(input$rho_custom)]
  } else {
    dt[, rho_used := rho_proxy]
  }

  # Option: surcharge via Excel anomalies (*_corrige) si présent
  # - RHO_corrige (si vous décidez d'ajouter cette colonne)
  if ("RHO_corrige" %in% names(dt)) {
    dt[, rho_used := fifelse(!is.na(RHO_corrige), as.numeric(RHO_corrige), rho_used)]
  }
  if ("RW_coll_corrige" %in% names(dt)) {
    dt[, RW_coll_used := fifelse(!is.na(RW_coll_corrige), as.numeric(RW_coll_corrige), RW_coll_proxy)]
  } else {
    dt[, RW_coll_used := RW_coll_proxy]
  }

  # IRBA : LGD secured (Excel LGD_corrige a déjà priorité dans votre code)
  # Ici on ne remplace LGD_used que si Surete_used==1 et si LGD_corrige absent
  # -> on crée LGD_secured_used
  dt[, LGD_secured_used := LGD_secured_proxy]
}


# 1.5 Recalcul RWA avec sûreté — btn_compute_impacts (Module 3)

# ----- Standard: si sûreté prise en compte -> split couvert/non couvert
dt[idx_sa, RWA_calc := {
  E <- EAD_used
  RWorig <- RW_used

  # Si le scénario SURETE n'a pas été appliqué, Surete_used peut ne pas exister
  S <- if ("Surete_used" %in% names(dt)) Surete_used else 0L

  # Valeurs proxy (si non définies, fallback)
  rho <- if ("rho_used" %in% names(dt)) rho_used else 0
  RWc <- if ("RW_coll_used" %in% names(dt)) RW_coll_used else RWorig

  E_couv <- pmin(E, rho * E)
  E_non  <- E - E_couv

  # si S==1: split, sinon: classique
  fifelse(S >= 1L,
          E_couv * RWc + E_non * RWorig,
          E * RWorig)
}]


# 1.5.2 IRBA (MTBAL=3) : LGD_used remplacée si sûreté prise en compte

# ----- IRBA: si sûreté prise en compte -> LGD_used = LGD_secured_proxy
# sauf si LGD_corrige existe déjà (prioritaire)
if ("Surete_used" %in% names(dt)) {
  # si votre code a déjà défini LGD_used via LGD_corrige -> on ne le touche pas
  # sinon on applique proxy pour les lignes S=1
  if (!"LGD_corrige" %in% names(dt)) dt[, LGD_corrige := NA_real_]

  dt[Surete_used >= 1L & is.na(LGD_corrige) & "LGD_secured_used" %in% names(dt),
     LGD_used := LGD_secured_used]
}


# 1) Règle de présence de sûreté (à partir de TypSurete)

# ============================================================
# 02_io_retour4c.R - Détection présence sûreté depuis TypSurete
# TypSurete vide => pas de sûreté ; renseigné => sûreté présente
# ============================================================

if (!"TypSurete" %in% names(dt)) dt[, TypSurete := NA_character_]
dt[, TypSurete := as.character(TypSurete)]
dt[, Surete_base := fifelse(!is.na(TypSurete) & nzchar(trimws(TypSurete)), 1L, 0L)]




# 0) Pré-requis : créer Surete_base depuis TypSurete (Retour4C)

# ============================================================
# [02_io_retour4c.R] - Détection présence sûreté depuis TypSurete
# Règle : TypSurete vide/NA => pas de sûreté ; renseigné => sûreté présente
# ============================================================

if (!"TypSurete" %in% names(dt)) dt[, TypSurete := NA_character_]
dt[, TypSurete := as.character(TypSurete)]

# 1 = sûreté présente ; 0 = pas de sûreté
dt[, Surete_base := fifelse(!is.na(TypSurete) & nzchar(trimws(TypSurete)), 1L, 0L)]


# 2) Module 3 — UI dynamique : case “SURETE” (0/1)
"SURETE" = tagList(
  selectInput("surete_corr", "Sûreté corrigée (0=non, 1=oui)", choices = c(0,1), selected = 1),
  checkboxInput("rho_custom_on", "Surcharger ρ (taux de couverture SA)", value = FALSE),
  conditionalPanel(
    condition = "input.rho_custom_on == true",
    numericInput("rho_custom", "ρ (0-1)", value = 0.50, min = 0, max = 1, step = 0.05)
  ),
  tags$small("Sûreté base dérivée de TypSurete. SA: split couvert/non couvert. IRBA: LGD=secured (Retour4C) si sûreté=1, sinon LGD_unsec proxy.")
)


# 3) Module 3 — Application scénario SURETE dans btn_apply_dq

# ============================================================
# [06_module3_impacts_server.R] - Scénario DQ : Sûreté (TypSurete)
# - Surete_base existe (créée en 02_io_retour4c.R)
# - surete_corr est donné par l'IHM (0/1)
# - Proxies par PRTF/SSPRTF :
#   rho_proxy, RW_coll_proxy, LGD_unsec_proxy
# ============================================================

if (input$dq_type == "SURETE") {

  # Sécuriser Surete_base
  if (!"Surete_base" %in% names(dt)) dt[, Surete_base := 0L]
  dt[, Surete_base := as.integer(Surete_base)]
  dt[is.na(Surete_base), Surete_base := 0L]

  # Surete corrigée (scénario)
  dt[, Surete_used := as.integer(input$surete_corr)]

  # Charger proxies par ligne (PRTF/SSPRTF)
  px <- mapply(proxy_surete_params, dt$PRTF, dt$SSPRTF, SIMPLIFY = FALSE)
  dt[, rho_proxy := vapply(px, `[[`, numeric(1), "rho")]
  dt[, RW_coll_proxy := vapply(px, `[[`, numeric(1), "RW_coll")]
  dt[, LGD_unsec_proxy := vapply(px, `[[`, numeric(1), "LGD_unsec")]

  # rho_used (SA) : surcharge globale optionnelle
  if (isTRUE(input$rho_custom_on)) {
    dt[, rho_used := as.numeric(input$rho_custom)]
  } else {
    dt[, rho_used := rho_proxy]
  }
  dt[, rho_used := pmin(1, pmax(0, rho_used))]

  # Option : surcharge via Excel (si vous décidez de l’autoriser)
  if ("RHO_corrige" %in% names(dt)) {
    dt[, rho_used := fifelse(!is.na(RHO_corrige), as.numeric(RHO_corrige), rho_used)]
    dt[, rho_used := pmin(1, pmax(0, rho_used))]
  }
  if ("RW_coll_corrige" %in% names(dt)) {
    dt[, RW_coll_used := fifelse(!is.na(RW_coll_corrige), as.numeric(RW_coll_corrige), RW_coll_proxy)]
  } else {
    dt[, RW_coll_used := RW_coll_proxy]
  }

  # IRBA : LGD utilisée
  # - si sûreté prise en compte -> LGD_used = LGD (secured, déjà dans dt)
  # - si sûreté non prise en compte -> LGD_used = LGD_unsec_proxy
  # - si LGD_corrige existe, elle reste prioritaire (déjà appliquée dans votre code)
  if (!"LGD_corrige" %in% names(dt)) dt[, LGD_corrige := NA_real_]
  # On applique uniquement si Excel ne fournit pas LGD_corrige
  dt[Surete_used == 0L & is.na(LGD_corrige), LGD_used := LGD_unsec_proxy]

  # Si Surete_used==1 et pas de LGD_corrige, on garde LGD_used (secured) tel quel.
}



# 4) Module 3 — Recalcul RWA : intégrer split SA + LGD_unsec IRBA

# 4.1 SA (MTBAL==1) : split si Surete_used==1

# ============================================================
# SA (MTBAL=1) - CRM sûreté (best practice proxy)
# si Surete_used==1 : RWA = rho*EAD*RW_coll + (1-rho)*EAD*RW_orig
# sinon : RWA = EAD*RW_orig
# ============================================================
dt[idx_sa, RWA_calc := {
  E <- EAD_used
  RWorig <- RW_used

  S <- if ("Surete_used" %in% names(dt)) Surete_used else 0L
  rho <- if ("rho_used" %in% names(dt)) rho_used else 0
  RWc  <- if ("RW_coll_used" %in% names(dt)) RW_coll_used else RWorig

  rho <- pmin(1, pmax(0, rho))
  # RW normalisation sécurité (si RW en %)
  RWorig_n <- ifelse(RWorig > 1.5, RWorig/100, RWorig)
  RWc_n    <- ifelse(RWc    > 1.5, RWc/100, RWc)

  # split
  RWA_split <- (rho * E) * RWc_n + ((1 - rho) * E) * RWorig_n
  RWA_std   <- E * RWorig_n

  fifelse(S >= 1L, RWA_split, RWA_std)
}]


4.2 IRBA (MTBAL==3) : LGD_used déjà ajustée si Surete_used==0

dt[idx_irb, RWA_calc := compute_rwa_irb(
  EAD = EAD_used, PD = PD_used, LGD = LGD_used, M = M_used,
  PRTF = PRTF, SSPRTF = SSPRTF,
  default_flag = as.integer(PD_used == 1), scaling = 1.0
)]








# ============================================================
# ============================================================



