# ============================================================
# ----- DEBUT MODULE 2 : Anomalies -> Extraction (SERVER) -----
# Objectif : preview anomalies, extraction (match ID dans 5 colonnes)
# Performance : lit seulement colonnes nécessaires Retour4C
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(DT)
  library(readxl)
  library(openxlsx)
})

read_anomalies <- function(path) {
  dt <- readxl::read_excel(path, n_max = 200000)
  dt <- as.data.table(dt)

  if (!"ID_CONTRAT" %in% names(dt)) {
    cand <- names(dt)[toupper(names(dt)) == "ID_CONTRAT"]
    if (length(cand) == 1) setnames(dt, cand, "ID_CONTRAT")
  }
  need_cols(dt, c("ID_CONTRAT"), "anomalies_dq.xlsx")
  dt[, ID_CONTRAT := normalize_id(ID_CONTRAT)]
  dt
}

extract_from_retour <- function(retour_path, anomalies_dt) {
  ids <- unique(anomalies_dt[, .(ID_CONTRAT)])
  ids[, ID_CONTRAT := normalize_id(ID_CONTRAT)]
  ids <- ids[nzchar(ID_CONTRAT)]
  if (nrow(ids) == 0) stop("Aucun ID_CONTRAT exploitable dans anomalies.", call. = FALSE)

  # On lit un subset Retour4C (perf)
  dt <- read_retour_select(retour_path, RET_COLS_EXTRACT)

  # Vérifier quelles colonnes ID existent
  match_cols <- intersect(RET_ID_COLS, names(dt))
  if (length(match_cols) == 0) {
    stop("Aucune colonne ID Retour4C trouvée parmi: SG_CONTRACT_ID, SG_CONTR_BCE_ID, SG_CONTR_3D_ID, SG_FACILITY_ID, SG_ID_ENREGISTRMNT", call. = FALSE)
  }

  # Filter rapide via %chin%
  id_vec <- ids$ID_CONTRAT
  out_list <- list()
  for (cc in match_cols) {
    tmp <- dt[get(cc) %chin% id_vec]
    if (nrow(tmp) > 0) {
      tmp[, matched_on := cc]
      tmp[, ID_CONTRAT := get(cc)]
      out_list[[cc]] <- tmp
    }
  }
  out <- rbindlist(out_list, use.names = TRUE, fill = TRUE)

  if (nrow(out) == 0) {
    ex_ano <- paste(head(id_vec, 5), collapse = " | ")
    ex_ret <- paste(unlist(lapply(match_cols, function(cc) paste(head(unique(dt[[cc]]), 5), collapse = " | "))), collapse = "\n")
    stop(paste0(
      "Aucune correspondance.\n\n",
      "Exemples ID_CONTRAT anomalies: ", ex_ano, "\n\n",
      "Exemples IDs dans Retour4C:\n", ex_ret, "\n",
      "Causes probables: espaces/zeros/format; vérifier que les IDs sont strictement identiques."
    ), call. = FALSE)
  }

  keep <- intersect(c(
    "ID_CONTRAT","matched_on","ASOF_DT","ASOF","PRTF","SSPRTF","Segment","PRODUCT_ID",
    "Encours","Encours_B","Encours_HB","EAD","RWA","RW","CCF","LGD","PD","M",
    "GoldenSource",
    RET_ID_COLS
  ), names(out))

  out <- out[, ..keep]
  setorder(out, ID_CONTRAT, matched_on, ASOF)
  out
}

module2_server <- function(input, output, rv) {

  output$ano_log <- renderText(rv$logs$ano)

  observeEvent(input$btn_preview_ano, {
    req(input$ano_file)
    res <- safe_run(
      "Module 2 - Preview anomalies",
      {
        dt <- read_anomalies(input$ano_file$datapath)
        dt
      },
      prereq = "anomalies_dq.xlsx doit contenir ID_CONTRAT."
    )
    rv$logs$ano <- res$message
    if (res$ok) {
      rv$anomalies_dt <- res$value
      output$ano_preview_dt <- renderDT(datatable(head(rv$anomalies_dt, PREVIEW_N), options = list(dom="t", scrollX = TRUE)))
    }
  })

  observeEvent(input$btn_extract, {
    req(rv$retour_ready)
    req(rv$anomalies_dt)

    res <- safe_run(
      "Module 2 - Extraction dans Retour4C",
      {
        out <- extract_from_retour(rv$retour_path, rv$anomalies_dt)
        out
      },
      prereq = "Module 1: chemin Retour4C prêt (Préparer). Module 2: anomalies preview OK."
    )
    rv$logs$ano <- res$message
    if (res$ok) {
      rv$extract_dt <- res$value

      output$extract_head_dt <- renderDT(datatable(head(rv$extract_dt, 5), options = list(dom="t", scrollX=TRUE)))
      output$extract_tail_dt <- renderDT(datatable(tail(rv$extract_dt, 5), options = list(dom="t", scrollX=TRUE)))

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
}

# ============================================================
# ----- FIN MODULE 2 : Anomalies -> Extraction (SERVER) -----
# ============================================================
