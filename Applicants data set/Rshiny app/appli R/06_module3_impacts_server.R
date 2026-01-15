# ============================================================
# ----- DEBUT MODULE 3 : ImpactsRWA (SERVER) -----
# Objectif : recalcul RWA avec corrections partielles (RW_corrige OU EAD_corrige suffisent)
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(DT)
  library(openxlsx)
})

module3_server <- function(input, output, rv) {

  output$impact_log <- renderText(rv$logs$impact)

  observeEvent(input$btn_compute_impacts, {
    req(rv$extract_dt)
    req(rv$anomalies_dt)

    res <- safe_run(
      "Module 3 - Recalcul + comparaison",
      {
        base <- copy(rv$extract_dt)
        ano  <- copy(rv$anomalies_dt)

        base[, ID_CONTRAT := normalize_id(ID_CONTRAT)]
        ano[, ID_CONTRAT  := normalize_id(ID_CONTRAT)]

        setkey(base, ID_CONTRAT)
        setkey(ano, ID_CONTRAT)
        dt <- ano[base]  # conserve les lignes extraites, enrichies des colonnes anomalies

        # Normaliser base
        if (!"EAD" %in% names(dt)) dt[, EAD := NA_real_]
        if (!"RWA" %in% names(dt)) dt[, RWA := NA_real_]
        if (!"RW" %in% names(dt)) dt[, RW := fifelse(!is.na(RWA) & !is.na(EAD) & EAD > 0, RWA/EAD, NA_real_)]

        # Colonnes correction optionnelles
        if (!"EAD_corrige" %in% names(dt)) dt[, EAD_corrige := NA_real_]
        if (!"RW_corrige" %in% names(dt)) dt[, RW_corrige := NA_real_]
        if (!"Surete" %in% names(dt)) dt[, Surete := 0]

        dt[, EAD_base := smart_as_numeric(EAD)]
        dt[, RW_base  := smart_as_numeric(RW)]
        dt[, RWA_base := smart_as_numeric(RWA)]

        dt[, EAD_used := fifelse(!is.na(EAD_corrige), smart_as_numeric(EAD_corrige), EAD_base)]
        dt[, RW_used  := fifelse(!is.na(RW_corrige),  smart_as_numeric(RW_corrige),  RW_base)]

        # Haircut app
        dt[, EAD_adj := apply_surety_haircut(EAD_used, Surete, input$haircut)]

        # Recalcul
        if (input$impact_method == "SA") {
          dt[, RWA_calc := compute_rwa_sa(EAD_adj, RW_used)]
        } else {
          if (!"PD" %in% names(dt)) dt[, PD := NA_real_]
          if (!"LGD" %in% names(dt)) dt[, LGD := NA_real_]
          if (!"M" %in% names(dt)) dt[, M := 2.5]
          if (!"PD_corrige" %in% names(dt)) dt[, PD_corrige := NA_real_]
          if (!"LGD_corrige" %in% names(dt)) dt[, LGD_corrige := NA_real_]
          if (!"M_corrige" %in% names(dt)) dt[, M_corrige := NA_real_]

          dt[, PD_used := fifelse(!is.na(PD_corrige), smart_as_numeric(PD_corrige), smart_as_numeric(PD))]
          dt[, LGD_used := fifelse(!is.na(LGD_corrige), smart_as_numeric(LGD_corrige), smart_as_numeric(LGD))]
          dt[, M_used := fifelse(!is.na(M_corrige), smart_as_numeric(M_corrige), smart_as_numeric(M))]

          if (!"PRTF" %in% names(dt)) dt[, PRTF := "Non Retail"]
          if (!"SSPRTF" %in% names(dt)) dt[, SSPRTF := "Non Retail"]

          dt[, RWA_calc := compute_rwa_irb(EAD_adj, PD_used, LGD_used, M_used, PRTF, SSPRTF, default_flag = 0L)]
        }

        dt[, Delta_RWA := RWA_calc - RWA_base]
        dt[, Delta_RWA_pct := fifelse(RWA_base > 0, Delta_RWA / RWA_base, NA_real_)]

        syn <- dt[, .(
          Nbre_observations = .N,
          EAD_MEUR = sum(EAD_base, na.rm=TRUE)/1e6,
          RWA_base_MEUR = sum(RWA_base, na.rm=TRUE)/1e6,
          RWA_calc_MEUR = sum(RWA_calc, na.rm=TRUE)/1e6,
          Delta_RWA_MEUR = sum(Delta_RWA, na.rm=TRUE)/1e6
        ), by = .(PRTF, SSPRTF)]
        setorder(syn, PRTF, SSPRTF)

        list(dt = dt, syn = syn)
      },
      prereq = "Module 2 doit avoir produit une extraction. Les colonnes corrigées sont optionnelles (EAD_corrige ou RW_corrige suffisent)."
    )

    rv$logs$impact <- res$message
    if (res$ok) {
      rv$impacts_dt <- res$value$dt

      output$impacts_dt <- renderDT({
        show <- copy(rv$impacts_dt)
        keep <- intersect(c("ID_CONTRAT","matched_on","ASOF","PRTF","SSPRTF","GoldenSource",
                            "EAD_base","RW_base","RWA_base","EAD_used","RW_used","EAD_adj","RWA_calc","Delta_RWA","Delta_RWA_pct"), names(show))
        show <- show[, ..keep]
        for (cc in intersect(names(show), c("EAD_base","RW_base","RWA_base","EAD_used","RW_used","EAD_adj","RWA_calc","Delta_RWA"))) {
          show[, (cc) := fmt_num(get(cc), 2)]
        }
        if ("Delta_RWA_pct" %in% names(show)) show[, Delta_RWA_pct := fmt_num(Delta_RWA_pct, 4)]
        datatable(show, options = list(pageLength = 25, scrollX = TRUE))
      })

      output$impact_syn_dt <- renderDT({
        syn <- copy(res$value$syn)
        syn[, `:=`(
          Nbre_observations = fmt_num(Nbre_observations,0),
          EAD_MEUR = fmt_num(EAD_MEUR,2),
          RWA_base_MEUR = fmt_num(RWA_base_MEUR,2),
          RWA_calc_MEUR = fmt_num(RWA_calc_MEUR,2),
          Delta_RWA_MEUR = fmt_num(Delta_RWA_MEUR,2)
        )]
        datatable(syn, options = list(pageLength = 25, scrollX = TRUE))
      })

      tmp <- file.path(tempdir(), "impacts_rwa.xlsx")
      wb <- createWorkbook()
      addWorksheet(wb, "detail")
      addWorksheet(wb, "synthese")
      writeDataTable(wb, "detail", rv$impacts_dt)
      writeDataTable(wb, "synthese", res$value$syn)
      saveWorkbook(wb, tmp, overwrite = TRUE)

      output$dl_impacts <- downloadHandler(
        filename = function() "impacts_rwa.xlsx",
        content = function(file) file.copy(tmp, file, overwrite = TRUE)
      )
    }
  })
}

# ============================================================
# ----- FIN MODULE 3 : ImpactsRWA (SERVER) -----
# ============================================================
