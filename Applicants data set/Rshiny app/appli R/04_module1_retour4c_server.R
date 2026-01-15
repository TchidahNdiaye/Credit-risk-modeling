# ============================================================
# ----- DEBUT MODULE 1 : Retour4C (SERVER) -----
# Objectif : preview, infos, description, synthèses
# Lecture "select" => performance / pas de RAM explosive
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(DT)
})

module1_server <- function(input, output, rv) {

  output$retour_log <- renderText(rv$logs$retour)

  output$retour_status_ui <- renderUI({
    p <- get_retour_path(input)
    if (!nzchar(p)) tags$div(tags$b("Statut:"), " aucun fichier/chemin valide.")
    else tags$div(tags$b("Chemin utilisé:"), p)
  })

  observeEvent(input$btn_preview, {
    res <- safe_run(
      "Module 1 - Preview Retour4C",
      {
        path <- get_retour_path(input)
        if (!nzchar(path)) stop("Aucun chemin Retour4C valide.", call. = FALSE)
        read_retour_preview(path, PREVIEW_N)
      },
      prereq = "Renseigner soit un chemin direct (recommandé pour gros fichiers), soit via fileInput."
    )
    rv$logs$retour <- res$message
    if (res$ok) {
      rv$retour_preview <- res$value
      output$retour_preview_dt <- renderDT(datatable(rv$retour_preview, options = list(pageLength = PREVIEW_N, scrollX = TRUE)))
    }
  })

  observeEvent(input$btn_prepare, {
    res <- safe_run(
      "Module 1 - Préparer Retour4C (entête)",
      {
        path <- get_retour_path(input)
        if (!nzchar(path)) stop("Aucun chemin Retour4C valide.", call. = FALSE)
        hdr <- read_retour_header(path)
        list(path = path, cols = names(hdr))
      },
      prereq = "Le fichier Retour4C doit être accessible (chemin direct conseillé si >1Go)."
    )
    rv$logs$retour <- res$message
    if (res$ok) {
      rv$retour_path <- res$value$path
      rv$retour_cols <- res$value$cols
      rv$retour_ready <- TRUE

      output$retour_infos_ui <- renderUI({
        tags$div(
          tags$b("Fichier prêt:"), rv$retour_path, tags$br(),
          tags$b("Nb colonnes:"), length(rv$retour_cols)
        )
      })

      # Description globale = calculée sur un subset léger (syn cols)
      desc_res <- safe_run(
        "Module 1 - Description globale (subset)",
        {
          dt <- read_retour_select(rv$retour_path, RET_COLS_SYNTH)
          n <- nrow(dt); p <- length(rv$retour_cols)
          asof_txt <- if ("ASOF_DT" %in% names(dt)) asof_label(dt$ASOF_DT) else NA_character_
          sums <- list(
            Encours = if ("Encours" %in% names(dt)) sum(dt$Encours, na.rm = TRUE) else NA_real_,
            EAD     = if ("EAD" %in% names(dt)) sum(dt$EAD, na.rm = TRUE) else NA_real_,
            RWA     = if ("RWA" %in% names(dt)) sum(dt$RWA, na.rm = TRUE) else NA_real_
          )
          data.table(
            Champ = c("Nb observations (subset)","Nb colonnes fichier","Arrêté (ASOF_DT)",
                      "Somme Encours","Somme EAD","Somme RWA"),
            Valeur = c(fmt_num(n,0), fmt_num(p,0), asof_txt,
                       fmt_num(sums$Encours,2), fmt_num(sums$EAD,2), fmt_num(sums$RWA,2))
          )
        }
      )
      if (desc_res$ok) {
        output$retour_desc_dt <- renderDT(datatable(desc_res$value, options = list(dom="t", scrollX = TRUE)))
      } else {
        rv$logs$retour <- paste(rv$logs$retour, "\n", desc_res$message)
      }
    }
  })

  observeEvent(input$btn_synth, {
    req(rv$retour_ready)

    res <- safe_run(
      "Module 1 - Synthèses",
      {
        dt <- read_retour_select(rv$retour_path, RET_COLS_SYNTH)
        list(gs = compute_syn_gs(dt), prtf = compute_syn_prtf(dt))
      },
      prereq = "Cliquer d’abord sur 'Préparer' (entête), puis 'Synthèses'."
    )
    rv$logs$retour <- res$message
    if (res$ok) {
      rv$syn_gs <- res$value$gs
      rv$syn_prtf <- res$value$prtf

      output$syn_gs_dt <- renderDT({
        dt <- copy(rv$syn_gs)
        dt[, `:=`(
          Nbre_observations = fmt_num(Nbre_observations, 0),
          Encours_MEUR = fmt_num(Encours_MEUR, 2),
          EAD_MEUR = fmt_num(EAD_MEUR, 2),
          RWA_MEUR = fmt_num(RWA_MEUR, 2)
        )]
        datatable(dt, options = list(pageLength = 30, scrollX = TRUE))
      })

      output$syn_prtf_dt <- renderDT({
        dt <- copy(rv$syn_prtf)
        dt[, `:=`(
          Nbre_observations = fmt_num(Nbre_observations, 0),
          Encours_MEUR = fmt_num(Encours_MEUR, 2),
          EAD_MEUR = fmt_num(EAD_MEUR, 2),
          RWA_MEUR = fmt_num(RWA_MEUR, 2)
        )]
        datatable(dt, options = list(pageLength = 30, scrollX = TRUE))
      })
    }
  })

}

# ============================================================
# ----- FIN MODULE 1 : Retour4C (SERVER) -----
# ============================================================
