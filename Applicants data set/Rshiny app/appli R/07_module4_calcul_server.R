# ============================================================
# ----- DEBUT MODULE 4 : Calcul RWA (SERVER) -----
# Objectif : calculette SA/IRBA + sûreté + Bilan/HB (optionnel)
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(DT)
  library(ggplot2)
})

module4_server <- function(input, output, rv) {

  output$calc_log <- renderText(rv$logs$calc)

  observeEvent(input$btn_calc, {
    res <- safe_run(
      "Module 4 - Calcul RWA",
      {
        method <- input$calc_method

        ead0 <- smart_as_numeric(input$ead_init)
        ead1 <- smart_as_numeric(input$ead_corr)
        rw0  <- smart_as_numeric(input$rw_init)
        rw1  <- smart_as_numeric(input$rw_corr)

        surete <- ifelse(isTRUE(input$surete_flag), 1, 0)
        hc <- smart_as_numeric(input$hc_calc)

        ead1 <- ifelse(is.na(ead1), ead0, ead1)
        rw1  <- ifelse(is.na(rw1),  rw0,  rw1)

        ead0_adj <- apply_surety_haircut(ead0, surete, hc)
        ead1_adj <- apply_surety_haircut(ead1, surete, hc)

        # IRB params
        pd0 <- smart_as_numeric(input$pd_init); pd1 <- smart_as_numeric(input$pd_corr)
        lgd0 <- smart_as_numeric(input$lgd_init); lgd1 <- smart_as_numeric(input$lgd_corr)
        m0 <- smart_as_numeric(input$m_init); m1 <- smart_as_numeric(input$m_corr)

        pd1 <- ifelse(is.na(pd1), pd0, pd1)
        lgd1 <- ifelse(is.na(lgd1), lgd0, lgd1)
        m0 <- ifelse(is.na(m0), 2.5, m0)
        m1 <- ifelse(is.na(m1), m0, m1)

        prtf_map <- ifelse(input$calc_prtf == "Retail", "Retail", "Non Retail")
        ss_map <- ifelse(input$calc_prtf == "Retail",
                         ifelse(input$calc_ssprtf == "Revolving", "Revolving", "Autres"),
                         "Non Retail")

        if (method == "SA") {
          rwa0 <- compute_rwa_sa(ead0_adj, rw0)
          rwa1 <- compute_rwa_sa(ead1_adj, rw1)
        } else {
          rwa0 <- compute_rwa_irb(ead0_adj, pd0, lgd0, m0, prtf_map, ss_map, default_flag = 0L)
          rwa1 <- compute_rwa_irb(ead1_adj, pd1, lgd1, m1, prtf_map, ss_map, default_flag = 0L)
        }

        main <- data.table(
          Indicateur = c("RWA initial","RWA corrigé","Delta (corr - init)"),
          Valeur = c(rwa0, rwa1, rwa1 - rwa0)
        )

        main
      },
      prereq = "SA: EAD + RW. IRBA: EAD + PD + LGD + M. Les valeurs corrigées sont optionnelles."
    )

    rv$logs$calc <- res$message
    if (res$ok) {
      tbl <- copy(res$value)
      output$calc_table_dt <- renderDT({
        tbl[, Valeur := fmt_num(Valeur,2)]
        datatable(tbl, options = list(dom="t", scrollX = TRUE))
      })

      output$calc_plot <- renderPlot({
        df <- data.table(type=c("Initial","Corrigé"), RWA=c(res$value$Valeur[1], res$value$Valeur[2]))
        ggplot(df, aes(x=type, y=RWA)) + geom_col() + labs(title="RWA – Initial vs Corrigé", x=NULL, y="RWA")
      })
    }
  })
}

# ============================================================
# ----- FIN MODULE 4 : Calcul RWA (SERVER) -----
# ============================================================
