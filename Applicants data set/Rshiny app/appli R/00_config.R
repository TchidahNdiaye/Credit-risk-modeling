# ============================================================
# calculetteRWA - Configuration globale
# Compatible Portable Windows R-4.5.1
# ============================================================

PKG_REQUIRED <- c("shiny","bslib","shinyjs","data.table","DT","ggplot2","readxl","openxlsx")

# Colonnes ID Retour4C (confirmées par toi)
RET_ID_COLS <- c(
  "SG_CONTRACT_ID",
  "SG_CONTR_BCE_ID",
  "SG_CONTR_3D_ID",
  "SG_FACILITY_ID",
  "SG_ID_ENREGISTRMNT"
)

# Colonnes utiles Retour4C (selon tes captures + besoins app)
RET_COLS_SYNTH <- c("ASOF_DT","GoldenSource","PRTF","Encours","EAD","RWA")
RET_COLS_EXTRACT <- unique(c(
  "ASOF_DT","PRTF","SSPRTF","Segment","PRODUCT_ID",
  "Encours","Encours_B","Encours_HB","EAD","RWA","RW","CCF","LGD","PD","M",
  "GoldenSource",
  RET_ID_COLS
))

# Limites d'affichage
PREVIEW_N <- 5L
