# ============================================================
# I/O Retour4C : lecture robuste + lecture "chemin direct"
# Objectif : éviter le double stockage temp de fileInput sur gros fichier
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
})

# --------- Résolution du chemin Retour4C ---------
# Priorité:
# 1) chemin direct fourni par l'utilisateur (pas de copie => perf + pas d'espace insuffisante)
# 2) fileInput (copie dans tempdir, OK pour petits fichiers)
get_retour_path <- function(input) {
  p <- trimws(as.character(input$retour_path %||% ""))
  if (nzchar(p) && file.exists(p)) return(p)
  if (!is.null(input$retour_file) && nzchar(input$retour_file$datapath)) return(input$retour_file$datapath)
  ""
}

# --------- Lire uniquement l'entête (0 ligne) ---------
read_retour_header <- function(path) {
  fread(path, nrows = 0, showProgress = FALSE, encoding = "UTF-8")
}

# --------- Preview ---------
read_retour_preview <- function(path, n = 5L) {
  dt <- fread(path, nrows = n, showProgress = FALSE, encoding = "UTF-8")
  setDT(dt)
  if ("ASOF_DT" %in% names(dt)) dt[, ASOF_DT := as.IDate(ASOF_DT)]
  dt
}

# --------- Lecture ciblée (performance) ---------
# On ne charge PAS tout le fichier : seulement les colonnes nécessaires à la fonctionnalité demandée.
read_retour_select <- function(path, cols) {
  # fread(select=) est très efficace et évite d'allouer 3Go en RAM
  dt <- fread(path, select = intersect(cols, names(read_retour_header(path))),
              showProgress = TRUE, encoding = "UTF-8")
  setDT(dt)

  # Préparation légère
  if ("ASOF_DT" %in% names(dt)) {
    dt[, ASOF_DT := as.IDate(ASOF_DT)]
    dt[, ASOF := format(ASOF_DT, "%m/%Y")]
  } else {
    dt[, ASOF := NA_character_]
  }

  # IDs en texte + normalisation (sur le subset seulement)
  for (cc in intersect(RET_ID_COLS, names(dt))) dt[, (cc) := normalize_id(get(cc))]

  # Numerics légères
  num_cols <- intersect(c("Encours","Encours_B","Encours_HB","EAD","RWA","RW","CCF","LGD","PD","M"), names(dt))
  for (cc in num_cols) dt[, (cc) := smart_as_numeric(get(cc))]

  # colonnes catégorielles
  if (!"PRTF" %in% names(dt)) dt[, PRTF := NA_character_]
  if (!"SSPRTF" %in% names(dt)) dt[, SSPRTF := NA_character_]
  if (!"GoldenSource" %in% names(dt)) dt[, GoldenSource := NA_character_]

  dt
}

# --------- Synthèses ---------
compute_syn_gs <- function(dt) {
  if (!"Encours" %in% names(dt)) dt[, Encours := NA_real_]
  if (!"EAD" %in% names(dt)) dt[, EAD := NA_real_]
  if (!"RWA" %in% names(dt)) dt[, RWA := NA_real_]

  syn <- dt[, .(
    Nbre_observations = .N,
    Encours_MEUR = sum(Encours, na.rm = TRUE)/1e6,
    EAD_MEUR     = sum(EAD, na.rm = TRUE)/1e6,
    RWA_MEUR     = sum(RWA, na.rm = TRUE)/1e6
  ), by = .(ASOF, GoldenSource)]

  tot <- syn[, .(
    Nbre_observations = sum(Nbre_observations, na.rm = TRUE),
    Encours_MEUR = sum(Encours_MEUR, na.rm = TRUE),
    EAD_MEUR     = sum(EAD_MEUR, na.rm = TRUE),
    RWA_MEUR     = sum(RWA_MEUR, na.rm = TRUE)
  ), by = .(ASOF)]
  tot[, GoldenSource := "TOTAL"]
  syn <- rbindlist(list(syn, tot), use.names = TRUE, fill = TRUE)
  setorder(syn, ASOF, GoldenSource)
  syn
}

compute_syn_prtf <- function(dt) {
  if (!"Encours" %in% names(dt)) dt[, Encours := NA_real_]
  if (!"EAD" %in% names(dt)) dt[, EAD := NA_real_]
  if (!"RWA" %in% names(dt)) dt[, RWA := NA_real_]

  syn <- dt[, .(
    Nbre_observations = .N,
    Encours_MEUR = sum(Encours, na.rm = TRUE)/1e6,
    EAD_MEUR     = sum(EAD, na.rm = TRUE)/1e6,
    RWA_MEUR     = sum(RWA, na.rm = TRUE)/1e6
  ), by = .(ASOF, PRTF)]

  tot <- syn[, .(
    Nbre_observations = sum(Nbre_observations, na.rm = TRUE),
    Encours_MEUR = sum(Encours_MEUR, na.rm = TRUE),
    EAD_MEUR     = sum(EAD_MEUR, na.rm = TRUE),
    RWA_MEUR     = sum(RWA_MEUR, na.rm = TRUE)
  ), by = .(ASOF)]
  tot[, PRTF := "TOTAL"]
  syn <- rbindlist(list(syn, tot), use.names = TRUE, fill = TRUE)
  setorder(syn, ASOF, PRTF)
  syn
}
