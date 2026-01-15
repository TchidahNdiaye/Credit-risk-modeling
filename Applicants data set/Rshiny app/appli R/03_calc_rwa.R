# ============================================================
# Calculs RWA : Standard + IRBA + sûreté (haircut simple app)
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
})

compute_rwa_sa <- function(EAD, RW) {
  EAD <- smart_as_numeric(EAD)
  RW  <- smart_as_numeric(RW)
  RWn <- ifelse(RW > 1.5, RW / 100, RW)
  EAD * RWn
}

apply_surety_haircut <- function(EAD, surete, hc) {
  EAD <- smart_as_numeric(EAD)
  s <- smart_as_numeric(surete); s <- ifelse(is.na(s), 0, s)
  hc <- smart_as_numeric(hc); hc <- ifelse(is.na(hc), 0, hc)
  EAD * ifelse(s >= 1, pmax(0, 1 - hc), 1)
}

# --- IRBA (même logique que version précédente) ---
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
  df <- as.integer(default_flag); df[is.na(df)] <- 0L

  K <- rep(NA_real_, length(PDn))
  nd <- df == 0L
  dd <- df == 1L

  K[nd] <- K_irb_nondefault(PDn[nd], LGDn[nd], R[nd], M = M[nd], apply_MA = apply_MA[nd])
  K[dd] <- K_irb_default_proxy(PDn[dd], LGDn[dd])

  12.5 * K * EAD * scaling
}
