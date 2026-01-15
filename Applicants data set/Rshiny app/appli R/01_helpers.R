# ============================================================
# Helpers : format, robustesse, gestion erreurs
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
})

fmt_num <- function(x, digits = 2, big_mark = " ", dec_mark = ",") {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), NA_character_,
         formatC(x, format = "f", digits = digits, big.mark = big_mark, decimal.mark = dec_mark))
}

normalize_id <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- gsub("[\r\n\t]", "", x)
  x <- ifelse(is.na(x), "", x)
  x
}

smart_as_numeric <- function(x) {
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
  tryCatch(
    {
      val <- eval.parent(substitute(expr))
      list(ok = TRUE, value = val, message = paste0("OK: ", step_name))
    },
    error = function(e) {
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

need_cols <- function(dt, cols, context = "Dataset") {
  miss <- setdiff(cols, names(dt))
  if (length(miss) > 0) {
    stop(paste0(context, " - colonnes manquantes: ", paste(miss, collapse = ", "), "."), call. = FALSE)
  }
  TRUE
}

asof_label <- function(d) {
  if (length(d) == 0) return(NA_character_)
  d <- d[!is.na(d)]
  if (length(d) == 0) return(NA_character_)
  if (length(unique(d)) == 1) {
    format(unique(d), "%d/%m/%Y")
  } else {
    paste0(format(min(d), "%d/%m/%Y"), " → ", format(max(d), "%d/%m/%Y"))
  }
}
