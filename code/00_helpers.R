analyse_col_dispersion <- function(
  col, digits = 2,
  measures = c("mean", "p25", "p75", "median", "min", "max", "sd"),
  measures_extra = c(), # sum, cv, iqr
  measures_to_exclude = c()
) {
  measures_to_consider = setdiff(measures, measures_to_exclude)
  measures_to_consider = unique(c(measures_to_consider, measures_extra))
  col_sym <- rlang::ensym(col)
  col_name <- rlang::as_string(col_sym)
  d <- digits
  out <- list()

  if ("sum" %in% measures_to_consider) {
    out[[paste0(col_name, "_sum")]] <- rlang::expr(round(sum(!!col_sym, na.rm = TRUE), !!d))
  }
  if ("cv" %in% measures_to_consider) {
    out[[paste0(col_name, "_cv")]] <- rlang::expr(round(sd(!!col_sym, na.rm = TRUE) / mean(!!col_sym, na.rm = TRUE), !!d))
  }
  if ("iqr" %in% measures_to_consider) {
    out[[paste0(col_name, "_iqr")]] <- rlang::expr(round(IQR(!!col_sym, na.rm = TRUE), !!d))
  }
  if ("mean" %in% measures_to_consider) {
    out[[paste0(col_name, "_mean")]] <- rlang::expr(round(mean(!!col_sym, na.rm = TRUE), !!d))
  }
  if ("p25" %in% measures_to_consider) {
    out[[paste0(col_name, "_p25")]] <- rlang::expr(round(quantile(!!col_sym, 0.25, na.rm = TRUE), !!d))
  }
  if ("p75" %in% measures_to_consider) {
    out[[paste0(col_name, "_p75")]] <- rlang::expr(round(quantile(!!col_sym, 0.75, na.rm = TRUE), !!d))
  }
  if ("median" %in% measures_to_consider) {
    out[[paste0(col_name, "_median")]] <- rlang::expr(round(median(!!col_sym, na.rm = TRUE), !!d))
  }
  if ("min" %in% measures_to_consider) {
    out[[paste0(col_name, "_min")]] <- rlang::expr(round(min(!!col_sym, na.rm = TRUE), !!d))
  }
  if ("max" %in% measures_to_consider) {
    out[[paste0(col_name, "_max")]] <- rlang::expr(round(max(!!col_sym, na.rm = TRUE), !!d))
  }
  if ("sd" %in% measures_to_consider) {
    out[[paste0(col_name, "_sd")]] <- rlang::expr(round(sd(!!col_sym, na.rm = TRUE), !!d))
  }
  if ("sum" %in% measures_to_consider) {
    out[[paste0(col_name, "_sum")]] <- rlang::expr(round(sum(!!col_sym, na.rm = TRUE), !!d))
  }

  out
}
