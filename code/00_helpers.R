analyse_col_dispersion <- function(
  col, digits = 2,
  measures = c("mean", "p25", "p75", "median", "min", "max", "sd"),
  measures_extra = c(), # sum, cv, iqr
  measures_to_exclude = c()
) {
  measures_to_consider <- setdiff(measures, measures_to_exclude)
  measures_to_consider <- unique(c(measures_to_consider, measures_extra))
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

  out
}


# Aggregates point or grid attributes to a polygon layer using a common layer_id column.
#
# Parameters:
# - points: data.frame or sf object containing the records/attributes to aggregate,
#           with a column matching `layer_id`.
# - layer: sf object with polygon geometries and the unique `layer_id`.
# - layer_id: Name of unique ID column present in both `layer` and `points` (e.g. "gid", "pid", "mid").
#             If NULL, searches for common ID names ("gid", "pid", "mid", "id", "ID").
# - col_to_agg: Column(s) and aggregation functions. Can be:
#     * NULL: Only computes record count `n`.
#     * Named list (different methods per col):
#         list(capacity = c("sum", "mean"), requests = "sum")
#     * Named vector:
#         c(capacity = "sum", score = "mean")
#     * Character vector / string (uses methods in `agg_measures`):
#         c("capacity", "slots") or "capacity"
# - agg_measures: Measures used when `col_to_agg` does not specify them explicitly.
#                 Supported: "sum", "mean", "median", "min", "max", "sd". Default: c("sum").
# - join: Deprecated/unused parameter kept for backwards compatibility.
# - keep_empty: If TRUE (default), keeps polygons with 0 points (fills `n` and `*_sum` with 0).
#
# Examples:
#   aggregate_at_layer(grid_dashboard, parishes_dashboard, layer_id = "pid")
#   aggregate_at_layer(grid_dashboard, parishes_dashboard, layer_id = "pid", col_to_agg = "capacity")
#   aggregate_at_layer(grid_dashboard, parishes_dashboard, layer_id = "pid", col_to_agg = list(capacity = c("sum", "mean"), requests = "sum"))
aggregate_at_layer <- function(
  points,
  layer,
  layer_id = NULL,
  col_to_agg = NULL,
  agg_measures = c("sum"),
  join = sf::st_intersects,
  keep_empty = TRUE
) {
  stopifnot(inherits(layer, "sf"))

  # Identify unique ID column present in both layer and points
  if (!is.null(layer_id)) {
    layer_id_str <- if (is.character(layer_id)) layer_id else rlang::as_string(rlang::ensym(layer_id))
    if (!layer_id_str %in% names(layer)) {
      stop(sprintf("Column '%s' specified in `layer_id` not found in `layer`.", layer_id_str))
    }
    if (!layer_id_str %in% names(points)) {
      stop(sprintf("Column '%s' specified in `layer_id` not found in `points`.", layer_id_str))
    }
    id_var <- layer_id_str
  } else {
    id_candidates <- c("gid", "pid", "mid", "id", "ID")
    common_id <- intersect(id_candidates, intersect(names(layer), names(points)))
    if (length(common_id) > 0) {
      id_var <- common_id[1]
    } else {
      stop("`layer_id` was not specified and no common ID column ('gid', 'pid', 'mid', 'id', 'ID') was found in both `layer` and `points`.")
    }
  }

  # Drop geometry from points for fast tabular aggregation
  points_df <- if (inherits(points, "sf")) sf::st_drop_geometry(points) else as.data.frame(points)

  # Align ID types if they differ (e.g. integer vs character)
  if (typeof(points_df[[id_var]]) != typeof(layer[[id_var]])) {
    mode(points_df[[id_var]]) <- mode(layer[[id_var]])
  }

  # Parse aggregation specification into a list: list(col_name = c("sum", ...))
  agg_specs <- list()

  if (is.list(col_to_agg)) {
    # e.g. list(slots = c("sum", "mean"), requests = "sum")
    agg_specs <- col_to_agg
  } else if (!is.null(names(col_to_agg)) && any(names(col_to_agg) != "")) {
    # Named vector, e.g. c(slots = "sum", score = "mean")
    for (col_name in names(col_to_agg)) {
      agg_specs[[col_name]] <- unname(col_to_agg[[col_name]])
    }
  } else if (!is.null(col_to_agg)) {
    # Character vector (or single string/symbol) with agg_measures applied to each
    for (col in col_to_agg) {
      col_str <- if (is.character(col)) col else rlang::as_string(rlang::ensym(col))
      agg_specs[[col_str]] <- agg_measures
    }
  }

  # Filter out points with NA for id_var
  pts_to_agg <- points_df |>
    dplyr::filter(!is.na(.data[[id_var]]))

  # Always include point counts 'n'
  summary_exprs <- list(n = dplyr::expr(dplyr::n()))
  sum_cols <- character()

  for (col_str in names(agg_specs)) {
    if (!col_str %in% names(pts_to_agg)) next
    col_sym <- rlang::sym(col_str)
    measures <- agg_specs[[col_str]]
    single_measure <- length(measures) == 1

    for (m in measures) {
      col_out <- if (single_measure) col_str else paste0(col_str, "_", m)

      if (m == "sum") {
        summary_exprs[[col_out]] <- dplyr::expr(sum(!!col_sym, na.rm = TRUE))
        sum_cols <- c(sum_cols, col_out)
      } else if (m == "mean") {
        summary_exprs[[col_out]] <- dplyr::expr(mean(!!col_sym, na.rm = TRUE))
      } else if (m == "median") {
        summary_exprs[[col_out]] <- dplyr::expr(median(!!col_sym, na.rm = TRUE))
      } else if (m == "min") {
        summary_exprs[[col_out]] <- dplyr::expr(min(!!col_sym, na.rm = TRUE))
      } else if (m == "max") {
        summary_exprs[[col_out]] <- dplyr::expr(max(!!col_sym, na.rm = TRUE))
      } else if (m == "sd") {
        summary_exprs[[col_out]] <- dplyr::expr(sd(!!col_sym, na.rm = TRUE))
      } else if (m == "n") {
        # n is already included
      }
    }
  }

  agg_df <- pts_to_agg |>
    dplyr::group_by(.data[[id_var]]) |>
    dplyr::summarise(!!!summary_exprs, .groups = "drop")

  if (keep_empty) {
    res <- layer |>
      dplyr::left_join(agg_df, by = id_var)

    # Fill NA values for count and sums with 0
    cols_to_fill_zero <- intersect(c("n", sum_cols), names(res))
    if (length(cols_to_fill_zero) > 0) {
      res <- res |>
        dplyr::mutate(dplyr::across(dplyr::all_of(cols_to_fill_zero), ~ tidyr::replace_na(.x, 0)))
    }
  } else {
    res <- layer |>
      dplyr::inner_join(agg_df, by = id_var)
  }

  return(res)
}

