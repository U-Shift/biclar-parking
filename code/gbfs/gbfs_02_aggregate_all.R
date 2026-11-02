# Aggregates per-operator grid counts into a single grid per period
# Run with: $ Rscript code/analysis/gbfs_02_aggregate_all.R > code/analysis/gbfs_02_aggregate_all_$(date +%Y%m%d).log 2>&1

source("code/analysis/gbfs_00_params.R")

BASE_FOLDER <- OUTPUT_FOLDER
if (!dir.exists(OUTPUT_FOLDER)) dir.create(OUTPUT_FOLDER, recursive = TRUE)

all_period_grids <- list()

for (p in seq_len(nrow(PERIODS))) { # p = 1 
    period <- PERIODS[p, ]
    period_tag <- sprintf(
        "%s_%d-%d",
        period$day_type, period$hour_start, period$hour_end
    )
    period_file <- sprintf("grid_bikes_%s.csv", period_tag)

    message(sprintf(
        "Aggregating all operators for period %s (%02d:00-%02d:00)...",
        period$day_type, period$hour_start, period$hour_end
    ))

    operator_dfs <- list()

    for (r in REGIONS) {
        op_path <- file.path(BASE_FOLDER, r, period_file)
        if (file.exists(op_path)) {
            df <- read.csv(op_path)
            df$operator <- r
            operator_dfs[[r]] <- df
        } else {
            warning(sprintf("File not found for operator %s: %s", r, op_path))
        }
    }

    if (length(operator_dfs) == 0) {
        warning(sprintf(
            "No operator data found for period %s (%02d:00-%02d:00). Skipping.",
            period$day_type, period$hour_start, period$hour_end
        ))
        next
    }

    combined <- dplyr::bind_rows(operator_dfs)
    # View(combined)

    # Merge 'bike' and 'bicycle' into 'bicycle'
    if ("n_bike" %in% names(combined)) {
        if (!"n_bicycle" %in% names(combined)) combined$n_bicycle <- 0
        combined$n_bicycle <- rowSums(cbind(combined$n_bicycle, combined$n_bike), na.rm = TRUE)

        if ("avg_bike_per_update" %in% names(combined)) {
            if (!"avg_bicycle_per_update" %in% names(combined)) combined$avg_bicycle_per_update <- 0
            combined$avg_bicycle_per_update <- rowSums(cbind(combined$avg_bicycle_per_update, combined$avg_bike_per_update), na.rm = TRUE)
        }

        if ("n_updates_bike" %in% names(combined)) {
            if (!"n_updates_bicycle" %in% names(combined)) combined$n_updates_bicycle <- 0
            combined$n_updates_bicycle <- pmax(combined$n_updates_bicycle, combined$n_updates_bike, na.rm = TRUE)
        }

        combined <- combined |> select(-matches("^(n|avg)_(updates_)?bike(_per_update)?$"))
    }

    # Identify vehicle count and average columns
    count_cols <- grep("^n_", names(combined), value = TRUE)
    count_cols <- count_cols[!grepl("(_per_update|_updates|^n_updates$|^n_days$|^n_vehicles$)", count_cols)]
    count_cols <- c("n_vehicles", count_cols)

    avg_cols <- grep("^avg_.*_per_update$", names(combined), value = TRUE)
    avg_cols <- c("avg_vehicles_per_update", setdiff(avg_cols, "avg_vehicles_per_update"))

    updates_type_cols <- grep("^n_updates_", names(combined), value = TRUE)

    # Aggregate across all operators per grid cell
    grid_all <- combined |>
        group_by(gid) |>
        summarise(
            across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
            across(all_of(avg_cols), ~ round(sum(.x, na.rm = TRUE), 2)),
            across(all_of(updates_type_cols), ~ max( ifelse(is.na(.x), 0, .x) )),
            n_updates = max(n_updates, na.rm = TRUE),
            n_days = max(n_days, na.rm = TRUE),
            n_operators = n_distinct(operator),
            operators = paste(sort(unique(operator)), collapse = ", "),
            .groups = "drop"
        )

    out_path <- file.path(OUTPUT_FOLDER, period_file)
    write.csv(grid_all, out_path, row.names = FALSE)
    message(sprintf("Wrote %s (%d grid cells)", out_path, nrow(grid_all)))

    # Store for combined table with renamed columns to avoid overlaps
    grid_period_renamed <- grid_all |>
        dplyr::rename_with(~ paste0(.x, "_", period_tag), -gid)
    all_period_grids[[period_tag]] <- grid_period_renamed
}

# Combine all periods into a single CSV
if (length(all_period_grids) > 0) {
    message("Combining all periods into a single grid dataset...")
    grid_combined <- Reduce(function(x, y) dplyr::full_join(x, y, by = "gid"), all_period_grids) |>
        dplyr::arrange(gid)

    combined_out_path <- file.path(OUTPUT_FOLDER, "grid_bikes_combined.csv")
    write.csv(grid_combined, combined_out_path, row.names = FALSE)
    message(sprintf("Wrote combined file %s (%d grid cells, %d columns)", combined_out_path, nrow(grid_combined), ncol(grid_combined)))
} else {
    warning("No period grids to combine.")
}

