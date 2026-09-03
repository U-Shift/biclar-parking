# Aggregates per-operator grid counts into a single grid per period
# Run with: $ Rscript code/analysis/gbfs_02_aggregate_all.R > code/analysis/gbfs_02_aggregate_all_$(date +%Y%m%d).log 2>&1

source("code/analysis/gbfs_00_params.R")

output_folder <- "data/analysis/gbfs"
if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

for (p in seq_len(nrow(PERIODS))) {
    period <- PERIODS[p, ]
    period_file <- sprintf(
        "grid_bikes_%s_%d-%d.csv",
        period$day_type, period$hour_start, period$hour_end
    )

    message(sprintf(
        "Aggregating all operators for period %s (%02d:00-%02d:00)...",
        period$day_type, period$hour_start, period$hour_end
    ))

    operator_dfs <- list()

    for (r in REGIONS) {
        op_path <- file.path(output_folder, r, period_file)
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

    # Merge 'bike' and 'bicycle' into 'bicycle'
    if ("n_bike" %in% names(combined)) {
        if (!"n_bicycle" %in% names(combined)) combined$n_bicycle <- 0
        combined$n_bicycle <- rowSums(cbind(combined$n_bicycle, combined$n_bike), na.rm = TRUE)
        combined <- combined |> select(-matches("^(n|avg)_(updates_)?bike(_per_update)?$"))
    }

    # Identify vehicle count columns (e.g. n_vehicles, n_scooter, n_bicycle)
    count_cols <- grep("^n_", names(combined), value = TRUE)
    count_cols <- count_cols[!grepl("(_per_update|_updates|^n_updates$|^n_days$|^n_vehicles$)", count_cols)]
    count_cols <- c("n_vehicles", count_cols)

    # Aggregate across all operators per grid cell
    grid_all <- combined |>
        group_by(gid) |>
        summarise(
            across(all_of(count_cols), ~ sum(.x, na.rm = TRUE)),
            n_updates = max(n_updates, na.rm = TRUE),
            n_days = max(n_days, na.rm = TRUE),
            n_operators = n_distinct(operator),
            operators = paste(sort(unique(operator)), collapse = ", "),
            .groups = "drop"
        ) |>
        mutate(
            avg_vehicles_per_update = round(n_vehicles / n_updates, 2)
        )

    # Compute per-vehicle-type updates and per_update rates
    for (col in count_cols[count_cols != "n_vehicles"]) {
        type_name <- sub("^n_", "", col)
        grid_all[[paste0("n_updates_", type_name)]] <- ifelse(grid_all[[col]] > 0, grid_all$n_updates, 0)
        grid_all[[paste0("avg_", type_name, "_per_update")]] <- round(grid_all[[col]] / grid_all$n_updates, 2)
    }

    out_path <- file.path(output_folder, period_file)
    write.csv(grid_all, out_path, row.names = FALSE)
    message(sprintf("Wrote %s (%d grid cells)", out_path, nrow(grid_all)))
}
