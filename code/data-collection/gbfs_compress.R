# Run with: $ Rscript code/data-collection/gbfs_compress.R

BASE_FOLDERS <- c(
    "data/collection/gbfs_compressed/bird_lisboa",
    # "data/collection/gbfs_compressed/bird_cascais",
    # "data/collection/gbfs_compressed/bird_almada",
    # "data/collection/gbfs_compressed/lime_almada",
    # "data/collection/gbfs_compressed/lime_lisboa",
    # "data/collection/gbfs_compressed/lime_oeiras",
    "data/collection/gbfs_compressed/emel_gira"
)

today_str <- format(Sys.Date(), "%Y%m%d")

# Subdirectories to check and compress under <base_folder>/collection/
SUB_TYPES <- c("free_bike_status", "station_status", "station_information")

# Note: Requires the 'arrow' package (install.packages("arrow"))
for (base_folder in BASE_FOLDERS) {
    for (sub_type in SUB_TYPES) {
        status_dir <- file.path(base_folder, "collection", sub_type)

        if (!dir.exists(status_dir)) {
            next
        }

        # List immediate subdirectories in status_dir
        all_subdirs <- list.dirs(status_dir, full.names = TRUE, recursive = FALSE)

        # Keep only folders matching format YYYYMMDD strictly before today (< today_str)
        day_folders <- basename(all_subdirs)
        valid_idx <- grepl("^\\d{8}$", day_folders) & (day_folders < today_str)
        subdirs <- all_subdirs[valid_idx]

        if (length(subdirs) == 0) {
            message(sprintf("No eligible past-date folders found in '%s'.", status_dir))
            next
        }

        for (dir_path in subdirs) {
            parquet_file <- paste0(dir_path, ".parquet")
            message(sprintf("Processing and merging CSVs from '%s' into '%s'...", dir_path, parquet_file))

            csv_files <- list.files(dir_path, pattern = "\\.csv$", full.names = TRUE)

            if (length(csv_files) == 0) {
                message(sprintf("No CSV files found in '%s'. Skipping.", dir_path))
                next
            }

            # Read and bind all CSV files, tracking file path for timestamp extraction
            day_folder <- basename(dir_path)
            df <- purrr::map_dfr(csv_files, function(f) {
                data <- utils::read.csv(f, stringsAsFactors = FALSE)
                time_part <- sub("\\.csv$", "", basename(f))
                data$datetime <- as.POSIXct(paste(day_folder, time_part), format = "%Y%m%d %H%M%S")
                data
            })

            arrow::write_parquet(df, parquet_file) # , compression = "brotli"
            message(sprintf("Saved %d rows to '%s'.", nrow(df), parquet_file))

            # Delete the original folder
            unlink(dir_path, recursive = TRUE)

            # Read generated file, for testing purposes
            # df_read <- arrow::read_parquet(parquet_file)
        }
    }
}
