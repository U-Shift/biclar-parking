# This file defines the two functions (impt_read / impt_write) that every other script in
# this project uses to load and save data - nowhere in the pipeline is base R's read.csv(),
# st_read(), readRDS(), etc. called directly. Instead, every script asks for a file by a
# short logical path like "/geo/grelha_h3_r10.gpkg", and these functions figure out where
# that actually lives on disk and how to open it based on its extension.
#
# Why this indirection exists: the original IMPT project runs on a shared server
# ("ushift@alfa") where all data sits under one fixed folder, but external collaborators
# (like this project) don't have access to that server or its gated data store. This is the
# "external" variant of the data-handling functions - see 00a_impt_data_handle_internal.R
# (removed from this project, but present in the original IMPT-data repo) for the
# server/gated-access version. Both variants expose the exact same impt_read()/impt_write()
# call signature, so every downstream script works unmodified regardless of which one is
# loaded - only the actual storage location differs. This version always resolves paths to a
# local "./data" subfolder of wherever you're running R from, so it works standalone with no
# server access, no API key, and no network dependency beyond whatever a given script itself
# downloads (e.g. the Global Building Atlas fetch in 02b_buildings_volume_prep.R).
#
# To use this project: source this file once at the start of an R session (before running
# any numbered script), and it will have defined impt_read/impt_write for the rest of the
# session. Files are read from / written to ./data/<path>, e.g. impt_read("/geo/foo.gpkg")
# resolves to ./data/geo/foo.gpkg relative to your R working directory.

library(dplyr)
library(sf)
library(tidyr)

# impt_read(path, root, write_to, ...) - loads a file by logical path, auto-detecting how to
# read it from the file extension in `path`. `root` overrides the default "./data" prefix for
# one call (rarely needed here). `write_to`, if given, also caches a local copy at that path
# after reading - used elsewhere in this project only when reading from a remote source
# (not relevant to the local-only setup this variant is built for, but kept for compatibility
# with scripts written for the internal/server variant).
impt_read <- function(path, root = NULL, write_to = NULL, ...) { # Use ... for arguments specific for file format reading (e.g. layer for gpkg or delim for csv)
    # Logical paths are always written starting with "/" (e.g. "/geo/foo.gpkg") by convention
    # across the project - this just tolerates callers who forgot the leading slash.
    if (!startsWith(path, "/")) {
        path <- paste0("/", path)
    }
    # This is the one line that actually differs from the internal/server variant: there,
    # this branch also checks which machine R is running on and points at the shared server
    # data folder if so. Here, it's unconditionally "data" (a folder relative to the current
    # R working directory) - there's no server to detect, hence no branching logic.
    path_root <- root
    path_append <- ""
    if (is.null(path_root)) {
        path_root <- "data"
    }
    # Build the real filesystem (or URL) path by gluing the root and the logical path together.
    file_path <- paste(path_root, path, path_append, sep = "")
    if (grepl("^http", file_path, ignore.case = TRUE)) { # If http, encode
        file_path <- URLencode(file_path)
    }
    message(sprintf("Downloading file from %s...", file_path))
    # From here down: one branch per supported file extension, each calling the R package
    # function that actually knows how to read that format (readr for csv, sf for
    # geojson/gpkg, base R for Rds, readxl for Excel). Whichever branch matches returns
    # immediately, so only one of these blocks ever actually executes per call.
    if (grepl(".csv", file_path, ignore.case = TRUE)) {
        content <- readr::read_delim(file_path, show_col_types = FALSE, name_repair = make.names, ...)
        if (!is.null(write_to)) {
            impt_write(content, write_to)
        }
        return(content)
    }
    if (grepl(".geojson", file_path, ignore.case = TRUE) | grepl(".gpkg", file_path, ignore.case = TRUE)) {
        content <- sf::st_read(file_path, ...)
        if (!is.null(write_to)) {
            impt_write(content, write_to)
        }
        return(content)
    }
    if (grepl(".rds", file_path, ignore.case = TRUE) | grepl(".Rds", file_path, ignore.case = TRUE)) {
        # readRDS() (unlike read_delim/st_read) can't read directly from a URL, so if the
        # resolved path turned out to be an http(s) address, download it to a temp file
        # first and read from that instead. Not exercised in this local-only project, since
        # path_root is always the local "data" folder here - kept for parity with the
        # internal/server variant, where path_root can legitimately be a URL.
        if (grepl("^http", file_path, ignore.case = TRUE)) {
            file_local <- file.path(tempdir(), basename(path))
            download.file(file_path, file_local, quiet = TRUE, mode = "wb")
            file_path <- file_local
        }
        content <- readRDS(file_path, ...)
        if (!is.null(write_to)) {
            impt_write(content, write_to)
        }
        return(content)
    }
    if (grepl(".xls", file_path, ignore.case = TRUE) | grepl(".xlsx", file_path, ignore.case = TRUE)) {
        if (grepl("^http", file_path, ignore.case = TRUE)) {
            file_local <- file.path(tempdir(), basename(path))
            download.file(file_path, file_local, quiet = TRUE, mode = "wb")
            file_path <- file_local
        }
        content <- readxl::read_excel(file_path, ...)
        if (!is.null(write_to)) {
            impt_write(content, write_to)
        }
        return(content)
    }
    # Fallback for any other extension: if the caller passed write_to, treat this as "just
    # fetch the raw file, don't try to parse it" - download it as-is and hand back the local
    # path rather than a parsed R object. Used for things like zipped downloads.
    if (!is.null(write_to)) {
        destination <- file.path(write_to, basename(path))
        message(sprintf("Storing file at %s...", destination))
        download.file(file_path, destination)
        return(destination)
    }
    # No branch above matched and there was nothing else useful to do with this extension.
    stop("Invalid file extension! Consult the IMPT team to improve this method!")
}
# impt_write(content, path, root) - the save-side counterpart to impt_read(): given an R
# object (data frame, sf object, etc.) and a logical path, figures out the file format from
# the path's extension and calls the matching writer. Every script in this project calls this
# instead of write.csv()/st_write()/saveRDS() directly, so all output consistently lands
# under ./data/<path> without each script needing to know or construct that path itself.
impt_write <- function(content, path, root = NULL) { # Use ... for arguments specific for file format reading (e.g. layer for gpkg or delim for csv)
    # Same leading-slash normalization as impt_read() above.
    if (!startsWith(path, "/")) {
        path <- paste0("/", path)
    }
    # Leftover from the internal/server variant, where this value is actually used to decide
    # the storage root. Here it's computed but never referenced below (path_root is always
    # "data" regardless) - on Windows this line will typically just emit a harmless warning
    # ('cat' is not a Windows command) since there's no /etc/machine-id to read; it doesn't
    # stop execution because the result is discarded either way.
    machine_id <- system("cat /etc/machine-id", intern = TRUE)
    path_root <- root
    path_append <- ""
    if (is.null(path_root)) {
        path_root <- "data"
    }
    file_path <- paste(path_root, path, path_append, sep = "")
    # Auto-create any missing subfolders under ./data the first time something is written
    # there (e.g. the first impt_write() to "/landuse/..." creates ./data/landuse/ itself) -
    # scripts never need to mkdir their own output folders.
    file_folder <- dirname(file_path)
    if (!dir.exists(file_folder)) {
        dir.create(file_folder, recursive = TRUE)
    }
    message(sprintf("Writing file to %s...", file_path))
    # Re-running a script that writes the same output path just overwrites it (with a
    # warning), rather than erroring or silently refusing - convenient for iterating on a
    # script, but means there's no accidental protection against clobbering a previous run's
    # output under the same filename.
    if (file.exists(file_path)) {
        warning("File path already exists, overwritting...")
    }
    # One branch per supported output format, mirroring the read side above.
    if (grepl(".csv", file_path, ignore.case = TRUE)) {
        write.csv(content, file_path, row.names = FALSE)
        return(file_path)
    }
    if (grepl(".geojson", file_path, ignore.case = TRUE) | grepl(".gpkg", file_path, ignore.case = TRUE)) {
        sf::st_write(content, file_path, delete_dsn = TRUE)
        return(file_path)
    }
    if (grepl(".rds", file_path, ignore.case = TRUE) | grepl(".Rds", file_path, ignore.case = TRUE)) {
        # .Rds preserves the exact R object (including sf geometry/CRS, list structure,
        # etc.) with no format conversion - used throughout this project for intermediate
        # checkpoints where fidelity matters more than the file being human-readable/portable.
        saveRDS(content, file_path)
        return(file_path)
    }
    # Not currently used by any script in this project (all geo output here goes through the
    # .geojson/.gpkg branch above instead). Note that ".geojson" also contains ".json" as a
    # substring, so this branch would in principle also match a .geojson path - but since
    # each grepl() check above returns immediately on its own match, the .geojson/.gpkg
    # branch always wins for those files and this one is only ever reached for plain .json.
    if (grepl(".json", file_path, ignore.case = TRUE)) {
        # Consider content a json string
        if (is.character(content)) {
            writeLines(content, con = file_path)
            return(file_path)
        }
        jsonlite::write_json(content, file_path, auto_unbox = TRUE, pretty = TRUE)
        return(file_path)
    }
    # No branch above matched - same fallback message as impt_read()'s final stop().
    stop("Invalid file extension! Consult the IMPT team to improve this method!")
}
