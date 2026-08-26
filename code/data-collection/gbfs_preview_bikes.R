data_source <- "data/collection/gbfs/bird_lisboa"
data_source <- "data/collection/gbfs/bird_cascais"
data_source <- "data/collection/gbfs/bird_almada"
data_source <- "data/collection/gbfs/lime_almada"
data_source <- "data/collection/gbfs/lime_lisboa"
data_source <- "data/collection/gbfs/lime_oeiras"


files <- list.files(paste(data_source, "/collection/free_bike_status", sep = ""), pattern = "\\.csv", full.names = TRUE, recursive = TRUE)
files <- files[order(file.mtime(files))]

if (length(files) > 0) {}

sample <- read.csv(sample(files, 1))
sample_sf <- sample |> st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview::mapview(sample_sf)
