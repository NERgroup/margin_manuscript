#jogsmith@ucsc.edu

rm(list=ls())


#Raw data were obtained by request from Kate Cavanaugh (kateccavanaugh@gmail.com)
#and are stored on a secured UCSC server. 

################################################################################
#Load packages and set directories

#require(librarian)

librarian::shelf(sf, dplyr, googledrive, here, fs, glue, ggplot2)

folder_url_2024 <- "https://drive.google.com/drive/folders/1ay6kdezUx2E65ZOpFGWlyY4ZTyLd96lE"
folder_url_2025 <- "https://drive.google.com/drive/folders/1jx1rt_aRv9Z778eYFm74gYgYWtPrIEnx"

out_dir <- here("output", "planet_dat", "processed")

dir_create(out_dir)

################################################################################
#Step 1 - prep data

# ---- Monterey Peninsula bounding box ----
monterey_bbox_4326 <- st_as_sfc(
  st_bbox(
    c(
      xmin = -121.99,
      xmax = -121.88,
      ymin = 36.53,
      ymax = 36.64
    ),
    crs = 4326
  )
)

################################################################################
#Step 2 - process 2024 data

year <- 2024
month <- "09"
year_month <- glue("{year}_{month}")

folder_url <- folder_url_2024

raw_dir <- here("data", "raw", "planet_dat", year_month)

dir_create(raw_dir)

# ---- download September shapefile pieces from Google Drive ----
files <- drive_ls(as_id(folder_url))

sept_files <- files %>%
  filter(grepl(glue("^{year_month}_California_KelpCanopy_KelpPresence\\."), name))

sept_files$name

# Download all matching sidecar files
for (i in seq_len(nrow(sept_files))) {
  drive_download(
    sept_files[i, ],
    path = file.path(raw_dir, sept_files$name[i]),
    overwrite = TRUE
  )
}

# ---- identify shapefile ----
shp_path <- file.path(
  raw_dir,
  glue("{year_month}_California_KelpCanopy_KelpPresence.shp")
)

stopifnot(file.exists(shp_path))

# Read full layer once (avoids n_max geometry bug)
planet_raw <- st_read(
  shp_path,
  quiet = FALSE
)

# Transform bbox to layer CRS
bbox_layer_crs <- st_transform(
  monterey_bbox_4326,
  st_crs(planet_raw)
)

# Spatial crop
planet_monterey <- planet_raw %>%
  st_make_valid() %>%
  st_crop(bbox_layer_crs)

# Transform back to lon/lat
planet_monterey_4326 <- st_transform(planet_monterey, 4326)

# Add year/month fields
planet_monterey_2024_4326 <- planet_monterey_4326 %>%
  mutate(
    year = year,
    month = month,
    source_ym = year_month
  )

################################################################################
#Step 3 - process 2025 data

year <- 2025
month <- "09"
year_month <- glue("{year}_{month}")

folder_url <- folder_url_2025

raw_dir <- here("data", "raw", "planet_dat", year_month)

dir_create(raw_dir)

# ---- download September shapefile pieces from Google Drive ----
files <- drive_ls(as_id(folder_url))

sept_files <- files %>%
  filter(grepl(glue("^{year_month}_California_KelpCanopy_KelpPresence\\."), name))

sept_files$name

# Download all matching sidecar files
for (i in seq_len(nrow(sept_files))) {
  drive_download(
    sept_files[i, ],
    path = file.path(raw_dir, sept_files$name[i]),
    overwrite = TRUE
  )
}

# ---- identify shapefile ----
shp_path <- file.path(
  raw_dir,
  glue("{year_month}_California_KelpCanopy_KelpPresence.shp")
)

stopifnot(file.exists(shp_path))

# Read full layer once (avoids n_max geometry bug)
planet_raw <- st_read(
  shp_path,
  quiet = FALSE
)

# Transform bbox to layer CRS
bbox_layer_crs <- st_transform(
  monterey_bbox_4326,
  st_crs(planet_raw)
)

# Spatial crop
planet_monterey <- planet_raw %>%
  st_make_valid() %>%
  st_crop(bbox_layer_crs)

# Transform back to lon/lat
planet_monterey_4326 <- st_transform(planet_monterey, 4326)

# Add year/month fields
planet_monterey_2025_4326 <- planet_monterey_4326 %>%
  mutate(
    year = year,
    month = month,
    source_ym = year_month
  )

################################################################################
#Step 4 - combine 2024 and 2025 data

planet_monterey_2024_2025_4326 <- bind_rows(
  planet_monterey_2024_4326,
  planet_monterey_2025_4326
)

################################################################################
#Step 5 - quick plot check

ggplot(planet_monterey_2024_2025_4326) +
  geom_sf() +
  facet_wrap(~year) +
  coord_sf(
    xlim = c(-121.99, -121.88),
    ylim = c(36.53, 36.64)
  )

################################################################################
#Step 6 - export individual year files

out_gpkg_2024 <- here("output", "planet_dat", "processed",
                      "planet_kelp_presence_monterey_2024_09.gpkg")

st_write(
  planet_monterey_2024_4326,
  out_gpkg_2024,
  delete_dsn = TRUE
)

out_gpkg_2025 <- here("output", "planet_dat", "processed",
                      "planet_kelp_presence_monterey_2025_09.gpkg")

st_write(
  planet_monterey_2025_4326,
  out_gpkg_2025,
  delete_dsn = TRUE
)

################################################################################
#Step 7 - export combined 2024 and 2025 file

out_gpkg <- here("output", "planet_dat", "processed",
                 "planet_kelp_presence_monterey_2024_2025.gpkg")

st_write(
  planet_monterey_2024_2025_4326,
  out_gpkg,
  delete_dsn = TRUE
)

################################################################################
#Step 8 - check file size for GitHub

file_size_mb <- file_info(out_gpkg)$size / 1024^2

file_size_mb

if (file_size_mb > 100) {
  message(glue("File is {round(file_size_mb, 1)} MB — too large for GitHub"))
} else {
  message(glue("File is {round(file_size_mb, 1)} MB — OK for GitHub"))
}
