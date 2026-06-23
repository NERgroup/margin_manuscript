rm(list = ls())

################################################################################
# Load packages and set directories

librarian::shelf(
  tidyverse, sf, readr, here, fs, glue, janitor,
  ggplot2, ggspatial, rnaturalearth, rnaturalearthdata,
  ggpubr, grid, scales
)

datin <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database/processed"
datadir <- "/Volumes/enhydra/data/kelp_recovery/"

fig_dir <- here("figures")
dir_create(fig_dir)

################################################################################
# Step 1 - load data

planet_dat <- st_read(
  here("output", "planet_dat", "processed",
       "planet_kelp_presence_monterey_2024_2025.gpkg"),
  quiet = FALSE
) %>%
  clean_names() %>%
  st_transform(4326)

sitetab <- read_csv(
  file.path(datin, "site_tables", "margin_site_table.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

ca_counties <- st_read(
  file.path(datadir, "gis_data/raw/ca_county_boundaries/s7vc7n.shp"),
  quiet = TRUE
) %>%
  st_transform(4326)

forage_orig <- read_csv(
  "/Volumes/enhydra/data/foraging_data/processed/foraging_data_2024_2025_processed.csv",
  show_col_types = FALSE
) %>%
  clean_names()

usa <- ne_states(
  country = "United States of America",
  returnclass = "sf"
)

foreign <- ne_countries(
  country = c("Canada", "Mexico"),
  returnclass = "sf"
)

################################################################################
# Step 2 - themes and colors

my_theme <- theme(
  axis.text = element_text(size = 7),
  axis.title = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black"),
  legend.position = "none",
  panel.background = element_rect(fill = "white")
)

base_theme <- theme(
  axis.text = element_text(size = 10, color = "black"),
  axis.text.y = element_text(angle = 90, hjust = 0.5, color = "black"),
  axis.title = element_text(size = 10, color = "black"),
  plot.title = element_text(size = 12, face = "bold", color = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  legend.key = element_blank(),
  legend.background = element_rect(fill = alpha("blue", 0)),
  legend.key.height = unit(1, "lines"),
  legend.text = element_text(size = 9, color = "black"),
  legend.title = element_text(size = 10, color = "black"),
  strip.background = element_blank(),
  strip.text = element_text(size = 10, face = "bold", color = "black")
)

################################################################################
# Step 3 - collapse to one row per site

site_pts <- sitetab %>%
  group_by(site_name_2025) %>%
  summarize(
    latitude = first(latitude),
    longitude = first(longitude),
    heading_out = first(heading_out),
    .groups = "drop"
  ) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326,
    remove = FALSE
  )

################################################################################
# Step 4 - function to create 20 m x 80 m transect swath rectangles

make_transect_swath <- function(x, y, heading_deg, length_m = 80, width_m = 20) {
  
  theta <- heading_deg * pi / 180
  
  dx <- sin(theta)
  dy <- cos(theta)
  
  px <- cos(theta)
  py <- -sin(theta)
  
  half_width <- width_m / 2
  
  p1 <- c(x + px * half_width, y + py * half_width)
  p2 <- c(x - px * half_width, y - py * half_width)
  p3 <- c(x - px * half_width + dx * length_m,
          y - py * half_width + dy * length_m)
  p4 <- c(x + px * half_width + dx * length_m,
          y + py * half_width + dy * length_m)
  
  st_polygon(list(rbind(p1, p2, p3, p4, p1)))
}

################################################################################
# Step 5 - project to meters and build swaths

site_pts_utm <- site_pts %>%
  st_transform(32610)

site_coords <- st_coordinates(site_pts_utm)

swath_geom <- purrr::pmap(
  list(
    x = site_coords[, "X"],
    y = site_coords[, "Y"],
    heading_deg = site_pts_utm$heading_out
  ),
  make_transect_swath
)

transect_swaths <- site_pts_utm %>%
  st_drop_geometry() %>%
  st_as_sf(
    geometry = st_sfc(swath_geom, crs = 32610)
  ) %>%
  st_transform(4326)

################################################################################
# Step 6 - build land and benthic site buffer layers

land_utm <- ca_counties %>%
  st_transform(32610) %>%
  st_make_valid() %>%
  st_union() %>%
  st_as_sf() %>%
  st_make_valid() %>%
  mutate(land_layer = "land")

transect_centroids_utm <- transect_swaths %>%
  st_transform(32610) %>%
  mutate(
    geometry = st_centroid(geometry)
  )

benthic_site_buffers <- transect_centroids_utm %>%
  st_buffer(200) %>%
  st_make_valid() %>%
  mutate(buffer_m = 200)

benthic_site_buffer_union <- benthic_site_buffers %>%
  summarise() %>%
  st_union() %>%
  st_as_sf() %>%
  st_make_valid() %>%
  mutate(buffer_layer = "benthic site buffer")

################################################################################
# Step 7 - build year-specific kelp forest and edge layers

kelp_utm <- planet_dat %>%
  st_transform(32610) %>%
  st_make_valid()

kelp_forest <- kelp_utm %>%
  group_by(year) %>%
  summarise() %>%
  st_union(by_feature = TRUE) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  mutate(kelp_layer = "forest")

kelp_edge <- kelp_forest %>%
  group_by(year) %>%
  summarise() %>%
  st_boundary() %>%
  st_as_sf() %>%
  st_make_valid()

################################################################################
# Step 8 - extract all successful urchin prey dives in July-October 2024-2025

focal_dives_raw <- forage_orig %>%
  mutate(
    prey = str_to_lower(str_trim(prey)),
    date = as.Date(date),
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>%
  filter(
    year %in% 2024:2025,
    month %in% 7:10,
    prey %in% c("urc", "pur", "red"),
    success == "y"
  ) %>%
  st_as_sf(
    coords = c("long", "lat"),
    crs = 4326,
    remove = FALSE
  ) %>%
  st_transform(32610)

################################################################################
# Step 9 - function to screen dives and calculate distance to year-specific kelp edge

classify_dives_by_year <- function(focal_year) {
  
  message(glue("Classifying {focal_year} dives with {focal_year} kelp canopy"))
  
  focal_dives_year <- focal_dives_raw %>%
    filter(year == focal_year)
  
  kelp_forest_year <- kelp_forest %>%
    filter(year == focal_year)
  
  kelp_edge_year <- kelp_edge %>%
    filter(year == focal_year)
  
  kelp_edge_geom_year <- kelp_edge_year %>%
    st_geometry() %>%
    st_union()
  
  nearest_edge_lines <- st_nearest_points(
    st_geometry(focal_dives_year),
    kelp_edge_geom_year
  )
  
  nearest_edge_pts <- nearest_edge_lines %>%
    st_line_sample(sample = 1) %>%
    st_cast("POINT") %>%
    st_sf(geometry = .) %>%
    st_set_crs(32610)
  
  nearest_edge_pts <- nearest_edge_pts %>%
    mutate(
      edge_dist_to_land_m = as.numeric(st_distance(geometry, land_utm))
    )
  
  focal_dives_screened_year <- focal_dives_year %>%
    mutate(
      in_forest = lengths(st_intersects(geometry, kelp_forest_year)) > 0,
      dist_to_land_m = as.numeric(st_distance(geometry, land_utm)),
      edge_dist_to_land_m = nearest_edge_pts$edge_dist_to_land_m,
      landward_white_zone = !in_forest & dist_to_land_m < edge_dist_to_land_m,
      dist_to_nearest_kelp_edge_m = as.numeric(st_distance(geometry, kelp_edge_year)),
      signed_dist_to_nearest_kelp_edge_m = case_when(
        in_forest ~ dist_to_nearest_kelp_edge_m,
        TRUE ~ -dist_to_nearest_kelp_edge_m
      )
    )
  
  message(
    "Dropped ",
    sum(focal_dives_screened_year$landward_white_zone, na.rm = TRUE),
    " landward white-zone dives out of ",
    nrow(focal_dives_screened_year),
    " successful urchin dives in ",
    focal_year,
    "."
  )
  
  focal_dives_screened_year
}

################################################################################
# Step 10 - identify and drop landward white-zone dives

focal_dives_screened <- bind_rows(
  classify_dives_by_year(2024),
  classify_dives_by_year(2025)
)

# Optional QA map to inspect dropped points

p_white_zone_check <- ggplot() +
  geom_sf(
    data = ca_counties,
    color = "grey70",
    fill = "grey85",
    linewidth = 0.2
  ) +
  geom_sf(
    data = planet_dat,
    fill = "#1B9E77",
    color = NA,
    alpha = 0.65
  ) +
  geom_sf(
    data = focal_dives_screened %>% st_transform(4326),
    aes(fill = landward_white_zone),
    color = "black",
    shape = 21,
    size = 1,
    alpha = 0.7
  ) +
  facet_wrap(~year) +
  scale_fill_manual(
    values = c(
      "FALSE" = "#7570B3",
      "TRUE" = "red"
    ),
    labels = c(
      "FALSE" = "Retained",
      "TRUE" = "Dropped"
    ),
    name = NULL
  ) +
  coord_sf(
    xlim = c(-121.99, -121.88),
    ylim = c(36.53, 36.64),
    expand = FALSE
  ) +
  theme_bw() +
  base_theme +
  theme(
    legend.position = "right"
  )

p_white_zone_check

ggsave(
  here("figures", "Fig2_white_zone_dropped_points_check_2024_2025.png"),
  p_white_zone_check,
  width = 8,
  height = 6,
  dpi = 600,
  bg = "white"
)

# Final screened dive data used for all analyses below

focal_dives <- focal_dives_screened %>%
  filter(!landward_white_zone) %>%
  select(-dist_to_land_m, -edge_dist_to_land_m, -landward_white_zone)

################################################################################
# Step 11 - retain dives within 200 m of benthic transect centroids

#focal_dives <- focal_dives %>%
#  filter(
#    lengths(st_intersects(geometry, benthic_site_buffer_union)) > 0
#  )

################################################################################
# Step 12 - bin retained successful urchin dives by 20 m distance bins

all_urchin_dive_dist_sum <- focal_dives %>%
  st_drop_geometry() %>%
  filter(
    signed_dist_to_nearest_kelp_edge_m >= -500
  ) %>%
  mutate(
    dist_bin = round(signed_dist_to_nearest_kelp_edge_m / 20) * 20
  ) %>%
  count(dist_bin, name = "n_dives") %>%
  complete(
    dist_bin = seq(
      -500,
      ceiling(max(dist_bin, na.rm = TRUE) / 20) * 20,
      by = 20
    ),
    fill = list(n_dives = 0)
  )

################################################################################
# Step 13 - California inset

ca_inset <- ggplot() +
  geom_sf(
    data = foreign,
    fill = "grey85",
    color = "white",
    linewidth = 0.2
  ) +
  geom_sf(
    data = usa,
    fill = "grey85",
    color = "white",
    linewidth = 0.2
  ) +
  annotate(
    "rect",
    xmin = -122.6,
    xmax = -121,
    ymin = 36.2,
    ymax = 37.1,
    color = "black",
    fill = NA,
    lwd = 0.8
  ) +
  coord_sf(
    xlim = c(-124.5, -117),
    ylim = c(32.5, 42),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    plot.margin = margin(0, 0, 0, 0),
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5
    ),
    plot.background = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.5
    )
  )

ca_inset_grob <- ggplotGrob(ca_inset)

################################################################################
# Step 14 - Panel A: map of retained successful urchin foraging dives in 2024

A <- ggplot() +
  
  geom_sf(
    data = planet_dat %>% filter(year == 2024),
    fill = "#1B9E77",
    color = NA,
    alpha = 0.65
  ) +
  
  geom_sf(
    data = ca_counties,
    color = "grey70",
    fill = "grey85",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = focal_dives %>% filter(year == 2024) %>% st_transform(4326),
    color = "black",
    fill = "#7570B3",
    shape = 21,
    size = 0.5,
    alpha = 0.4
  ) +
  
  annotation_custom(
    grob = ca_inset_grob,
    xmin = -121.9020,
    xmax = -121.8750,
    ymin = 36.618,
    ymax = 36.640
  ) +
  
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    height = unit(0.45, "cm"),
    width = unit(0.45, "cm"),
    style = north_arrow_orienteering(text_col = NA)
  ) +
  
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.7
  ) +
  
  coord_sf(
    xlim = c(-121.99, -121.88),
    ylim = c(36.53, 36.64),
    expand = FALSE
  ) +
  
  labs(
    x = NULL,
    y = NULL,
    tag = "A",
    title = "2024"
  ) +
  
  theme_bw() +
  my_theme +
  
  theme(
    plot.tag = element_text(face = "plain", color = "black"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 7, color = "black"),
    axis.title = element_blank()
  )

A

################################################################################
# Step 15 - Panel B: map of retained successful urchin foraging dives in 2025

B <- ggplot() +
  
  geom_sf(
    data = planet_dat %>% filter(year == 2025),
    fill = "#1B9E77",
    color = NA,
    alpha = 0.65
  ) +
  
  geom_sf(
    data = ca_counties,
    color = "grey70",
    fill = "grey85",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = focal_dives %>% filter(year == 2025) %>% st_transform(4326),
    color = "black",
    fill = "#7570B3",
    shape = 21,
    size = 0.5,
    alpha = 0.4
  ) +
  
  annotation_north_arrow(
    location = "tr",
    which_north = "true",
    height = unit(0.45, "cm"),
    width = unit(0.45, "cm"),
    style = north_arrow_orienteering(text_col = NA)
  ) +
  
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    text_cex = 0.7
  ) +
  
  coord_sf(
    xlim = c(-121.99, -121.88),
    ylim = c(36.53, 36.64),
    expand = FALSE
  ) +
  
  labs(
    x = NULL,
    y = NULL,
    tag = "B",
    title = "2025"
  ) +
  
  theme_bw() +
  my_theme +
  
  theme(
    plot.tag = element_text(face = "plain", color = "black"),
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 7, color = "black"),
    axis.title = element_blank()
  )

B

################################################################################
# Step 16 - Panel C: retained successful urchin dives relative to nearest kelp patch

C <- ggplot(
  all_urchin_dive_dist_sum,
  aes(x = dist_bin, y = n_dives)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 1,
    color = "black"
  ) +
  geom_line(
    linewidth = 1.1,
    color = "black"
  ) +
  geom_point(
    size = 2,
    color = "black"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Distance to nearest kelp patch edge (m)",
    y = "Number of successful urchin dives",
    tag = "C"
  ) +
  theme_bw() +
  base_theme +
  theme(
    plot.tag = element_text(face = "plain", color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

C

################################################################################
# Step 17 - combine final figure

top_row <- ggarrange(
  A,
  B,
  ncol = 2,
  nrow = 1,
  widths = c(1, 1),
  align = "hv"
)

g_foraging_final <- ggarrange(
  top_row,
  C,
  ncol = 1,
  nrow = 2,
  heights = c(1, 0.75),
  align = "v"
)

g_foraging_final

################################################################################
# Step 18 - save final figure

ggsave(
  here("figures", "Fig2_foraging_dives_map_2024_2025.png"),
  g_foraging_final,
  width = 10,
  height = 8,
  dpi = 600,
  bg = "white"
)