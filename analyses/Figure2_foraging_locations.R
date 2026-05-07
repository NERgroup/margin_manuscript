#jogsmith@ucsc.edu

rm(list = ls())

################################################################################
#Load packages and set directories

librarian::shelf(
  tidyverse, sf, readr, here, fs, glue, janitor,
  ggplot2, ggspatial, rnaturalearth, rnaturalearthdata,
  patchwork, grid, scales
)

datin <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database/processed"
datadir <- "/Volumes/enhydra/data/kelp_recovery/"

fig_dir <- here("figures")
dir_create(fig_dir)

################################################################################
#Step 1 - load data

planet_dat <- st_read(
  here("output", "planet_dat", "processed",
       "planet_kelp_presence_monterey_2024_09.gpkg"),
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
#Step 2 - themes and colors

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

zone_cols <- c(
  "outside" = "#7570B3",
  "margin" = "#D95F02",
  "forest" = "forestgreen"
)

################################################################################
#Step 3 - collapse to one row per site

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
#Step 4 - function to create 20 m x 80 m transect swath rectangles

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
#Step 5 - project to meters and build swaths

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
#Step 6 - build kelp forest, margin, and benthic site buffer layers

kelp_utm <- planet_dat %>%
  st_transform(32610) %>%
  st_make_valid()

kelp_forest <- kelp_utm %>%
  summarise() %>%
  st_union() %>%
  st_as_sf() %>%
  st_make_valid() %>%
  mutate(kelp_layer = "forest")

kelp_margin <- kelp_forest %>%
  st_boundary() %>%
  st_buffer(80) %>%
  st_as_sf() %>%
  st_make_valid() %>%
  mutate(kelp_layer = "margin")

kelp_edge <- kelp_forest %>%
  st_boundary() %>%
  st_as_sf() %>%
  st_make_valid()

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
#Step 7 - extract all successful urchin prey dives in July-October 2024-2025

focal_dives <- forage_orig %>%
  mutate(
    prey = str_to_lower(str_trim(prey)),
    date = as.Date(date),
    year = year(date),
    month = month(date)
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
#Step 8 - identify focal bouts with >= 3 successful urchin prey dives

focal_bouts <- focal_dives %>%
  group_by(bout) %>%
  summarise(
    n_successful_focal_dives = n(),
    prey_types = paste(sort(unique(prey)), collapse = ", "),
    geometry = st_centroid(st_union(geometry)),
    .groups = "drop"
  ) %>%
  mutate(
    focal_patch = if_else(n_successful_focal_dives >= 3, "yes", "no")
  ) %>%
  filter(focal_patch == "yes")

################################################################################
#Step 9 - retain focal bouts within 200 m of benthic transect centroids

focal_bouts_buffered <- focal_bouts %>%
  filter(
    lengths(st_intersects(geometry, benthic_site_buffer_union)) > 0
  )

################################################################################
#Step 10 - classify buffered focal bouts relative to kelp canopy

focal_bouts_zone <- focal_bouts_buffered %>%
  mutate(
    in_margin = lengths(st_intersects(geometry, kelp_margin)) > 0,
    in_forest = lengths(st_intersects(geometry, kelp_forest)) > 0,
    kelp_zone = case_when(
      in_forest ~ "forest",
      in_margin ~ "margin",
      TRUE ~ "outside"
    ),
    kelp_zone = factor(
      kelp_zone,
      levels = c("outside", "margin", "forest")
    )
  )

################################################################################
#Step 11 - summarize proportion of focal bouts by canopy zone

focal_prop <- focal_bouts_zone %>%
  st_drop_geometry() %>%
  count(kelp_zone, name = "n_bouts") %>%
  complete(
    kelp_zone = factor(
      c("outside", "margin", "forest"),
      levels = c("outside", "margin", "forest")
    ),
    fill = list(n_bouts = 0)
  ) %>%
  mutate(
    prop_bouts = n_bouts / sum(n_bouts)
  )

################################################################################
#Step 12 - calculate distance of all successful urchin dives to nearest kelp patch edge

all_urchin_dives_margin_dist <- focal_dives %>%
  mutate(
    in_forest = lengths(st_intersects(geometry, kelp_forest)) > 0,
    dist_to_nearest_kelp_edge_m = as.numeric(st_distance(geometry, kelp_edge)),
    signed_dist_to_nearest_kelp_edge_m = case_when(
      in_forest ~ dist_to_nearest_kelp_edge_m,
      TRUE ~ -dist_to_nearest_kelp_edge_m
    )
  )

################################################################################
#Step 13 - bin all successful urchin dives by 20 m distance bins

all_urchin_dive_dist_sum <- all_urchin_dives_margin_dist %>%
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
#Step 14 - Panel A: map of all successful urchin foraging dives

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


p_map_dives <- ggplot() +
  
  geom_sf(
    data = planet_dat,
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
    data = focal_dives %>% st_transform(4326),
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
    tag = "A"
  ) +
  
  theme_bw() +
  my_theme +
  
  theme(
    plot.tag = element_text(face = "plain", color = "black"),
    axis.text = element_text(size = 7, color = "black"),
    axis.title = element_blank()
  )

################################################################################
#Step 15 - Panel B: barplot of focal bouts by canopy zone

p_focal_prop <- ggplot(focal_prop, aes(x = kelp_zone, y = prop_bouts, fill = kelp_zone)) +
  geom_col(
    width = 0.68,
    color = "black",
    linewidth = 0.3
  ) +
  geom_text(
    aes(label = paste0("n = ", n_bouts)),
    vjust = -0.45,
    size = 3.2
  ) +
  scale_fill_manual(values = zone_cols) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, max(focal_prop$prop_bouts, na.rm = TRUE) * 1.18),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = NULL,
    y = "Proportion of focal bouts",
    tag = "B"
  ) +
  theme_bw() +
  base_theme +
  theme(
    plot.tag = element_text(face = "plain", color = "black"),
    axis.text.x = element_text(size = 10, color = "black"),
    axis.text.y = element_text(size = 10, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),
    legend.position = "none"
  )

################################################################################
#Step 16 - Panel C: all successful urchin dives relative to nearest kelp patch

p_all_urchin_dives_margin <- ggplot(
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

################################################################################
#Step 17 - combine final figure

right_column <- p_focal_prop / p_all_urchin_dives_margin +
  plot_layout(heights = c(1, 1))

g_foraging_final <- p_map_dives | right_column +
  plot_layout(widths = c(1.05, 0.95)) &
  theme(
    plot.margin = margin(1, 1, 1, 1)
  )

g_foraging_final

################################################################################
#Step 18 - save final figure

ggsave(
  here("figures", "Fig2_foraging_dives_map.png"),
  g_foraging_final,
  width = 11,
  height = 6,
  dpi = 600,
  bg = "white"
)