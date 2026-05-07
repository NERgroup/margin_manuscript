#jogsmith@ucsc.edu

rm(list = ls())

################################################################################
#Load packages and set directories

librarian::shelf(
  tidyverse, sf, readr, here, fs, glue, janitor,
  ggplot2, ggspatial, rnaturalearth, rnaturalearthdata,
  patchwork, grid
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

usa <- ne_states(country = "United States of America", returnclass = "sf")
foreign <- ne_countries(country = c("Canada", "Mexico"), returnclass = "sf")

################################################################################
#Step 2 - collapse to one row per site

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
#Step 3 - function to create 20 m x 80 m transect swath rectangles

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
#Step 4 - project to meters and build swaths

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
#Step 5 - prep focal site

focal_site <- "MAR_06"

focal_pt <- site_pts %>%
  filter(site_name_2025 == focal_site)

focal_swath <- transect_swaths %>%
  filter(site_name_2025 == focal_site)

focal_bbox <- focal_pt %>%
  st_transform(32610) %>%
  st_buffer(230) %>%
  st_transform(4326) %>%
  st_bbox()

focal_bbox["xmin"] <- focal_bbox["xmin"] - 0.0006
focal_bbox["xmax"] <- focal_bbox["xmax"] - 0.0006
focal_bbox["ymin"] <- focal_bbox["ymin"] - 0.0005
focal_bbox["ymax"] <- focal_bbox["ymax"] - 0.0005

################################################################################
#Step 6 - theme

my_theme <- theme(
  axis.text = element_text(size = 7),
  axis.title = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.line = element_line(colour = "black"),
  legend.position = "none",
  panel.background = element_rect(fill = "white")
)

################################################################################
#Step 7 - build kelp forest, margin, and benthic site buffer layers

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
#Step 8 - extract successful urchin prey dives in July-October 2024-2025

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
#Step 9 - identify focal bouts with >= 3 successful urchin prey dives

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
#Step 10 - retain focal bouts within 200 m of benthic transect centroids

focal_bouts_buffered <- focal_bouts %>%
  filter(
    lengths(st_intersects(geometry, benthic_site_buffer_union)) > 0
  )

################################################################################
#Step 11 - classify buffered focal bouts relative to kelp canopy

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
#Step 12 - summarize proportion of focal bouts by canopy zone

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

focal_prop

################################################################################
#Step 13 - plot focal bout proportions

zone_cols <- c(
  "outside" = "grey75",
  "margin" = "#D95F02",
  "forest" = "#1B9E77"
)

g_focal_prop <- ggplot(focal_prop, aes(x = kelp_zone, y = prop_bouts, fill = kelp_zone)) +
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
    y = "Proportion of focal bouts"
  ) +
  theme_bw() +
  my_theme +
  theme(
    axis.text.x = element_text(size = 9, color = "black"),
    axis.text.y = element_text(size = 9, color = "black"),
    axis.title.y = element_text(size = 10, color = "black"),
    axis.line = element_line(color = "black"),
    legend.position = "none"
  )

g_focal_prop

################################################################################
#Step 14 - California inset map

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
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    plot.background = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

ca_inset_grob <- ggplotGrob(ca_inset)

################################################################################
#Step 15 - MAR_06 popout map

popout_map <- ggplot() +
  geom_sf(
    data = planet_dat,
    fill = "#1B9E77",
    color = NA,
    alpha = 0.75
  ) +
  geom_sf(
    data = focal_swath,
    fill = "yellow",
    color = "black",
    linewidth = 0.8,
    alpha = 0.6
  ) +
  geom_sf(
    data = focal_pt,
    color = "red",
    fill = "red",
    shape = 21,
    size = 1.5
  ) +
  geom_sf(
    data = ca_counties,
    color = "grey70",
    fill = "grey85",
    linewidth = 0.2
  ) +
  annotation_scale(
    location = "br",
    width_hint = 0.35,
    text_cex = 0.7
  ) +
  coord_sf(
    xlim = c(focal_bbox["xmin"], focal_bbox["xmax"]),
    ylim = c(focal_bbox["ymin"], focal_bbox["ymax"]),
    expand = FALSE
  ) +
  theme_bw() +
  my_theme +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    plot.margin = margin(0, 0, 0, 0)
  )

################################################################################
#Step 16 - left map: benthic site map

benthic_map_base <- ggplot() +
  geom_sf(
    data = planet_dat,
    fill = "#1B9E77",
    color = NA,
    alpha = 0.65
  ) +
  geom_sf(
    data = transect_swaths,
    fill = "yellow",
    color = "black",
    linewidth = 0.25,
    alpha = 0.55
  ) +
  geom_sf(
    data = site_pts,
    color = "black",
    fill = "yellow",
    shape = 21,
    size = 1.5
  ) +
  geom_sf(
    data = focal_pt,
    color = "red",
    fill = "red",
    shape = 21,
    size = 2.2
  ) +
  geom_sf(
    data = ca_counties,
    color = "grey70",
    fill = "grey85",
    linewidth = 0.2
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
  theme_bw() +
  my_theme +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0,
      margin = margin(b = 2)
    )
  ) + labs(tag = "A")

benthic_map <- benthic_map_base +
  inset_element(
    popout_map,
    left = 0.01,
    bottom = 0.39,
    right = 0.56,
    top = 0.99
  )

################################################################################
#Step 17 - right map: focal bouts within 200 m of benthic sites

focal_bout_map <- ggplot() +
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
    data = benthic_site_buffers %>% st_transform(4326),
    fill = NA,
    color = "black",
    linetype = "dashed",
    linewidth = 0.25
  ) +
  geom_sf(
    data = focal_bouts_zone %>% st_transform(4326),
    aes(fill = kelp_zone),
    color = "black",
    shape = 21,
    size = 2.5,
    alpha = 0.9
  ) +
  scale_fill_manual(values = zone_cols, name = "Bout location") +
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
    tag = "B"
  ) +
  theme_bw() +
  my_theme +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0,
      margin = margin(b = 2)
    ),
    legend.position = "right",
    axis.text.x = element_text(size = 7, color = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_blank()
  )

################################################################################
#Step 18 - combine maps

g_maps <- benthic_map + focal_bout_map +
  plot_layout(widths = c(1, 1))

g_maps

################################################################################
#Step 19 - save two-panel map figure

ggsave(
  here("figures", "Fig1_benthic_sites_and_focal_urchin_bouts.png"),
  g_maps,
  width = 10,
  height = 6,
  dpi = 600,
  bg = "white"
)

################################################################################
#Step 20 - save focal bout proportion plot

ggsave(
  here("figures", "focal_urchin_bouts_kelp_canopy_prop.png"),
  g_focal_prop,
  width = 5,
  height = 3.5,
  dpi = 600,
  bg = "white"
)