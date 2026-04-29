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
#Step 1 - load processed Planet kelp presence data

planet_dat <- st_read(
  here("output", "planet_dat", "processed",
       "planet_kelp_presence_monterey_2024_09.gpkg"),
  quiet = FALSE
) %>%
  clean_names() %>%
  st_transform(4326)

################################################################################
#Step 2 - load margin site table

sitetab <- read_csv(
  file.path(datin, "site_tables", "margin_site_table.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

################################################################################
#Step 3 - load land data

ca_counties <- st_read(
  file.path(datadir, "gis_data/raw/ca_county_boundaries/s7vc7n.shp"),
  quiet = TRUE
) %>%
  st_transform(4326)

################################################################################
#Step 4 - collapse to one row per site

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
#Step 5 - function to create 20 m x 80 m transect swath rectangles

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
#Step 6 - project to meters and build swaths

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
#Step 7 - prep focal site

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

# nudge bbox slightly left and down
focal_bbox["xmin"] <- focal_bbox["xmin"] - 0.0006
focal_bbox["xmax"] <- focal_bbox["xmax"] - 0.0006
focal_bbox["ymin"] <- focal_bbox["ymin"] - 0.0005
focal_bbox["ymax"] <- focal_bbox["ymax"] - 0.0005

################################################################################
#Step 8 - theme

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
#Step 9 - California inset map

usa <- ne_states(country = "United States of America", returnclass = "sf")
foreign <- ne_countries(country = c("Canada", "Mexico"), returnclass = "sf")

ca_inset <- ggplot() +
  geom_sf(data = foreign, fill = "grey85", color = "white", linewidth = 0.2) +
  geom_sf(data = usa, fill = "grey85", color = "white", linewidth = 0.2) +
  #annotate(
  #  "rect",
  #  xmin = -122.05,
  #  xmax = -121.85,
  #  ymin = 36.50,
  #  ymax = 36.66,
  #  color = "black",
  #  fill = NA,
  #  linewidth = 0.7
  #) +
  annotate("rect", xmin=-122.6, xmax=-121, ymin=36.2, ymax=37.1, color="black", fill=NA, lwd=0.8) +
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
#Step 10 - main Monterey Peninsula map

main_map <- ggplot() +
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
    xmin = -121.997,
    xmax = -121.960,
    ymin = 36.610,
    ymax = 36.640
  )+
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
  my_theme

################################################################################
#Step 11 - MAR_06 popout map

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
    size = 2.5
  ) +
  geom_sf(
    data = ca_counties,
    color = "grey70",
    fill = "grey85",
    linewidth = 0.2
  ) +
  #annotation_north_arrow(
  #  location = "tr",
  #  which_north = "true",
  #  height = unit(0.4, "cm"),
  #  width = unit(0.4, "cm"),
  #  style = north_arrow_orienteering(text_col = NA)
  #) +
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
    plot.margin = margin(0,0,0,0)
  )

################################################################################
#Step 12 - combine maps with slight overlap

g <- main_map +
  inset_element(
    popout_map,
    left = 0.48,
    bottom = -0.05,
    right = 0.98,
    top = 1.09
  )

g

################################################################################
#Step 13 - save

ggsave(
  here("figures", "Fig1_map_timeseries_fig.png"),
  g,
  width = 8,
  height = 4.5,
  dpi = 600,
  bg = "white"
)
