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

margin_dat_raw <- read_csv(
  here("output", "margin_data", "Processed", "margin_data.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

usa <- ne_states(country = "United States of America", returnclass = "sf")
foreign <- ne_countries(country = c("Canada", "Mexico"), returnclass = "sf")

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

map_tag_theme <- theme(
  plot.margin = margin(2, 2, 2, 2),
  plot.tag = element_text(face = "plain", color = "black")
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
#Step 6 - prep focal site

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
#Step 7 - process margin data

margin_plot_dat <- margin_dat_raw %>%
  mutate(
    margin_zone = case_when(
      dist_from_margin < 0 ~ "Barren side",
      dist_from_margin == 0 ~ "Margin",
      dist_from_margin > 0 ~ "Forest side",
      TRUE ~ NA_character_
    ),
    margin_zone = factor(
      margin_zone,
      levels = c("Barren side", "Margin", "Forest side")
    ),
    macro_density =
      coalesce(den_nereocystis, 0) +
      coalesce(den_laminaria_setchellii, 0) +
      coalesce(den_pterygophora, 0)
  )

behavior_sum <- margin_plot_dat %>%
  group_by(dist_from_margin) %>%
  summarize(
    mean = mean(prop_concealed, na.rm = TRUE),
    sd = sd(prop_concealed, na.rm = TRUE),
    n = sum(!is.na(prop_concealed)),
    se = sd / sqrt(n),
    metric = "Behavior",
    .groups = "drop"
  ) %>%
  filter(n > 0)

urchin_sum <- margin_plot_dat %>%
  group_by(dist_from_margin) %>%
  summarize(
    mean = mean(den_purple_urchin, na.rm = TRUE),
    sd = sd(den_purple_urchin, na.rm = TRUE),
    n = sum(!is.na(den_purple_urchin)),
    se = sd / sqrt(n),
    metric = "Urchin density",
    .groups = "drop"
  ) %>%
  filter(n > 0)

macro_sum <- margin_plot_dat %>%
  group_by(dist_from_margin) %>%
  summarize(
    mean = mean(macro_density, na.rm = TRUE),
    sd = sd(macro_density, na.rm = TRUE),
    n = sum(!is.na(macro_density)),
    se = sd / sqrt(n),
    metric = "Macroalgal density",
    .groups = "drop"
  ) %>%
  filter(n > 0)

gonad_sum <- margin_plot_dat %>%
  group_by(dist_from_margin) %>%
  summarize(
    mean = mean(mean_gonad_index, na.rm = TRUE),
    sd = sd(mean_gonad_index, na.rm = TRUE),
    n = sum(!is.na(mean_gonad_index)),
    se = sd / sqrt(n),
    metric = "Gonad index",
    .groups = "drop"
  ) %>%
  filter(n > 0)

combo_long <- bind_rows(
  behavior_sum %>%
    transmute(
      dist_from_margin,
      mean_plot = mean,
      se_plot = se,
      metric = "Behavior"
    ),
  urchin_sum %>%
    transmute(
      dist_from_margin,
      mean_plot = mean,
      se_plot = se,
      metric = "Urchin density"
    ),
  macro_sum %>%
    transmute(
      dist_from_margin,
      mean_plot = log1p(mean),
      se_plot = log1p(mean + se) - log1p(mean),
      metric = "Macroalgal density"
    ),
  gonad_sum %>%
    transmute(
      dist_from_margin,
      mean_plot = mean,
      se_plot = se,
      metric = "Gonad index"
    )
) %>%
  mutate(
    metric = factor(
      metric,
      levels = c(
        "Macroalgal density",
        "Behavior",
        "Urchin density",
        "Gonad index"
      )
    )
  )

################################################################################
#Step 8 - Panel B: faceted margin-transition figure

p_faceted_margin <- ggplot(
  combo_long,
  aes(
    x = dist_from_margin,
    y = mean_plot,
    color = metric,
    fill = metric
  )
) +
  
  annotate(
    "rect",
    xmin = -Inf,
    xmax = 0,
    ymin = -Inf,
    ymax = Inf,
    fill = "#7570B3",
    alpha = 0.2
  ) +
  
  annotate(
    "rect",
    xmin = 0,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    fill = "#1B9E77",
    alpha = 0.2
  ) +
  
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 1,
    color = "black"
  ) +
  
  geom_ribbon(
    aes(
      ymin = mean_plot - se_plot,
      ymax = mean_plot + se_plot
    ),
    alpha = 0.18,
    color = NA
  ) +
  
  geom_line(linewidth = 1.1) +
  
  geom_point(size = 1.7) +
  
  scale_color_manual(
    values = c(
      "Behavior" = "#333333",
      "Urchin density" = "#7570B3",
      "Macroalgal density" = "#1B9E77",
      "Gonad index" = "#1F78B4"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Behavior" = "#333333",
      "Urchin density" = "#7570B3",
      "Macroalgal density" = "#1B9E77",
      "Gonad index" = "#1F78B4"
    )
  ) +
  
  facet_wrap(
    ~ metric,
    ncol = 1,
    scales = "free_y",
    labeller = labeller(
      metric = c(
        "Macroalgal density" =
          "Macroalgal density [log(x + 1)\n(no. stipe per 10m²)]",
        "Behavior" =
          "Proportion concealed",
        "Urchin density" =
          "Purple urchin density\n(no. indiv. per 10m²)",
        "Gonad index" =
          "Gonad index"
      )
    )
  ) +
  
  labs(
    x = "Distance from macroalgal margin (m)",
    y = NULL,
    color = NULL,
    fill = NULL,
    tag = "B"
  ) +
  
  theme_bw() +
  base_theme +
  
  theme(
    legend.position = "none",
    strip.text = element_text(size = 10, face = "bold"),
    panel.grid.minor = element_blank(),
    plot.tag = element_text(face = "plain", color = "black"),
    plot.margin = margin(2, 2, 2, 2)
  )

p_faceted_margin

################################################################################
#Step 9 - California inset map

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
#Step 10 - MAR_06 popout map

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
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.6
    ),
    plot.margin = margin(0, 0, 0, 0)
  )

################################################################################
#Step 11 - Panel A: benthic survey map

benthic_map_base <- ggplot() +
  geom_sf(
    data = planet_dat,
    fill = "#1B9E77",
    color = NA,
    alpha = 0.65,
    show.legend = FALSE
  ) +
  geom_sf(
    data = transect_swaths,
    fill = "yellow",
    color = "black",
    linewidth = 0.25,
    alpha = 0.55,
    show.legend = FALSE
  ) +
  geom_sf(
    data = site_pts,
    aes(fill = "Survey location"),
    color = "black",
    shape = 21,
    size = 3
  ) +
  geom_sf(
    data = focal_pt,
    color = "red",
    fill = "red",
    shape = 21,
    size = 2.2,
    show.legend = FALSE
  ) +
  geom_sf(
    data = ca_counties,
    color = "grey70",
    fill = "grey85",
    linewidth = 0.2,
    show.legend = FALSE
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
  scale_fill_manual(
    values = c("Survey location" = "yellow"),
    name = NULL
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(
        shape = 21,
        color = "black",
        size = 3
      )
    )
  ) +
  coord_sf(
    xlim = c(-121.99, -121.88),
    ylim = c(36.53, 36.64),
    expand = FALSE
  ) +
  labs(tag = "A") +
  theme_bw() +
  my_theme +
  map_tag_theme +
  theme(
    legend.position = c(0.82, 0.16),
    legend.background = element_rect(
      fill = alpha("white", 0.75),
      color = NA
    ),
    legend.key = element_blank(),
    legend.text = element_text(
      size = 8,
      color = "black"
    ),
    axis.text.x = element_blank()
  )

benthic_map <- benthic_map_base +
  inset_element(
    popout_map,
    left = 0.01,
    bottom = 0.39,
    right = 0.56,
    top = 0.99
  )

################################################################################
#Step 12 - combine panels

g_final <- benthic_map | p_faceted_margin +
  plot_layout(widths = c(1, 0.92)) &
  theme(
    plot.margin = margin(0.5, 0.5, 0.5, 0.5)
  )

g_final

################################################################################
#Step 13 - save figure

ggsave(
  here("figures", "Fig1_map_and_margin_gradients.png"),
  g_final,
  width = 11,
  height = 7,
  dpi = 600,
  bg = "white"
)
