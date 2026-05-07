#jogsmith@ucsc.edu

rm(list = ls())

################################################################################
#Load packages and set directories

librarian::shelf(
  tidyverse, sf, readr, here, fs, janitor, vegan,
  ggplot2, ggspatial, rnaturalearth, rnaturalearthdata,
  patchwork, grid, scales, ggrepel
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

forage_orig <- read_csv(
  "/Volumes/enhydra/data/foraging_data/processed/foraging_data_2024_2025_processed.csv",
  show_col_types = FALSE
) %>%
  clean_names()

margin_dat_raw <- read_csv(
  here("output", "margin_data", "Processed", "margin_data.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

################################################################################
#Step 2 - theme and colors

base_theme <- theme(
  axis.text = element_text(size = 11, color = "black"),
  axis.text.y = element_text(angle = 90, hjust = 0.5, color = "black"),
  axis.title = element_text(size = 12, color = "black"),
  plot.tag = element_text(size = 12, color = "black"),
  plot.title = element_text(size = 12, face = "bold", color = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  legend.key = element_blank(),
  legend.background = element_rect(fill = alpha("blue", 0)),
  legend.text = element_text(size = 10, color = "black"),
  legend.title = element_text(size = 11, color = "black"),
  strip.background = element_blank(),
  strip.text = element_text(size = 10, face = "bold", color = "black")
)

state_cols <- c(
  "Barren side" = "#7570B3",
  "Margin" = "#D95F02",
  "Forest side" = "forestgreen"
)

################################################################################
#Step 3 - build kelp edge from 2024 canopy

kelp_utm <- planet_dat %>%
  st_transform(32610) %>%
  st_make_valid()

kelp_forest <- kelp_utm %>%
  summarise() %>%
  st_union() %>%
  st_as_sf() %>%
  st_make_valid() %>%
  mutate(kelp_layer = "forest")

kelp_edge <- kelp_forest %>%
  st_boundary() %>%
  st_as_sf() %>%
  st_make_valid()

################################################################################
#Step 4 - extract all successful urchin prey dives

urchin_dives <- forage_orig %>%
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
#Step 5 - calculate signed distance of dives to nearest kelp patch edge

urchin_dives_dist <- urchin_dives %>%
  mutate(
    in_forest = lengths(st_intersects(geometry, kelp_forest)) > 0,
    dist_to_nearest_kelp_edge_m = as.numeric(st_distance(geometry, kelp_edge)),
    signed_dist_to_nearest_kelp_edge_m = case_when(
      in_forest ~ dist_to_nearest_kelp_edge_m,
      TRUE ~ -dist_to_nearest_kelp_edge_m
    ),
    dist_bin_20m = round(signed_dist_to_nearest_kelp_edge_m / 20) * 20
  ) %>%
  st_drop_geometry() %>%
  filter(
    signed_dist_to_nearest_kelp_edge_m >= -500
  )

urchin_dive_bins <- urchin_dives_dist %>%
  count(dist_bin_20m, name = "n_urchin_dives")

################################################################################
#Step 6 - calculate biodiversity and urchin state across margin gradient

upc_names <- names(margin_dat_raw)[str_detect(names(margin_dat_raw), "^upc_")]

margin_state <- margin_dat_raw %>%
  mutate(
    across(all_of(upc_names), ~ replace_na(.x, 0)),
    prop_exposed = 1 - prop_concealed,
    margin_zone = case_when(
      dist_from_margin < 0 ~ "Barren side",
      dist_from_margin == 0 ~ "Margin",
      dist_from_margin > 0 ~ "Forest side",
      TRUE ~ NA_character_
    ),
    margin_zone = factor(
      margin_zone,
      levels = c("Barren side", "Margin", "Forest side")
    )
  ) %>%
  group_by(dist_from_margin, margin_zone) %>%
  summarize(
    mean_prop_exposed = mean(prop_exposed, na.rm = TRUE),
    mean_urchin_density_10m2 = mean(den_purple_urchin, na.rm = TRUE),
    mean_urchin_density_m2 = mean_urchin_density_10m2 / 10,
    across(all_of(upc_names), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    shannon = diversity(c_across(all_of(upc_names)), index = "shannon"),
    richness = sum(c_across(all_of(upc_names)) > 0),
    evenness = if_else(
      richness > 1,
      shannon / log(richness),
      NA_real_
    )
  ) %>%
  ungroup() %>%
  mutate(
    dist_bin_20m = round(dist_from_margin / 20) * 20
  )

################################################################################
#Step 7 - join otter urchin-foraging intensity to margin-gradient states

fig4_dat <- margin_state %>%
  left_join(urchin_dive_bins, by = "dist_bin_20m") %>%
  mutate(
    n_urchin_dives = replace_na(n_urchin_dives, 0),
    dive_size = n_urchin_dives + 1
  ) %>%
  arrange(dist_from_margin)

################################################################################
#Step 8 - focal labels

label_dat <- fig4_dat %>%
  filter(
    dist_from_margin %in% c(
      min(dist_from_margin, na.rm = TRUE),
      0,
      max(dist_from_margin, na.rm = TRUE)
    )
  ) %>%
  mutate(
    label = case_when(
      dist_from_margin < 0 ~ "Barren",
      dist_from_margin == 0 ~ "Margin",
      dist_from_margin > 0 ~ "Forest"
    )
  )

################################################################################
#Step 9 - synthesis state-space figure

p_fig4 <- ggplot(
  fig4_dat,
  aes(
    x = mean_prop_exposed,
    y = shannon
  )
) +
  stat_ellipse(
    aes(
      color = margin_zone,
      group = margin_zone
    ),
    type = "norm",
    linewidth = 0.7,
    alpha = 0.5,
    show.legend = FALSE
  ) +
  geom_path(
    aes(group = 1),
    linewidth = 0.9,
    color = "grey35",
    arrow = arrow(
      type = "closed",
      length = unit(0.12, "inches")
    )
  ) +
  geom_point(
    aes(
      fill = margin_zone,
      size = mean_urchin_density_m2
    ),
    shape = 21,
    color = "black",
    alpha = 0.90,
    stroke = 0.35
  ) +
  geom_text_repel(
    data = label_dat,
    aes(label = label),
    size = 3.5,
    color = "black",
    min.segment.length = 0,
    box.padding = 0.35,
    point.padding = 0.3
  ) +
  scale_fill_manual(
    values = state_cols,
    name = NULL
  ) +
  scale_color_manual(
    values = state_cols,
    guide = "none"
  ) +
  scale_size_continuous(
    name = expression(
      "Purple urchin density\n(indiv. per m²)"
    ),
    range = c(2.5, 8),
    breaks = pretty_breaks(n = 4)
  ) +
  labs(
    x = "Mean proportion of urchins exposed",
    y = "Shannon diversity"
  ) +
  theme_bw() +
  base_theme +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    axis.text.y = element_text(
      angle = 90,
      hjust = 0.5,
      color = "black"
    )
  )

p_fig4

################################################################################
#Step 10 - save figure

ggsave(
  filename = file.path(
    fig_dir,
    "Fig4_state_space.png"
  ),
  plot = p_fig4,
  width = 7.5,
  height = 5.5,
  dpi = 600,
  bg = "white"
)
