# jogsmith@ucsc.edu

rm(list = ls())

################################################################################
# About
# plot margin data
# Then explore ecology across the margin
################################################################################

################################################################################
# Load packages and set directories
################################################################################

librarian::shelf(
  tidyverse,
  here,
  scales,
  grid
)

# set dir
datin <- here::here("output")
figdir <- here::here("figures")

# load data
margin_dat_raw <- read_csv(file.path(datin, "margin_data", "Processed", "margin_data.csv"))

glimpse(margin_dat_raw)

################################################################################
# Create plotting dataset


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

################################################################################
# Plot 1: Raw transect-level trajectories of proportion concealed
################################################################################

base_theme <- theme(
  axis.text = element_text(size = 12, color = "black"),
  axis.text.y = element_text(angle = 90, hjust = 0.5, color = "black"),
  axis.title = element_text(size = 12, color = "black"),
  plot.tag = element_text(size = 10, color = "black"),
  plot.title = element_text(size = 12, face = "bold", color = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_blank(),
  axis.line = element_line(colour = "black"),
  legend.key = element_blank(),
  legend.background = element_rect(fill = alpha("blue", 0)),
  legend.key.height = unit(1, "lines"),
  legend.text = element_text(size = 10, color = "black"),
  legend.title = element_text(size = 11, color = "black"),
  strip.background = element_blank(),
  strip.text = element_text(size = 10, face = "bold", color = "black")
)


p_prop_raw <- ggplot(
  margin_plot_dat,
  aes(
    x = dist_from_margin,
    y = prop_concealed,
    group = transect_id
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.8,
    color = "firebrick"
  ) +
  geom_line(alpha = 0.30, linewidth = 0.7, color = "gray35") +
  geom_point(alpha = 0.45, size = 1.8, color = "gray25") +
  labs(
    x = "Distance from margin (m)",
    y = "Proportion concealed",
    subtitle = "Each line is an independent transect aligned to first stipitate macroalgae encounter"
  ) +
  theme_bw() +
  base_theme

p_prop_raw


################################################################################
# Plot 2: Site-level raw trajectories
################################################################################

p_prop_raw_site <- ggplot(
  margin_plot_dat,
  aes(
    x = dist_from_margin,
    y = prop_concealed,
    group = transect_id
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.8,
    color = "firebrick"
  ) +
  geom_line(alpha = 0.35, linewidth = 0.7, color = "gray35") +
  geom_point(alpha = 0.45, size = 1.6, color = "gray25") +
  facet_wrap(~ site, scales = "fixed") +
  labs(
    x = "Distance from margin (m)",
    y = "Proportion concealed",
    title = "Transect-level concealment across margins by site"
  ) +
  theme_bw() +
  base_theme

p_prop_raw_site



ggsave(
  filename = file.path(figdir, "prop_concealed_raw_transects_by_site.png"),
  plot = p_prop_raw_site,
  width = 11,
  height = 8,
  dpi = 600
)

################################################################################
# Summarize mean concealment by aligned distance bin
################################################################################

prop_summary <- margin_plot_dat %>%
  group_by(dist_from_margin) %>%
  summarize(
    n = sum(!is.na(prop_concealed)),
    mean_prop_concealed = mean(prop_concealed, na.rm = TRUE),
    sd_prop_concealed = sd(prop_concealed, na.rm = TRUE),
    se_prop_concealed = sd_prop_concealed / sqrt(n),
    .groups = "drop"
  ) %>%
  filter(n > 0)

################################################################################
# Plot 3: Mean concealment across aligned distance
################################################################################

p_prop_mean <- ggplot(
  prop_summary,
  aes(x = dist_from_margin, y = mean_prop_concealed)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.8,
    color = "firebrick"
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_errorbar(
    aes(
      ymin = mean_prop_concealed - se_prop_concealed,
      ymax = mean_prop_concealed + se_prop_concealed
    ),
    width = 1.2
  ) +
  labs(
    x = "Distance from margin (m)",
    y = "Mean proportion concealed ± SE"
  ) +
  theme_bw() +
  base_theme

p_prop_mean


ggsave(
  filename = file.path(figdir, "prop_concealed_mean_by_distance.png"),
  plot = p_prop_mean,
  width = 8,
  height = 6,
  dpi = 600
)

################################################################################
# Plot 4: Smoothed trend for concealment
################################################################################

p_prop_smooth <- ggplot(
  margin_plot_dat,
  aes(x = dist_from_margin, y = prop_concealed)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.8,
    color = "firebrick"
  ) +
  geom_point(alpha = 0.25, size = 1.8) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.1) +
  labs(
    x = "Distance from margin (m)",
    y = "Proportion concealed",
    title = "Smoothed concealment transition across the margin"
  ) +
  theme_bw() +
  base_theme

p_prop_smooth

ggsave(
  filename = file.path(figdir, "prop_concealed_smooth_transition.png"),
  plot = p_prop_smooth,
  width = 8,
  height = 6,
  dpi = 600
)

################################################################################
# Summarize by margin zone
################################################################################

zone_summary <- margin_plot_dat %>%
  group_by(margin_zone) %>%
  summarize(
    n = sum(!is.na(prop_concealed)),
    mean_prop_concealed = mean(prop_concealed, na.rm = TRUE),
    sd_prop_concealed = sd(prop_concealed, na.rm = TRUE),
    se_prop_concealed = sd_prop_concealed / sqrt(n),
    .groups = "drop"
  )

print(zone_summary)

################################################################################
# Summarize each variable by aligned distance
################################################################################

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

################################################################################
# Faceted plot on original scales (4 panels, single column)
# Keep raw means + SE, but log-transform macroalgal density for plotting
################################################################################

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

p_faceted_margin <- ggplot(
  combo_long,
  aes(
    x = dist_from_margin,
    y = mean_plot,
    color = metric,
    fill = metric
  )
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
  geom_line(linewidth = 1.3) +
  geom_point(size = 2.0) +
  scale_color_manual(
    values = c(
      "Behavior" = "#333333",
      "Urchin density" = "#7570b3",
      "Macroalgal density" = "#1b9e77",
      "Gonad index" = "#1f78b4"
    )
  ) +
  scale_fill_manual(
    values = c(
      "Behavior" = "#333333",
      "Urchin density" = "#7570b3",
      "Macroalgal density" = "#1b9e77",
      "Gonad index" = "#1f78b4"
    )
  ) +
  facet_wrap(
    ~ metric,
    ncol = 1,
    scales = "free_y",
    labeller = labeller(
      metric = c(
        "Macroalgal density" = "Macroalgal density [log(x + 1) \n (no. stipe per 10m²)]",
        "Behavior" = "Proportion concealed",
        "Urchin density" = "Purple urchin density \n (no. indiv. per 10m²)",
        "Gonad index" = "Gonad index"
      )
    )
  ) +
  labs(
    x = "Distance from macroalgal margin (m)",
    y = NULL,
    color = NULL,
    fill = NULL
  ) +
  theme_bw() +
  base_theme +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )

p_faceted_margin


ggsave(
  filename = file.path(figdir, "margin_faceted_behavior_urchin_macro_gonad.png"),
  plot = p_faceted_margin,
  width = 5,
  height = 9,
  dpi = 600
)

################################################################################
# other metrics
################################################################################

# ==========================================
# AGGREGATE SHANNON DIVERSITY ACROSS MARGIN
# ==========================================

library(vegan)

# 1. identify UPC columns
upc_names <- names(margin_dat_raw)[str_detect(names(margin_dat_raw), "^upc_")]

# 2. aggregate UPC cover across all transects within each aligned margin position
shannon_margin <- margin_dat_raw %>%
  select(dist_from_margin, all_of(upc_names)) %>%
  mutate(across(all_of(upc_names), ~ replace_na(., 0))) %>%
  group_by(dist_from_margin) %>%
  summarise(
    across(all_of(upc_names), mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    shannon = diversity(c_across(all_of(upc_names)), index = "shannon")
  ) %>%
  ungroup()

# 3. plot one line
p_shannon_agg <- ggplot(
  shannon_margin,
  aes(x = dist_from_margin, y = shannon)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    linewidth = 0.8,
    color = "firebrick"
  ) +
  geom_line(linewidth = 1.2, color = "black") +
  geom_point(size = 2, color = "black") +
  labs(
    x = "Distance from macroalgal margin (m)",
    y = "Shannon diversity"
  ) +
  theme_bw() +
  base_theme

p_shannon_agg

ggsave(
  filename = file.path(figdir, "shannon_diversity_across_margin_aggregated.png"),
  plot = p_shannon_agg,
  width = 7,
  height = 5,
  dpi = 600
)
