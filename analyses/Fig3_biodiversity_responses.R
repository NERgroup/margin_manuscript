#jogsmith@ucsc.edu

rm(list = ls())

################################################################################
#Load packages and set directories

librarian::shelf(
  tidyverse, here, readr, janitor, scales, grid, vegan, patchwork
)

datin <- here::here("output")
fig_dir <- here::here("figures")

dir_create(fig_dir)

################################################################################
#Step 1 - load margin data

margin_dat_raw <- read_csv(
  file.path(datin, "margin_data", "Processed", "margin_data.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

################################################################################
#Step 2 - theme

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
  legend.position = "none",
  strip.background = element_blank(),
  strip.text = element_text(size = 11, face = "bold", color = "black")
)

################################################################################
#Step 3 - identify UPC columns

upc_names <- names(margin_dat_raw)[str_detect(names(margin_dat_raw), "^upc_")]

################################################################################
#Step 4 - aggregate community matrix by margin distance

comm_margin <- margin_dat_raw %>%
  select(dist_from_margin, all_of(upc_names)) %>%
  mutate(
    across(all_of(upc_names), ~ replace_na(.x, 0))
  ) %>%
  group_by(dist_from_margin) %>%
  summarise(
    across(all_of(upc_names), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(dist_from_margin)

comm_mat <- comm_margin %>%
  select(all_of(upc_names)) %>%
  as.data.frame()

rownames(comm_mat) <- comm_margin$dist_from_margin

################################################################################
#Step 5 - calculate biodiversity metrics

biodiv_margin <- comm_margin %>%
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
  select(dist_from_margin, shannon, richness, evenness)

################################################################################
#Step 6 - calculate Bray-Curtis dissimilarity from margin community

margin_reference <- comm_margin %>%
  filter(dist_from_margin == 0) %>%
  select(all_of(upc_names)) %>%
  as.numeric()

bray_from_margin <- comm_margin %>%
  select(dist_from_margin, all_of(upc_names)) %>%
  rowwise() %>%
  mutate(
    bray_curtis_from_margin = as.numeric(
      vegdist(
        rbind(
          margin_reference,
          c_across(all_of(upc_names))
        ),
        method = "bray"
      )
    )[1]
  ) %>%
  ungroup() %>%
  select(dist_from_margin, bray_curtis_from_margin)

biodiv_margin <- biodiv_margin %>%
  left_join(bray_from_margin, by = "dist_from_margin")

################################################################################
#Step 7 - make long plotting table

biodiv_long <- biodiv_margin %>%
  pivot_longer(
    cols = c(
      shannon,
      richness,
      evenness,
      bray_curtis_from_margin
    ),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = recode(
      metric,
      shannon = "Shannon diversity",
      richness = "Taxonomic richness",
      evenness = "Pielou's evenness",
      bray_curtis_from_margin = "Community dissimilarity\nfrom margin"
    ),
    metric = factor(
      metric,
      levels = c(
        "Shannon diversity",
        "Taxonomic richness",
        "Pielou's evenness",
        "Community dissimilarity\nfrom margin"
      )
    )
  )

################################################################################
#Step 8 - plot biodiversity responses across margin

p_biodiversity_margin <- ggplot(
  biodiv_long,
  aes(x = dist_from_margin, y = value)
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
    linewidth = 0.8,
    color = "black"
  ) +
  geom_line(
    linewidth = 1.15,
    color = "black"
  ) +
  geom_point(
    size = 2,
    color = "black"
  ) +
  facet_wrap(
    ~ metric,
    ncol = 1,
    scales = "free_y"
  ) +
  labs(
    x = "Distance from macroalgal margin (m)",
    y = NULL
  ) +
  theme_bw() +
  base_theme +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_biodiversity_margin

################################################################################
#Step 9 - save biodiversity figure

ggsave(
  filename = file.path(fig_dir, "Fig3_biodiversity_responses.png"),
  plot = p_biodiversity_margin,
  width = 4,
  height = 7,
  dpi = 600,
  bg = "white"
)
