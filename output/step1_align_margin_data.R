# jogsmith@ucsc.edu

rm(list = ls())

################################################################################
# About
# process margin data
# Align each transect by the first encounter of stipitate macroalgae so that:
#   0  = first kelp / first stipitate macroalgae encounter (margin)
#   <0 = barren side of margin
#   >0 = forest side of margin
#
################################################################################

# Load packages and set directories

librarian::shelf(
  tidyverse
)

# set dir
datin <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database"
datout <- here::here("output","margin_data","processed")

# load data
margin_dat_raw <- read_csv(file.path(datin, "processed/margin_combined_datav2.csv"))

glimpse(margin_dat_raw)

################################################################################
# Basic cleaning + define variables
################################################################################

margin_dat <- margin_dat_raw %>%
  mutate(
    date = as.Date(date)
  )

################################################################################
# Define the margin
#
# Margin is the first segment where ANY of the following indicate stipitate
# macroalgae:
#   - n_macro_plants > 0
#   - den_nereocystis > 0
#   - den_laminaria_setchellii > 0
#   - den_pterygophora > 0
#
# Notes:
# - We treat NA values as FALSE for the purpose of defining presence
# - Each transect is treated independently
################################################################################

margin_dat <- margin_dat %>%
  mutate(
    macro_present = case_when(
      coalesce(n_macro_plants, 0) > 0 ~ TRUE,
      coalesce(den_nereocystis, 0) > 0 ~ TRUE,
      coalesce(den_laminaria_setchellii, 0) > 0 ~ TRUE,
      coalesce(den_pterygophora, 0) > 0 ~ TRUE,
      TRUE ~ FALSE
    )
  )

################################################################################
# Find the first margin segment within each independent transect
#
# Grouping keys:
#   site + date + transect
#
# If a transect never encounters macroalgae, margin_segment will be NA
################################################################################

margin_dat <- margin_dat %>%
  group_by(site, date, transect) %>%
  arrange(segment, .by_group = TRUE) %>%
  mutate(
    margin_segment = if_else(
      any(macro_present, na.rm = TRUE),
      min(segment[macro_present], na.rm = TRUE),
      NA_real_
    )
  ) %>%
  ungroup()

################################################################################
# Align transects relative to the margin
#
# dist_from_margin:
#   negative = barren side
#   zero     = first macroalgae encounter
#   positive = forest side
################################################################################

margin_dat <- margin_dat %>%
  mutate(
    dist_from_margin = segment - margin_segment
  )

################################################################################
# Calculate response variables
################################################################################

margin_dat <- margin_dat %>%
  mutate(
    prop_concealed = case_when(
      is.na(den_purple_urchin) ~ NA_real_,
      den_purple_urchin <= 0 ~ NA_real_,
      TRUE ~ den_purple_conceiled / den_purple_urchin
    ),
    prop_concealed = case_when(
      is.na(prop_concealed) ~ NA_real_,
      prop_concealed < 0 ~ 0,
      prop_concealed > 1 ~ 1,
      TRUE ~ prop_concealed
    )
  )

################################################################################
# Quick checks
################################################################################

transect_check <- margin_dat %>%
  group_by(site, date, transect) %>%
  summarize(
    has_margin = any(macro_present, na.rm = TRUE),
    has_barren = any(!macro_present, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    crosses_margin = has_margin & has_barren
  )

transect_check %>% count(crosses_margin)

transects_keep <- transect_check %>%
  filter(crosses_margin)

transects_drop <- transect_check %>%
  filter(!crosses_margin)

print(transects_drop)

################################################################################
# Keep only transects that actually crossed a margin
################################################################################

margin_crossers <- margin_dat %>%
  semi_join(transects_keep, by = c("site", "date", "transect"))

################################################################################
# Trim to a common window around the margin for cleaner comparison
################################################################################

plot_window <- 40

margin_plot_dat <- margin_crossers %>%
  filter(
    dist_from_margin >= -plot_window,
    dist_from_margin <=  plot_window
  )

################################################################################
# Create a transect ID for plotting
################################################################################

margin_plot_dat <- margin_plot_dat %>%
  mutate(
    transect_id = str_c(site, date, "T", transect, sep = "_")
  )


write_csv(margin_plot_dat, file.path(datout,"margin_data.csv"))

