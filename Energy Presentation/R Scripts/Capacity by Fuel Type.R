################################
# Create file-specific objects
################################
elec_source_lvls <- c(
  "Other",
  "Natural Gas",
  "Batteries",
  "Wind",
  "Solar"
)

color_values <- c(
  "black",
  "gray60",
  "forestgreen",
  "deepskyblue4",
  "goldenrod3"
)

########################################
# Import Historical Data from the EIA
########################################
import_file <- file.path(
  BASE_PATH,
  "Data - raw",
  "2026 01 Generator Report.xlsx"
)

# Custom function for importing generator data
wb_to_df_generator <- function(
  sheet_name = NULL
) {
  wb_to_df(import_file, sheet = sheet_name, start_row = 3) %>%
    clean_names() %>%
    mutate(across(ends_with("_capacity_mw"), ~ as.numeric(.x))) %>%
    # Location data unnecessary for analysis below
    select(-c(latitude, longitude))
}

# Import generator inventory data
operating <- wb_to_df_generator(sheet_name = "Operating")
planned <- wb_to_df_generator(sheet_name = "Planned")
retired <- wb_to_df_generator(sheet_name = "Retired")

#####################################
# Bind data from all status sheets
#####################################
generator_inventory <- bind_rows(
  "Operating" = operating,
  "Planned" = planned,
  "Retired" = retired,
  .id = "status"
)

# Create an "Other" category for smaller sources
x <- generator_inventory %>%
  filter(operating_year >= 2020 | planned_operation_year >= 2020) %>%
  mutate(
    elec_source = case_match(
      energy_source_code,
      "MWH" ~ "Batteries",
      "SUN" ~ "Solar",
      "WND" ~ "Wind",
      "NG" ~ "Natural Gas",
      .default = "Other"
    ),
    .keep = "unused"
  )

# Create an object for already installed capacity
installed_cap <- x %>%
  filter(status %in% c("Operating", "Retired")) %>%
  summarise(
    installed_additions = sum(net_summer_capacity_mw, na.rm = TRUE),
    .by = c(operating_year, elec_source)
  ) %>%
  rename(year = operating_year)

# Create an object for planned capacity
planned_cap <- x %>%
  filter(status == "Planned") %>%
  summarise(
    planned_additions = sum(net_summer_capacity_mw),
    .by = c(planned_operation_year, elec_source)
  ) %>%
  rename(year = planned_operation_year)

# Join installed and planned capacity
installed_planned_cap <- full_join(
  installed_cap,
  planned_cap,
  by = c("year", "elec_source")
) %>%
  # Create column for all additions (both installed and planned)
  mutate(
    planned_installed_additions = rowSums(
      across(c(installed_additions, planned_additions)), na.rm = TRUE
    ),
    # Factor source column for ordered plotting
    elec_source = factor(elec_source, levels = elec_source_lvls)
  )

# Calculate each source's share of total additions
installed_planned_cap <- installed_planned_cap %>%
  summarise(
    total_additions = sum(planned_installed_additions),
    .by = year
  ) %>%
  left_join(installed_planned_cap, .) %>%
  mutate(
    pct_of_additions = planned_installed_additions / total_additions
  ) %>%
  arrange(year)

#################
# Generate plot
#################
p <- ggplot(
  data = installed_planned_cap %>%
    filter(year <= 2027),
  aes(
    x = year,
    y = pct_of_additions,
    fill = elec_source,
    alpha = ifelse(year >= 2026, 0.5, 1)
  )
) +
  geom_col() +
  scale_alpha_identity(guide = "none") +
  scale_x_continuous_nrg(
    breaks = seq(
      min(installed_planned_cap$year), max(installed_planned_cap$year), 1
    )
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    labels = label_percent()
  ) +
  scale_fill_manual(values = setNames(color_values, elec_source_lvls)) +
  labs_nrg(
    title = "Electric Generating Capacity Additions",
    subtitle = "By Source",
    caption = paste(
      "Source: Energy Information Administration.",
      "\nNote: 2026-27 values are for planned projects."
    ),
    y = "Percentage of Capacity Additions"
  ) +
  theme_nrg()

print(p)

save_nrg_plot("Capacity Additions by Fuel Type.png")
