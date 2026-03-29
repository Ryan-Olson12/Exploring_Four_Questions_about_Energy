setwd(paste0(BASE_PATH, "/Plots"))

################################
# Creating file-specific objects
################################
scenario_levels <- c(
  "Historical",
  "NREL (No IRA)",
  "EIA (Base Case)",
  "NREL (Base Case)"
)

linetype_values <- c("solid", "dashed", "dashed", "dashed")
color_values <- c("black", "orangered1", "goldenrod3", "forestgreen")

########################################
# Importing Historical Data from the EIA
########################################
wind_solar_hist <- eia_data(
  dir = "electricity/electric-power-operational-data/data",
  data = "generation",
  facets = list(
    fueltypeid = c("ALL", "SUN", "WND"),
    sectorid = "98",
    location = "US"
  ),
  freq = "annual"
) %>%
  clean_names() %>%
  rename(year = period, elec_gen_fuel = generation) %>%
  mutate(elec_gen_fuel = as.numeric(elec_gen_fuel))

# Standardizing fuel names
wind_solar_hist <- wind_solar_hist %>%
  mutate(
    fuel = case_match(
      fueltypeid,
      "ALL" ~ "Total",
      "SUN" ~ "Solar",
      "WND" ~ "Wind",
      .default = fueltypeid
    ),
    scenario = "Historical"
  )

# Adding a column for total generation
wind_solar_hist <- wind_solar_hist %>%
  select(year, scenario, fuel, elec_gen_fuel) %>%
  left_join(
    wind_solar_hist %>%
      filter(fuel == "Total") %>%
      select(year, elec_gen_tot = elec_gen_fuel),
    by = "year"
  ) %>%
  mutate(year = as.numeric(year))

######################################
# Importing Forecast Data from the EIA
######################################

# Annual Energy Outlook data
wind_solar_aeo <- eia_data(
  dir = "aeo/2025/data",
  data = "value",
  facets = list(
    scenario = "ref2025", # Reference scenario
    regionId = "5-0",     # United States
    seriesId = c(
      "gen_NA_elep_NA_ofw_NA_NA_blnkwh",    # Offshore wind
      "gen_NA_elep_NA_wnd_NA_NA_blnkwh",    # Onshore wind
      "gen_NA_elep_NA_slr_phtvl_NA_blnkwh", # Solar PV
      "gen_NA_elep_NA_slr_therm_NA_blnkwh", # Solar thermal
      "gen_NA_elep_NA_teg_NA_NA_blnkwh"     # Total power-sector generation
    )
  ),
  freq = "annual"
) %>%
  clean_names() %>%
  rename(year = period, elec_gen = value) %>%
  mutate(elec_gen = as.numeric(elec_gen))

# Combining wind and solar as consolidated categories
wind_solar_aeo <- wind_solar_aeo %>%
  select(year, scenario, series_id, elec_gen) %>%
  mutate(fuel = case_match(
    series_id,
    c(
      "gen_NA_elep_NA_ofw_NA_NA_blnkwh", "gen_NA_elep_NA_wnd_NA_NA_blnkwh"
    ) ~ "Wind",
    c(
      "gen_NA_elep_NA_slr_therm_NA_blnkwh", "gen_NA_elep_NA_slr_phtvl_NA_blnkwh"
    ) ~ "Solar",
    "gen_NA_elep_NA_teg_NA_NA_blnkwh" ~ "Total",
    .default = series_id
  ),
  # Reference Scenario is the only one gathered
  scenario = "EIA (Base Case)") %>%
  group_by(year, scenario, fuel) %>%
  summarise(elec_gen_fuel = sum(elec_gen), .groups = "drop")

# Adding a column for total generation
wind_solar_aeo <- wind_solar_aeo %>%
  left_join(
    wind_solar_aeo %>%
      filter(fuel == "Total") %>%
      select(year, elec_gen_tot = elec_gen_fuel),
    by = "year"
  ) %>%
  mutate(year = as.numeric(year))

##############################
# Importing data from the NREL
##############################
import_file <- file.path(
  BASE_PATH,
  "Data - raw",
  "Annual National - Standard Scenarios.csv"
)

wind_solar_nrel <- read_csv(import_file, skip = 3) %>%
  rename_with(tolower) %>%
  clean_names() %>%
  rename(year = t)

# Keeping only data of interest
wind_solar_nrel <- wind_solar_nrel %>%
  select(
    scenario,
    year,
    csp_mwh,
    upv_mwh,
    wind_offshore_mwh,
    wind_onshore_mwh,
    generation_for_aer
  ) %>%
  # Optimizing scenario names for legend
  mutate(
    scenario = case_match(
      scenario,
      "Mid_Case" ~ "NREL (Base Case)",
      "Mid_Case_No_TC_Expiration" ~ "NREL (Permanent IRA)",
      "Mid_Case_No_IRA" ~ "NREL (No IRA)",
      .default = scenario
    )
  ) %>%
  # Keeping only scenarios specified above
  filter(str_starts(scenario, "NREL"))

# Combining wind and solar as consolidated categories
wind_solar_nrel <- wind_solar_nrel %>%
  # Giving capitalized names for immediate reshaping
  mutate(
    Solar = csp_mwh + upv_mwh,
    Wind = wind_offshore_mwh + wind_onshore_mwh,
    elec_gen_tot = generation_for_aer,
    .keep = "unused"
  ) %>%
  pivot_longer(
    cols = c("Solar", "Wind"),
    names_to = "fuel",
    values_to = "elec_gen_fuel"
  )

############################
# Binding EIA and NREL data
############################
wind_solar_gen <- bind_rows(
  wind_solar_hist,
  wind_solar_aeo,
  wind_solar_nrel
) %>%
  mutate(scenario = factor(scenario, levels = scenario_levels))

# Calculating fuels' share of total generation
wind_solar_gen <- wind_solar_gen %>%
  mutate(fuel_pct = elec_gen_fuel / elec_gen_tot)

# Aggregating solar and wind into a single category
plot_data <- wind_solar_gen %>%
  filter(fuel != "Total") %>%
  mutate(wind_solar = case_match(
    fuel,
    c("Wind", "Solar") ~ "Wind and Solar",
    .default = fuel
  )) %>%
  group_by(year, scenario, wind_solar) %>%
  summarise(fuel_pct = sum(fuel_pct)) %>%
  filter(scenario != "NREL (Permanent IRA)")

#################
# Generating plot
#################
p <- ggplot(
  data = plot_data, aes(
    x = year, y = fuel_pct, color = scenario, linetype = scenario
  )
) +
  geom_line_nrg() +
  geom_hline(yintercept = 0.5, color = "red", linewidth = 1) +
  scale_x_continuous_nrg() +
  scale_y_continuous(
    n.breaks = 6,
    labels = label_percent(),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.07))
  ) +
  scale_linetype_manual(values = setNames(linetype_values, scenario_levels)) +
  scale_color_manual(values = setNames(color_values, scenario_levels)) +
  labs_nrg(
    y = "Share of Total Generation",
    title = "Electricity Generation from Wind and Solar",
    subtitle = "By Scenario",
    caption = paste(
      "Sources: National Renewable Energy Laboratory",
      "and Energy Information Administration.",
      "\nNote: Utility-scale, power-sector generation only."
    )
  ) +
  theme_nrg()

print(p)

# Exporting plot
save_nrg_plot("Wind and Solar Generation.png")
