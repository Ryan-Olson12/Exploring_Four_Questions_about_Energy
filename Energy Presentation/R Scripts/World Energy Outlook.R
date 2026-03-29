setwd(paste0(BASE_PATH, "/Plots"))

################################
# Creating file-specific objects
################################
scenario_levels <- c(
  "Historical",
  "Current Policies",
  "Stated Policies",
  "Net Zero Emissions by 2050"
)

linetype_values <- c("solid", "dashed", "dashed", "dashed")
color_values <- c("black", "orangered1", "goldenrod3", "springgreen4")

#############################
# Importing data from the IEA
#############################

# World Energy Outlook
import_file <- file.path(
  BASE_PATH,
  "Data - raw",
  "Global Data - World Energy Outlook.csv"
)

weo <- read_csv(import_file) %>%
  clean_names()

# Filtering to include series of interest
weo <- weo %>%
  mutate(scenario = str_replace(scenario, " Scenario", "")) %>%
  filter(
    category == "Energy",
    product == "Oil",
    flow == "Total final consumption"
  )

# Joining last historical with first projected data
last_historical <- weo %>%
  filter(scenario == "Historical") %>%
  slice_tail(n = 1) %>%
  select(-scenario)

bridged_scenarios <- last_historical %>%
  crossing(scenario = c(
    "Current Policies",
    "Stated Policies",
    "Net Zero Emissions by 2050"
  ))

weo <- bind_rows(weo, bridged_scenarios) %>%
  mutate(scenario = factor(scenario, levels = scenario_levels)) %>%
  arrange(desc(scenario))

#################
# Generating plot
#################
p <- ggplot(data = weo, aes(
  x = year,
  y = value,
  color = scenario,
  linetype = scenario
)) +
  geom_point(size = 5) +
  # Legend only necessary for one geom
  geom_line_nrg(show.legend = FALSE) +
  scale_x_continuous(
    breaks = seq(2010, max(weo$year), 10),
    # Leaving extra padding for first and last points
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_linetype_manual(values = setNames(linetype_values, scenario_levels)) +
  scale_color_manual(values = setNames(color_values, scenario_levels)) +
  scale_y_continuous(
    limits = c(0, NA),
    # Leaving extra padding for highest point
    expand = expansion(mult = c(0, 0.015))
  ) +
  labs_nrg(
    title = "Energy Consumption from Oil",
    subtitle = "By Scenario",
    caption = "Source: International Energy Agency",
    y = "Exajoules"
  ) +
  theme_nrg()

print(p)

save_nrg_plot("TFC from Oil.png")
