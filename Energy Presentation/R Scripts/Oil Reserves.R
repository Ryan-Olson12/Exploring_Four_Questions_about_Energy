##########################
# Import data from OPEC
##########################

# Crude oil reserves
import_file <- file.path(
  BASE_PATH,
  "Data - raw",
  "World proven crude oil reserves by country.xlsx"
)

oil_reserves <- wb_to_df(import_file, rows = 3:62) %>%
  rename(location = 1) %>%
  mutate(across(-location, ~ as.character(.x))) %>%
  pivot_longer(
    cols = -location,
    names_to = "year",
    values_to = "reserves"
  ) %>%
  filter(location == "Total World")

# Oil demand
import_file <- file.path(
  BASE_PATH,
  "Data - raw",
  "World oil demand by country.xlsx"
)

oil_demand <- wb_to_df(import_file, rows = 3:79) %>%
  rename(location = 1) %>%
  mutate(across(-location, ~ as.character(.x))) %>%
  pivot_longer(
    cols = -location,
    names_to = "year",
    values_to = "demand"
  ) %>%
  # Match global observations w/ naming conventions in reserves data
  mutate(location = case_match(
    location,
    "Total world" ~ "Total World",
    .default = location
  )) %>%
  filter(location == "Total World")

# Join reserves and demand data
world_oil_data <- full_join(
  oil_reserves,
  oil_demand,
  by = c("location", "year")
) %>%
  select(-location)

# Convert data to numeric and standardize units
world_oil_data <- world_oil_data %>%
  mutate(
    across(everything(), ~ as.numeric(.x)),
    # Convert from thousand bbl / day to million bbl
    demand = demand * 365.25 / 1000,
    years_of_oil = reserves / demand
  )

# Generate plot
p <- ggplot(data = world_oil_data, aes(x = year)) +
  geom_area(aes(y = reserves / 1000, fill = "Reserves")) +
  geom_line_nrg(aes(y = years_of_oil * 32, color = "Years of Oil")) +
  scale_fill_manual(values = c("Reserves" = "deepskyblue4")) +
  scale_color_manual(values = c("Years of Oil" = "deepskyblue")) +
  scale_y_continuous(
    labels = label_comma(),
    limits = c(0, NA),
    expand = expansion(mult = c(0, .05)),
    sec.axis = sec_axis(~ . / 32, name = "Years of Oil Remaining")
  ) +
  scale_x_continuous_nrg(
    breaks = seq(1964, max(world_oil_data$year), 10),
    expand = expansion(mult = c(0, 0))
  ) +
  labs_nrg(
    title = "Global Proved Oil Reserves",
    subtitle = "1963 to 2024",
    caption = "Source: Organization of the Petroleum Exporting Countries",
    y = "Billion Barrels"
  ) +
  theme_nrg() +
  theme(legend.position = "none")

print(p)

# Export plot
save_nrg_plot("Proved Reserves.png")
