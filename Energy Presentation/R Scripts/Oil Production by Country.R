#############################
# Import data from the EIA
#############################

# International data
oil_prod_country <- eia_data(
  dir = "international/data",
  data = "value",
  facets = list(
    activityId = "1", # Production
    productId = "57", # Crude oil incl. lease condensate
    countryRegionId = c("CAN", "IRN", "RUS", "SAU", "USA", "WORL"),
    unit = "TBPD"     # Thousand barrels / day
  ),
  freq = "annual"
) %>%
  clean_names() %>%
  rename(year = period, oil_prod = value) %>%
  mutate(
    # No data for Russia before the USSR's breakup
    oil_prod = as.numeric(str_remove(oil_prod, "--")),
    year = as.numeric(year)
  )

# Calculate regional percentages of world oil production
oil_prod_country <- oil_prod_country %>%
  select(year, country_region_id, oil_prod) %>%
  # Pivot wider for easier transformations
  pivot_wider(
    names_from = country_region_id,
    values_from = oil_prod,
    names_glue = "{.value}_{country_region_id}"
  ) %>%
  # Generate bespoke regions and calculating percentages
  mutate(
    oil_prod_CAN_and_USA = oil_prod_CAN + oil_prod_USA,
    oil_prod_REST = oil_prod_WORL - oil_prod_CAN - oil_prod_USA,
    across(
      starts_with("oil_prod_"),
      ~ . / oil_prod_WORL,
      .names = "pct_{.col}"
    )
  ) %>%
  # Pivot back to long format for plotting
  pivot_longer(
    cols = starts_with("oil_prod_") | starts_with("pct_oil_prod_"),
    names_to = c(".value", "region"),
    names_pattern = "(oil_prod|pct_oil_prod)_(.*)"
  )

# Prepare region names for clearer plotting
oil_prod_country <- oil_prod_country %>%
  mutate(region = case_match(
    region,
    "CAN" ~ "Canada",
    "IRN" ~ "Iran",
    "RUS" ~ "Russia",
    "SAU" ~ "Saudi Arabia",
    "USA" ~ "U.S.",
    .default = region
  ))

###############################
# Generate plot
# U.S. + Canada Oil Production
###############################
plot_data <- oil_prod_country %>%
  filter(region %in% c("Canada", "U.S.", "CAN_and_USA"), year >= 2000)

p <- ggplot(data = plot_data, aes(x = year)) +
  # Column plot: U.S. and Canada oil + lease cond. prod.
  geom_col(
    data = plot_data %>%
      filter(region %in% c("Canada", "U.S.")),
    # Converting from thousand bbl / day to billion bbl / yr
    aes(y = oil_prod / 1000000 * 365.25, fill = region)
  ) +
  # Line plot: U.S. + Canada share of global oil + lease cond. prod.
  geom_line_nrg(
    data = plot_data %>%
      filter(region == "CAN_and_USA"),
    aes(y = pct_oil_prod * 30, color = "Share"),
    show.legend = FALSE
  ) +
  scale_x_continuous_nrg(
    breaks = seq(2001, max(plot_data$year), 4),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0)),
    sec.axis = sec_axis(
      ~ . / 30,
      name = "Share of World Total",
      labels = label_percent()
    )
  ) +
  scale_color_manual(values = c("Share" = "deepskyblue")) +
  scale_fill_manual(
    values = c("U.S." = "deepskyblue4", "Canada" = "darkorange3")
  ) +
  labs_nrg(
    title = "Oil Production in Canada and the U.S.",
    subtitle = "2000-Present",
    caption = "Source: Energy Information Administration",
    y = "Billion Barrels"
  ) +
  theme_nrg()

print(p)

# Export plot
save_nrg_plot("Oil Production in US and Canada.png")

###############################
# Generate plot
# Oil Production by Country
###############################
plot_data <- oil_prod_country %>%
  filter(region %in% c(
    "Canada",
    "Iran",
    "Russia",
    "Saudi Arabia",
    "U.S."
  ))

p <- ggplot(data = plot_data, aes(
  x = year,
  # Convert from thousand bbl / day to billion bbl / yr
  y = oil_prod  / 1000000 * 365.25,
  color = region
)) +
  geom_line_nrg() +
  scale_x_continuous_nrg(breaks = seq(1975, max(plot_data$year), 10)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.04))) +
  scale_color_manual(values = c(
    "U.S." = "deepskyblue4",
    "Canada" = "darkorange3",
    "Saudi Arabia" = "forestgreen",
    "Russia" = "mediumpurple3",
    "Iran" = "firebrick3"
  )) +
  labs_nrg(
    title = "Oil Production in Selected Countries",
    subtitle = "1973-Present",
    caption = "Source: Energy Information Administration",
    y = "Billion Barrels"
  ) +
  theme_nrg()

print(p)

# Export plot
save_nrg_plot("Oil Production in Selected Countries.png")
