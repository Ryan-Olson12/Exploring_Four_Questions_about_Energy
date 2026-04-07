################
# Initial setup
################

# Set paths
BASE_PATH <- file.path(
  "C:/Users/derek/OneDrive/Documents/GitHub",
  "Exploring_Four_Questions_about_Energy/Energy Presentation"
)

SCRIPTS_PATH <- file.path(BASE_PATH, "R Scripts")

# Import necessary libraries
library(tidyverse)
library(openxlsx2)
library(scales)
library(janitor)
library(eia)

# Set EIA API key
eia_set_key(Sys.getenv("EIA_API_KEY"))

##########################
# Create custom functions
##########################

# Theme
theme_nrg <- function(base_size = 20) {
  theme_minimal() +
    theme(
      # Plot
      plot.caption = element_text(
        size = base_size * 0.8,
        margin = margin(t = 11)
      ),
      plot.title = element_text(
        size = base_size * 2,
        face = "bold",
        hjust = 0.5
      ),
      plot.subtitle = element_text(size = base_size * 1.5, hjust = 0.5),

      # Panel
      panel.grid.major.x = element_blank(),
      panel.grid.minor = ggplot2::element_blank(),

      # Axis
      axis.line = element_line(color = "black", linewidth = 0.3),
      axis.ticks = element_line(linewidth = 0.3),
      axis.text.x = element_text(size = base_size),
      axis.text.y = element_text(size = base_size),
      axis.title.x = element_text(size = base_size),
      axis.title.y.left = element_text(size = base_size),
      axis.title.y.right = element_text(size = base_size),

      # Legend
      legend.background = element_rect(),
      legend.key.size = unit(1, "cm"),
      legend.key.spacing.x = unit(12, "pt"),
      legend.position = "bottom",
      legend.text = element_text(size = base_size),
      legend.title = element_blank(),
    )
}

# Line geom
geom_line_nrg <- function(..., linewidth = 1.8) {
  geom_line(..., linewidth = linewidth)
}

# Labels specification
labs_nrg <- function(..., x = NULL) {
  labs(..., x = x)
}

# Continuous x-axis scale specification
scale_x_continuous_nrg <- function(..., expand = expansion(mult = c(0, 0.017))) {
  scale_x_continuous(..., expand = expand)
}

# Save
save_nrg_plot <- function(..., width = 16, height = 9) {
  ggsave(..., width = width, height = height)
}

# Set working directory for plot export
setwd(paste0(BASE_PATH, "/Plots"))

###################
# Run all R scripts
###################

# Electric Capacity Additions by Fuel Type
source(file.path(SCRIPTS_PATH, "Capacity by Fuel Type.R"))

# Energy Consumption from Oil
source(file.path(SCRIPTS_PATH, "World Energy Outlook.R"))

# Oil Reserves
source(file.path(SCRIPTS_PATH, "Oil Reserves.R"))

# Oil Production by Country
source(file.path(SCRIPTS_PATH, "Oil Production by Country.R"))

# Wind and Solar Generation
source(file.path(SCRIPTS_PATH, "Wind and Solar Generation.R"))
