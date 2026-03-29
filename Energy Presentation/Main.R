################
# Initial setup
################

# Setting paths
BASE_PATH <- "c:/Users/derek/Downloads/Energy Presentation"
SCRIPTS_PATH <- file.path(BASE_PATH, "R Scripts")

# Importing necessary libraries
library(tidyverse)
library(openxlsx2)
library(scales)
library(janitor)
library(eia)

# Set EIA API key
eia_set_key("pclmugwhOX4mhsjgp80hHSF4pLLiqdEYynC2WNjc")

##########################
# Setting custom functions
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
scale_x_continuous_nrg <- function(..., expand = expansion(mult = c(0, 0))) {
  scale_x_continuous(..., expand = expand)
}

# Save
save_nrg_plot <- function(..., width = 16, height = 9) {
  ggsave(..., width = width, height = height)
}

###################
# Run all R scripts
###################

# Energy Consumption from Oil
source(file.path(SCRIPTS_PATH, "World Energy Outlook.R"))

# Oil Reserves
source(file.path(SCRIPTS_PATH, "Oil Reserves.R"))

# Oil Production by Country
source(file.path(SCRIPTS_PATH, "Oil Production by Country.R"))

# Wind and Solar Generation
source(file.path(SCRIPTS_PATH, "Wind and Solar Generation.R"))






# CES ggplot2 Theme
ces_theme <- function(base_size = 18, base_family = "") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      # Background and region
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = ces_colors$grid_gray, linewidth = 0.5),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      
      # Axes and lines
      axis.line = ggplot2::element_line(color = ces_colors$black, linewidth = 0.5),
      axis.ticks = ggplot2::element_line(color = ces_colors$black, linewidth = 0.5),
      axis.ticks.length = ggplot2::unit(.3, "cm"),
      axis.text.x = ggplot2::element_text(color = ces_colors$black, size = base_size, 
                                          face = "plain", margin = ggplot2::margin(t = 5)),
      axis.text.y = ggplot2::element_text(color = ces_colors$black, size = base_size, 
                                          face = "plain", margin = ggplot2::margin(r = 5)),      
      axis.title.x = ggplot2::element_text(color = ces_colors$black, size = base_size * 0.7,
                                           margin = ggplot2::margin(t = 8, b = 5)),
      axis.title.y.left = ggplot2::element_text(color = ces_colors$black, angle = 90, vjust = 0.5,
                                                size = base_size * 1, margin = ggplot2::margin(r = 8)),
      axis.title.y.right = ggplot2::element_text(color = ces_colors$black, angle = 90, vjust = 0.5,
                                                 size = base_size * 1, margin = ggplot2::margin(l = 8)),
      
      # Titles and captions
      plot.title = ggplot2::element_text(color = ces_colors$bluish_black, size = base_size * 1.5,
                                         face = "bold", hjust = 0.5, margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(color = ces_colors$subtitle_gray, size = base_size * 1.2,
                                            hjust = 0.5, margin = ggplot2::margin(b = 15)),
      plot.caption = ggplot2::element_text(color = ces_colors$black, size = base_size * 0.7,
                                           hjust = 0, margin = ggplot2::margin(t = 10)),
      
      # Legend
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(color = ces_colors$black,
                                          size = base_size * 1,
                                          margin = ggplot2::margin(l = 4, r = 8, unit = "pt")),
      legend.background = ggplot2::element_rect(fill = "white", color = "black", linewidth = 0.6),
      legend.key = ggplot2::element_rect(fill = "white", color = NA),
      legend.key.width = ggplot2::unit(12, "mm"),
      legend.key.height = ggplot2::unit(3, "mm"),
      legend.spacing.x = ggplot2::unit(3, "mm"),
      legend.spacing.y = ggplot2::unit(2, "mm"),
      legend.margin = ggplot2::margin(t = 8, r = 8, b = 8, l = 8, unit = "pt"),
      legend.box.margin = ggplot2::margin(t = -4, r = 0, b = 4, l = 0, unit = "pt"),
      
      # Margins
#      plot.margin = ggplot2::margin(t = 20, r = 25, b = 30, l = 20, unit = "pt")
    )
}
