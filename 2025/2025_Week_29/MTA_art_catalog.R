
# 📚 TidyTuesday 2025 - Week 29
# 📊 MTA Permanent Art Catalog
# 🙋️ Author: Aghata Charnobay

# Load packages -----------------------------------------------------------

library(tidytuesdayR) # To load TidyTuesday data
library(tidyverse) # For data manipulation and ggplot2
library(ggiraph) # For making ggplot interactive with tooltips & hover
library(ggtext) # To enable rich text formatting in ggplot
library(sysfonts) # To add custom fonts
library(showtext) # To render Google fonts nicely in plots
library(glue)  # For easy string interpolation

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load("2025-07-22")
mta_art <- tuesdata$mta_art

# Load fonts --------------------------------------------------------------

font_add_google("Jost")
gdtools::register_gfont("Jost") # Register for HTML output
showtext_auto()
showtext_opts(dpi = 300)

# Define colors and fonts ------------------------------------------------

bg_col <- "white"
bar_col <- "#00933C"
text_col <- "#1D1D1F"
cap_col <- "#5d5d5d"

body_font <- "Jost"
title_font <- "Jost"

# Data wrangling ----------------------------------------------------------

mta_yearly <- mta_art %>%
  mutate(art_date = as.integer(art_date)) %>%
  filter(!is.na(art_date)) %>%
  group_by(art_date) %>%
  summarise(
    n_artworks = n(),
    n_artists = n_distinct(artist),
    n_stations = n_distinct(station_name)
  ) %>%
  mutate(
    tooltip = glue(
      "<span style='font-weight:bold;'>Year:</span> {art_date}<br/>",
      "<span style='font-weight:bold;'>Artworks:</span> {n_artworks}<br/>",
      "<span style='font-weight:bold;'>Artists:</span> {n_artists}<br/>",
      "<span style='font-weight:bold;'>Stations:</span> {n_stations}"
    )
  )

# Plot --------------------------------------------------------------------

p <- ggplot(mta_yearly, aes(x = art_date, y = n_artworks)) +
  geom_col_interactive(
    aes(tooltip = tooltip),
    fill = bar_col,
    width = 0.8,
    alpha = 0.95
  ) +
  geom_point_interactive(
    aes(y = n_artworks, tooltip = tooltip),
    shape = 21,
    fill = "white",
    color = "black",
    size = 2.8,
    stroke = 1
  ) +
  labs(
    title = "The Evolution of Art in NYC Transit Stations",
    subtitle = "Hover the bars to learn the number of new artworks,<br> contributing artists, and stations with new art each year.",
    caption = "Source: New York MTA | #TidyTuesday 2025 - Week 29 | Designed by: Aghata Charnobay",
    x = "",
    y = ""
  ) +
  theme_minimal(base_family = body_font, base_size = 11) +
  theme(
    plot.background = element_rect(fill = bg_col, color = NA),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(face = "bold", color = text_col, size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(
      color = text_col, size = 10, face = "italic", lineheight = 1.4,
      margin = margin(t = 8)
    ),
    plot.caption = element_text(size = 9, color = cap_col, margin = margin(t = 5)),
    panel.grid.major.y = element_line(color = "#E0E0E0", size = 0.3),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = text_col, angle = 45, hjust = 1)
  )

# Interactive plot --------------------------------------------------------

interactive_p <- girafe(
  ggobj = p,
  width_svg = 7,
  height_svg = 4.5,
  options = list(
    opts_tooltip(css = glue("
      background: white;
      padding: 8pt;
      border: 2px solid black;
      border-radius: 4px;
      font-family: {body_font};
      font-size: 18px;
      box-shadow: 2px 2px 5px rgba(0,0,0,0.2);
    ")),
    opts_hover(css = "fill:#FF6319; transition: fill 0.3s ease;")
  )
)

interactive_p

# Save plot ----------------------------------------------------------------

ggsave(
  filename = "2025/2025_Week_29/plots/mta_art_catalog_refined.png",
  plot = p,
  width = 7,
  height = 4.5,
  dpi = 300,
  bg = bg_col
)
