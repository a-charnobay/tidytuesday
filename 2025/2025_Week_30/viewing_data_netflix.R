
# 📚 TidyTuesday 2025 - Week 30
# 📊 What have we been watching on Netflix?
# 🙋️ Author: Aghata Charnobay

# Load packages -----------------------------------------------------------

library(tidytuesdayR) # Load TidyTuesday data
library(tidyverse)    # Data manipulation + ggplot2
library(ggtext)       # Rich text in ggplot
library(sysfonts)     # Custom fonts
library(showtext)     # Render Google fonts
library(patchwork)    # Combine plots
library(stringr)      # String manipulation

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2025-07-29')

movies <- tuesdata$movies
shows <- tuesdata$shows

# Load fonts --------------------------------------------------------------

font_add_google("Jost")
gdtools::register_gfont("Jost")
showtext_auto()

# Define colors and fonts ------------------------------------------------

## Dark bg

bg_col <- "#0a0a0a"
bar_col <- "#b30000"
text_col <- "white"

# ## Light bg

# bg_col <- "white"
# bar_col <- "#b30000"
# text_col <- "black"


body_font <- "Jost"
title_font <- "Jost"

# Data wrangling ----------------------------------------------------------

top10_movies <- movies %>%
  filter(report == "2025Jan-Jun") %>%
  slice_max(order_by = views, n = 10, with_ties = FALSE) %>%
  mutate(
    views = round(views / 1000000),
    title = str_remove(title, "//.*")
  )

top10_shows <- shows %>%
  filter(report == "2025Jan-Jun") %>%
  slice_max(order_by = views, n = 10, with_ties = FALSE) %>%
  mutate(
    views = round(views / 1000000),
    title = str_remove(title, "//.*"),
    title = str_replace_all(title, ": Season ([0-9]+)", " S0\\1")
  )

common_max <- max(max(top10_movies$views), max(top10_shows$views))


# Plot --------------------------------------------------------------------

## Movies

p1 <- ggplot(top10_movies, aes(x = reorder(title, views), y = views)) +
  geom_col(show.legend = FALSE, 
           fill = bar_col, 
           width = 0.9) +
  geom_text(aes(label = paste0(views, " M")),
            hjust = -0.1, 
            size = 3.5, 
            color = text_col, 
            family = body_font,
            fontface = "italic") +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    limits = c(0, common_max)
  ) +
  labs(
    title = "<b>What have we been watching on <span style='color:#b30000;'>NETFLIX</span>?</b><br>
<span style='font-weight:400; font-size:12pt;'>The most watched movies and shows on the streaming platform in the first half of 2025.</span>",
    subtitle = "Movies",
    x = "",
    y = "Views (in millions)"
  ) +
  theme_minimal(base_size = 13, 
                base_family = body_font) +
  theme(
    plot.background = element_rect(fill = bg_col, color = "NA"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = "#272727", linewidth = .3, linetype = "dashed"),
    plot.title = ggtext::element_markdown(face = "plain", size = 19, color = text_col, lineheight = 1.2),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    plot.title.position = "plot",
    plot.subtitle = element_text(face = "italic", color = text_col, size = 15, margin = margin(t = 15)),
    plot.margin = margin(20,20,10,20),
    axis.text.x = element_blank(),
    axis.text.y = element_text(color = text_col)
  )

p1

## Series

p2 <- ggplot(top10_shows, aes(x = reorder(title, views), y = views)) +
  geom_col(show.legend = FALSE,
           fill = bar_col,
           width = 0.9) +
  geom_text(aes(label = paste0(views, " M")),
            hjust = -0.1,
            size = 3.5,
            color = text_col,
            family = body_font, 
            fontface = "italic") +
  coord_flip() +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    limits = c(0, common_max)
  ) +
  labs(
    subtitle = "Shows",
    caption = "Source: Netflix Engagement Report | #TidyTuesday 2025 - Week 30 | Designed by: Aghata Charnobay",
    x = "",
    y = "Views (millions)"
  ) +
  theme_minimal(base_size = 13, base_family = body_font) +
  theme(
    plot.background = element_rect(fill = bg_col, color = "NA"),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = "#272727", linewidth = .3, linetype = "dashed"),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 12, color = text_col),
    axis.title.y = element_text(),
    plot.title.position = "plot",
    plot.subtitle = element_text(face = "italic", color = text_col, size = 15, margin = margin(t = 8)),
    plot.caption = element_text(size = 11, color = text_col, margin = margin(t = 15, b = 1, l= 20)),
    plot.margin = margin(5,20,10,20),
    axis.text.x = element_text(color = text_col, angle = 1, hjust = 0.5, size = 12),
    axis.text.y = element_text(color = text_col)
  )

p2

# Combine plots

p3 <- p1 / p2 + 
  plot_layout(guides = "collect") & 
  theme(
    plot.background = element_rect(fill = bg_col, color = NA)
  )

p3

# Save plot ----------------------------------------------------------------

ggsave(
  filename = "2025/2025_Week_30/plots/viewing_data_netflix_dark_bg.png",
  plot = p3,
  width = 8,
  height = 9,
  dpi = 300,
  bg = "NA"
)
