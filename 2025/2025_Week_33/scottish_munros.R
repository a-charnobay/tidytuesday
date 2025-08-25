
# 📚 TidyTuesday 2025 - Week 33
# 🏔️ Scottish Munros
# 🙋 Author: Aghata Charnobay

# Load packages -----------------------------------------------------------

library(tidytuesdayR) # Load TidyTuesday data
library(tidyverse)    # Data manipulation + ggplot2
library(ggtext)       # Rich text in ggplot
library(showtext)     # Render Google fonts

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load(2025, week = 33)
munros <- tuesdata$scottish_munros

# Load fonts and define styles --------------------------------------------

font_add_google("Jost")
showtext_auto()
showtext_opts(dpi = 300)

bg_col   <- "#c6d9f1"
text_col <- "grey20"

# Data wrangling ----------------------------------------------------------

## Select year columns to track Munro / Munro Top classifications
year_cols <- c("1891","1921","1933","1953","1969","1974","1981","1984","1990","1997","2021")

## Reshape data to long format
munros_long <- munros |>
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "class"
  ) |>
  mutate(year = as.integer(year)) |>
  arrange(DoBIH_number, year)

## Identify reclassifications over time
munros_long <- munros_long |>
  group_by(DoBIH_number) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    class_prev = lag(class),
    reclassified = case_when(
      is.na(class_prev) & !is.na(class)         ~ TRUE,  # addition
      !is.na(class_prev) & is.na(class)         ~ TRUE,  # removal
      class_prev != class & !is.na(class_prev)  ~ TRUE,  # Munro ↔ Munro Top
      TRUE                                      ~ FALSE
    )
  ) |>
  ungroup()

## Summarise reclassifications by year and class
reclass_summary <- munros_long |>
  filter(class %in% c("Munro", "Munro Top")) |>
  group_by(year, class) |>
  summarise(
    n_changes = sum(reclassified, na.rm = TRUE),
    total = sum(!is.na(class)), 
    .groups = "drop"
  ) |>
  mutate(
    n_changes = replace(n_changes, year == 1891, 0)  
  )

# Calculations ------------------------------------------------------------

## Total Munros and Tops in 1891
count_1891 <- munros_long |>
  filter(year == 1891 & !is.na(class)) |>
  count(class, name = "count")

## Peaks always classified as Munros
always_munro_count <- munros |>
  mutate(always_munro = rowSums(across(all_of(year_cols), ~ .x == "Munro"), na.rm = TRUE) == length(year_cols)) |>
  filter(always_munro) |>
  nrow()

## Peaks always classified as Munro Tops
always_munro_top_count <- munros |>
  mutate(always_munro_top = rowSums(across(all_of(year_cols), ~ .x == "Munro Top"), na.rm = TRUE) == length(year_cols)) |>
  filter(always_munro_top) |>
  nrow()

# Plot --------------------------------------------------------------------

p <- ggplot(
  reclass_summary |>
    mutate(class = factor(class, levels = c("Munro Top", "Munro"))), 
  aes(x = year, y = n_changes)
) +
  geom_area(aes(fill = class), position = "identity") +
  scale_fill_manual(values = c("Munro" = "forestgreen", "Munro Top" = "saddlebrown")) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_x_continuous(breaks = unique(reclass_summary$year)) +
  labs(
    title = "The changing face of Scottish Munros",
    subtitle = "<span style='color:forestgreen'>Munros</span> are Scottish mountains over 3,000 feet, while <span style='color:saddlebrown'>Munro Tops</span> are subsidiary summits above that height <br>
    but lacking the prominence to be considered separate peaks. This plot shows how often these classifications <br>
    have changed since the first list in 1891, which included <span style='color:forestgreen'>283 Munros</span> and <span style='color:saddlebrown'>255 Tops</span>. Of those, <span style='color:forestgreen'>243 Munros</span> <br>
    and <span style='color:saddlebrown'>156 Tops</span> never had their original status changed. Reclassifications peaked in 1921, 1981, and 1997.",
    x = "",
    y = "Number of reclassified munros",
    caption = "Source: The Database of British and Irish Hills v18.2 | #TidyTuesday 2025 - Week 33 | Designed by: Aghata Charnobay"
  ) +
  theme_minimal(base_size = 13, base_family = "Jost") +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(face = "bold", size = 17),
    plot.subtitle = element_markdown(margin = margin(t = 5, b = 15)),
    plot.caption = element_text(size = 11, margin = margin(t = 10, b = 5, l = 20, r = 20), color = text_col),
    plot.margin = margin(t = 25, r = 25, b = 10, l = 25),
    axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "grey70", linetype = "dashed"),
    plot.background = element_rect(fill = bg_col, color = NA)
  )

# Save plot ---------------------------------------------------------------

ggsave(
  filename = "2025/2025_Week_33/plots/scottish_munros.png",
  plot = p,
  height = 6,
  width = 9,
  dpi = 300
)