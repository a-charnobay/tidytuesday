
# 📚 TidyTuesday 2025 - Week 31
# 📊 Income Inequality (Gini coefficient) Before and After Taxes
# 🙋️ Author: Aghata Charnobay

# Load packages -----------------------------------------------------------

library(tidytuesdayR) # Load TidyTuesday data
library(tidyverse)    # Data manipulation + ggplot2
library(ggtext)       # Rich text in ggplot
library(showtext)     # Render Google fonts
library(forcats)      # Factor manipulation

# Load data ---------------------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2025-08-05')

income_inequality_processed <- tuesdata$income_inequality_processed
income_inequality_raw <- tuesdata$income_inequality_raw

# Load fonts and define colors --------------------------------------------------------------

font_add_google("Jost")
showtext_auto()
showtext_opts(dpi = 300)

bg_col   <- "white"
text_col <- "#2b2d2d"

# EDA --------------------------------------------------------------

## How many countries do we have?

income_inequality_processed |>
  count(Entity, sort = TRUE)

## How many years with data?

income_inequality_processed |>
  count(Year, sort = TRUE)

min(income_inequality_processed$Year)

## Do we have data for every country at every year?

NA_heatmap <- income_inequality_processed |>
  pivot_longer(cols = c(gini_mi_eq, gini_dhi_eq),
               names_to = "measure",
               values_to = "value") |>
  mutate(missing = is.na(value))


NA_plot <- ggplot(NA_heatmap, aes(x = Year, y = fct_reorder(Entity, Year, .fun = min),
                                  fill = missing)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("FALSE" = "steelblue", "TRUE" = "grey80"),
                    name = "Gini index data",
                    labels = c("Available", "One Missing Measure")) +
  facet_wrap(~ measure) +
  labs(title = "",
       x = "Year", y = "Country") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

NA_plot

# Data wrangling --------------------------------------------------------------

## Country names

unique(income_inequality_processed$Entity)

## Classify countries based on owid region

regions <- income_inequality_raw |>
  filter(!is.na(owid_region)) |>
  distinct(Code, Entity, owid_region)

income_inequality_processed <- income_inequality_processed |>
  left_join(
    select(regions, Entity, owid_region),
    by = "Entity"
  ) |>
  rename(region = owid_region)

## Get latest data per country

### Calculate redistribution data

latest_data <- income_inequality_processed |>
  filter(!is.na(gini_mi_eq) & !is.na(gini_dhi_eq)) |>  
  group_by(Entity) |>
  filter(Year == max(Year, na.rm = TRUE)) |>
  ungroup() |>
  mutate(gini_diff = gini_mi_eq - gini_dhi_eq) 

### Add diff plotting coordinate

latest_data <- latest_data |>
  mutate(
    Entity_year = paste0(Entity, " (", Year, ")"),
    Entity_year = fct_reorder(Entity_year, gini_mi_eq),
    diff_label_x = (gini_mi_eq + gini_dhi_eq) / 2  
  )

## Plot

p <- ggplot(latest_data) +
  geom_segment(aes(x = gini_mi_eq, xend = gini_dhi_eq, y = Entity_year, yend = Entity_year),
               color = "#aeb6bf", size = 4.5, alpha = 0.5) +
  geom_point(aes(x = gini_mi_eq, y = Entity_year), color = "#762a83", size = 4) +
  geom_point(aes(x = gini_dhi_eq, y = Entity_year), color = "#009688", size = 4) +
  geom_text(aes(x = diff_label_x, y = Entity_year, label = paste0("- ", round(gini_diff, 2))),
            color = text_col, size = 3, nudge_y = 0.04) +
  facet_grid(region ~ ., scales = "free_y", space = "free", switch = "y") +
  coord_cartesian(clip = "off") +
  labs(
    title = "Does Government Taxation Reduce Income Inequality?",
    subtitle = "An analysis of Gini coefficients <span style='color:#762a83'><b>before</b></span> and <span style='color:#009688'><b>after</b></span> taxes across world regions and countries*.",
    caption = paste0(
      "* Latest available data for each country.\n\n",
      "Source: Our World in Data | #TidyTuesday 2025 - Week 31 | Designed by: Aghata Charnobay"
    ),
    x = "Gini Coefficient",
    y = NULL
  ) +
  theme_minimal(base_size = 13, base_family = "Jost") +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    axis.text.y = element_text(hjust = 1),
    axis.title.x = element_text(color = text_col, size = 11),
    strip.placement = "outside",
    strip.text.y.left = element_text(
      angle = 0, hjust = 0, face = "bold", size = 11
    ),
    strip.background = element_blank(),
    plot.margin = margin(t = 20, r = 25, b = 20, l = 25),
    plot.background = element_rect(fill = bg_col, color = bg_col),
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_markdown(
      color = text_col,
      margin = margin(t = 5, b = 15)
    ),
    plot.caption = element_text(
      size = 11, color = text_col,
      margin = margin(t = 15, b = 1, l = 20)
    )
  )
p


# Save plot ----------------------------------------------------------------

ggsave(
  filename ="2025/2025_Week_31/plots/gini_coefficient_taxes.png",
  plot = p,
  height = 8,
  width = 10,
  dpi = 300
)
