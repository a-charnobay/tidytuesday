
# 📚 TidyTuesday 2025 - Week 28
# 📊 British Library Funding Visualization
# 🙋️ Author: Aghata Charnobay

# Loading packages -----

library(tidytuesdayR)  # To load TidyTuesday data
library(tidyverse)     # For data manipulation and ggplot2
library(scales)        # For scale adjustments
library(ggtext)        # To enable rich text formatting in ggplot

# Load TidyTuesday data for Week 28----

tuesdata <- tt_load(2025, week = 28)
bl_funding <- tuesdata$bl_funding

# Data wrangling ----

# Select relevant columns: year, nominal budget, real budget
bl_filt <- bl_funding |>
  select("year", "nominal_gbp_millions", "total_y2000_gbp_millions")

# Convert to long format for plotting
df_long <- bl_filt %>%
  pivot_longer(
    cols = c(nominal_gbp_millions, total_y2000_gbp_millions),
    names_to = "series",
    values_to = "value"
  )

# Create the line chart ----

ggplot(df_long, aes(x = year, y = value, color = series, linetype = series)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "nominal_gbp_millions" = "#15134BFF",
      "total_y2000_gbp_millions" = "#F56455FF"
    ),
    labels = c("Nominal GBP", "Real GBP (Y2000)")
  ) +
  scale_linetype_manual(
    values = c(
      "nominal_gbp_millions" = "solid",
      "total_y2000_gbp_millions" = "solid"
    )
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  labs(
    x = "",
    y = "",
    title = "Reading Between the Budget Lines: The British Library's Funding History",
    subtitle = "The total annual funding (in millions of GBP) shows that while the <span style='color:#15134BFF'><b>nominal budget</b></span> has slightly increased,<br>the <span style='color:#F56455FF'><b>real budget</b></span> (adjusted for inflation) has dropped significantly since 2006.",
    caption = "Source: British Library and Bank of England | TidyTuesday 2025 - Week 28 | Designed by: Aghata Charnobay"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#FAFAFA", color = NA),
    axis.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(face = "bold", size = 17, margin = margin(t = 5)),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 13, lineheight = 1.2, margin = margin(t = 15, b = 15)),
    plot.caption = element_text(size = 11, margin = margin(t = 10, l = -5, b = -5), color = "#5d5d5d"),
    axis.text.x = element_text(hjust = 1, size = 11),
    axis.text.y = element_text(size = 11)
  )

# Save plot as PNG ----

ggsave(
  "plots/british_library_funding1.png",
  width = 9,
  height = 6,
)
