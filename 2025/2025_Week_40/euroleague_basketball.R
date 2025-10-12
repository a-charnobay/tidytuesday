
# 📚 TidyTuesday 2025 - Week 40
# 🏀 EuroLeague Basketball
# 🙋 Author: Aghata Charnobay

library(tidytuesdayR)
library(tidyverse)
library(ggtext)       
library(showtext) 
library(treemapify)
library(ggbranding)


# Load data -------------------------------

tuesdata <- tidytuesdayR::tt_load(2025, week = 40)
euroleague_basketball <- tuesdata$euroleague_basketball

# Font setup -------------------------------

font_add_google("Jost")
showtext_auto()
showtext_opts(dpi = 300)

# Quick EDA -------------------------------

# Championship

country_titles <- euroleague_basketball |>
  group_by(Country) |>
  summarise(
    titles = sum(Titles_Won, na.rm = TRUE)
  ) |>
  arrange(desc(titles))

teams_titles <- euroleague_basketball |>
  group_by(Team) |>
  summarise(
    titles = sum(Titles_Won, na.rm = TRUE)
  ) |>
  arrange(desc(titles))


# Final Four Appereances

country_final4 <- euroleague_basketball |>
  group_by(Country) |>
  summarise(
    titles = sum(Titles_Won, na.rm = TRUE)
  ) |>
  arrange(desc(titles))

teams_final4 <- euroleague_basketball |>
  group_by(Team) |>
  summarise(
    titles = sum(Titles_Won, na.rm = TRUE)
  ) |>
  arrange(desc(titles))


# Prepare data for plotting -------------------------------

titles_summary <- euroleague_basketball |>
  filter(Titles_Won > 0) |>
  select(Country, Team, Titles_Won) |>
  mutate(
    Country = factor(Country),
    Team = factor(Team),
    Titles_Won = as.numeric(as.character(Titles_Won))  
  )


# Plot -------------------------------

orange_pal <- c("#F16913","#FDD0A2", "#FEE6CE","#F28C40", "#FDAE6B")

p <- ggplot(
  titles_summary, 
  aes(
    area = Titles_Won, 
    fill = factor(Country), 
    subgroup = Country
  )
) +
  
  # ---- Treemap layers ----
geom_treemap(color = "grey20", size = 2) +           
  geom_treemap_subgroup_border(color = "grey20", size = 2.5) + 
  
  # ---- Text labels ----
geom_treemap_text(
  aes(label = paste0(Team, " (", Titles_Won, ")")),
  place = "bottomleft",
  reflow = TRUE,
  grow = FALSE,
  color = "black",
  family = "Jost",
  size = 15
) +
  geom_treemap_subgroup_text(
    place = "topleft",           
    grow = FALSE,
    colour = "black",
    fontface = "bold",
    size = 18
  ) +
  
  # ---- Color scale ----
scale_fill_manual(values = orange_pal) +
  
  # ---- Titles and subtitles ----
labs(
  title = "The Big Five of European Basketball",
  subtitle = "The EuroLeague brings together teams from across Europe, but teams <i>(n = number of championships)</i> <br> from only five countries have historically dominated the competition."
) +
  
  # ---- Theme and appearance ----
theme_minimal(base_family = "Jost") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 20, color = "black"),
    plot.subtitle = element_markdown(
      margin = margin(t = 10, b = 10), 
      lineheight = 1.3, 
      color = "black", 
      size = 14
    ),
    legend.position = "none",
    plot.margin = margin(t = 30, r = 30, b = 10, l = 30),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  ) +
  
  # ---- Branding and credits ----
add_branding(
  github = "a-charnobay",
  linkedin = "Aghata Charnobay",
  website = "a-charnobay.github.io",
  additional_text = "Data source: EuroLeague Basketball R package | #TidyTuesday 2025 - Week 40",
  text_position = "before",
  line_spacing = 2L, 
  icon_color = "orange",
  text_color = "black",
  caption_halign = 1, 
  icon_size = "12pt", 
  text_size = "12pt",
  additional_text_color = "black",
  caption_margin = margin(t = 10, b = 5, unit = "pt")
)


# Save plot -------------------------------

ggsave(
  filename = "2025/2025_Week_40/plots/euroleague_basketball.png",
  plot = p,
  height = 6,
  width = 9,
  dpi = 300
)
