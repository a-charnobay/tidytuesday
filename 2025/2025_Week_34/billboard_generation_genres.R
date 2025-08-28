
# 📚 TidyTuesday 2025 - Week 34
# 🎵 Billboard Hot 100 Number Ones
# 🙋 Author: Aghata Charnobay

library(tidytuesdayR)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggtext)       
library(showtext)   


# Load data -------------------------------

tuesdata <- tidytuesdayR::tt_load(2025, week = 34)
billboard <- tuesdata$billboard

# Font setup -------------------------------

font_add_google("Jost")
showtext_auto()
showtext_opts(dpi = 300)

# Preprocess data -------------------------------

billboard <- billboard |>
  mutate(
    year = as.numeric(format(as.Date(date), "%Y")),
    generation = case_when(
      year >= 1946 & year <= 1964 ~ "Baby Boomers",
      year >= 1965 & year <= 1980 ~ "Gen X",
      year >= 1981 & year <= 1996 ~ "Millennials",
      year >= 1997 & year <= 2012 ~ "Gen Z",
      year >= 2013 & year <= 2025 ~ "Gen Alpha"
    ),
    first_genre_class = str_split(discogs_genre, ";", simplify = TRUE)[,1],
    first_genre_class = ifelse(first_genre_class == "Folk, World, & Country", "Folk", first_genre_class),
    first_genre_class = factor(first_genre_class,
                               levels = c("Rock", "Pop", "Folk", "Funk/Soul", "Jazz", "Electronic", "Hip Hop"))
  )


## Count genres per generation -------------------------------

genre_counts <- billboard |>
  filter(generation != "Gen Alpha") |>
  group_by(generation, first_genre_class) |>
  summarise(n = n(), .groups = "drop")


## Top 5 genres per generation -------------------------------

top5_genres <- genre_counts |>
  group_by(generation) |>
  slice_max(order_by = n, n = 5, with_ties = FALSE) |>
  ungroup() |>
  mutate(generation = fct_recode(generation,
                                 "Baby Boomers\n(1946–1964)" = "Baby Boomers",
                                 "Gen X\n(1965–1980)" = "Gen X",
                                 "Millennials\n(1981–1996)" = "Millennials",
                                 "Gen Z\n(1997–2012)" = "Gen Z"))


## Create baseline lines for all genres × generations -------------------------------

all_genre_gen <- expand_grid(
  generation = levels(top5_genres$generation),
  first_genre_class = levels(billboard$first_genre_class)
)

baseline_lines <- all_genre_gen |>
  left_join(top5_genres, by = c("generation", "first_genre_class")) |>
  mutate(
    n = replace_na(n, 0),
    generation = factor(
      generation,
      levels = c("Baby Boomers\n(1946–1964)", 
                 "Gen X\n(1965–1980)", 
                 "Millennials\n(1981–1996)", 
                 "Gen Z\n(1997–2012)")
    ),
    first_genre_class = factor(
      first_genre_class,
      levels = c("Rock", "Pop", "Folk", "Funk/Soul", "Jazz", "Electronic", "Hip Hop")
    )
  )

# Plot -------------------------------

p <- ggplot() +
  # Full line (all genres × generations)
  geom_segment(
    data = baseline_lines,
    aes(x = first_genre_class, xend = first_genre_class, y = 0, yend = 210), 
    color = "#f4a300", size = 0.5
  ) +
  # Square knob with adjusted y position for both n = 0 and n > 0
  geom_point(
    data = baseline_lines,
    aes(x = first_genre_class, y = ifelse(n == 0, 0, n + 10)),  # Shift down for zero counts, and up for others
    color = "#f4a300", size = 3.5, shape = 15
  ) +
  scale_y_continuous(limits = c(0, 210), breaks = seq(0, 200, 50)) +
  facet_wrap(~ generation, ncol = 1, strip.position = "left") +
  labs(
    title = "The Sound of Generations",
    subtitle = "A look at the top  music genres dominating the Billboard Hot 100 <br> charts during the **birth years** of each generation.",
    caption = "Billboard Hot 100 Number Ones Database <br> #TidyTuesday 2025 - Week 34 <br>Designed by Aghata Charnobay"
  ) +
  theme_minimal(base_size = 14, base_family = "Jost") +
  theme(
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", size = 16, color = "#f4a300"),
    plot.subtitle = element_markdown(margin = margin(t = 10, b = 15), lineheight = 1.2, color = "white", size = 13),
    plot.caption = element_markdown(size = 10, margin = margin(t = 15, b = 5, r = -10), color = "white", lineheight = 1.4),
    plot.margin = margin(t = 30, r = 30, b = 10, l = 30),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "gray30", linetype = "dotted"),
    strip.placement = "outside",
    strip.text = element_text(face = "bold", size = 11, color = "#f4a300"),
    axis.text.x = element_text(angle = 45, hjust = 1, color = "white"),
    axis.text.y = element_text(size= 8, color = "white"),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "#111111", color = NA),
    panel.background = element_rect(fill = "#111111", color = NA),
    strip.background = element_rect(fill = "#111111", color = NA)
  )

# Save plot -------------------------------
ggsave(
  filename = "2025/2025_Week_34/plots/billboard_generation_genres.png",
  plot = p,
  height = 7.5,
  width = 6,
  dpi = 300
)
