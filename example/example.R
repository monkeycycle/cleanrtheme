# load the necessary packages
if (!require('upstartr')) install.packages('upstartr'); library('upstartr')
if (!require('devtools')) install.packages('devtools'); library('devtools')
if (!require('extrafont')) install.packages('extrafont'); library('extrafont')

library(grid)
library(scales)
library(tidyverse)
loadfonts(quiet = TRUE)


library(ggplot2) ; library(tidyverse) ; library(scales) ; library(svglite)
devtools::install_github("monkeycycle/cleanrtheme", force = TRUE)
library(cleanrtheme)


# load some data
df <- data.frame(
  religion = c("Christian", "Buddhist", "Hindu", "Jewish", "Muslim", "Sikh", "Other Religion", "No Religion", "Not Stated"),
  count = c(143639, 768, 2271, 2413, 12994, 1652, 566, 47968, 14307)
)

# create a ggplot object
plot <- df %>%
  arrange(count) %>%
  mutate(religion = factor(religion, levels = religion)) %>%
  ggplot(aes(religion, count)) +
  geom_col(show.legend = FALSE, fill=cleanrColours[5], alpha=.8) +
  geom_text(data = df %>% filter(religion != "No Religion"), aes(label = comma(count)), colour = "#212121", size = 3.3, hjust = 0, nudge_y = 2000) +

  geom_col(data = df %>% filter(religion == "No Religion"), show.legend = FALSE, fill=cleanrColours[2]) +
  geom_text(data = df %>% filter(religion == "No Religion"), aes(label = comma(count)), colour = "#212121", size = 3.3, hjust = 0, nudge_y = 2000) +

  scale_y_continuous(label = comma, limits=c(0, 200000), expand = c(0,0)) +
  coord_flip() +
  labs(x = NULL, y = "Residents",
       title = "A fifth of residents have no religion",
       subtitle = "Religious affiliation, 2011",
       ) +
  theme_cleanr() +
  scale_colour_cleanr() +
  scale_fill_cleanr()


# finalize_plot(plot_final, plot, 'plots/', width_pixels = 640, height_pixels = 450)
finalize_plot(plot_name = plot,
              source = "Source: Sample Census 2011",
              save_filepath = "example/plots/plot.png",
              width_pixels = 640,
              height_pixels = 450)
