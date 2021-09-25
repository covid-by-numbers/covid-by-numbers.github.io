## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(patchwork)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

theme_cbn_clean <- theme_cbn +
theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank())
theme_set(theme_cbn_clean)

## Creating 100,000 dots
ten_thousand_df <- expand.grid(x = 1:100, y = 1:100) %>% as_tibble()

ten_thousand_gg <- ten_thousand_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(colour = "grey90",
             alpha = 0.5) +
  labs(x = "", y = "") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0,0,0,0, unit = "pt")) +
  coord_fixed()

ten_thousand_one_gg <- ten_thousand_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(aes(colour = stat(x == 47 & y == 79)),
             alpha = 0.5) +
  scale_colour_manual(values = c("grey90", "grey10")) + 
  labs(x = "", y = "") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0,0,0,0, unit = "pt"),
        legend.position = "none") +
  coord_fixed()

ten_thousand_dots_gg <- ten_thousand_one_gg +
  labs(title = "This is 10,000 dots.",
       subtitle = "One dot in the square of 10,000 is highlighted.")

empty_gg <- ten_thousand_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_point(alpha = 0) +
  labs(x = "", y = "") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(0,0,0,0, unit = "pt")) +
  coord_fixed()

# Using patchwork we stitch together these plots
one_hundred_thousand_dots_gg <-
  (ten_thousand_gg|ten_thousand_gg|ten_thousand_gg)/
  (ten_thousand_gg|ten_thousand_one_gg|ten_thousand_gg)/
  (ten_thousand_gg|ten_thousand_gg|ten_thousand_gg)/
  (empty_gg|ten_thousand_gg|empty_gg)

## Saving graphs
# Save the figures in the required dimensions
ggsave(file = "R/COVID By Numbers/INT Figures/INT_ten_thousand_dots_gg.jpeg",
       plot = ten_thousand_dots_gg,
       device = "jpeg",
       height = 15, width = 15)

ggsave(file = "R/COVID By Numbers/INT Figures/INT_one_hundred_thousand_dots_gg.jpeg",
       plot = one_hundred_thousand_dots_gg,
       device = "jpeg",
       height = 15, width = 15)