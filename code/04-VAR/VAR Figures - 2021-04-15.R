## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw data
# I draw the data from a prepared file
# https://covariants.org/per-variant
variants_b117_df <- read_excel("R/COVID By Numbers/VAR Figures/VAR Figures - 2021-04-15.xlsx",
                               sheet = "DATA-1",
                               col_types = c("date", "numeric", "numeric", "numeric", "numeric"))

variants_tidy_df <- variants_b117_df %>%
  mutate(week_start_date = as_date(week_start_date)) %>%
  pivot_longer(cols = 2:5,
               names_to = "country",
               values_to = "proportion") %>%
  mutate(share = 100*proportion)

variants_labels <- variants_tidy_df %>%
  filter(week_start_date == as_date("2021-04-19"))

## Creating the graph
variants_b117_gg <- variants_tidy_df %>%
  ggplot(aes(x = week_start_date, y = share, group = country)) +
  geom_line(aes(colour = country),
            size = 1.5, na.rm = TRUE) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  scale_colour_manual(values = c("grey75", "grey50", "grey25", "black")) +
  scale_x_date(date_breaks = "4 weeks",
               date_labels = "%d-%b\n%Y",
               expand = c(0.08, 0)) +
  geom_point(data = variants_labels,
             aes(x= week_start_date, y = share,
                 colour = country),
             size = 5) +
  geom_text(data = variants_labels,
            aes(x = week_start_date, y = share,
                label = country, colour = country),
            size = 7, fontface = "bold", hjust = -0.2) +
  guides(colour = "none") +
  labs(title = "The Alpha variant rose to dominate sequences in several countries.",
       subtitle = "Rounded proportion of GISAID sequences [%] in the 20I/501Y.V1 lineage (B.1.1.7, Alpha). Smoothed values are subject to change.",
       caption = "Source: CoVariants (GISAID).",
       x = "Week start date", y = "Proportion of sequences [%]")

## Saving the graph
# Saving the graph with the required dimensions
ggsave(file = "R/COVID By Numbers/VAR Figures/VAR_variants_b117_gg.jpeg",
       plot = variants_b117_gg,
       device = "jpeg",
       height = 10, width = 15)