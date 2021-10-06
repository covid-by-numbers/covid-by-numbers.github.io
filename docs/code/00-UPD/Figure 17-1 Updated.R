## Packages and Themes
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)

# Next, we load the plotting theme
source("/cloud/project/code/00-UPD/CBN Theme.R")

## Drawing the data
# We draw the data from the prepared file
phe_flucovid_df <- read_excel("/cloud/project/code/17-COL/COL Figures - 2021-04-05.xlsx",
                              sheet = "DATA-2") %>%
  mutate(week_start_date = as_date(week_start_date),
         week_end_date = as_date(week_end_date)) %>%
  pivot_longer(cols = 4:9,
               names_to = "measures",
               values_to = "rates_per_100000") %>%
  drop_na()

# We set the breaks for the graphs
phe_flucovid_breaks <- c("2020-07-05", "2020-10-04", "2021-01-03", "2021-03-28") %>%
  as_date()

## Making the graph
phe_flucovid_gg1 <- phe_flucovid_df %>%
  filter(measures %in% c("covid19_like_indicator_rate", "ili_rate", "ili_rate_2018_19")) %>%
  ggplot(aes(x = week_end_date, y = rates_per_100000, group = measures)) +
  geom_line(aes(colour = measures), size = 1.5) +
  scale_x_date(breaks = phe_flucovid_breaks,
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 400)) +
  scale_colour_manual(name = "",
                      labels = c("Covid-19-like", "Influenza-like", "Influenza-like (2018-19)"),
                      values = c("#15c6d4", "#ec6752", "#ffef00")) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "RCGP consultation rates (per 100,000 people)",
       x = "Week end date", y = "")

phe_flucovid_gg2 <- phe_flucovid_df %>%
  filter(measures %in% c("covid19_hospital_admission_rate", "influenza_hospital_admission_rate")) %>%
  ggplot(aes(x = week_end_date, y = rates_per_100000, group = measures)) +
  geom_line(aes(colour = measures), size = 1.5) +
  scale_x_date(breaks = phe_flucovid_breaks,
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 40)) +
  scale_colour_manual(name = "",
                      labels = c("Covid-19", "Influenza"),
                      values = c("#15c6d4", "#ec6752")) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Hospital admission rates (per 100,000 people)",
       x = "Week end date", y = "")

fig_17_1_gg_upd <- phe_flucovid_gg1 + phe_flucovid_gg2 +
  plot_annotation(title = "Flu remains suppressed, with Covid-19 admission rates in England rising until January 2021.",
                  subtitle = "RCGP consultant rates and hospital admissions rates per 100,000 people in England. 29th June 2020 to 2nd May 2021.",
                  caption = "Source: PHE Weekly national Influenza and COVID-19 surveillance report, Week 18 2021; National flu report data: 5 March 2020 (week 10).")

## Saving the graph
ggsave(file = "/cloud/project/code/00-UPD/fig_17_1_gg_upd.jpeg",
       plot = fig_17_1_gg_upd,
       device = "jpeg",
       height = 800/96, width = 1600/96, dpi = 96)
