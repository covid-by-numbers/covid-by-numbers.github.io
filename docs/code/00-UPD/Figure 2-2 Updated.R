## Packages and Themes
library(tidyverse)
library(readxl)
library(lubridate)

# Next, we load the plotting theme
source("/cloud/project/code/00-UPD/CBN Theme.R")

## Drawing the data
# I draw the data from a prepared file
# This was sent following correspondence with the authors at the University of Oxford
# https://science.sciencemag.org/content/371/6530/708/tab-figures-data
importation_events_df <- read_excel("/cloud/project/code/02-SPR/SPR Figures - 2021-04-16.xlsx",
                                    col_types = c("date", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric",
                                                  "numeric", "numeric", "numeric",
                                                  "numeric", "numeric"))

importation_tidy_df <- importation_events_df %>%
  mutate(date = as_date(date),
         other_countries = Ireland + Switzerland + US + Other) %>%
  select(date, China, Italy, Spain, France, Netherlands, other_countries) %>%
  rename(Other = other_countries) %>%
  pivot_longer(cols = 2:7,
               names_to = "country",
               values_to = "assigned_events")

importation_tidy_df$country <- factor(importation_tidy_df$country,
                                      levels = c("China", "France", "Italy", "Spain", "Netherlands", "Other"))

importation_labels_df <- importation_tidy_df %>%
  group_by(country) %>%
  summarise(total_assigned_events = sum(assigned_events)) %>%
  mutate(label = paste0("Total estimate: ",
                        round(total_assigned_events, digits = 0)))

## Making the graph
fig_2_2_gg_upd <- importation_tidy_df %>%
  ggplot(aes(x = date, y = assigned_events,
             group = fct_rev(country))) +
  geom_col(aes(fill = country),
           position = "identity",
           width = 1) +
  geom_text(data = importation_labels_df,
            aes(x = as_date("2020-01-05"),
                y = 28,
                label = label,
                colour = country,
                group = country),
            size = 7, fontface = "bold", hjust = 0,
            family = "Freight Text Pro") +
  facet_wrap(~country) +
  scale_x_date(expand = c(0,0),
               date_breaks = "4 weeks",
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(limits = c(0, 30),
                     expand = c(0,0)) +
  scale_colour_manual(aesthetics = c("colour", "fill"),
                      values = c("#1c1d1a", "#15c6d4", "#ec6752", "#009fdb", "#ffef00", "grey50")) +
  guides(fill = "none", colour = "none") +
  labs(title = "About 6 in 10 imported lineages came from France, Italy, and Spain.",
       subtitle = str_wrap("Estimated virus lineage importations with observed onward transmission per day (imputed) from the lag model. Statistical assignment to each country is a point estimate: there is uncertainty around each of these estimates. Total estimates are rounded.",
                           width = 130),
       x = "Imputed date of import event",
       y = "Importation events leading to observed onward transmission",
       caption = "Source: Establishment and lineage dynamics of the SARS-CoV-2 epidemic in the UK (Science, 2021).")

## Saving the graph
ggsave(file = "/cloud/project/code/00-UPD/fig_2_2_gg_upd.jpeg",
       plot = fig_2_2_gg_upd,
       device = "jpeg",
       height = 750/96, width = 1500/96, dpi = 96)
