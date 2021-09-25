## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw and tidy data
# I draw a table of the Google mobility index from a prepared file
# https://ourworldindata.org/covid-google-mobility-trends
google_mobility_df <- read_excel("R/COVID By Numbers/BEH Figures/BEH Figures - 2021-04-01.xlsx", 
                                 sheet = "DATA-1",
                                 col_types = c("text", "text","date",
                                               "numeric", "numeric", "numeric",
                                               "numeric", "numeric", "numeric")) %>%
  mutate(Day = as_date(Day)) %>%
  pivot_longer(cols = 4:9,
               names_to = "category",
               values_to = "index")

# We draw police-recorded crime statistics from the ONS
# https://www.ons.gov.uk/peoplepopulationandcommunity/crimeandjustice/bulletins/crimeinenglandandwales/yearendingseptember2020
ons_police_recorded_crime_df <- read_excel("R/COVID By Numbers/BEH Figures/BEH Figures - 2021-04-01.xlsx",
                                           sheet = "DATA-2",
                                           col_types = c("text", "date", "numeric"))

ons_record_crime_tidy_df <- ons_police_recorded_crime_df %>%
  mutate(date_month = as_date(date_month)) %>%
  filter(date_month %in% as_date(c("2019-04-01", "2020-04-01"))) %>%
  pivot_wider(names_from = date_month, values_from = value) %>%
  rename(apr19_index = 2, apr20_index = 3) %>%
  mutate(rel_change = 100*(apr20_index - apr19_index)/apr19_index)

## Create graphs
# The graph shows the change in the UK Google mobility index for three categories
google_mobility_gg <- google_mobility_df %>%
  filter(category %in% c("residential", "parks", "workplaces")) %>%
  ggplot(aes(x = Day,
             y = index,
             colour = category)) +
  geom_line(size = 1.5) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  scale_colour_manual(name = "Category",
                      labels = c("Parks", "Residential (change in time spent)", "Places of work"),
                      values = c("black", "grey40", "grey80")) +
  scale_x_date(date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  labs(title = "Since the start of the pandemic, visitors to workplaces remained under baseline values.",
       subtitle = "Rolling seven-day average of change [%] in visitors (or time spent for residences), between 17th February 2020 and 5th May 2021.",
       x = "Day",
       y = "Relative change [%]",
       caption = "Source: Google COVID-19 Community Mobility Trends, via Our World in Data.")

# The final graph is of the relative change in police-recorded crime
ons_record_crime_gg <- ons_record_crime_tidy_df %>%
  ggplot(aes(y = fct_reorder(category, rel_change),
             x = round(rel_change, digits = 0))) +
  geom_point(size = 5) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  scale_x_continuous(limits = c(-60, 60)) +
  labs(title = "In April 2020, almost all categories of police-recorded crimes decreased compared to 2019.",
       subtitle = str_wrap("Relative change [%] of police-recorded crime levels by category, comparing April 2019 to April 2020 in England and Wales (excludes Greater Manchester Police.)", width = 120),
       x = "Rounded relative change [%] from April 2019 to April 2020",
       y = "Crime categories",
       caption = "Source: Office for National Statistics: Crime in England and Wales: year ending September 2020.") +
  geom_text(x = 22, y = 8.5,
            label = "The increase in drug offenses reflects\nincreased policing activity in hotspots.",
            size = 7) +
  geom_curve(x = 22, xend = 22,
             y = 8.9, yend = 9.8,
             curvature = -0.2, size = 2,
             arrow = arrow(length = unit(0.5, "cm")))

## Saving graphs
# Saving the figures in the required dimensions
ggsave(file = "R/COVID By Numbers/BEH Figures/BEH_google_mobility_gg.jpeg",
       plot = google_mobility_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/BEH Figures/BEH_ons_record_crime_gg.jpeg",
       plot = ons_record_crime_gg,
       device = "jpeg",
       height = 10, width = 15)