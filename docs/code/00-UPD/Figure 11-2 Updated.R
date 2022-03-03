## Packages and themes
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
library(curl)
library(janitor)

# Next, we load the plotting theme
source("/cloud/project/code/00-UPD/CBN Theme.R")

## Drawing the data
# I draw these figures straight from the online file
ons_url <- "https://www.ons.gov.uk/visualisations/dvc1836/fig1/datadownload.xlsx"
ons_range <- "A7:E119"
ons_first_date <- as_date("2020-01-03")
ons_last_date <- as_date("2022-02-18")
ons_subtitle <- "Number of deaths registered by week in England and Wales. Registrations are between 28th December 2019 and 18th February 2022. Registrations are influenced by public holidays."
ons_caption <- "Source: Office for National Statistics: Deaths registered weekly in England and Wales, provisional: week ending 18th February 2022."

temp <- tempfile()
temp <- curl_download(url = ons_url, destfile = temp,
                      quiet = TRUE, mode = "wb")

ons_deathreg_df <- read_excel(temp, sheet = "Data",
                              range = ons_range) %>%
  janitor::clean_names() %>%
  rename(deaths_not_involving = 3,
         all_deaths_5yr_avg = 4) %>%
  mutate(week_end_date = seq(ons_first_date, ons_last_date,
                             by = "7 days"))

ons_5yr_df <- ons_deathreg_df %>%
  dplyr::select(week_end_date, all_deaths_5yr_avg)

ons_deathreg_tidy_df <- ons_deathreg_df %>%
  dplyr::select(week_end_date, deaths_not_involving, covid_19) %>%
  pivot_longer(cols = 2:3,
               names_to = "ons_measure",
               values_to = "registration_count")

ons_date_breaks <- seq(ons_first_date + days(14),
                       ons_last_date - days(14),
                       by = "13 weeks")

## Making the graph
fig_11_2_gg_upd <- ons_deathreg_tidy_df %>%
  ggplot(aes(x = week_end_date)) +
  geom_bar(aes(y = registration_count,
               fill = ons_measure),
           position = "stack",
           stat = "identity") +
  geom_errorbarh(data = ons_5yr_df,
                 aes(xmin = week_end_date - days(3),
                     xmax = week_end_date + days(3),
                     y = all_deaths_5yr_avg),
                 colour = "#222220", stat = "identity",
                 size = 1.5, height = 0) +
  scale_x_date(date_labels = "%d-%b\n%Y",
               breaks = ons_date_breaks,
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(),
                     limits = c(0, 25000),
                     expand = c(0,0)) +
  scale_fill_manual(guide = "none",
                    values = c("#ec6752", "#15c6d4")) +
  labs(title = "In 2020, death registrations exceeded the 2015-2019 average in two extended periods.",
       subtitle = str_wrap(ons_subtitle, 110),
       x = "Week end date (Friday)",
       y = "Weekly death registrations",
       caption = ons_caption) +
  annotate("text", x = as_date("2020-06-12"), y = 17000,
           label = "Deaths involving Covid-19", hjust = 0,
           family = "Freight Text Pro", size = 7,
           fontface = "bold", colour = "#ec6752") +
  annotate("text", x = as_date("2020-06-12"), y = 15000,
           label = "Other deaths", hjust = 0,
           family = "Freight Text Pro", size = 7,
           fontface = "bold", colour = "#15c6d4") +
  annotate("text", x = as_date("2021-04-11"), y = 15000,
           label = "In 2020-21, the past average is 2015-2019.\nIn 2022, it is 2016-2019 and 2021.",
           hjust = 0, size = 6, fontface = "bold",
           colour = "black", family = "Freight Text Pro") +
  annotate("curve", x = as_date("2021-04-10"), xend = as_date("2021-04-02"),
           y = 15000, yend = 11500, curvature = 0.2,
           arrow = arrow(length = unit(2, "mm")), colour = "black")


## Saving the graph
ggsave(file = "/cloud/project/code/00-UPD/fig_11_2_gg_upd.jpeg",
       plot = fig_11_2_gg_upd,
       device = "jpeg",
       height = 800/96, width = 1600/96, dpi = 96)
