## Packages and themes
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
library(curl)
library(janitor)

# You may need to install the next package straight from GitHub
# devtools::install_github("wilkelab/ungeviz")
library(ungeviz)

# Next, we load the plotting theme
source("/cloud/project/code/00-UPD/CBN Theme.R")

## Drawing the data
# I draw these figures straight from the online file
ons_url <- "https://www.ons.gov.uk/visualisations/dvc1593/fig2/datadownload.xlsx"
ons_range <- "A7:E97"
ons_first_date <- as_date("2020-01-03")
ons_last_date <- as_date("2021-09-17")
ons_subtitle <- "Number of deaths registered by week in England and Wales. Registrations are between 28th December 2019 and 17th September 2021."
ons_caption <- "Source: Office for National Statistics: Deaths registered weekly in England and Wales, provisional: week ending 17th September 2021."

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

## Making the graph
fig_11_2_gg_upd <- ons_deathreg_tidy_df %>%
  ggplot(aes(x = week_end_date)) +
  geom_bar(aes(y = registration_count,
               fill = ons_measure),
           position = "stack",
           stat = "identity") +
  geom_hpline(data = ons_5yr_df,
              aes(x = week_end_date,
                  y = all_deaths_5yr_avg,
                  colour = factor(1)),
              stat = "identity",
              width = 6, size = 2, na.rm = TRUE) +
  scale_x_date(date_labels = "%d-%b\n%Y",
               date_breaks = "3 months",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(),
                     limits = c(0, 25000),
                     expand = c(0,0)) +
  scale_colour_manual(name = "",
                      labels = c("2015-2019 average"),
                      values = c("#1c1d1a")) +
  scale_fill_manual(name = "",
                    labels = c("Deaths involving COVID-19",
                               "Deaths not involving COVID-19"),
                    values = c("#ec6752", "#15c6d4")) +
  theme(legend.margin = margin(0, 0, 0, 0, unit = "cm")) +
  labs(title = "In 2020, death registrations exceeded the 2015-2019 average in two extended periods.",
       subtitle = ons_subtitle,
       x = "Week end date",
       y = "Weekly death registrations",
       caption = ons_caption) +
  geom_text(x = as_date("2021-01-15"), y = 21000,
            label = "Bank holidays\naffected registrations",
            size = 6, family = "Freight Text Pro")

## Saving the graph
ggsave(file = "/cloud/project/code/00-UPD/fig_11_2_gg_upd.jpeg",
       plot = fig_11_2_gg_upd,
       device = "jpeg",
       height = 800/96, width = 1600/96, dpi = 96)
