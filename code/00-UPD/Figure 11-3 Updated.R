## Packages and themes
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
library(janitor)
library(curl)

# Next, we load the plotting theme
source("/cloud/project/code/00-UPD/CBN Theme.R")

## Drawing the data
# I draw these figures straight from the online file
ons_url <- "https://www.ons.gov.uk/visualisations/dvc1593/fig3/datadownload.xlsx"
ons_range <- "A8:R88"
ons_first_date <- as_date("2020-03-13")
ons_last_date <- as_date("2021-09-17")
ons_subtitle <- "Number of excess deaths by place of occurrence in England and Wales, registered between 7 March 2020 and 17 September 2021."
ons_caption <- "Source: Office for National Statistics: Deaths registered weekly in England and Wales, provisional: week ending 17th September 2021."

temp <- tempfile()
temp <- curl_download(url = ons_url, destfile = temp,
                      quiet = TRUE, mode = "wb")

ons_deathlocation_df <- read_excel(temp, sheet = "data",
                                   range = ons_range) %>%
  janitor::clean_names() %>%
  dplyr::select(year, week_no, starts_with("excess_deaths_")) %>%
  dplyr::rename(home = 3, hospital = 4, care_home = 5, other = 6) %>%
  dplyr::mutate(week_end_date = seq(ons_first_date, ons_last_date,
                             by = "7 days"),
                week_end_date = as_date(week_end_date)) %>%
  tidyr::pivot_longer(cols = 3:6,
                      names_to = "location",
                      values_to = "excess_deaths")

ons_location_names <- c(home = "Home",
                        hospital = "Hospital (acute or community)",
                        care_home = "Care home",
                        other = "Other")

ons_deathlocation_df$location <- factor(ons_deathlocation_df$location,
                                             levels = c("home", "hospital",
                                                        "care_home", "other"))

ons_deathlocation_dates <- c("2020-03-27", "2020-09-11", "2021-03-12", "2021-08-13") %>%
  as_date()

## Making the graph
fig_11_3_gg_upd <- ons_deathlocation_df %>%
  ggplot(aes(x = week_end_date,
             y = excess_deaths)) +
  geom_col(fill = "#15c6d4") +
  facet_wrap(~location,
             labeller = as_labeller(ons_location_names)) +
  scale_x_date(date_labels = "%d-%b\n%Y",
               breaks = ons_deathlocation_dates,
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Deaths at home continue to be above the 2015-2019 average.",
       subtitle = ons_subtitle,
       x = "Week end date",
       y = "Excess deaths (weekly deaths minus the 2015-2019 average)",
       caption = ons_caption)

## Saving the graph
ggsave(file = "/cloud/project/code/00-UPD/fig_11_3_gg_upd.jpeg",
       plot = fig_11_3_gg_upd,
       device = "jpeg",
       height = 800/96, width = 1600/96, dpi = 96)
