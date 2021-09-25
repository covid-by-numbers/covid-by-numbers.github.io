## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
library(readr)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Drawing and tidying data
# I draw a table collated by Our World in Data from a prepared file
# https://ourworldindata.org/excess-mortality-covid
usa_deaths_df <- read_excel("R/COVID By Numbers/COU Figures/COU Figures - 2021-03-28.xlsx",
                            sheet = "DATA-1", col_types = c("text","text", "date", "numeric",
                                                            "numeric", "numeric", "numeric", "numeric",
                                                            "numeric", "numeric", "numeric")) %>%
  mutate(Day = as_date(Day)) %>%
  filter(Code == "USA",
         Day < "2021-01-01")

usa_summ_df <- usa_deaths_df %>%
  select(Day, 6:10) %>%
  pivot_longer(2:6,
               names_to = "owid_measure",
               values_to = "deaths_all_ages") %>%
  group_by(Day) %>%
  summarise(deaths_2015_2019_min = min(deaths_all_ages, na.rm = TRUE),
            deaths_2015_2019_max = max(deaths_all_ages, na.rm = TRUE))

usa_deaths_tidy_df <- full_join(usa_deaths_df, usa_summ_df, by = "Day") %>%
  select(Day, average_deaths_2015_2019_all_ages, deaths_2020_all_ages,
         deaths_2015_2019_min, deaths_2015_2019_max) %>%
  pivot_longer(2:3,
               names_to = "owid_measure",
               values_to = "deaths_all_ages")

# We can change the factor
usa_deaths_tidy_df$owid_measure <- factor(usa_deaths_tidy_df$owid_measure,
                                          levels = c("deaths_2020_all_ages", "average_deaths_2015_2019_all_ages"))

# I draw the two data frames for countries and cities in the ONS analysis
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/articles/comparisonsofallcausemortalitybetweeneuropeancountriesandregions/2020
ons_countries_df <- read_excel("R/COVID By Numbers/COU Figures/COU Figures - 2021-03-28.xlsx",
                            sheet = "DATA-2") %>%
  rename(area = country)

ons_cities_df <- read_excel("R/COVID By Numbers/COU Figures/COU Figures - 2021-03-28.xlsx",
                            sheet = "DATA-3") %>%
  rename(area = city)

# This table gives the week end dates for the period
ons_week_tbl <- tibble(week_number = 1:53,
                       seq.Date(from = as_date("2020-01-03"),
                                to = as_date("2021-01-01"),
                                by = "week")) %>%
  rename(week_end_date = 2)

# We then bind the rows of the tables, and tidy
ons_tidy_df <- bind_rows(ons_countries_df, ons_cities_df) %>%
  pivot_longer(cols = 4:56,
               names_to = "week_number",
               values_to = "rASMR") %>%
  drop_na() %>%
  mutate(week_number = parse_number(week_number)) %>%
  full_join(y = ons_week_tbl, by = "week_number")

## Creating graphs
# First, I make an ONS-style graph for the CDC statistics for United States mortality
usa_date_breaks <- c("2020-01-12", "2020-04-05", "2020-06-28", "2020-09-20", "2020-12-13") %>%
  as_date()

usa_deaths_label <- usa_deaths_tidy_df %>%
  filter(Day == as_date("2020-06-28"),
         owid_measure == "deaths_2020_all_ages")

usa_deaths_gg <- usa_deaths_tidy_df %>%
  ggplot(aes(x = Day)) +
  geom_line(aes(y = deaths_all_ages,
                colour = owid_measure,
                linetype = owid_measure),
            size = 1.5) +
  geom_ribbon(aes(ymin = deaths_2015_2019_min,
                  ymax = deaths_2015_2019_max,
                  fill = owid_measure),
              alpha = 0.3) +
  scale_linetype_manual(values = c("solid", "dotted")) +
  scale_fill_manual(values = c("grey10", "white")) +
  scale_colour_manual(values = c("black", "grey30")) +
  scale_x_date(date_labels = "%d-%b\n%Y",
               breaks = usa_date_breaks,
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(),
                     limits = c(0,90000)) +
  geom_text(data = usa_deaths_label,
            aes(x = Day, y = deaths_all_ages),
            label = "Deaths in 2020",
            size = 7, vjust = -2.5,
            fontface = "bold", colour = "black") +
  geom_text(data = usa_deaths_label,
            aes(x = Day, y = deaths_all_ages),
            label = "Average and range of deaths\n2015 to 2019",
            vjust = 2.5, size = 7,
            colour = "grey30", fontface = "bold") +
  theme(legend.position = "none") +
  labs(title = "Once US death numbers elevated, they did not return to the past range in 2020.",
       subtitle = "Estimated all-cause mortality in the United States, by week from CDC modelled estimates. These figures may be revised.",
       x = "Week end date",
       y = "Weekly deaths from all causes",
       caption = "Source: Our World in Data (using CDC estimates): Excess mortality during the Coronavirus pandemic (COVID-19).")

# The latter two graphs are of relative age-standardised mortality rates in countries and cities
selected_countries <- c("Poland", "Spain", "Belgium", "Bulgaria", "Czechia",
                        "France", "Slovenia", "Netherlands", "United Kingdom")

selected_cities <- c("Madrid", "Barcelona", "Brussels", "Birmingham", "Paris",
                     "Manchester", "Glasgow City", "London", "Stockholm")

ons_analysis_caption <- "Source: Office for National Statistics: Comparisons of all-cause mortality between European countries and regions: 2020."
ons_date_breaks <- c("2020-01-03", "2020-06-26") %>% as_date()

# This graph is for selected countries
ons_countries_gg <- ons_tidy_df %>%
  filter(area %in% selected_countries,
         week_number <= 51) %>%
  ggplot(aes(x = week_end_date,
             y = rASMR,
             group = age_group)) +
  geom_line(aes(colour = age_group),
            size = 1.5) +
  facet_wrap(~area) +
  scale_x_date(date_labels = "%d-%b",
               breaks = ons_date_breaks) +
  scale_y_continuous(limits = c(-50, 150),
                     expand = c(0,0)) +
  scale_colour_manual(name = "",
                      values = c("black", "grey")) +
  geom_hline(yintercept = 0, colour = "grey50",
             linetype = "dashed", size = 1.1) +
  labs(title = "Spain had the highest peak in relative age-standardised mortality in 2020.",
       subtitle = "Relative age-standardised mortality rates [%] by age group for selected European countries. This is for weeks 1 to 51 in 2020.",
       x = "Week end date",
       y = "",
       caption = ons_analysis_caption)

# This graphs is for selected cities
ons_cities_gg <- ons_tidy_df %>%
  filter(area %in% selected_cities,
         week_number <= 51) %>%
  ggplot(aes(x = week_end_date,
             y = rASMR,
             group = age_group)) +
  geom_line(aes(colour = age_group),
            size = 1.5) +
  facet_wrap(~area) +
  scale_x_date(date_labels = "%d-%b",
               breaks = ons_date_breaks) +
  scale_y_continuous(limits = c(-50, 500),
                     expand = c(0,0)) +
  scale_colour_manual(name = "",
                      values = c("black", "grey")) +
  geom_hline(yintercept = 0, colour = "grey50",
             linetype = "dashed", size = 1.1) +
  labs(title = "Madrid had the highest peak excess mortality of large cities during Spring 2020.",
       subtitle = "Relative age-standardised mortality rates [%] by age group for selected European cities. This is for weeks 1 to 51 in 2020.",
       x = "Week end date",
       y = "",
       caption = ons_analysis_caption)

## Saving graphs
# Save the figures in the required dimensions
ggsave(file = "R/COVID By Numbers/COU Figures/COU_usa_deaths_gg.jpeg",
       plot = usa_deaths_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/COU Figures/COU_ons_countries_gg.jpeg",
       plot = ons_countries_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/COU Figures/COU_ons_cities_gg.jpeg",
       plot = ons_cities_gg,
       device = "jpeg",
       height = 10, width = 15)