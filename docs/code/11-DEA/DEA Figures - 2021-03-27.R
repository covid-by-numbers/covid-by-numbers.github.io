## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)

# You may need to install the next package straight from GitHub
# devtools::install_github("wilkelab/ungeviz")
library(ungeviz)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw and tidy data
# I draw a table of  death measures under different measures from a prepared file
phe_deaths_df <- read_excel("R/COVID By Numbers/DEA Figures/DEA Figures - 2021-03-27.xlsx", 
                            sheet = "DATA-1", col_types = c("text", "text", "text", "date", "numeric", 
                                                            "numeric", "numeric", "numeric"))

phe_deaths_tidy_df <- phe_deaths_df %>%
  mutate(date = as_date(date)) %>%
  select("date", 5:8) %>%
  pivot_longer(cols = 2:5,
               names_to = "measure",
               values_to = "death_count")

# We need to set the factors, so the measures are in the right order
phe_deaths_tidy_df$measure <- factor(phe_deaths_tidy_df$measure,
                                     levels = c("newDailyNsoDeathsByDeathDate", "newDeaths28DaysByDeathDate",
                                                "newDeaths60DaysByDeathDate", "newDeathsByDeathDate"))

phe_deaths_tidy_df %>% filter(date >= as_date("2020-03-07"),
                              date <= as_date("2021-03-19")) %>%
  group_by(measure) %>%
  summarise(sum_count = sum(death_count))

#   measure                      sum_count
# <fct>                            <dbl>
# 1 newDailyNsoDeathsByDeathDate    129911
# 2 newDeaths28DaysByDeathDate      111199
# 3 newDeaths60DaysByDeathDate      127526
# 4 newDeathsByDeathDate            140693

# I draw a frame of death measures by date of report
phe_report_df <- read_excel("R/COVID By Numbers/DEA Figures/DEA Figures - 2021-03-27.xlsx", 
                            sheet = "DATA-2",
                            col_types = c("text", "text", "text", "date",
                                          "numeric", "numeric", "numeric"))

phe_report_tidy_df <- phe_report_df %>%
  mutate(date = as_date(date),
         week_floor_date = floor_date(date, "weeks", week_start = 1),
         week_floor_date = as_date(date)) %>%
  select(date, week_floor_date, newDeaths28DaysByPublishDate) %>%
  drop_na() %>%
  mutate(week_day = weekdays(date))

phe_report_tidy_df$week_day <- factor(phe_report_tidy_df$week_day,
                                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                                 "Friday", "Saturday", "Sunday"))

# I draw a frame of death registrations for England and Wales (ONS), in a prepared file
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/bulletins/deathsregisteredweeklyinenglandandwalesprovisional/weekending23april2021
ons_deathreg_df <- read_excel("R/COVID By Numbers/DEA Figures/DEA Figures - 2021-03-27.xlsx",
                              sheet = "DATA-3",
                              col_types = c("date", "numeric", "numeric",
                                            "numeric", "numeric", "numeric"))

ons_deathreg_tidy_df <- ons_deathreg_df %>%
  mutate(week_end_date = as_date(week_end_date)) %>%
  select("week_end_date", "deaths_not_involving_covid19",
         "deaths_involving_covid19", "all_deaths_5yr_avg") %>%
  pivot_longer(cols = 2:3,
               names_to = "ons_measure",
               values_to = "registration_count") %>%
  dplyr::mutate(all_deaths_5yr_avg = case_when(
    ons_measure == "deaths_involving_covid19" ~ NA_real_,
    TRUE ~ all_deaths_5yr_avg))

# I draw a frame of excess deaths by location in England and Wales
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/bulletins/deathsregisteredweeklyinenglandandwalesprovisional/weekending23april2021
ons_deathlocation_df <- read_excel("R/COVID By Numbers/DEA Figures/DEA Figures - 2021-03-27.xlsx", 
                                   sheet = "DATA-4",
                                   col_types = c("date", "numeric", "numeric", "numeric",
                                                 "numeric", "numeric", "numeric"))

ons_deathlocation_tidy_df <- ons_deathlocation_df %>%
  mutate(week_end_date = as_date(week_end_date)) %>%
  select(week_end_date, 4:7) %>%
  pivot_longer(cols = 2:5,
               names_to = "location",
               values_to = "excess_deaths")

ons_deathlocation_tidy_df$location <- factor(ons_deathlocation_tidy_df$location,
                                             levels = c("Home", "Hospital (acute or community)",
                                                        "Care home", "Other"))

## Create the graphs
phe_caption_source <- "Source: Public Health England COVID-19 Dashboard data download."

phe_deaths_gg <- phe_deaths_tidy_df %>%
  filter(measure != "newDeathsByDeathDate",
         date <= as_date("2021-03-31"),
         date >= as_date("2020-03-01")) %>%
  ggplot(aes(x = date,
             y = death_count,
             group = measure)) +
  geom_line(aes(colour = measure),
            size = 1.5, na.rm = TRUE) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma()) +
  scale_colour_manual(name = "",
                      labels = c("Death registrations involving COVID-19 (ONS)",
                                 "Within 28 days (PHE)",
                                 "Within 60 days, or COVID-19 on the certificate (PHE)"),
                      values = c("black", "grey40", "grey80")) +
  labs(title = "For England, COVID-19 death registrations are higher than PHE surveillance death counts.",
       subtitle = "COVID-19 related deaths by date of death in England, under different measures. These provisional counts may be revised.",
       x = "Date of death",
       y = "Count of deaths",
       caption = phe_caption_source) +
  annotate("text", x = as_date("2020-05-01"), y = 1200,
           label = "Due to limited testing,\ndeath registrations were higher early in the pandemic.",
           size = 7, hjust = 0) +
  annotate("curve", x = as_date("2020-04-30"), xend = as_date("2020-04-21"),
           y = 1150, yend = 1010,
           arrow = arrow(), colour = "black",
           curvature = 0.2, size = 1.5)

phe_report_gg <- phe_report_tidy_df %>%
  ggplot(aes(x = week_floor_date,
             y = newDeaths28DaysByPublishDate)) +
  geom_line(size = 1.5, colour = "black") +
  facet_wrap(~week_day,
             ncol = 4) +
  scale_x_date(date_breaks = "6 months",
               date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Reported deaths show a weekly cycle: Tuesday and Wednesday are higher than other days.",
       subtitle = "Deaths in England within 28 days of a positive test for SARS-CoV-2. These figures are by publication week.",
       x = "Week start date (Monday) of publication",
       y = "Deaths within 28 days of a positive test",
       caption = phe_caption_source)
  
phe_deaths_gg <- phe_deaths_tidy_df %>%
  filter(date <= as_date("2021-03-21")) %>%
  ggplot(aes(x = date,
             y = death_count,
             group = phe_measure)) +
  geom_line(aes(colour = phe_measure),
            size = 1.5) +
  scale_x_date(date_labels = "%d-%b-%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma()) +
  scale_colour_grey(name = "Measures",
                    labels = c("Within 28 days",
                               "Within 60 days, or COVID-19 on the death certificate",
                               "No time restriction")) +
  labs(title = "Public Health England changed their lead measure to deaths within 28 days of a positive test.",
       subtitle = "COVID-19 related deaths by date of death in England, under different measures. These provisional figures may be revised.",
       x = "Date of death",
       y = "",
       caption = "Source: Public Health England COVID-19 Dashboard data download.") +
  annotate("text", x = as_date("2020-07-01"), y = 500,
           label = "The count of deaths within 28 days of a positive test\ndiverges from the other measures in mid-April",
           size = 6) +
  annotate("curve", x = as_date("2020-07-01"), xend = as_date("2020-05-23"),
           y = 400, yend = 300,
           arrow = arrow(), colour = "black",
           curvature = -0.2, size = 1.5)

# The second graph replicates Figure 3 in the ONS weekly deaths report
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/bulletins/deathsregisteredweeklyinenglandandwalesprovisional/weekending12march2021
ons_date_breaks <- c("2020-01-10", "2020-03-19", "2020-05-29", "2020-08-07",
                    "2020-10-16", "2020-12-25", "2021-03-19") %>%
  as_date()

ons_caption_source <- "Source: Office for National Statistics: Deaths registered weekly in England and Wales, provisional: week ending 23 April 2021."

ons_deathreg_gg_base <- ons_deathreg_tidy_df %>%
  ggplot(aes(x = week_end_date)) +
  geom_bar(aes(y = registration_count,
               fill = ons_measure),
           position = "stack",
           stat = "identity") +
  geom_hpline(aes(x = week_end_date,
                  y = all_deaths_5yr_avg,
                  colour = factor(ons_measure)),
              stat = "identity",
              width = 6, size = 2, na.rm = TRUE) +
  scale_x_date(date_labels = "%d-%b\n%Y",
               breaks = ons_date_breaks,
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(),
                     limits = c(0, 25000),
                     expand = c(0,0)) +
  scale_colour_manual(name = "",
                      labels = c("2015-2019 average", ""),
                      values = c("black", "black")) +
  theme(legend.margin = margin(0, -1.5, 0, 0, unit = "cm")) +
  labs(title = "Death registrations exceeded the 2015-2019 average in two extended periods.",
       subtitle = "Number of deaths registered by week in England and Wales. Registrations are between 28th December 2019 and 23rd April 2021.",
       x = "Week end date",
       y = "Weekly death registrations",
       caption = ons_caption_source) +
  geom_text(x = as_date("2021-01-15"), y = 21000,
            label = "Bank holidays\naffected registrations",
            size = 6) +
  geom_curve(x = as_date("2021-01-01"), xend = as_date("2020-12-30"),
             y = 19000, yend = 13000,
             arrow = arrow(), curvature = 0.2, size = 1.2) +
  geom_curve(x = as_date("2021-02-20"), xend = as_date("2021-04-02"),
             y = 19000, yend = 12000,
             arrow = arrow(), curvature = -0.2, size = 1.2)

ons_deathreg_gg <- ons_deathreg_gg_base + 
  scale_fill_manual(name = "",
                    labels = c("Deaths involving COVID-19",
                               "Deaths not involving COVID-19"),
                    values = c("grey40", "grey80"))

ons_deathreg_gg_col <- ons_deathreg_gg_base + 
  scale_fill_manual(name = "",
                    labels = c("Deaths involving COVID-19",
                               "Deaths not involving COVID-19"),
                    values = c("#800000", "#008080"))

ons_deathlocation_gg <- ons_deathlocation_tidy_df %>%
  ggplot(aes(x = week_end_date,
             y = excess_deaths)) +
  geom_col() +
  facet_wrap(~location) +
  scale_x_date(date_labels = "%d-%b\n%Y",
               breaks = ons_date_breaks,
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma()) +
  labs(title = "Deaths at home continue to be above the 2015-2019 average.",
       subtitle = "Excess deaths in England and Wales are registered deaths minus the 2015-2019 average for those weeks.",
       x = "Week end date",
       y = "Excess deaths (weekly deaths minus the 2015-2019 average)",
       caption = ons_caption_source)

## Saving the graphs
# Save the figures in the required dimensions
ggsave(file = "R/COVID By Numbers/DEA Figures/DEA_phe_deaths_gg.jpeg",
       plot = phe_deaths_gg,
       device = "jpeg",
       height = 12, width = 17)

ggsave(file = "R/COVID By Numbers/DEA Figures/DEA_phe_report_gg.jpeg",
       plot = phe_report_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/DEA Figures/DEA_ons_deathreg_gg.jpeg",
       plot = ons_deathreg_gg,
       device = "jpeg",
       height = 10, width = 16)

ggsave(file = "R/COVID By Numbers/DEA Figures/DEA_ons_deathreg_gg_col.jpeg",
       plot = ons_deathreg_gg_col,
       device = "jpeg",
       height = 10, width = 16)

ggsave(file = "R/COVID By Numbers/DEA Figures/DEA_ons_deathlocation_gg.jpeg",
       plot = ons_deathlocation_gg,
       device = "jpeg",
       height = 10, width = 15)