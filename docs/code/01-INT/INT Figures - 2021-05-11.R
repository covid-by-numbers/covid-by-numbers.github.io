## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)
library(patchwork)
library(scales)
library(zoo)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Drawing the data
# Draw the 'dashboard' figures from a prepared file
phe_dashboard_df <- read_excel("R/COVID By Numbers/INT Figures/INT Figures - 2021-05-11.xlsx", 
                               sheet = "DATA-1")

phe_dashboard_tidy_df <- phe_dashboard_df %>%
  mutate(date = as_date(date)) %>%
  pivot_longer(cols = 5:9,
               names_to = "measures",
               values_to = "count")
# The measure names are:
# newAdmissions, newCasesByPublishDate, newDeaths28DaysByPublishDate,
# newPCRTestsByPublishDate, newVaccinesGivenByPublishDate

dashboard_date_min <- min(phe_dashboard_df$date) %>% as_date()
dashboard_date_max <- max(phe_dashboard_df$date) %>% as_date()

# We draw global statistics via Our World in Data
# https://ourworldindata.org/explorers/coronavirus-data-explorer
owid_cases_df <- read_excel("R/COVID By Numbers/INT Figures/INT Figures - 2021-05-11.xlsx", 
                            sheet = "DATA-2") %>%
  select(location, date, new_cases_smoothed) %>%
  filter(location %in% c("Asia", "Europe", "North America", "World")) %>%
  mutate(date = as_date(date))

owid_cases_7da_df <- owid_cases_df %>%
  pivot_wider(names_from = location, values_from = new_cases_smoothed) %>%
  rename(North_America = 4) %>%
  mutate(Rest_of_the_World = World - North_America - Europe - Asia) %>%
  filter(date >= as_date("2020-01-29")) %>%
  select(-World) %>%
  rename("North America" = 4, "Rest of the World" = 5) %>%
  pivot_longer(cols = 2:5, names_to = "location", values_to = "new_cases_7da")

owid_deaths_df <- read_excel("R/COVID By Numbers/INT Figures/INT Figures - 2021-05-11.xlsx", 
                              sheet = "DATA-2") %>%
  select(iso_code, location, date,
         new_deaths, new_deaths_smoothed, new_deaths_smoothed_per_million, total_deaths) %>%
  filter(location %in% c("United Kingdom", "United States", "Italy")) %>%
  mutate(date = as_date(date),
         new_deaths_smoothed_per_onehundredthousand = new_deaths_smoothed_per_million/10)

# Create a plotting function for the dashboard
phe_plot_function <- function(measure_name, plot_title, plot_colour){
  phe_measure_tidy_df <- phe_dashboard_tidy_df %>%
    filter(measures == measure_name) %>%
    drop_na() %>%
    arrange(date) %>%
    mutate(count_7da = zoo::rollmean(count, k = 7, fill = NA, align = "right"))
  
  phe_measure_date_max <- max(phe_measure_tidy_df$date)
  
  phe_measure_gg <- phe_measure_tidy_df %>%
    ggplot(aes(x = date)) +
    geom_col(aes(y = count),
             fill = plot_colour,
             width = 1, alpha = 0.2,
             position = "identity", na.rm = TRUE) +
    geom_line(aes(y = count_7da),
              colour = plot_colour, size = 1.5, na.rm = TRUE) +
    geom_point(data = filter(phe_measure_tidy_df, date == phe_measure_date_max),
               aes(x = date, y = count_7da),
               size = 5, colour = plot_colour) +
    geom_text(data = filter(phe_measure_tidy_df, date == phe_measure_date_max),
              aes(x = date, y = count_7da,
                  label = scales::comma(round(count_7da, digits = 0))),
              size = 7, vjust = -2, hjust = 1,
              fontface = "bold", colour = plot_colour) +
    scale_x_date(limits = c(dashboard_date_min, dashboard_date_max),
                 date_breaks = "3 months",
                 date_labels = "%d-%b\n%Y",
                 expand = c(0.02,0)) +
    scale_y_continuous(breaks = pretty_breaks(),
                       labels = label_comma(),
                       expand = c(0,0)) +
    theme(plot.subtitle = element_text(size = 20, face = "bold",
                                       colour = plot_colour)) +
    labs(subtitle = plot_title,
         x = "Publication date",
         y = "")
  return(phe_measure_gg)
}

owid_plot_function <- function(measure_name, plot_title){
  owid_plot_gg <- owid_deaths_df %>%
    ggplot(aes(x = date,
               y = measure_name,
               group = location)) +
    geom_line(aes(colour = location),
              size = 1.5, na.rm = TRUE) +
    scale_x_date(date_breaks = "6 months",
                 date_labels = "%d-%b\n%Y",
                 expand = c(0.02, 0)) +
    scale_y_continuous(breaks = pretty_breaks(),
                       label = label_comma(),
                       expand = c(0,0)) +
    scale_colour_manual(name = "Country",
                        values = c("grey80", "grey40", "black")) +
    theme(plot.subtitle = element_text(size = 20, face = "bold")) +
    labs(subtitle = plot_title,
         x = "Publication date",
         y = "")
  return(owid_plot_gg)
}

# That creates our dashboard
phe_dashboard_tests_gg <- phe_plot_function(
  measure_name = "newPCRTestsByPublishDate",
  plot_title = "New PCR tests",
  plot_colour = "grey75")

phe_dashboard_vaccines_gg <- phe_plot_function(
  measure_name = "newVaccinesGivenByPublishDate",
  plot_title = "New Covid vaccine doses",
  plot_colour = "grey60")

phe_dashboard_cases_gg <- phe_plot_function(
  measure_name = "newCasesByPublishDate",
  plot_title = "New confirmed Covid cases",
  plot_colour = "grey50")

phe_dashboard_admissions_gg <- phe_plot_function(
  measure_name = "newAdmissions",
  plot_title = "New Covid hospital admissions",
  plot_colour = "grey25")

phe_dashboard_deaths_gg <- phe_plot_function(
  measure_name = "newDeaths28DaysByPublishDate",
  plot_title = "New confirmed Covid deaths",
  plot_colour = "black")

phe_dashboard_gg <- (phe_dashboard_cases_gg + phe_dashboard_admissions_gg) /
  (phe_dashboard_deaths_gg + phe_dashboard_vaccines_gg) +
  plot_annotation(title = "Better tracked with more testing, confirmed Covid cases peaked in the second winter wave.",
                  subtitle = "Covid-19 public health surveillance measures for the United Kingdom, by publication date, with a seven-day rolling average. There are lags between occurrence and publication.",
                  caption = "Source: Public Health England COVID-19 dashboard data download.")

# This create a stacked area graph of confirmed cases around the world
owid_cases_7da_gg <- owid_cases_7da_df %>%
  ggplot(aes(x = date,
             y = new_cases_7da,
             group = location)) +
  geom_area(aes(fill = location),
            position = position_stack(reverse = TRUE)) +
  scale_fill_manual(name = "Continent",
                    values = c("black", "grey25", "grey50", "grey75")) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(),
                     expand = c(0,0)) +
  labs(title = "Confirmed Covid cases quickly spread out of Asia, to Europe and North America.",
       subtitle = "Rolling seven-day average of confirmed Covid-19 cases by continent, by publication date.",
       caption = "Source: Our World in Data Coronavirus data explorer (Johns Hopkins University CSSE COVID-19 Data).",
       x = "Publication date",
       y = "Rolling seven-day average of confirmed cases")

# This is a patchwork graph of different ways of showing reports
owid_deaths_gg1 <- owid_plot_function(measure_name = owid_deaths_df$new_deaths,
                                      plot_title = "New reported Covid-19 deaths")

owid_deaths_gg2 <- owid_plot_function(measure_name = owid_deaths_df$new_deaths_smoothed,
                                      plot_title = "7-day rolling average")

owid_deaths_gg3 <- owid_plot_function(measure_name = owid_deaths_df$new_deaths_smoothed_per_onehundredthousand,
                                      plot_title = "7-day rolling average per 100,000 people")

owid_deaths_gg4 <- owid_deaths_df %>%
  filter(new_deaths_smoothed > 0) %>%
  ggplot(aes(x = date,
             y = new_deaths_smoothed,
             group = location)) +
  geom_line(aes(colour = location),
            size = 1.5, na.rm = TRUE) +
  scale_x_date(date_breaks = "6 months",
               date_labels = "%d-%b\n%Y",
               expand = c(0.02, 0)) +
  scale_y_continuous(trans = "log10",
                     labels = label_comma(accuracy = 1),
                     limits = c(1, 5000)) +
  scale_colour_manual(name = "Country",
                      values = c("grey80", "grey40", "black")) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "7-day rolling average, on a log scale",
       x = "Publication date",
       y = "")

owid_deaths_gg <- (owid_deaths_gg1 + owid_deaths_gg2) /
  (owid_deaths_gg3 + owid_deaths_gg4) +
  plot_annotation(title = "There are different ways of showing statistics for reported Covid deaths.",
                  subtitle = "New reported Covid-19 surveillance deaths by country. Surveillance deaths are not a complete measure of all Covid-19 deaths.",
                  caption = "Source: Our World in Data Coronavirus data explorer (Johns Hopkins University CSSE COVID-19 Data).") +
  plot_layout(guides = "collect")

## Saving the graphs
# Save the graphs in the required dimensions
ggsave(file = "R/COVID By Numbers/INT Figures/INT_phe_dashboard_gg.jpeg",
       plot = phe_dashboard_gg,
       device = "jpeg",
       height = 10, width = 20)

ggsave(file = "R/COVID By Numbers/INT Figures/INT_owid_cases_7da_gg.jpeg",
       plot = owid_cases_7da_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/INT Figures/INT_owid_deaths_gg.jpeg",
       plot = owid_deaths_gg,
       device = "jpeg",
       height = 10, width = 15)