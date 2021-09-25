## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)
library(ggrepel)
library(zoo)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw the data
# I draw the cases and tests values from a prepared file
# This is based on the Our World in Data COVID-19 CSV
# https://ourworldindata.org/covid-cases#confirmed-cases
# The countries for this comparison are the UK (GBR), US (USA), Germany (DEU), and Italy (ITA)
owid_covid_df <- read_excel("R/COVID By Numbers/CAS Figures/CAS Figures - 2021-04-05.xlsx",
                            sheet = "DATA-1") %>%
  mutate(date = as_date(date))

owid_casetest_df <- owid_covid_df %>%
  filter(iso_code %in% c("USA", "GBR", "DEU", "ITA"),
         date >= as_date("2020-03-01"),
         date < as_date("2021-05-01")) %>%
  mutate(cases_7dayavg_per100000 = new_cases_smoothed_per_million*0.1,
         tests_7dayavg_per100000 = new_tests_smoothed_per_thousand*1000)

owid_casetest_end <- owid_casetest_df %>%
  filter(date == as_date("2021-04-30"))

owid_continent_df <- owid_covid_df %>%
  filter(location %in% c("Asia", "North America", "South America", "Europe"),
         date >= as_date("2020-03-01"),
         date < as_date("2021-05-01")) %>%
  mutate(cases_7dayavg_per100000 = new_cases_smoothed_per_million*0.1)

owid_continent_end <- owid_continent_df %>%
  filter(date == as_date("2021-04-30"))

# We draw data on testing in England
# https://coronavirus.data.gov.uk/details/testing?areaType=nation&areaName=England
phe_tests2_df <- read_excel("R/COVID By Numbers/CAS Figures/CAS Figures - 2021-04-05.xlsx", 
                            sheet = "DATA-3")

phe_tests3_df <- read_excel("R/COVID By Numbers/CAS Figures/CAS Figures - 2021-04-05.xlsx", 
                            sheet = "DATA-4")

phe_tests_uk_df <- read_excel("R/COVID By Numbers/CAS Figures/CAS Figures - 2021-04-05.xlsx", 
                            sheet = "DATA-5") %>%
  mutate(date = as_date(date)) %>%
  filter(date <= as_date("2021-05-10")) %>%
  arrange(date) %>%
  mutate(newPCRTestsByPublishDate_7da = rollmean(newPCRTestsByPublishDate,
                                                 k = 7, fill = NA, align = "right")) %>%
  select(areaName, date, newPCRTestsByPublishDate, newPCRTestsByPublishDate_7da)

# I then join these together
phe_tests_eng_df <- full_join(phe_tests2_df, phe_tests3_df,
                          by = c("areaType", "areaName", "areaCode", "date")) %>%
  arrange(date) %>%
  mutate(date = as_date(date),
         newLFDTests_7da = rollmean(newLFDTests, k = 7, fill = NA, align = "right"),
         newPCRTestsByPublishDate_7da = rollmean(newPCRTestsByPublishDate,
                                                 k = 7, fill = NA, align = "right"))

phe_tests_pcr_df <- phe_tests_eng_df %>%
  select(areaName, date, newPCRTestsByPublishDate, newPCRTestsByPublishDate_7da) %>%
  bind_rows(phe_tests_uk_df)

## Create the graphs
# The first graph is for tests
owid_covid_gg1 <- owid_casetest_df %>%
  ggplot(aes(x = date, y = tests_7dayavg_per100000, group = location)) +
  geom_line(aes(colour = location), size = 1.5, na.rm = TRUE) +
  scale_colour_grey(start = 0, end = 0.7, name = "") +
  scale_x_date(date_labels = "%d-%b\n%Y",
               expand = c(0,40)) +
  scale_y_continuous(labels = label_comma()) +
  geom_point(data = owid_casetest_end,
             aes(y = tests_7dayavg_per100000,
                 colour = location),
             size = 3, na.rm = TRUE) +
  geom_text(data = owid_casetest_end,
            aes(label = iso_code,
                colour = location),
            size = 5,
            hjust = -0.2) +
  theme(plot.subtitle = element_text(size = 20, face = "bold"),
        legend.position = "none") +
  labs(title = "COVID-19 tests per 100,000 people",
       x = "Date", y = "")

# The second graph is for cases
owid_covid_gg2 <- owid_casetest_df %>%
  ggplot(aes(x = date, y = cases_7dayavg_per100000, group = location)) +
  geom_line(aes(colour = location), size = 1.5, na.rm = TRUE) +
  scale_colour_grey(start = 0, end = 0.7, name = "") +
  scale_x_date(date_labels = "%d-%b\n%Y",
               expand = c(0,40)) +
  scale_y_continuous(labels = label_comma()) +
  geom_point(data = owid_casetest_end,
             aes(y = cases_7dayavg_per100000,
                 colour = location),
             size = 3, na.rm = TRUE) +
  geom_text(data = owid_casetest_end,
            aes(label = iso_code,
                colour = location),
            size = 5,
            hjust = -0.2) +
  theme(plot.subtitle = element_text(size = 20, face = "bold"),
        legend.position = "none") +
  labs(title = "Confirmed cases per 100,000 people",
       x = "Date", y = "")

# The patchwork package then puts these two graphs together
owid_covid_gg <- owid_covid_gg1 + owid_covid_gg2 +
  plot_annotation(title = "Testing capacities increased, as new found cases declined in some countries.",
                  subtitle = str_wrap("Rolling 7-day averages of COVID-19 tests and confirmed cases, standardised to population. This is for 1st March 2020 to 30th April 2021 in Germany (DEU), Italy (ITA), United Kingdom (GBR) and the United States (USA).", width = 120),
                  caption = "Source: Our World in Data Coronavirus Data Explorer.")

# This graph for cases per 100,000 by continent
owid_continent_gg <- owid_continent_df %>%
  ggplot(aes(x = date, y = cases_7dayavg_per100000, group = location)) +
  geom_line(aes(colour = location),
            size = 1.5, na.rm = TRUE) +
  geom_point(data = owid_continent_end,
             aes(colour = location),
             size = 3) +
  scale_x_date(date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_colour_manual(values = c("grey75", "grey25", "grey50", "black")) +
  facet_wrap(~location) +
  theme(legend.position = "none") +
  labs(title = "Europe and North America recorded high numbers of cases over the winter of 2020.",
       subtitle = "Seven-day rolling average of confirmed cases per 100,000 people. This is for 1st March 2020 to 30th April 2021.",
       x = "Reporting date",
       y = "Seven-day rolling average: confirmed cases per 100,000 people",
       caption = "Source: Our World in Data Coronavirus Data Explorer.")

## This is a patchwork graph for four measures of testing
# newPCRTestsByPublishDate, newLFDTests
# uniquePeopleTestedBySpecimenDateRollingSum, uniqueCasePositivityBySpecimenDateRollingSum

phe_tests_gg1 <- phe_tests_pcr_df %>%
  ggplot(aes(x = date,
             y = newPCRTestsByPublishDate_7da,
             group = areaName)) +
  geom_line(aes(colour = areaName),
            size = 1.5, na.rm = TRUE) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_number(accuracy = 0.5, scale = 1/1e06, suffix = "M"),
                     expand = c(0,0),
                     limits = c(0, 1500000)) +
  geom_text_repel(data = filter(phe_tests_pcr_df,
                                date == as_date("2020-10-01")),
                  aes(y = newPCRTestsByPublishDate_7da,
                      label = areaName, colour = areaName),
                  size = 6, box.padding = 1, segment.color = "NA", seed = "4744") +
  guides(colour = "none") +
  scale_colour_manual(values = c("grey30", "black")) +
  theme(plot.subtitle = element_text(size = 20, face = "bold",
                                     colour = "grey30")) +
  labs(subtitle = "New Covid PCR tests: 7-day average",
       x = "Publication date",
       y = "")

phe_tests_gg2 <- phe_tests_eng_df %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = newLFDTests_7da),
           size = 1.5, colour = "grey60", na.rm = TRUE) +
  geom_line(aes(y = newLFDTests_7da),
            colour = "grey60", size = 1.5, na.rm = TRUE) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_number(accuracy = 0.5, scale = 1/1e06, suffix = "M"),
                     expand = c(0,0),
                     limits = c(0, 1500000)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold",
                                     colour = "grey60")) +
  labs(subtitle = "New lateral flow tests in England: 7-day average",
       x = "Publication date",
       y = "")

phe_tests_gg3 <- phe_tests_eng_df %>%
  ggplot(aes(x = date, y = uniquePeopleTestedBySpecimenDateRollingSum)) +
  geom_col(width = 1, fill = "grey20", position = "identity",
           na.rm = TRUE, alpha = 0.5) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_number(accuracy = 0.5, scale = 1/1e06, suffix = "M"),
                     expand = c(0,0)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold",
                                     colour = "grey20")) +
  labs(subtitle = "PCR-tested people in 7-day periods in England",
       x = "Specimen date",
       y = "")

phe_tests_gg4 <- phe_tests_eng_df %>%
  ggplot(aes(x = date, y = uniqueCasePositivityBySpecimenDateRollingSum)) +
  geom_line(size = 1.5, colour = "black",
           na.rm = TRUE) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_percent(scale = 1),
                     expand = c(0,0),
                     limits = c(0,45)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold",
                                     colour = "black")) +
  labs(subtitle = "PCR positivity in 7-day periods in England",
       x = "Specimen date",
       y = "")

# we put those graphs together
phe_tests_gg <- (phe_tests_gg1 + phe_tests_gg2) /
  (phe_tests_gg3 + phe_tests_gg4) +
  plot_annotation(title = "Covid testing expanded in the UK, with many lab and rapid tests being done each day.",
                  subtitle = "Covid-19 testing measures in the UK and England by publication date (PCR and LFD tests, 7-day averages) and specimen date (positivity).",
                  caption = "Source: Public Health England COVID-19 Dashboard data download.")

## Saving graphs
# Saving those figures in the required dimensions
ggsave(file = "R/COVID By Numbers/CAS Figures/CAS_owid_covid_gg.jpeg",
       plot = owid_covid_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/CAS Figures/CAS_owid_continent_gg.jpeg",
       plot = owid_continent_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/CAS Figures/CAS_phe_tests_gg.jpeg",
       plot = phe_tests_gg,
       device = "jpeg",
       height = 10, width = 15)