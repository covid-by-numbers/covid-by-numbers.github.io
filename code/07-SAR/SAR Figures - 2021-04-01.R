## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw the data
# The first prepared data frame is on weekly estimates of COVID-19 positivity
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/bulletins/coronaviruscovid19infectionsurveypilot/7may2021
ons_positivity_df <- read_excel("R/COVID By Numbers/SAR Figures/SAR Figures - 2021-04-01.xlsx",
                                sheet = "DATA-1",
                                col_types = c("date", "numeric", "numeric",
                                              "numeric", "text", "text", "text")) %>%
  mutate(date = as_date(date))

ons_infection_source <- "Source: Office for National Statistics - COVID-19 infection survey."

# The second comes from modelled estimates of antibodies and vaccinations
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/articles/coronaviruscovid19infectionsurveyantibodydatafortheuk/28april2021
ons_antibody_df <- read_excel("R/COVID By Numbers/SAR Figures/SAR Figures - 2021-04-01.xlsx",
                              sheet = "DATA-2") %>%
  mutate(week_start_date = as_date(week_start_date),
         week_end_date = as_date(week_end_date))

ons_antibody_df$measure <- factor(ons_antibody_df$measure,
                                  levels = c("Antibodies", "Vaccines", "Fully Vaccinated"))

## Creating graphs
# The first graph shows estimated COVID-19 swab positivity by nation over time
ons_positivity_gg <- ons_positivity_df %>%
  ggplot(aes(x = date, y = 100*est_central)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = 100*est_lower,
                    ymax = 100*est_upper),
                size = 1) +
  facet_wrap(~nation) +
  scale_x_date(date_breaks = "12 weeks",
               date_labels = "%d-%b\n%Y") +
  labs(title = "The trend in positivity differs between the nations.",
       subtitle = "Estimated COVID-19 positive rate from nose and throat swabs (with 95% credible intervals), in the community in the UK.",
       x = "Estimated period end date",
       y = "Proportion testing positive [%]",
       caption = ons_infection_source)

# The second graph shows estimated COVID-19 antibody percentages by age in each nation
# The estimates for Northern Ireland use different age bands
ons_antibody_gg <- ons_antibody_df %>%
  ggplot(aes(x = week_end_date,
             y = 100*estimate_central,
             ymin = 100*estimate_lower,
             ymax = 100*estimate_upper,
             group = measure)) +
  geom_line(aes(colour = measure),
            size = 1.5) +
  geom_ribbon(aes(fill = measure),
              alpha = 0.2) +
  facet_wrap(~age_group) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_colour_manual(aesthetics = c("colour", "fill"),
                      values = c("black", "grey60", "grey80"),
                      name = "Modelled estimates (with 95% credible intervals)",
                      labels = c("% testing positive for antibodies",
                                 "% vaccinated (at least one dose)",
                                 "% fully vaccinated (two doses)")) +
  labs(title = "In England, antibody positivity has risen sharply in older age groups.",
       subtitle = "Modelled percentage of adults in England testing positive for antibodies to SARS-CoV-2, and adults receiving vaccination dose, by age group.",
       x = "Week end date",
       y = "Proportion of adults [%]",
       caption = ons_infection_source)

## Saving graphs
# Saving those figures in the required dimensions
ggsave(file = "R/COVID By Numbers/SAR Figures/SAR_ons_positivity_gg.jpeg",
       plot = ons_positivity_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/SAR Figures/SAR_ons_antibody_gg.jpeg",
       plot = ons_antibody_gg,
       device = "jpeg",
       height = 18, width = 20)