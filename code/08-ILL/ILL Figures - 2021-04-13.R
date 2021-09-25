## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(RcmdrPlugin.KMggplot2)
library(ggdist)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw the data
# We draw the data on Long Covid from a prepared file
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/bulletins/prevalenceofongoingsymptomsfollowingcoronaviruscovid19infectionintheuk/1april2021
ons_longcovid_df <- read_excel("R/COVID By Numbers/ILL Figures/ILL Figures - 2021-04-13.xlsx", 
                               sheet = "DATA-1")

# We draw the symptoms data by nation from a prepared file
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/articles/coronaviruscovid19infectionsinthecommunityinengland/characteristicsofpeopletestingpositiveforcovid19incountriesoftheuk5may2021
ons_symptoms_df <- read_excel("R/COVID By Numbers/ILL Figures/ILL Figures - 2021-04-13.xlsx", 
                             sheet = "DATA-2")

## Create the graphs
longcovid_colours <- c("grey50", "grey10")
longcovid_labels <- c("Control", "Participants who tested positive for COVID-19")
ons_infectionstudy_source <- "Source: Office for National Statistics - Coronavirus (COVID-19) Infection Survey."

ons_longcovid_gg <- ons_longcovid_df %>%
  ggplot(aes(x = day, y = central_estimate,
             ymin = lower_95_ci, ymax = upper_95_ci,
             group = group)) +
  geom_step(aes(colour = group, linetype = group),
            size = 2) +
  geom_stepribbon(aes(fill = group),
                  alpha = 0.1) +
  scale_fill_manual(values = longcovid_colours,
                    labels = longcovid_labels) +
  scale_colour_manual(values = longcovid_colours,
                      labels = longcovid_labels) +
  scale_linetype_manual(values = c("dotted", "solid"),
                        labels = longcovid_labels) +
  scale_x_continuous(expand = c(0,0)) +
  geom_vline(xintercept = 84, linetype = "dashed", size = 1.1) +
  geom_text(x = 85, y = 24,
            label = "12 weeks after infection",
            size = 7, hjust = 0) + 
  labs(title = "The share reporting symptoms after COVID-19 was higher than for control participants.",
       subtitle = str_wrap("Estimated percentage [%] of study participants (with 95% confidence intervals) reporting any symptom after time from assumed infection date or equivalent date in the UK: 26 April 2020 to 6 March 2021.", width = 120),
       x = "Days after assumed infection (those who tested positive) or equivalent date (control group)",
       y = "Proportion with symptoms [%]",
       fill = "", colour = "", linetype = "",
       caption = ons_infectionstudy_source)

ons_symptoms_gg <- ons_symptoms_df %>%
  ggplot(aes(y = fct_reorder(symptom, central_estimate),
             colour = nation,
             x = central_estimate,
             xmin = lower_95_ci,
             xmax = upper_95_ci)) +
  geom_pointinterval(size = 10,
                     show.legend = FALSE) +
  facet_wrap(~nation) +
  scale_x_continuous(limits = c(0,55),
                     expand = c(0,0)) +
  scale_colour_manual(values = c("black", "grey20", "grey40", "grey60")) +
  labs(title = "The most common symptoms were fatigue, coughs, and headaches.",
       subtitle = str_wrap("Share [%] of people with symptoms for those with strong SARS-CoV-2 positive tests (Ct less than 30) in UK countries. 1st December 2020 to 4th April 2021.", width = 120),
       x = "Share of people with symptoms [%]",
       y = "",
       caption = ons_infectionstudy_source)

## Saving graphs
# Save the figures in the required dimensions
ggsave(file = "R/COVID By Numbers/ILL Figures/ILL_ons_longcovid_gg.jpeg",
       plot = ons_longcovid_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/ILL Figures/ILL_ons_symptoms_gg.jpeg",
       plot = ons_symptoms_gg,
       device = "jpeg",
       height = 10, width = 15)