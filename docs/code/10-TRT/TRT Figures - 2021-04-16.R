## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw and tidy the data
# I draw the data from a prepared file
# https://www.nejm.org/doi/10.1056/NEJMoa2021436?
recovery_dexa_df <- read_excel("R/COVID By Numbers/TRT Figures/TRT Figures - 2021-04-16.xlsx", 
                               sheet = "DATA-1")

recovery_tidy_df <- recovery_dexa_df %>%
  pivot_longer(cols = 3:4,
               names_to = "trial_arm",
               values_to = "mortality_rate")

recovery_tidy_df$patient_type <- factor(recovery_tidy_df$patient_type,
                                         levels = c("All participants", "Invasive mechanical ventilation", "Oxygen only", "No oxygen received"))

## Create the graph
recovery_dexa_gg <- recovery_tidy_df %>%
  ggplot(aes(x = day, y = mortality_rate, group = trial_arm)) +
  geom_step(aes(colour = trial_arm,
                linetype = trial_arm),
            size = 1.5) +
  facet_wrap(~patient_type) +
  scale_x_continuous(breaks = c(0, 7, 14, 21, 28)) +
  scale_y_continuous(limits = c(0, 100),
                     expand = c(0,0)) +
  scale_colour_manual(name = "", values = c("black", "grey50"),
                      labels = c("Dexamethasone", "Usual care")) +
  scale_linetype_manual(name = "", values = c("solid", "dotdash"),
                        labels = c("Dexamethasone", "Usual care")) +
  labs(title = "Dexamethasone lowered mortality for patients needing respiratory support.",
       subtitle = "Cumulative mortality by day [%] in all participating patients and respiratory sub-groups in the RECOVERY trial for dexamethasone.",
       x = "Days after randomisation",
       y = "Mortality [%]",
       caption = "Graphical reconstruction. Source: Dexamethasone in Hospitalized Patients with Covid-19 (NEJM, 2021).")

## Save the graph
# Saving the graph with the required dimensions
ggsave(file = "R/COVID By Numbers/TRT Figures/TRT_recovery_dexa_gg.jpeg",
       plot = recovery_dexa_gg,
       device = "jpeg",
       height = 10, width = 15)