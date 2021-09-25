## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(patchwork)
library(zoo)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw data
# I draw the data from a prepared file
# It was a graphical reconstruction using WebPlotDigitizer
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/971017/SP_PH__VE_report_20210317_CC_JLB.pdf
phe_siren_df <- read_excel("R/COVID By Numbers/DIN Figures/DIN Figures - 2021-04-10.xlsx",
                           sheet = "DATA-1")

# The vaccine doses are for the UK from Public Health England
# https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=newPeopleVaccinatedCompleteByPublishDate&metric=newPeopleVaccinatedFirstDoseByPublishDate&metric=newPeopleVaccinatedSecondDoseByPublishDate&metric=newVaccinesGivenByPublishDate&format=csv
phe_vaccine_df <- read_excel("R/COVID By Numbers/DIN Figures/DIN Figures - 2021-04-10.xlsx", 
                             sheet = "DATA-2")

phe_vaccine_tidy_df <- phe_vaccine_df %>%
  mutate(date = as_date(date)) %>%
  select(date, newPeopleVaccinatedFirstDoseByPublishDate, newPeopleVaccinatedSecondDoseByPublishDate) %>%
  rename("First doses" = 2,
         "Second doses" = 3) %>%
  pivot_longer(cols = 2:3,
               names_to = "measure",
               values_to = "doses") %>%
  arrange(date) %>%
  group_by(measure) %>%
  mutate(doses_7da = zoo::rollmean(doses, k = 7, fill = NA, align = "right"))

phe_vaccine_end <- phe_vaccine_tidy_df %>%
  filter(date == as_date("2021-05-08"))

## Create the graphs
phe_siren_subtitle <- "Approximate adjusted hazard ratios for PCR-confirmed cases by interval after first and second doses, in England."
phe_siren_caption <- "Graphical reconstruction. Source: Public Health England Vaccine Effectiveness Report: March 2021 (SIREN study)."

# The left-hand graph is for first doses
phe_siren_gg1 <- phe_siren_df %>%
  filter(dose_number == "first dose") %>%
  ggplot(aes(x = day_number,
             y = 0.01*round(100*hr_est, digits = 0),
             ymin = 0.01*round(100*hr_lower, digits = 0),
             ymax = 0.01*round(100*hr_upper, digits = 0))) +
  geom_pointrange(size = 1.5) +
  geom_ribbon(alpha = 0.1) +
  scale_x_continuous(breaks = c(3, 6, 9, 13, 20, 27, 41, 55, 81)) +
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     limits = c(0,1.3)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Hazard ratios after the first dose (negative cohort)",
       x = "End day (of period) after the first dose",
       y = "") +
  geom_text(x = 40, y = 0.75,
  label = "95% confidence intervals
  around the central estimate",
  size = 5, fontface = "bold") +
  geom_curve(x = 40, xend = 41,
             y = 0.65, yend = 0.4,
             curvature = -0.2, size = 2,
             arrow = arrow(length = unit(0.5, "cm")))

# The right-hand graph is for second doses
phe_siren_gg2 <- phe_siren_df %>%
  filter(dose_number == "second dose") %>%
  ggplot(aes(x = day_number,
             y = 0.01*round(100*hr_est, digits = 0),
             ymin = 0.01*round(100*hr_lower, digits = 0),
             ymax = 0.01*round(100*hr_upper, digits = 0))) +
  geom_pointrange(size = 1.5) +
  geom_ribbon(alpha = 0.1) +
  scale_x_continuous(breaks = c(3, 6, 13, 41, 59)) +
  scale_y_continuous(breaks = c(0, 0.5, 1),
                     limits = c(0,1.3)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Hazard ratios after the second dose (negative cohort).",
       x = "End day (of period) after the second dose",
       y = "")

# Patchwork then puts these two graphs together
phe_siren_gg <- phe_siren_gg1 + phe_siren_gg2 +
  plot_annotation(title = "Estimated vaccine effectiveness improves as time lapses after the first dose in England.",
                  subtitle = phe_siren_subtitle,
                  caption = phe_siren_caption)

# Vaccine doses graph
phe_vaccine_gg <- phe_vaccine_tidy_df %>%
  ggplot(aes(x = date, y = doses,
             group = measure)) +
  geom_col(aes(fill = measure),
           alpha = 0.2, width = 1) +
  geom_line(aes(y = doses_7da,
                colour = measure),
            size = 1.5, na.rm = TRUE) +
  geom_point(data = phe_vaccine_end,
             aes(x = date, y = doses_7da,
                 colour = measure),
             size = 5) +
  facet_wrap(~measure) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%d-%b\n%Y",
               expand = c(0.02,0)) +
  scale_y_continuous(labels = label_comma(),
                     expand = c(0,0)) +
  theme(legend.position = "none") +
  scale_colour_manual(aesthetics = c("fill", "colour"),
                      values = c("black", "grey50")) +
  labs(title = "After an initial period of first doses, UK focus shifted to second doses in March 2021.",
       subtitle = "UK vaccinations doses for each dose type with a seven-day rolling average, by publication date.",
       caption = "Source: Public Health England COVID-19 dashboard data download",
       x = "Publication date",
       y = "Number of daily doses")

## Saving the graphs
# Save the figures with the required dimensions
ggsave(file = "R/COVID By Numbers/DIN Figures/DIN_phe_siren_gg.jpeg",
       plot = phe_siren_gg,
       device = "jpeg",
       height = 10, width = 16)

ggsave(file = "R/COVID By Numbers/DIN Figures/DIN_phe_vaccine_gg.jpeg",
       plot = phe_vaccine_gg,
       device = "jpeg",
       height = 10, width = 15)