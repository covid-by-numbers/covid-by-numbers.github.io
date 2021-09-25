## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(scales)
library(patchwork)
library(ggrepel)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw and tidy the data
# I draw the age-standardised mortality rates by age group and sex from NOMIS
agest_mortality_df <- read_excel("R/COVID By Numbers/DAN Figures/DAN Figures - 2021-04-12.xlsx", 
                                 sheet = "DATA-1")

# Weekly death registrations involving COVID-19 come from the ONS
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales
covid19_registrations_df <- read_excel("R/COVID By Numbers/DAN Figures/DAN Figures - 2021-04-12.xlsx",
                                       sheet = "DATA-2")

# The 2019 mid-year population estimate is also from the ONS
# https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
mye2019_engwales_df <- read_excel("R/COVID By Numbers/DAN Figures/DAN Figures - 2021-04-12.xlsx",
                                  sheet = "DATA-3")

# The Verity et al estimates of COVID-19 infection fatality rates in Oct-20
# https://www.imperial.ac.uk/media/imperial-college/medicine/mrc-gida/2020-10-29-COVID19-Report-34.pdf
ifr_imperial_df <- read_excel("R/COVID By Numbers/DAN Figures/DAN Figures - 2021-04-12.xlsx", 
                              sheet = "DATA-4")

# The national life tables of expected mortality for UK, 2016-2018
# https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesunitedkingdomreferencetables/current/nationallifetables3yearuk.xlsx
ons_natlifetable_df <- read_excel("R/COVID By Numbers/DAN Figures/DAN Figures - 2021-04-12.xlsx", 
                                  sheet = "DATA-5")

ons_natlife_tidy_df <- ons_natlifetable_df %>%
  pivot_longer(cols = 2:3,
               names_to = "sex",
               values_to = "mortality_rate") %>%
  mutate(exp_mortality = 100000*mortality_rate) %>%
  select(age_number, sex, exp_mortality)

mortality_rate_df <- full_join(agest_mortality_df, covid19_registrations_df,
                               by = c("age_group", "age_number", "sex")) %>%
  full_join(mye2019_engwales_df,
            by = c("age_group", "age_number", "sex")) %>%
  full_join(ifr_imperial_df,
            by = "age_number") %>%
  mutate(average_2015_2019 = (y2015 + y2016 + y2017 + y2018 + y2019)/5,
         covid19_reg = round(100000*(covid19_registrations_2020 + covid19_registrations_2021_uptow10)/eng_wales_2019_population_estimate,
                             digits = 2),
         ifr_estimate = 1000*ifr_estimate_with_seroreversion) %>%
  select(age_number, sex, average_2015_2019, covid19_reg, ifr_estimate) %>%
  full_join(ons_natlife_tidy_df,
            by = c("age_number", "sex"))

mortality_compare_df <- mortality_rate_df %>%
  select(age_number, sex, covid19_reg, exp_mortality) %>%
  pivot_longer(cols = 3:4,
               names_to = "measure",
               values_to = "rates") %>%
  filter(measure == "covid19_reg" & age_number >= 5 | measure == "exp_mortality") %>%
  filter(age_number <= 95, rates > 0)

mortality_compare_label <- mortality_compare_df %>%
  filter(age_number == 35, measure == "exp_mortality")

## Creating the graphs
# Each pair of graphs will show the information but on different scales
mortality_rate_breaks <- c(1, 2, 5, 10, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000)

mortality_ifr_gg1 <- mortality_rate_df %>%
  ggplot(aes(x = age_number)) +
  geom_line(aes(y = average_2015_2019, colour = sex),
            size = 1.5,
            linetype = "dotted") +
  geom_point(data = filter(mortality_rate_df, age_number >= 5, sex == "female"),
             aes(y = ifr_estimate, fill = sex),
             size = 4, na.rm = TRUE) +
  scale_colour_manual(values = c("grey60", "grey30"),
                      labels = c("Female", "Male")) +
  scale_fill_manual(values = "black",
                    labels = "Additional estimated mortality if infected") +
  scale_x_continuous(breaks = 10*0:9) +
  scale_y_continuous(breaks = pretty_breaks(),
                     labels = label_comma(),
                     limits = c(0.5, 32000)) + 
  theme(plot.subtitle = element_text(size = 20, face = "plain")) +
  labs(subtitle = "Deaths per 100,000 people (on a natural scale)",
       x = "Age (age band shown by oldest age)",
       y = "",
       colour = "Average mortality (2015 to 2019)", fill = "")

mortality_ifr_gg1 <- mortality_rate_df %>%
  filter(age_number <= 95) %>%
  ggplot(aes(x = age_number)) +
  geom_line(aes(y = exp_mortality,
                colour = sex),
            size = 1.5, linetype = "dashed") +
  geom_point(data = filter(mortality_rate_df, age_number >=5, sex == "female"),
             aes(y = ifr_estimate),
             size = 5, na.rm = TRUE) +
  scale_x_continuous(breaks = 10*0:9) +
  scale_y_continuous(breaks = pretty_breaks(),
                     labels = comma_format(accuracy = 1),
                     limits = c(0.5, 32000)) +
  geom_text_repel(data = mortality_compare_label,
                  aes(x = age_number, y = rates,
                      colour = sex,
                      label = str_to_title(sex)),
                  size = 5, box.padding = 0.5, segment.color = "NA") +
  scale_colour_manual(values = c("grey50", "grey10")) +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Deaths per 100,000 people (on a natural scale)",
       x = "Age (age band shown by oldest age)",
       y = "")

mortality_ifr_gg2 <- mortality_rate_df %>%
  filter(age_number <= 95) %>%
  ggplot(aes(x = age_number)) +
  geom_line(aes(y = exp_mortality,
                colour = sex),
            size = 1.5, linetype = "dashed") +
  geom_point(data = filter(mortality_rate_df, age_number >=5, sex == "female"),
             aes(y = ifr_estimate),
             size = 5, na.rm = TRUE) +
  scale_x_continuous(breaks = 10*0:9) +
  scale_y_continuous(breaks = mortality_rate_breaks,
                     labels = comma_format(accuracy = 1),
                     trans = "log10",
                     limits = c(0.5, 32000)) +
  geom_text_repel(data = mortality_compare_label,
                  aes(x = age_number, y = rates,
                      colour = sex,
                      label = str_to_title(sex)),
                  size = 5, box.padding = 1, segment.color = "NA") +
  annotate("text", x = 1, y = 1000,
           label = "The dashed lines are expected deaths by age,\nbased on UK life tables for 2016-2018.",
           size = 5, hjust = 0) +
  annotate("curve", x = 20, xend = 25, y = 500, yend = 100,
           curvature = 0.2, size = 2, arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", x = 50, y = 5,
           label = "The dots are estimated\nadditional mortality if infected.",
           size = 5, hjust = 0) +
  annotate("curve", x = 50, xend = 45, y = 11, yend = 80,
           curvature = 0.2, size = 2,
           arrow = arrow(length = unit(0.5, "cm"))) +
  scale_colour_manual(values = c("grey50", "grey10")) +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Deaths per 100,000 people (on a log scale)",
       x = "Age (age band shown by oldest age)",
       y = "")

# We then put those two graphs together
mortality_ifr_gg <- mortality_ifr_gg1 + mortality_ifr_gg2 +
  plot_annotation(title = "Additional estimated mortality from infection is similar to the average past mortality rates.",
                  subtitle = "UK actuarial mortality rates by single year of age (per 100,000 people) by age group and sex in England and Wales, and estimated infection fatality by age.",
                  caption = "Sources: Office for National Statistics: UK National Life Tables, Imperial College London: Report 34 - COVID-19 Infection Fatality Ratio Estimates from Seroprevalence.")

# These graphs compare COVID-19 mortality as a percentage of population estimates to that average past rate
mortality_covid_gg1 <- mortality_compare_df %>%
  ggplot(aes(x = age_number, y = rates, colour = sex)) +
  geom_line(aes(linetype = measure),
            size = 1.5, na.rm = TRUE) +
  scale_x_continuous(breaks = 10*(1:9)) +
  scale_y_continuous(breaks = pretty_breaks(),
                     labels = comma_format(accuracy = 1),
                     limits = c(0.5, 32000)) +
  geom_text_repel(data = mortality_compare_label,
                  aes(x = age_number, y = rates,
                      colour = sex,
                      label = str_to_title(sex)),
                  size = 5) +
  scale_colour_manual(values = c("grey50", "grey10")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Deaths per 100,000 people (on a natural scale)",
       x = "Age (age band shown by oldest age)",
       y = "")

mortality_covid_gg2 <- mortality_compare_df %>%
  ggplot(aes(x = age_number, y = rates, colour = sex)) +
  geom_line(aes(linetype = measure),
            size = 1.5, na.rm = TRUE) +
  scale_x_continuous(breaks = 10*(1:9)) +
  scale_y_continuous(trans = "log10",
                     limits = c(0.5, 32000),
                     labels = comma_format(accuracy = 1),
                     breaks = mortality_rate_breaks) +
  geom_text_repel(data = mortality_compare_label,
            aes(x = age_number, y = rates,
                colour = sex,
                label = str_to_title(sex)),
            size = 5, box.padding = 1, segment.color = "NA") +
  annotate("text", x = 1, y = 1000,
           label = "The dashed lines are expected deaths by age,\nbased on UK life tables for 2016-2018.",
           size = 5, hjust = 0) +
  annotate("curve", x = 20, xend = 25, y = 500, yend = 100,
           curvature = 0.2, size = 2,
           arrow = arrow(length = unit(0.5, "cm"))) +
  annotate("text", x = 50, y = 5,
           label = "The solid lines are COVID-19 registrations,\nby age group up to 12th March 2021.",
           size = 5, hjust = 0) +
  annotate("curve", x = 60, xend = 55, y = 11, yend = 40,
           curvature = 0.2, size = 2,
           arrow = arrow(length = unit(0.5, "cm"))) +
  scale_colour_manual(values = c("grey50", "grey10")) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Deaths per 100,000 people (on a log scale)",
       x = "Age (age band shown by oldest age)",
       y = "")

# We put these two graphs together
mortality_covid_gg <- mortality_covid_gg1 + mortality_covid_gg2 +
  plot_annotation(title = "Deaths involving COVID-19 followed a similar age pattern to past death rates.",
       subtitle = "UK actuarial mortality rates by single year of age and COVID-19 registrations (per 100,000 people) in England and Wales by age group and sex. Rates under 0.5 are not shown.",
       caption = "Authors' calculations. Sources: Office for National Statistics: UK National Life Tables, Weekly death registrations (provisional) and 2019 mid-year population estimates.")

## Saving the graphs
# Save the figures in the required dimensions
ggsave(file = "R/COVID By Numbers/DAN Figures/DAN_mortality_ifr_gg.jpeg",
       plot = mortality_ifr_gg,
       device = "jpeg",
       height = 10, width = 20)

ggsave(file = "R/COVID By Numbers/DAN Figures/DAN_mortality_covid_gg.jpeg",
       plot = mortality_covid_gg,
       device = "jpeg",
       height = 10, width = 20)