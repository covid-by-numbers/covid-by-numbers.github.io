## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw and tidy data
# First, I draw a prepared table on deaths in 2018 and other years
# https://wintoncentre.maths.cam.ac.uk/coronavirus/how-have-covid-19-fatalities-compared-other-causes-death/
wcrc_causes_df <- read_excel("R/COVID By Numbers/COM Figures/COM Figures - 2021-04-05.xlsx", 
                             sheet = "DATA-1") %>%
  pivot_longer(cols = 3:10,
               names_to = "causes",
               values_to = "count")

wcrc_causes_select <- c("covid_deaths_jul2020", "injuries_accidents_2018", "road_accidents_2018")

wcrc_select_df <- wcrc_causes_df %>%
  filter(causes %in% wcrc_causes_select,
         age_rank <= 12) %>%
  mutate(age_group = fct_reorder(age_group, age_rank))

# Second, I draw a prepared file of death registrations in England and Wales
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/12735annualdeathsandmortalityrates1938to2020provisional
ons_deaths_df <- read_excel("R/COVID By Numbers/COM Figures/COM Figures - 2021-04-05.xlsx",
                            sheet = "DATA-2")

ons_deathregs_df <- ons_deaths_df %>%
  filter(year >= 1900) %>%
  select(year, number_of_deaths)

ons_mortality_rates_df <- ons_deaths_df %>%
  filter(year >= 1900) %>%
  select(year, crude_mortality_rate_per100000,
         age_standardised_mortality_rate_per100000) %>%
  pivot_longer(cols = 2:3,
               names_to = "measures",
               values_to = "rate") %>%
  drop_na()

# I created a second version of the comparisons graph
ons_causes_df <- read_excel("R/COVID By Numbers/COM Figures/COM Figures - 2021-04-05.xlsx", 
                             sheet = "DATA-3") %>%
  pivot_longer(cols = 3:5,
               names_to = "causes",
               values_to = "count") %>%
  filter(age_rank <= 12) %>%
  mutate(age_group = fct_reorder(age_group, age_rank))

## Creating the graphs
# The first graph is for deaths among under-60s
wcrc_select_gg <- wcrc_select_df %>%
  ggplot(aes(x = age_group, y = count, group = causes)) +
  geom_col(aes(fill = causes),
           position = "dodge",
           size = 1.5) +
  scale_y_continuous(labels = label_comma(),
                     limits = c(0,1500)) +
  scale_fill_manual(name = "",
                    labels = c("Deaths involving COVID-19 (up to 3rd July 2020)", "Injuries and accidents (2018)", "Road accidents (2018)"),
                    values = c("black", "grey40", "grey80")) +
  labs(title = "For under-60s in England and Wales, how did COVID-19 deaths compare to normal risks?",
       subtitle = "Estimated deaths in England and Wales from different causes and time periods. COVID-19 registrations are up to 3rd July 2020.",
       caption = "Source: Winton Centre for Risk and Evidence Communication (collating ONS statistics).",
       x = "Age group", y = "Deaths")

# The second graph has two parts: the first half is death registrations
ons_mortality_gg1_base <- ons_deaths_df %>%
  ggplot(aes(x = year,
             y = number_of_deaths)) +
  geom_bar(stat = "identity",
           width = 1,
           aes(fill = number_of_deaths <= 600000)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,700000),
                     labels = label_comma(),
                     breaks = breaks_pretty(n = 7),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 20,
                                     face = "bold")) +
  labs(subtitle = "Number of death registrations in England and Wales",
       x = "Year",
       y = "") +
  annotate("text", x = 1925, y = 670000, size = 6,
  label = "There are two years where death registrations exceeded 600,000:
  in 1918 (flu pandemic) and 2020 (COVID-19 pandemic).")

ons_mortality_gg1 <- ons_mortality_gg1_base +
  scale_fill_manual(values = c("grey10", "grey90"))

ons_mortality_gg1_col <- ons_mortality_gg1_base +
  scale_fill_manual(values = c("#FFC857", "#8FD694"))

# The second part is one of mortality rates over time
ons_mortality_gg2_base <- ons_mortality_rates_df %>%
  ggplot(aes(x = year,
             y = rate,
             group = measures)) +
  geom_line(aes(color = measures),
            size = 1.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,2500),
                     labels = label_comma(),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 20,
                                     face = "bold")) +
  labs(x = "Year",
       y = "",
       subtitle = "Mortality rates in England and Wales (per 100,000 people)")

ons_mortality_gg2 <- ons_mortality_gg2_base +
  scale_colour_manual(values = c("black", "grey50")) +
  annotate("text", x = 1925, y = 1000,
           label = "Crude mortality rate",
           color = "grey50", fontface = "bold", size = 7) +
  annotate("text", x = 1985, y = 2200,
           label = "Age-standardised mortality rate",
           color = "black", fontface = "bold", size = 7)

ons_mortality_gg2_col <- ons_mortality_gg2_base +
  scale_colour_manual(values = c("#008080", "#800000")) +
  annotate("text", x = 1925, y = 1000,
           label = "Crude mortality rate",
           color = "#800000", fontface = "bold", size = 7) +
  annotate("text", x = 1985, y = 2200,
           label = "Age-standardised mortality rate",
           color = "#008080", fontface = "bold", size = 7)

# We put these two mini-graphs together
ons_mortality_gg <- ons_mortality_gg1 + ons_mortality_gg2 +
  plot_annotation(title = "In 2020, there were over 608,000 death registrations in England and Wales.",
                  subtitle = "Deaths registrations from all causes in England and Wales, with age-standardised and crude mortality rates by year.",
                  caption = "Source: Office for National Statistics, 1900 to 2020 (provisional).")

ons_mortality_gg_col <- ons_mortality_gg1_col + ons_mortality_gg2_col +
  plot_annotation(title = "In 2020, there were over 608,000 death registrations in England and Wales.",
                  subtitle = "Deaths registrations from all causes in England and Wales, with age-standardised and crude mortality rates by year.",
                  caption = "Source: Office for National Statistics, 1900 to 2020 (provisional).")

# This is the second version of the comparisons graph
ons_causes_gg <- ons_causes_df %>%
  ggplot(aes(x = age_group, y = count, group = causes)) +
  geom_col(aes(fill = causes), position = "dodge", size = 1.5) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(), limits = c(0,4000),
                     expand = c(0,0)) + 
  scale_fill_manual(name = "",
                    labels = c("Deaths involving Covid-19 (up to 12th March 2021)", "Injuries & accidents (2015-2019 avg.)", "Road incidents (2015-2019 avg.)"),
                    values = c("black", "grey40", "grey80")) +
  labs(title = "For under-60s in England and Wales, how did Covid deaths compare to normal risks?",
       subtitle = "Estimated deaths in England and Wales from different causes and time periods. Covid-19 registrations are up to 12th March 2021.",
       caption = "Source: Office for National Statistics (Weekly provisional death statistics and NOMIS).",
       x = "Age group", y = "Registered deaths")

## Saving graphs
# Saving the figures in the required dimensions
ggsave(file = "R/COVID By Numbers/COM Figures/COM_wcrc_select_gg.jpeg",
       plot = wcrc_select_gg,
       device = "jpeg",
       height = 10, width = 16)

ggsave(file = "R/COVID By Numbers/COM Figures/COM_ons_mortality_gg.jpeg",
       plot = ons_mortality_gg,
       device = "jpeg",
       height = 12, width = 20)

ggsave(file = "R/COVID By Numbers/COM Figures/COM_ons_mortality_gg_col.jpeg",
       plot = ons_mortality_gg_col,
       device = "jpeg",
       height = 12, width = 20)

ggsave(file = "R/COVID By Numbers/COM Figures/COM_ons_causes_gg.jpeg",
       plot = ons_causes_gg,
       device = "jpeg",
       height = 10, width = 17)