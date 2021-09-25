## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(scales)
library(patchwork)
library(ggdist)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

# Draw data
# I draw the data frame for ONS model hazard ratios from a prepared file
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/articles/updatingethniccontrastsindeathsinvolvingthecoronaviruscovid19englandandwales/deathsoccurring2marchto28july2020
ons_hazard_df <- read_excel("R/COVID By Numbers/RSK Figures/RSK Figures - 2021-04-03.xlsx",
                            sheet = "DATA-1")

# I also draw the data frame for age-standardised mortality rates involving COVID-19 by occupation
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/bulletins/coronaviruscovid19relateddeathsbyoccupationenglandandwales/deathsregisteredbetween9marchand28december2020
ons_occupation_df <- read_excel("R/COVID By Numbers/RSK Figures/RSK Figures - 2021-04-03.xlsx", 
                                sheet = "DATA-2")

# I draw the estimated infection fatality ratios by 'Covid age'
# https://alama.org.uk/covid-19-medical-risk-assessment/
covid_age_df <- read_excel("R/COVID By Numbers/RSK Figures/RSK Figures - 2021-04-03.xlsx",
                           sheet = "DATA-3")

# We draw the hazard ratio estimates from a prepared file
# https://www.nature.com/articles/s41586-020-2521-4#Tab2
opensafely_hr_df <- read_excel("R/COVID By Numbers/RSK Figures/RSK Figures - 2021-04-03.xlsx", 
                               sheet = "DATA-4") %>%
  mutate(rank_level = 1:27)

# We can set the factors
ons_hazard_df$sex <- factor(ons_hazard_df$sex,
                            levels = c("Male", "Female"))
ons_hazard_df$adjusted_for <- factor(ons_hazard_df$adjusted_for,
                                     levels = c("Age", "+ Geography", "+ Socio-economics", "+ Health status"))

# For the latter graphs, we should use the following constants
ons_occ_male_df <- ons_occupation_df %>% filter(sex == "Male" &
                                                  Description == "All aged 20 to 64 years")

ons_occ_female_df <- ons_occupation_df %>% filter(sex == "Female" &
                                                    Description == "All aged 20 to 64 years")

# We also need to set the factors for the OpenSAFELY hazard ratios
opensafely_hr_df$Characteristic <- factor(opensafely_hr_df$Characteristic,
                                          levels = c("Age", "Sex", "BMI", "Ethnicity",
                                                     "Smoking", "Cancer", "Asthma", "Other"))

## Create the graphs
# This is the hazard ratio graphs for ethnic minority mortality in England
ons_hazard_gg <- ons_hazard_df %>%
  mutate(ethnic_group = fct_reorder(ethnic_group, hazard_ratio)) %>%
           filter(adjusted_for %in% c("Age", "+ Health status")) %>%
  ggplot(aes(y = ethnic_group,
             x = hazard_ratio,
             group = adjusted_for)) +
  geom_pointrange(aes(colour = adjusted_for,
                      xmin = ci_95_lower,
                      xmax = ci_95_upper),
                  size = 1.5,
           position = position_dodge(width = 0.4)) +
  facet_wrap(~sex) +
  scale_colour_manual(name = "Adjusted for",
                    labels = c("Age", "Age, geography, socio-economics, and health status"),
                    values = c("grey70", "grey40")) +
  scale_x_continuous(limits = c(0, 4.5),
                     breaks = 0:4,
                     labels = c("0x", "Same\nrate", "2x", "3x", "4x")) +
  geom_vline(xintercept = 1, linetype = "dotted", size = 1.1) +
  labs(title = "There are heightened rates of COVID-19 deaths in English ethnic minorities.",
       subtitle = str_wrap("Rate of death involving COVID-19 by ethnic group and sex, relative to white people. This is for deaths in England between 2nd March and 28th July 2020.", width = 120),
       x = "Hazard ratio (rates of death involving COVID-19 relative to white people)",
       y = "",
       caption = "Source: Office for National Statistics - Explaining ethnic background contrasts in deaths involving Coronavirus (COVID-19).")

# This graph is for male age-standardised mortality involving COVID-19 by occupation
ons_occ_male_gg <- ons_occupation_df %>%
  filter(sex == "Male", soc > 0) %>%
  mutate(Description = fct_reorder(Description, -soc)) %>%
  ggplot(aes(x = rate, y = Description)) +
  geom_pointrange(aes(xmin = lower_ci,
                 xmax = upper_ci),
             size = 2) +
  geom_vline(xintercept = ons_occ_male_df$rate,
             size = 1.2, linetype = "dashed", colour = "grey40") +
  geom_rect(aes(xmin = ons_occ_male_df$lower_ci,
                xmax = ons_occ_male_df$upper_ci,
                ymin = -Inf, ymax = Inf),
            alpha = 0.1, colour = "grey40") +
  geom_text(x = ons_occ_male_df$rate, y = 9, hjust = -0.1,
            label = "Rate among men aged 20-64 in E&W
            with confidence intervals",
            size = 7, fontface = "bold", colour = "grey40") +
  geom_curve(x = 40, xend = 33, y = 8.7, yend = 8,
             curvature = -0.2, size = 2, colour = "grey40",
             arrow = arrow(length = unit(0.5, "cm"))) +
  scale_x_continuous(limits = c(0, 85),
                     expand = c(0,0)) +
  labs(title = "Men in elementary and caring occupations had the highest COVID-19 death rates.",
       subtitle = str_wrap("Male age-standardised mortality rates by major occupational group. This is for deaths in England and Wales between 9th March and 28th December 2020.",
                           width = 120),
       x = "Age-standardised mortality involving COVID-19 (per 100,000 people)",
       y = "Occupational categories",
       caption = "Source: Office for National Statistics - Deaths registered in England and Wales.")

# This graph is for female age-standardised mortality involving COVID-19 by occupation
ons_occ_female_gg <- ons_occupation_df %>%
  filter(sex == "Female" & soc > 0) %>%
  mutate(Description = fct_reorder(Description, -soc)) %>%
  ggplot(aes(x = rate, y = Description)) +
  geom_pointrange(aes(xmin = lower_ci,
                      xmax = upper_ci),
                  size = 2) +
  geom_vline(xintercept = ons_occ_female_df$rate,
             size = 1.2, linetype = "dashed", colour = "grey40") +
  geom_rect(aes(xmin = ons_occ_female_df$lower_ci,
                xmax = ons_occ_female_df$upper_ci,
                ymin = -Inf, ymax = Inf),
            alpha = 0.1, colour = "grey40") +
  scale_x_continuous(limits = c(0, 55),
                     expand = c(0,0)) +
  geom_text(x = ons_occ_female_df$rate, y = 9, hjust = -0.1,
            label = "Rate among women aged 20-64 in E&W
            with confidence intervals",
            size = 7, fontface = "bold", colour = "grey40") +
  geom_curve(x = 25, xend = 18, y = 8.5, yend = 8,
             curvature = -0.2, size = 2, colour = "grey40",
             arrow = arrow(length = unit(0.5, "cm"))) +
  labs(title = "Women in processing and caring occupations had the highest COVID-19 death rates.",
       subtitle = str_wrap("Female age-standardised mortality rates by major occupational group. This is for deaths in England and Wales between 9th March and 28th December 2020.",
                           width = 120),
       x = "Age-standardised mortality involving COVID-19 (per 100,000 people)",
       y = "Occupational categories",
       caption = "Source: Office for National Statistics - Deaths registered in England and Wales.")

# This is the OpenSAFELY graph
opensafely_hr_gg <- opensafely_hr_df %>%
  ggplot(aes(x = hr_full_est, xmin = hr_full_lower, xmax = hr_full_upper,
         y = fct_reorder(Category, -rank_level))) +
  geom_pointrange(size = 1) +
  scale_x_log10(breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10),
                labels = c("0.1x", "0.2x", "0.5x", "Same\nrate", "2x", "5x", "10x")) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  facet_grid(Characteristic ~ .,
             scales = "free", space = "free",
             switch = "y") +
  theme(strip.text.y.left = element_text(angle = 0),
        strip.placement = "outside") +
  labs(title = "In the OpenSAFELY model, age dominates other factors for COVID-19 related deaths.",
       subtitle = "Estimated hazard ratios of COVID-19-related deaths for selected characteristics, for English patients (1st February to 6th May 2020.)",
       caption = "Source: Nature: Factors associated with COVID-19-related death using OpenSAFELY.",
       x = "Estimated hazard ratio (with 95% confidence intervals) on a log scale",
       y = "")

# This graph shows infection fatality ratios by Covid age
# This is done on both a natural and logarithmic scale
covid_age_gg1 <- covid_age_df %>%
  ggplot(aes(x = covid_age, y = 100*est_ifr_central,
             ymin = 100*est_ifr_lower, ymax = 100*est_ifr_upper)) +
  geom_line(size = 2) +
  geom_ribbon(alpha = 0.1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(labels = label_comma())  +
  theme(plot.subtitle = element_text(size = 20, face = "plain")) +
  labs(subtitle = "IFR per 100,000 people (on a natural scale)",
       x = "Covid-age",
       y = "Estimated infection fatality rate (per 100,000 people)")

covid_age_gg2 <- covid_age_df %>%
  ggplot(aes(x = covid_age, y = 100*est_ifr_central,
             ymin = 100*est_ifr_lower, ymax = 100*est_ifr_upper)) +
  geom_line(size = 2) +
  geom_ribbon(alpha = 0.1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_log10(labels = label_comma())  +
  theme(plot.subtitle = element_text(size = 20, face = "plain")) +
  labs(subtitle = "IFR per 100,000 people (on a log scale)",
       x = "Covid-age",
       y = "Estimated infection fatality rate (per 100,000 people)")

# Patchwork then puts the two smaller graphs together
covid_age_gg <-  covid_age_gg1 + covid_age_gg2 +
  plot_annotation(title = "The estimated infection fatality rate increases with Covid-age.",
                  subtitle = str_wrap("Estimated infection fatality rates (per 100,000 people) for single Covid-age.  Covid-age is calculated starting with the person's actual age and then adding or subtracting years for each risk factor that applies.",
                                      width = 120),
                  caption = "Source: Joint Occupational Health COVID-19 Group: Covid-19 Medical Risk Assessment.")

## Saving graphs
# Saving the figures in the required dimensions
ggsave(file = "R/COVID By Numbers/RSK Figures/RSK_ons_hazard_gg.jpeg",
       plot = ons_hazard_gg,
       device = "jpeg",
       height = 12, width = 16)

ggsave(file = "R/COVID By Numbers/RSK Figures/RSK_ons_occ_male_gg.jpeg",
       plot = ons_occ_male_gg,
       device = "jpeg",
       height = 10, width = 20)

ggsave(file = "R/COVID By Numbers/RSK Figures/RSK_ons_occ_female_gg.jpeg",
       plot = ons_occ_female_gg,
       device = "jpeg",
       height = 10, width = 20)

ggsave(file = "R/COVID By Numbers/RSK Figures/RSK_opensafely_hr_gg.jpeg",
       plot = opensafely_hr_gg,
       device = "jpeg",
       height = 17, width = 22)