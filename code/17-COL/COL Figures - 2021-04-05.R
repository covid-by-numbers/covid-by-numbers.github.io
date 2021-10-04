## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw and tidy the data
# I draw the road user casualty estimates for Jan-Jun 2020 from a prepared file
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/956446/ras45012.ods
dot_casualties_df <- read_excel("R/COVID By Numbers/COL Figures/COL Figures - 2021-04-05.xlsx",
                                sheet = "DATA-1",
                                col_types = c("date", "numeric", "numeric",
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric")) %>%
  mutate(month = as_date(month))

dot_incidents_df_1 <- dot_casualties_df %>%
  select(1:6) %>%
  pivot_longer(cols = 2:6,
               names_to = "road_user_type",
               values_to = "road_casualties")

dot_incidents_df_2 <- dot_casualties_df %>%
  select(1, 7:11) %>%
  pivot_longer(cols = 2:6,
               names_to = "road_user_type",
               values_to = "change") %>%
  mutate(road_user_type = str_replace(road_user_type, "_change", ""))

dot_incidents_df <- full_join(dot_incidents_df_1,
                               dot_incidents_df_2)

# I draw the Public Health England COVID-19 and flu measures from a prepared file
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/975533/Weekly_Influenza_and_COVID19_report_data_w13.xlsx
phe_flucovid_df <- read_excel("R/COVID By Numbers/COL Figures/COL Figures - 2021-04-05.xlsx",
                                sheet = "DATA-2") %>%
  mutate(week_start_date = as_date(week_start_date),
         week_end_date = as_date(week_end_date)) %>%
  pivot_longer(cols = 4:9,
               names_to = "measures",
               values_to = "rates_per_100000") %>%
  drop_na()

# We set the breaks for the graphs
phe_flucovid_breaks <- c("2020-07-05", "2020-10-04", "2021-01-03", "2021-03-28") %>%
  as_date()

## Creating the graphs
# The first graph is of road incident levels
dot_incidents_gg1 <- dot_incidents_df %>%
  filter(road_user_type %in% c("all_road_users", "car_users", "pedal_cyclists")) %>%
  ggplot(aes(x = month, y = road_casualties, group = road_user_type)) +
  geom_line(aes(colour = road_user_type), size = 1.5) +
  scale_colour_grey(start = 0, end = 0.9,
                    name = "Road user type",
                    labels = c("All road users", "Car users", "Pedal cyclists")) +
  scale_x_date(date_labels = "%b\n%Y",
               expand = c(0.02,0)) +
  scale_y_continuous(breaks = pretty_breaks(),
                     labels = label_comma(),
                     expand = c(0,0),
                     limits = c(0, 14000)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Provisional road casualty estimates",
       x = "Month", y = "Number of road casualties")

# The second graph is of the change relative to the previous year
dot_incidents_gg2 <- dot_incidents_df %>%
  filter(road_user_type %in% c("all_road_users", "car_users", "pedal_cyclists")) %>%
  ggplot(aes(x = month, y = change, group = road_user_type)) +
  geom_line(aes(colour = road_user_type), size = 1.5) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  scale_colour_manual(values = c("black", "grey40", "grey80")) +
  scale_x_date(date_labels = "%b\n%Y",
               expand = c(0.02,0)) +
  scale_y_continuous(breaks = pretty_breaks(),
                     limits = c(-80, 20)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold"),
        legend.position = "none") +
  labs(subtitle = "Change [%] compared to 2019",
       x = "Month", y = "Change [in %]")

# Those two smaller graphs are put in a patchwork layout
dot_incidents_gg <- dot_incidents_gg1 + dot_incidents_gg2 +
  plot_annotation(title = "Provisional estimates in Great Britain suggest a large fall in road casualties in April to June 2020.",
                  subtitle = "Numbers and annual change [%] of reported road casualties by different types of road user, in January to June 2020.",
                  caption = "Source: Department for Transport: Reported road casualties by road user type, first half 2020.") +
  plot_layout(guides = "collect")

# This graph is of PHE consultation rates
phe_flucovid_gg1_base <- phe_flucovid_df %>%
  filter(measures %in% c("covid19_like_indicator_rate", "ili_rate", "ili_rate_2018_19")) %>%
  ggplot(aes(x = week_end_date, y = rates_per_100000, group = measures)) +
  geom_line(aes(colour = measures), size = 1.5) +
  scale_x_date(breaks = phe_flucovid_breaks,
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 400)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "RCGP consultation rates (per 100,000 people)",
       x = "Week end date", y = "")

phe_flucovid_gg1 <-  phe_flucovid_gg1_base +
  scale_colour_manual(name = "",
                      labels = c("COVID-19-like", "Influenza-like", "Influenza-like (2018-19)"),
                      values = c("black", "grey40", "grey80"))

phe_flucovid_gg1_col <- phe_flucovid_gg1_base +
  scale_colour_manual(name = "",
                      labels = c("COVID-19-like", "Influenza-like", "Influenza-like (2018-19)"),
                      values = c("#008080", "#800000", "#8FD694"))

# The second small graph is for hospital admission rates
phe_flucovid_gg2_base <- phe_flucovid_df %>%
  filter(measures %in% c("covid19_hospital_admission_rate", "influenza_hospital_admission_rate")) %>%
  ggplot(aes(x = week_end_date, y = rates_per_100000, group = measures)) +
  geom_line(aes(colour = measures), size = 1.5) +
  scale_x_date(breaks = phe_flucovid_breaks,
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 40)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Hospital admission rates (per 100,000 people)",
       x = "Week end date", y = "")

phe_flucovid_gg2 <- phe_flucovid_gg2_base +
  scale_colour_manual(name = "",
                      labels = c("COVID-19", "Influenza"),
                      values = c("black", "grey40"))

phe_flucovid_gg2_col <- phe_flucovid_gg2_base +
  scale_colour_manual(name = "",
                      labels = c("COVID-19", "Influenza"),
                      values = c("#008080", "#800000"))

# We create another patchwork graph (900 x 1800)
phe_flucovid_gg <- phe_flucovid_gg1 + phe_flucovid_gg2 +
  plot_annotation(title = "Flu remains suppressed, with COVID-19 admission rates in England rising until January 2021.",
                  subtitle = "RCGP consultant rates and hospital admissions rates per 100,000 people in England. 29th June 2020 to 2nd May 2021.",
                  caption = "Source: PHE Weekly national Influenza and COVID-19 surveillance report, Week 18 2021; National flu report data: 5 March 2020 (week 10).")

phe_flucovid_gg_col <- phe_flucovid_gg1_col + phe_flucovid_gg2_col +
  plot_annotation(title = "Flu remains suppressed, with COVID-19 admission rates in England rising until January 2021.",
                  subtitle = "RCGP consultant rates and hospital admissions rates per 100,000 people in England. 29th June 2020 to 2nd May 2021.",
                  caption = "Source: PHE Weekly national Influenza and COVID-19 surveillance report, Week 18 2021; National flu report data: 5 March 2020 (week 10).")

## Saving the graphs
# Save the figures in the required dimension
ggsave(file = "R/COVID By Numbers/COL Figures/COL_dot_incidents_gg.jpeg",
       plot = dot_incidents_gg,
       device = "jpeg",
       height = 10, width = 16)

ggsave(file = "R/COVID By Numbers/COL Figures/COL_phe_flucovid_gg.jpeg",
       plot = phe_flucovid_gg,
       device = "jpeg",
       height = 10, width = 17)

ggsave(file = "R/COVID By Numbers/COL Figures/COL_phe_flucovid_gg_col.jpeg",
       plot = phe_flucovid_gg_col,
       device = "jpeg",
       height = 10, width = 17)
