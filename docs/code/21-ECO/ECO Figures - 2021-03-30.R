## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
library(patchwork)
library(ggalt)
library(ggrepel)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Drawing and tidying the data
# I draw data frames on labour market statistics from a prepared file
# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/bulletins/employmentintheuk/april2021
ons_hours_df <- read_excel("R/COVID By Numbers/ECO Figures/ECO Figures - 2021-03-30.xlsx",
                           sheet = "DATA-1",
                           col_types = c("text", "date", "numeric")) %>%
  mutate(period_date = as_date(period_date))

# https://www.ons.gov.uk/employmentandlabourmarket/peopleinwork/employmentandemployeetypes/bulletins/uklabourmarket/april2021
ons_lms_df <- read_excel("R/COVID By Numbers/ECO Figures/ECO Figures - 2021-03-30.xlsx",
                                                   sheet = "DATA-2",
                                                   col_types = c("text", "date",
                                                                 "numeric", "numeric", "numeric")) %>%
  mutate(period_date = as_date(period_date))

ons_lms_end <- ons_lms_df %>% filter(period_date == as_date("2021-02-01"))

# https://www.ons.gov.uk/economy/grossdomesticproductgdp/bulletins/quarterlynationalaccounts/julytoseptember2020
ons_gdp_df <- read_excel("R/COVID By Numbers/ECO Figures/ECO Figures - 2021-03-30.xlsx", 
                         sheet = "DATA-3") %>%
  rename(q4_2019 = `2019_q4`,
         q2_2020 = `2020_q2`,
         q3_2020 = `2020_q3`) %>%
  mutate(q3_2020_rank = case_when(services == "Index of Services" ~ 16,
                                  TRUE ~ rank(q3_2020)))

# https://www.ons.gov.uk/businessindustryandtrade/retailindustry/timeseries/ms6y/drsi
ons_drsi_df <- read_excel("R/COVID By Numbers/ECO Figures/ECO Figures - 2021-03-30.xlsx",
                          sheet = "DATA-4",
                          col_types = c("text", "date", "numeric")) %>%
  mutate(month_date = as_date(month_date))

## Creating graphs
ons_hours_date_breaks <- c("2009-02-01", "2013-02-01", "2017-02-01", "2021-02-01") %>%
  as_tibble() %>%
  mutate(period_date = as_date(value)) %>%
  left_join(ons_hours_df, by = "period_date") %>%
  select(period_date, period)

ons_hours_long_labels <- c("Dec to\nFeb 2009", "Dec to\nFeb 2013",
                            "Dec to\nFeb 2017", "Dec to\nFeb 2021")

ons_hours_short_labels <- c("2009", "2013","2017", "Dec to\nFeb 2021")

ons_lms_caption <- "Source: Office for National Statistics: Labour Force Survey."

# The first graph is for actual hours worked
ons_hours_gg <-  ons_hours_df %>%
  ggplot(aes(x = period_date,
             y = total_actual_weekly_hours_mill)) +
  geom_line(size = 1.5) +
  scale_x_date(breaks = ons_hours_date_breaks$period_date,
               labels = ons_hours_long_labels) +
  scale_y_continuous(limits = c(800, 1100)) +
  labs(title = "The recovery in total actual weekly hours was impacted by new restrictions.",
       subtitle = "UK estimated total actual weekly hours worked (all people aged 16 over) in millions, seasonally adjusted.",
       x = "Rolling three-month period",
       y = "",
       caption = ons_lms_caption)

# This is a patchwork of three smaller graphs, for employment, unemployment, and inactivity
# Employment
ons_lms_gg1 <- ons_lms_df %>%
  ggplot(aes(x = period_date, y = employment)) +
  geom_line(size = 1.5) +
  geom_point(data = ons_lms_end,
             aes(x = period_date, y = employment),
             size = 5, na.rm = TRUE) +
  scale_colour_manual(values = c(NA, "black")) +
  scale_x_date(breaks = ons_hours_date_breaks$period_date,
               labels = ons_hours_short_labels) +
  scale_y_continuous(limits = c(65, 80)) +
  theme(legend.position = "None",
        plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(title = "",
       subtitle = "Employment [%], all aged 16 to 64",
       x = "", y = "", caption = "") +
  geom_text(data = ons_lms_end,
            aes(x = period_date, y = employment,
                label = round(employment, digits = 1)),
            size = 7, vjust = 2)

# Unemployment
ons_lms_gg2 <- ons_lms_df %>%
  ggplot(aes(x = period_date, y = unemployment)) +
  geom_line(size = 1.5) +
  geom_point(data = ons_lms_end,
             aes(x = period_date, y = unemployment),
             size = 5, na.rm = TRUE) +
  scale_colour_manual(values = c(NA, "black")) +
  scale_x_date(breaks = ons_hours_date_breaks$period_date,
               labels = ons_hours_short_labels) +
  scale_y_continuous(limits = c(2, 10)) +
  theme(legend.position = "None",
        plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(title = "",
       subtitle = "Unemployment [%], all aged 16 and over",
       x = "", y = "", caption = "") +
  geom_text(data = ons_lms_end,
                  aes(x = period_date, y = unemployment,
                      label = round(unemployment, digits = 1)),
                  size = 7, vjust = -1)

# Inactivity
ons_lms_gg3 <- ons_lms_df %>%
  ggplot(aes(x = period_date, y = inactivity)) +
  geom_line(size = 1.5) +
  geom_point(data = ons_lms_end,
             aes(x = period_date, y = inactivity),
             size = 5, na.rm = TRUE) +
  scale_colour_manual(values = c(NA, "black")) +
  scale_x_date(breaks = ons_hours_date_breaks$period_date,
               labels = ons_hours_short_labels) +
  scale_y_continuous(limits = c(20, 24)) +
  theme(legend.position = "None",
        plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(title = "",
       subtitle = "Economic inactivity [%], all aged 16 to 64",
       x = "", y = "", caption = "") +
  geom_text(data = ons_lms_end,
                  aes(x = period_date, y = inactivity,
                      label = round(inactivity, digits = 1)),
                  size = 7, vjust = -1)

# This is the patchwork graph
ons_lms_gg <- ons_lms_gg1 + ons_lms_gg2 + ons_lms_gg3 +
  plot_annotation(title = "There was an estimated quarterly decrease in the unemployment rate, while the economic inactivity rate increased.",
                  subtitle = "Estimated employment, unemployment and economic inactivity rates for the UK, seasonally adjusted. December to February 2006 to December to February 2021.",
                  caption = ons_lms_caption)

# This is a dumbbell graph, with annotations
ons_gdp_gg <- ons_gdp_df %>%
  mutate(services = fct_reorder(services, q3_2020_rank)) %>%
  ggplot(aes(y = services,
             x = q2_2020,
             xend = q3_2020)) +
  geom_dumbbell(size = 3, size_x = 7, size_xend = 7,
                color ="grey80", colour_x = "grey80", colour_xend = "grey10") +
  scale_x_continuous(limits = c(0, 105)) +
  geom_vline(xintercept = 100,
             linetype = "dashed",
             size = 1.1) +
  labs(title = "Services grew by a revised 17% in Q3 2020, but remained 9% under the Q4 2019 level.",
       subtitle = "UK services gross domestic product index, Q4 (Oct to Dec) 2019 to Q3 (Jul to Sep) 2020.",
       x = "",
       y = "",
       caption = "Source: Office for National Statistics, GDP quarterly national accounts, July to September 2020.") +
  geom_text(data = filter(ons_gdp_df, services == "Index of Services"),
            aes(x = q2_2020, y = services, label = "2020 Q2"),
            colour = "grey80", size = 5, vjust = -1,
            fontface = "bold") +
  geom_text(data = filter(ons_gdp_df, services == "Index of Services"),
            aes(x = q3_2020, y = services, label = "2020 Q3"),
            colour = "grey10", size = 5, vjust = -1,
            fontface = "bold") +
  geom_text(aes(x = 93, y = 2),
            label = "2019 Q4 levels\n= 100",
            size = 5) +
  geom_text(aes(x = 25, y = 5),
            label = "In 2020 Q2, accommodation and food services\nwere at 15% of their 2019 Q4 level.",
            size = 7) +
  annotate(geom = "curve",
           x = 25, xend = 15,
           y = 4, yend = 2.3,
           arrow = arrow(), colour = "grey80",
           curvature = 0.2, size = 1.5)

# This graph is of the internet sales share over time
ons_drsi_breaks <- c("2010-01-01", "2015-01-01", "2020-01-01") %>%
  as_date()

ons_drsi_gg <- ons_drsi_df %>%
  ggplot(aes(x = month_date,
             y = internet_sales_ratio)) +
  geom_line(size = 1.5) +
  geom_point(data = filter(ons_drsi_df,
                           month == "2021 MAR"),
             size = 5) +
  scale_x_date(date_labels = "%b\n%Y",
               breaks = ons_drsi_breaks,
               expand = c(0.02,0)) +
  scale_y_continuous(limits = c(0,40),
                     expand = c(0,0)) +
  labs(title = "Excluding auto fuels, the internet share of all sales reached a record proportion of 36% in February 2021.",
       subtitle = "Estimated internet share of sales value (all retail excluding auto fuel, seasonally adjusted) in Great Britain [%], for January 2008 to March 2021.",
       x = "Month",
       y = "",
       caption = "Source: Office for National Statistics: Monthly Business survey - Retail Sales Inquiry, April 2021.")

## Saving the graphs
# Save the figures in the required dimensions
ggsave(file = "R/COVID By Numbers/ECO Figures/ECO_ons_hours_gg.jpeg",
       plot = ons_hours_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/ECO Figures/ECO_ons_lms_gg.jpeg",
       plot = ons_lms_gg,
       device = "jpeg",
       height = 10, width = 20)

ggsave(file = "R/COVID By Numbers/ECO Figures/ECO_ons_gdp_gg.jpeg",
       plot = ons_gdp_gg,
       device = "jpeg",
       height = 10, width = 20)

ggsave(file = "R/COVID By Numbers/ECO Figures/ECO_ons_drsi_gg.jpeg",
       plot = ons_drsi_gg,
       device = "jpeg",
       height = 10, width = 15)