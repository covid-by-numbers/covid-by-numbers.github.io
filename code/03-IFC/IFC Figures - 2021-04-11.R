## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Negative binomial
# Create a data frame with the negative Binomial (mean is 3 and k is 0.1)
negbinom_df <- tibble(x = 0:20,
                      y = dnbinom(x = 0:20, size = 0.1, mu = 3))

# We can then plot that table as a column graph
negbinom_gg <- negbinom_df %>%
  ggplot(aes(x = x, y = y)) +
  geom_col(aes(fill = stat(x > 0))) +
  scale_fill_manual(values = c("grey50", "grey90"),
                    guide = FALSE) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.8),
                     labels = percent_format(accuracy = 1)) +
  labs(title = "This is a possible distribution of secondary cases of COVID-19.",
       subtitle  = "The negative binomial distribution where the mean (R0) is 3, and the dispersion parameter (k) is 0.1.",
       x = "Number of secondary cases",
       y = "Probability",
       caption = "Source: Estimating the overdispersion in COVID-19 transmission using outbreak sizes outside China\n(Wellcome Open Research, 2020).") +
  geom_text(x = 3.5, y = 0.7, label = "Under this model,\nmost infected people do not\npass on the virus",
            size = 7, fontface = "bold", colour = "grey10",
            hjust = 0) +
  geom_curve(x = 3, xend = 1,
             y = 0.65, yend = 0.5,
             curvature = -0.2, size = 2, colour = "grey10",
             arrow = arrow(length = unit(0.5, "cm"))) +
  geom_text(x = 3, y = 0.3,
            label = "Less than 3% have\nthe average value of 3",
            size = 7, fontface = "bold", colour = "grey50",
            hjust = 0) +
  geom_curve(x = 3, xend = 3,
             y = 0.25, yend = 0.05,
             curvature = 0.2, size = 2, colour = "grey50",
             arrow = arrow(length = unit(0.5, "cm")))

## Draw data
# We draw the model estimates from a prepared file
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/976328/S1164_SPI-M-O_Consensus_Statement.pdf
r_est_uk_df <- read_excel("R/COVID By Numbers/IFC Figures/IFC Figures - 2021-04-11.xlsx",
                          sheet = "DATA-1")

# SPI-O-M consensus estimates for England are then put into a time series
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/977358/R-and-growth-rate-time-series-09-Apr-2021.ods
consensus_eng_df <- read_excel("R/COVID By Numbers/IFC Figures/IFC Figures - 2021-04-11.xlsx", 
           sheet = "DATA-2") %>%
  mutate(date_end = as_date(date_end))

## Creating the graphs
# That produces a graph of different ranged estimates of the reproduction number
r_est_uk_gg <- r_est_uk_df %>%
  ggplot(aes(x = model,
             ymin = round(r_est_lower, digits = 2),
             ymax = round(r_est_upper, digits = 2))) +
  geom_errorbar(size = 1.5, width = 0.2) +
  scale_x_continuous(breaks = 1:8,
                     expand = c(0,0)) +
  scale_y_continuous(limits= c(0, 1.5),
                     expand = c(0,0),
                     breaks = c(0, 0.5, 1)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Different models produce different median estimates of the reproduction number.",
       subtitle = "The bars represent different independent median estimates of R in the UK. The estimate is for 13th to 19th March 2021.",
       caption = "Graphical reconstruction. Source: SPI-M-O: Consensus Statement on COVID-19: 24th March 2021.",
       x = "Independent models from different modelling groups",
       y = "Reproduction number estimate")

# This produces the ribbon of consensus R estimates for England
consensus_eng_breaks <- c("2020-07-17", "2020-09-18", "2020-11-20", "2021-01-29", "2021-04-02") %>%
  as_date()

consensus_labels <- tribble(
  ~date, ~label,
  "2020-06-15", "1",
  "2020-07-04", "2",
  "2020-08-03", "3",
  "2020-09-01", "4",
  "2020-09-14", "5",
  "2020-10-14", "6",
  "2020-11-05", "7",
  "2021-01-04", "8") %>%
  mutate(date = as_date(date))

consensus_eng_gg <- consensus_eng_df %>%
  ggplot(aes(x = date_end,
             ymin = england_est_lower,
             ymax = england_est_upper)) +
  geom_ribbon(alpha = 0.1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_x_date(breaks = consensus_eng_breaks,
               date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(limits = c(0,2),
                     expand = c(0,0)) +
  geom_vline(xintercept = consensus_labels$date, linetype = "dashed") +
  annotate("text", x = consensus_labels$date, y = 1.8,
           label = consensus_labels$label, size = 7, hjust = -0.2) +
  geom_label(x = as_date("2020-06-01"), y = 0.3, fill = "white",
             size = 5, hjust = 0,
             label = "1. Shops re-open.\n2. Pubs and restaurants re-open.\n3. Eat Out to Help Out starts.\n4. Schools re-open.\n5. 'Rule of six' begins.\n6. English tiers introduced.\n7. Second national lockdown starts.\n8. Third national lockdown starts.") +
  labs(title = "The estimated range of the reproduction number in England fluctuates over time.",
       subtitle = "Consensus estimates of R in England, from weeks ending in 29th September 2020 to 30th April 2021.",
       x = "Week end date",
       y = "",
       caption = "Source: The R value and growth rate in England, 30th April 2021.")

## Saving graphs
# Saving the graphs with the required dimensions
ggsave(file = "R/COVID By Numbers/IFC Figures/IFC_negbinom_gg.jpeg",
       plot = negbinom_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/IFC Figures/IFC_r_est_uk_gg.jpeg",
       plot = r_est_uk_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/IFC Figures/IFC_consensus_eng_gg.jpeg",
       plot = consensus_eng_gg,
       device = "jpeg",
       height = 10, width = 15)