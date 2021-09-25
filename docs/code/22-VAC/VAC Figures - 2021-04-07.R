## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(scales)
library(patchwork)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw and tidy data
# First, I draw in the Pfizer/BioNTech estimated incidence rates from a prepared file
# https://www.nejm.org/doi/full/10.1056/NEJMoa2034577
pfizer_biontech_graph_df <- read_excel("R/COVID By Numbers/VAC Figures/VAC Figures - 2021-04-07.xlsx", 
                                       sheet = "DATA-1")

# Next, we draw the University of Edinburgh vaccine effectiveness estimates
# https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(21)00677-2/fulltext
edinburgh_vaccine_df <- read_excel("R/COVID By Numbers/VAC Figures/VAC Figures - 2021-04-07.xlsx",
                                   sheet = "DATA-2")

## Creating the graphs
# We recreate the famous Pfizer/BioNTech trial graph
pfizer_biontech_graph_gg <- pfizer_biontech_graph_df %>%
  ggplot(aes(x = day_round, y = incidence_rate, group = type)) +
  geom_step(aes(colour = type), size = 1.5) +
  geom_text(data = filter(pfizer_biontech_graph_df, day_round == 48),
            aes(y = incidence_rate,
                colour = type,
                label = str_to_title(type)),
            vjust = -1.5, fontface = "bold", size = 7) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 2.4),
                     breaks = pretty_breaks()) +
  scale_colour_manual(values = c("grey50", "black")) +
  theme(legend.position = "none") + 
  labs(title = "COVID-19 cases keep rising in the placebo group, soon after the first dose.",
      subtitle = "Cumulative incidence rates [%] of COVID-19 cases, from day of the first dose in the Pfizer/BioNTech phase III trial.",
      x = "Days since first dose",
      y = "Cumulative COVID-19 case rate [%]",
      caption = "Graphical reconstruction. Source: Pfizer/BioNTech/NEJM and FDA.")

# This is a patchwork graph, following a similar style to the Financial Times
# The first half shows vaccine effectiveness estimates over time
edinburgh_vaccine_labels <- c("7 to 13", "14 to 20", "21 to 27", "28 to 34", "35 to 41")

edinburgh_vaccine_gg1 <- edinburgh_vaccine_df %>%
  filter(age_group == "All adults") %>%
  ggplot(aes(x = day_end, y = ve_central,
             ymin = ve_lower, ymax = ve_upper)) +
  geom_pointrange(size = 1.5) +
  geom_ribbon(alpha = 0.1) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Vaccine effectiveness estimate [%]\nfor both vaccines",
       x = "Days since vaccination", y = "") +
  scale_x_continuous(breaks = c(13, 20, 27, 34, 41),
                     limits = c(12, 42),
                     labels = edinburgh_vaccine_labels) +
  scale_y_continuous(limits = c(0,100),
                     expand = c(0,0)) +
  geom_text(x = 20, y = 25,
           label = "95% confidence intervals
           around the central estimate",
           size = 5, fontface = "bold") +
  geom_curve(x = 20, xend = 20,
             y = 30, yend = 48,
             curvature = -0.2, size = 2,
             arrow = arrow(length = unit(0.5, "cm")))

# The second graph shows the estimates by age group
edinburgh_vaccine_gg2 <- edinburgh_vaccine_df %>%
  filter(age_group != "All adults") %>%
  ggplot(aes(x = age_group, y = ve_central,
             ymin = ve_lower, ymax = ve_upper)) +
  geom_pointrange(size = 1.5) +
  scale_y_continuous(limits = c(0,100), expand = c(0,0)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Vaccine effectiveness estimate [%]\nbetween 28 to 34 days",
       x = "Age group", y = "")

# Using patchwork, we put these two graphs together
edinburgh_vaccine_gg <- edinburgh_vaccine_gg1 + edinburgh_vaccine_gg2 +
  plot_annotation(title = "Scottish data suggests one dose of COVID-19 vaccines offer protection against admission.",
                  subtitle = "Vaccine effectiveness (combined for both vaccines) for reducing hospitalisation, by the number of days since vaccination.",
                  caption = "Source: Interim findings from first-dose mass COVID-19 vaccination roll-out and COVID-19 hospital admissions in Scotland:\na national prospective cohort study (Lancet, 2021).")

## Saving graphs
# Save those figures in the required dimensions
ggsave(file = "R/COVID By Numbers/VAC Figures/VAC_pfizer_biontech_graph_gg.jpeg",
       plot = pfizer_biontech_graph_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/VAC Figures/VAC_edinburgh_vaccine_gg.jpeg",
      plot = edinburgh_vaccine_gg,
      device = "jpeg",
      height = 10, width = 15)