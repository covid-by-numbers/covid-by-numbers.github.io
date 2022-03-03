## Packages and Themes
library(tidyverse)
library(scales)

# Next, we load the plotting theme
source("/cloud/project/code/00-UPD/CBN Theme.R")

## Create the frame
# Create a data frame with a negative binomial
negbinom_df <- tibble(x = 0:20,
                      y_3 = dnbinom(x, size = 0.1, mu = 3),
                      y_7 = dnbinom(x, size = 0.1, mu = 7)) %>%
  pivot_longer(cols = 2:3,
               names_to = "measure",
               values_to = "prob")

## Making the graph
fig_3_3_gg_upd <- negbinom_df %>%
  ggplot(aes(x = x, y = prob, fill = measure, colour = measure)) +
  geom_col(position = "dodge") +
  scale_fill_manual(aesthetics = c("fill", "colour"),
                    values = c("#15c6d4", "#ec6752"),
                    guide = "none") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.8),
                     labels = percent_format(accuracy = 1)) +
  labs(title = "This is a possible distribution of secondary cases of Covid-19.",
       subtitle  = "The negative binomial distributions where the means (R0) are 3 and 7, and the dispersion parameter (k) is 0.1.",
       x = "Number of secondary cases",
       y = "Probability",
       caption = "Source: Estimating the overdispersion in COVID-19 transmission using outbreak sizes outside China\n(Wellcome Open Research, 2020).") +
  geom_text(x = 0.2, y = 0.73, label = "Mean = 3", hjust = 0,
            fontface = "bold", size = 7, colour = "#15c6d4",
            family = "Freight Text Pro") +
  geom_text(x = 0.4, y = 0.68, label = "Mean = 7", hjust = 0,
            fontface = "bold", size = 7, colour = "#ec6752",
            family = "Freight Text Pro") +
  geom_text(x = 3.5, y = 0.7, label = "Under this model,\nmost infected people do not\npass on the virus",
            size = 7, fontface = "plain", colour = "grey10",
            hjust = 0, family = "Freight Text Pro") +
  geom_curve(x = 3, xend = 1, y = 0.65, yend = 0.5,
             curvature = -0.2, size = 1.5, colour = "#15c6d4",
             arrow = arrow(length = unit(0.5, "cm")))

## Saving the graph
# Saving the graph with the required dimensions
ggsave(file = "/cloud/project/code/00-UPD/fig_3_3_gg_upd.jpeg",
       plot = fig_3_3_gg_upd,
       device = "tiff",
       height = 750/96, width = 1500/96, dpi = 96)
