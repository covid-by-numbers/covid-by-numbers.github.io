## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw data
# I draw data from a prepared Excel file
# https://www.ons.gov.uk/peoplepopulationandcommunity/healthandsocialcare/healthandwellbeing/bulletins/coronavirusandthesocialimpactsongreatbritain/26march2021
ons_lifestyle_df <- read_excel("R/COVID By Numbers/WEL Figures/WEL Figures - 2021-04-03.xlsx", 
                               sheet = "DATA-1",
                               col_types = c("date", "text", "numeric", "numeric",
                                             "numeric", "numeric", "numeric", "numeric",
                                             "numeric", "numeric")) %>%
  mutate(survey_end_date = as_date(survey_end_date))


## Creating the graphs
# This is patchwork graph with four elements
ons_lifestyle_breaks = c("2020-03-30", "2020-09-20", "2020-12-20", "2021-03-28") %>%
  as_date()

# This is for the satisfaction question
ons_lifesatisfaction_gg <- ons_lifestyle_df %>%
  ggplot(aes(x = survey_end_date, y = life_satisfaction)) +
  geom_line(size = 1.5) +
  scale_x_date(breaks = ons_lifestyle_breaks,
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(limits = c(5.5, 8.0)) +
  geom_hline(yintercept = median(ons_lifestyle_df$life_satisfaction_feb2020),
             linetype = "dashed", colour = "grey50", size = 1.1) +
  geom_text(aes(x = as_date("2020-09-20"), y = life_satisfaction_feb2020),
            color = "grey50", size = 5, fontface = "plain",
            vjust = -1, label = "February 2020 level") +
  theme(plot.subtitle = element_text(size = 16,
                                     face = "plain")) +
  labs(x = "", y = "",
       subtitle = expression('Overall, how'~bold(satisfied)~'are you with your life nowadays?'))

ons_lifesatisfaction_gg_col <- ons_lifesatisfaction_gg +
  geom_line(size = 1.5, colour = "#008080") +
  theme(plot.subtitle = element_text(colour = "#008080"))

# This is for the worthwhile question
ons_worthwhile_gg <- ons_lifestyle_df %>%
  ggplot(aes(x = survey_end_date, y = worthwhile)) +
  geom_line(size = 1.5) +
  scale_x_date(breaks = ons_lifestyle_breaks,
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(limits = c(5.5, 8.0)) +
  geom_hline(yintercept = median(ons_lifestyle_df$worthwhile_feb2020),
             linetype = "dashed", colour = "grey50", size = 1.1) +
  geom_text(aes(x = as_date("2020-09-20"), y = worthwhile_feb2020),
            color = "grey50", size = 5, fontface = "plain",
            vjust = -1, label = "February 2020 level") +
  theme(plot.subtitle = element_text(size = 16,
                                     face = "plain")) +
  labs(x = "", y = "",
       subtitle = expression('Overall, to what extent do you feel that the things you do in your life are'~bold(worthwhile)~'?'))

ons_worthwhile_gg_col <- ons_worthwhile_gg +
  geom_line(size = 1.5, colour = "#800000") +
  theme(plot.subtitle = element_text(colour = "#800000"))

# This is for the happiness question
ons_happiness_gg <- ons_lifestyle_df %>%
  ggplot(aes(x = survey_end_date, y = happiness)) +
  geom_line(size = 1.5) +
  scale_x_date(breaks = ons_lifestyle_breaks,
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(limits = c(5.5, 8.0)) +
  geom_hline(yintercept = median(ons_lifestyle_df$happiness_feb2020),
             linetype = "dashed", colour = "grey50", size = 1.1) +
  geom_text(aes(x = as_date("2020-09-20"), y = happiness_feb2020),
            color = "grey50", size = 5, fontface = "plain",
            vjust = -1, label = "February 2020 level") +
  theme(plot.subtitle = element_text(size = 16,
                                     face = "plain")) +
  labs(x = "", y = "",
       subtitle = expression('Overall, how'~bold(happy)~'did you feel yesterday?'))

ons_happiness_gg_col <- ons_happiness_gg +
  geom_line(size = 1.5, colour = "#8FD694") +
  theme(plot.subtitle = element_text(colour = "#8FD694"))

# This is for the anxiety question
ons_anxiety_gg <- ons_lifestyle_df %>%
  ggplot(aes(x = survey_end_date, y = anxiety)) +
  geom_line(size = 1.5) +
  scale_x_date(breaks = ons_lifestyle_breaks,
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(limits = c(2.5, 5.5),
                     breaks = c(2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5)) +
  geom_hline(yintercept = median(ons_lifestyle_df$anxiety_feb2020),
             linetype = "dashed", colour = "grey50", size = 1.1) +
  geom_text(aes(x = as_date("2020-09-20"), y = anxiety_feb2020),
            color = "grey50", size = 5, fontface = "plain",
            vjust = 1, label = "February 2020 level") +
  theme(plot.subtitle = element_text(size = 16,
                                     face = "plain")) +
  labs(x = "", y = "",
       subtitle = expression('Overall, how'~bold(anxious)~'did you feel yesterday?'))

ons_anxiety_gg_col <- ons_anxiety_gg +
  geom_line(size = 1.5, colour = "#FFC857") +
  theme(plot.subtitle = element_text(colour = "#FFC857"))

# This is the patchwork of the four graphs
ons_lifestyle_gg <-
  (ons_lifesatisfaction_gg | ons_happiness_gg ) / ( ons_worthwhile_gg | ons_anxiety_gg ) +
  plot_annotation(title = "Estimated life satisfaction in Great British adults has yet to recover to its pre-lockdown level.",
                  subtitle = "Mean estimated scores (out of 10), for a survey of adults in Great Britain between March 2020 and March 2021.",
                  caption = "Source: Office for National Statistics - Opinions and Lifestyle surveys.")

ons_lifestyle_gg_col <-
  (ons_lifesatisfaction_gg_col | ons_happiness_gg_col ) / ( ons_worthwhile_gg_col | ons_anxiety_gg_col ) +
  plot_annotation(title = "Estimated life satisfaction in Great British adults has yet to recover to its pre-lockdown level.",
                  subtitle = "Mean estimated scores (out of 10), for a survey of adults in Great Britain between March 2020 and March 2021.",
                  caption = "Source: Office for National Statistics - Opinions and Lifestyle surveys.")

## Saving the graph
# Save the figure in the required dimensions
ggsave(file = "R/COVID By Numbers/WEL Figures/WEL_ons_lifestyle_gg.jpeg",
       plot = ons_lifestyle_gg,
       device = "jpeg",
       height = 10, width = 20)

ggsave(file = "R/COVID By Numbers/WEL Figures/WEL_ons_lifestyle_gg_col.jpeg",
       plot = ons_lifestyle_gg_col,
       device = "jpeg",
       height = 10, width = 20)