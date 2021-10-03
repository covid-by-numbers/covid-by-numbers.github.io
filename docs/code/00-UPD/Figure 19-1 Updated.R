## Packages and Themes
library(tidyverse)
library(readxl)
library(lubridate)
library(curl)
library(scales)
library(patchwork)

# Next, we load the plotting theme
source("/cloud/project/code/0-UPD/CBN Theme.R")

## Drawing the data
# We draw the data directly from the online ONS file
temp <- tempfile()
ons_url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fhealthandwellbeing%2fdatasets%2fcoronavirusandthesocialimpactsongreatbritaindata%2fcurrent/referencetables240921.xlsx"
ons_range <- "A4:BW10"
temp <- curl_download(url = ons_url, destfile = temp,
                      quiet = TRUE, mode = "wb")

ons_opn_tbl <- read_excel(temp, sheet = "Table 1b",
                          range = ons_range) %>%
  drop_na() %>% t() %>%
  as_tibble(rownames = NA, .name_repair = "unique") %>%
  slice(-1) %>%
  rename(life_satisfaction = 1, worthwhile = 2,
         happiness = 3, anxious = 4) %>%
  rownames_to_column(var = "date_text")

# Dates are in a text format, so I provide a tibble
ons_dates_df <- tribble(~start_date, ~end_date,
                        "2020-03-20", "2020-03-30",
                        "2020-03-27", "2020-04-06",
                        "2020-04-03", "2020-04-13",
                        "2020-04-09", "2020-04-20",
                        "2020-04-17", "2020-04-27",
                        "2020-04-24", "2020-05-03",
                        "2020-05-01", "2020-05-10",
                        "2020-05-07", "2020-05-17",
                        "2020-05-14", "2020-05-17",
                        "2020-05-21", "2020-05-24",
                        "2020-05-28", "2020-05-31",
                        "2020-06-04", "2020-06-07",
                        "2020-06-11", "2020-06-14",
                        "2020-06-18", "2020-06-21",
                        "2020-06-25", "2020-06-28",
                        "2020-07-02", "2020-07-05",
                        "2020-07-08", "2020-07-12",
                        "2020-07-15", "2020-07-19",
                        "2020-07-22", "2020-07-26",
                        "2020-07-29", "2020-08-02",
                        "2020-08-05", "2020-08-09",
                        "2020-08-12", "2020-08-16",
                        "2020-08-26", "2020-08-30",
                        "2020-09-09", "2020-09-13",
                        "2020-09-16", "2020-09-20",
                        "2020-09-24", "2020-09-27",
                        "2020-09-30", "2020-10-04",
                        "2020-10-07", "2020-10-11",
                        "2020-10-14", "2020-10-18",
                        "2020-10-21", "2020-10-25",
                        "2020-10-28", "2020-11-01",
                        "2020-11-05", "2020-11-08",
                        "2020-11-11", "2020-11-15",
                        "2020-11-18", "2020-11-22",
                        "2020-11-25", "2020-11-29",
                        "2020-12-02", "2020-12-06",
                        "2020-12-10", "2020-12-13",
                        "2020-12-16", "2020-12-20",
                        "2020-12-22", "2021-01-03",
                        "2021-01-07", "2021-01-10",
                        "2021-01-13", "2021-01-17",
                        "2021-01-20", "2021-01-24",
                        "2021-01-27", "2021-01-31",
                        "2021-02-03", "2021-02-07",
                        "2021-02-10", "2021-02-14",
                        "2021-02-17", "2021-02-21",
                        "2021-02-24", "2021-02-28",
                        "2021-03-03", "2021-03-07",
                        "2021-03-10", "2021-03-14",
                        "2021-03-17", "2021-03-21",
                        "2021-03-24", "2021-03-28",
                        "2021-03-31", "2021-04-04",
                        "2021-04-07", "2021-04-11",
                        "2021-04-14", "2021-04-18",
                        "2021-04-21", "2021-04-25",
                        "2021-04-28", "2021-05-03",
                        "2021-05-05", "2021-05-09",
                        "2021-05-12", "2021-05-16",
                        "2021-05-19", "2021-05-23",
                        "2021-05-26", "2021-05-30",
                        "2021-06-02", "2021-06-06",
                        "2021-06-09", "2021-06-13",
                        "2021-06-16", "2021-06-20",
                        "2021-06-23", "2021-06-27",
                        "2021-06-30", "2021-07-04",
                        "2021-07-07", "2021-07-11",
                        "2021-07-14", "2021-07-18",
                        "2021-07-21", "2021-07-25",
                        "2021-07-28", "2021-08-01",
                        "2021-08-04", "2021-08-08",
                        "2021-08-11", "2021-08-15",
                        "2021-08-18", "2021-08-22",
                        "2021-08-25", "2021-09-05",
                        "2021-09-08", "2021-09-19") %>%
  mutate(start_date = as_date(start_date),
         end_date = as_date(end_date))

# Bind the dates to the main table
ons_opn_df <- bind_cols(ons_opn_tbl, ons_dates_df) %>%
  filter(start_date != as_date("2020-05-07")) %>%
  mutate(life_satisfaction = as.numeric(life_satisfaction),
         worthwhile = as.numeric(worthwhile),
         happiness = as.numeric(happiness),
         anxious = as.numeric(anxious))

# We set the February 2020 levels
ons_feb2020_df <- tribble(
  ~life_satisfaction, ~worthwhile, ~happiness, ~anxious,
  7.3, 7.6, 7.2, 3.5)

## Creating the graph
# This is a patchwork with four elements
# This is for the satisfaction question
ons_lifesatisfaction_gg <- ons_opn_df %>%
  ggplot(aes(x = end_date, y = life_satisfaction)) +
  geom_line(size = 1.5, colour = "#15c6d4") +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(limits = c(5.5, 8.0)) +
  geom_hline(yintercept = ons_feb2020_df$life_satisfaction,
             linetype = "dashed", colour = "grey50", size = 1.1) +
  geom_text(aes(x = as_date("2020-09-20"),
                y = ons_feb2020_df$life_satisfaction),
            color = "grey50", size = 5, fontface = "plain",
            vjust = -1, label = "February 2020 level",
            family = "Freight Text Pro") +
  theme(plot.subtitle = element_text(size = 16,
                                     face = "plain",
                                     colour = "#15c6d4")) +
  labs(x = "", y = "",
       subtitle = expression('Overall, how'~bold(satisfied)~'are you with your life nowadays?'))

# This is for the worthwhile question
ons_worthwhile_gg <- ons_opn_df %>%
  ggplot(aes(x = end_date, y = worthwhile)) +
  geom_line(size = 1.5, colour = "#ec6752") +
  scale_x_date(date_breaks ="3 months",
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(limits = c(5.5, 8.0)) +
  geom_hline(yintercept = ons_feb2020_df$worthwhile,
             linetype = "dashed", colour = "grey50", size = 1.1) +
  geom_text(aes(x = as_date("2020-09-20"),
                y = ons_feb2020_df$worthwhile),
            color = "grey50", size = 5, fontface = "plain",
            vjust = -1, label = "February 2020 level",
            family = "Freight Text Pro") +
  theme(plot.subtitle = element_text(size = 16,
                                     face = "plain",
                                     colour = "#ec6752")) +
  labs(x = "", y = "",
       subtitle = expression('Overall, to what extent do you feel that the things you do in your life are'~bold(worthwhile)~'?'))

# This is for the happiness question
ons_happiness_gg <- ons_opn_df %>%
  ggplot(aes(x = end_date, y = happiness)) +
  geom_line(size = 1.5, colour = "#009fdb") +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(limits = c(5.5, 8.0)) +
  geom_hline(yintercept = ons_feb2020_df$happiness,
             linetype = "dashed", colour = "grey50", size = 1.1) +
  geom_text(aes(x = as_date("2020-09-20"),
                y = ons_feb2020_df$happiness),
            color = "grey50", size = 5, fontface = "plain",
            vjust = -1, label = "February 2020 level",
            family = "Freight Text Pro") +
  theme(plot.subtitle = element_text(size = 16,
                                     face = "plain",
                                     colour = "#009fdb")) +
  labs(x = "", y = "",
       subtitle = expression('Overall, how'~bold(happy)~'did you feel yesterday?'))

# This is for the anxiety question
ons_anxiety_gg <- ons_opn_df %>%
  ggplot(aes(x = end_date, y = anxious)) +
  geom_line(size = 1.5, colour = "#ffef00") +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(limits = c(2.5, 5.5),
                     breaks = c(2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5)) +
  geom_hline(yintercept = ons_feb2020_df$anxious,
             linetype = "dashed", colour = "grey50", size = 1.1) +
  geom_text(aes(x = as_date("2020-09-20"),
                y = ons_feb2020_df$anxious),
            color = "grey50", size = 5, fontface = "plain",
            vjust = 1.2, label = "February 2020 level",
            family = "Freight Text Pro") +
  theme(plot.subtitle = element_text(size = 16,
                                     face = "plain",
                                     colour = "#ffef00")) +
  labs(x = "", y = "",
       subtitle = expression('Overall, how'~bold(anxious)~'did you feel yesterday?'))

# This is the patchwork of the four graphs
fig_19_1_gg_upd <-
  (ons_lifesatisfaction_gg | ons_happiness_gg ) / ( ons_worthwhile_gg | ons_anxiety_gg ) +
  plot_annotation(title = "Estimated life satisfaction in Great British adults has yet to recover to its pre-lockdown level.",
                  subtitle = "Mean estimated scores (out of 10), for a survey of adults in Great Britain between March 2020 and September 2021.",
                  caption = "Source: Office for National Statistics - Opinions and Lifestyle surveys.")

## Saving the graph
ggsave(file = "/cloud/project/code/00-UPD/fig_19_1_gg_upd.jpeg",
       plot = fig_19_1_gg_upd,
       device = "jpeg",
       height = 800/96, width = 1800/96, dpi = 96)
