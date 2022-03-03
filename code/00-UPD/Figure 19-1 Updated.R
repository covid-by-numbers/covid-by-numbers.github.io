## Packages and Themes
library(tidyverse)
library(readxl)
library(lubridate)
library(curl)
library(scales)
library(patchwork)

# Next, we load the plotting theme
source("/cloud/project/code/00-UPD/CBN Theme.R")

## Drawing the data
# We draw the data directly from the online ONS file
temp <- tempfile()
ons_url <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fhealthandwellbeing%2fdatasets%2fcoronavirusandthesocialimpactsongreatbritaindata%2f18february2022/referencetables180222.xlsx"
ons_range <- "A4:CG10"
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

# Dates are in a text format
ons_date_tbl <- str_split(str_squish(ons_opn_tbl$date_text),
                          " to ", 2, simplify = TRUE) %>%
  as_tibble(.name_repair = "unique") %>%
  rename(text_1 = 1, text_2 = 2)

ons_date_tbl <- ons_date_tbl %>%
  mutate(day_1 = as.numeric(word(text_1)),
         day_2 = as.numeric(word(text_2)),
         month_1 = str_trim(str_replace_all(text_1, "[:digit:]", "")),
         month_2 = str_trim(str_replace_all(text_2, "[:digit:]", ""))) %>%
  mutate(month_1 = case_when(month_1 == "" ~ month_2,
                             TRUE ~ month_1),
         month_1 = match(str_sub(month_1, 1, 3), month.abb),
         month_2 = match(str_sub(month_2, 1, 3), month.abb)) %>%
  mutate(year_change_1 = case_when(month_1 - lag(month_1, 1) < 0 ~ 1,
                                   TRUE ~ 0),
         year_change_2 = case_when(month_2 - lag(month_2, 1) < 0 ~ 1,
                                   TRUE ~ 0),
         year_1 = 2020 + cumsum(year_change_1),
         year_2 = 2020 + cumsum(year_change_2)) %>%
  mutate(start_date = make_date(year = year_1,
                                month = month_1,
                                day = day_1),
         end_date = make_date(year = year_2,
                              month = month_2,
                              day = day_2))

# We then use the final two columns
ons_dates_df <- ons_date_tbl %>%
  dplyr::select(start_date, end_date)

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
  geom_line(size = 1.5, colour = "#B8AB00") +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(limits = c(2.5, 5.5),
                     breaks = seq(2.5, 5.5, 0.5)) +
  geom_hline(yintercept = ons_feb2020_df$anxious,
             linetype = "dashed", colour = "grey50", size = 1.1) +
  geom_text(aes(x = as_date("2020-09-20"),
                y = ons_feb2020_df$anxious),
            color = "grey50", size = 5, fontface = "plain",
            vjust = 1.2, label = "February 2020 level",
            family = "Freight Text Pro") +
  theme(plot.subtitle = element_text(size = 16,
                                     face = "plain",
                                     colour = "#B8AB00")) +
  labs(x = "", y = "",
       subtitle = expression('Overall, how'~bold(anxious)~'did you feel yesterday?'))

# This is the patchwork of the four graphs
fig_19_1_gg_upd <-
  (ons_lifesatisfaction_gg | ons_happiness_gg ) / ( ons_worthwhile_gg | ons_anxiety_gg ) +
  plot_annotation(title = "Estimated life satisfaction in Great British adults has yet to recover to its pre-lockdown level.",
                  subtitle = "Mean estimated scores (out of 10), for a survey of adults in Great Britain between March 2020 and February 2022.",
                  caption = "Source: Office for National Statistics - Opinions and Lifestyle surveys.")

## Saving the graph
ggsave(file = "/cloud/project/code/00-UPD/fig_19_1_gg_upd.jpeg",
       plot = fig_19_1_gg_upd,
       device = "jpeg",
       height = 800/96, width = 1800/96, dpi = 96)
