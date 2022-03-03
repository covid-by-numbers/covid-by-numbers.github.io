## Packages and Themes
library(tidyverse)
library(readxl)
library(curl)
library(janitor)
library(lubridate)

# Next, set the plotting theme
source("/cloud/project/code/00-UPD/CBN Theme.R")

## Drawing the data
# I draw these figures straight from the online files
nisra_deaths_url_2021 <- "https://www.nisra.gov.uk/system/files/statistics/Weekly_Deaths%20-%20w%20e%2031st%20December%202021_0.XLSX"
nisra_deaths_url_2022 <- "https://www.nisra.gov.uk/system/files/statistics/Weekly_Deaths%20-%20w%20e%2011th%20February%202022.XLSX"
nisra_caption <- "Source: Northern Ireland Statistics and Research Agency: Weekly Deaths - Week ending 31 Dec 2021 and 11 Feb 2022."
nisra_2020_first_date <- as_date("2020-01-10")
nisra_2020_last_date <- as_date("2021-01-01")

# Next, we draw two tables from the first spreadsheet
temp <- tempfile()
temp <- curl_download(url = nisra_deaths_url_2021, destfile = temp,
                      quiet = TRUE, mode = "wb")

nisra_table1_tbl <- read_excel(temp,
                               sheet = "Table 1",
                               range = "A4:E56") %>%
  janitor::clean_names() %>%
  dplyr::rename(date = 2, total_deaths = 3, total_deaths_2020 = 4,
                total_deaths_5year_average = 5) %>%
  dplyr::mutate(date_2020 = seq(nisra_2020_first_date,
                                nisra_2020_last_date, "7 days"),
                date = as_date(date))

# From that table, create data frames with 2020 and 2021 values
nisra_table1_2020_df <- nisra_table1_tbl %>%
  dplyr::select(4:6) %>%
  dplyr::rename(total_deaths = total_deaths_2020,
                date = date_2020) %>%
  dplyr::select(date, total_deaths, total_deaths_5year_average)

nisra_table1_2021_df <- nisra_table1_tbl %>%
  dplyr::select(2, 3, 5) %>%
  dplyr::select(date, total_deaths, total_deaths_5year_average)

# A full weekly series of Covid-19 deaths is available in Table 5
nisra_table5_tbl <- read_excel(temp,
                               sheet = "Table 5",
                               range = "D3:CS5") %>%
  t() %>% as_tibble(.name_repair = "minimal") %>%
  dplyr::rename(date = 1, covid_deaths = 2) %>%
  mutate(date = gsub(".*\\\n", "", date),
         date = as_date(parse_date_time(date, orders = "dBY")),
         covid_deaths = as.numeric(covid_deaths))

# We then bind the rows
nisra_deaths_2020_2021_df <- bind_rows(nisra_table1_2020_df,
                                       nisra_table1_2021_df)

# In the 2022 file, we can then draw the figures in the latest year
temp <- curl_download(url = nisra_deaths_url_2022, destfile = temp,
                      quiet = TRUE, mode = "wb")

nisra_table1_2022_tbl <- read_excel(temp,
                                    sheet = "Table 1",
                                    range = "A4:K10") %>%
  janitor::clean_names() %>%
  dplyr::rename(date = 2, total_deaths = 3,
                total_deaths_5year_average = 6, covid_deaths = 11) %>%
  dplyr::select(date, total_deaths, total_deaths_5year_average, covid_deaths) %>%
  dplyr::mutate(date = as_date(date))

# We then join the 2022 values to our previous set
nisra_deaths_df <- nisra_deaths_2020_2021_df %>%
  dplyr::left_join(nisra_table5_tbl, by = "date") %>%
  dplyr::bind_rows(nisra_table1_2022_tbl) %>%
  dplyr::mutate(covid_deaths = replace_na(covid_deaths, 0),
                other_deaths = total_deaths - covid_deaths)

nisra_deaths_tidy_df <- nisra_deaths_df %>%
  dplyr::select(date, covid_deaths, other_deaths) %>%
  tidyr::pivot_longer(cols = 2:3,
                      names_to = "nisra_measure",
                      values_to = "count")

## Making the graph
# First, we set date breaks
nisra_date_breaks <- seq(as_date("2020-03-27"),
                         as_date("2022-02-04"), "13 weeks")

fig_11_2_gg_nisra <- nisra_deaths_tidy_df %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = count, fill = nisra_measure),
           position = "stack") +
  geom_errorbarh(data = nisra_deaths_df,
                 aes(xmin = date - days(3),
                    xmax = date + days(3),
                    y = total_deaths_5year_average),
                 stat = "identity", size = 1.5, height = 0) +
  scale_x_date(breaks = nisra_date_breaks,
               date_labels = "%d %b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  scale_fill_manual(values = c("#ec6752", "#15c6d4"),
                    guide = "none") +
  annotate("text", x = as_date("2020-06-12"), y = 500,
           label = "Deaths involving Covid-19", hjust = 0,
           family = "Freight Text Pro", size = 7,
           fontface = "bold", colour = "#ec6752") +
  annotate("text", x = as_date("2020-06-12"), y = 450,
           label = "Other deaths", hjust = 0,
           family = "Freight Text Pro", size = 7,
           fontface = "bold", colour = "#15c6d4") +
  annotate("text", x = as_date("2021-06-11"), y = 450,
           label = "In 2020-21, the past average is 2015-2019.\nIn 2022, it is 2016-2019 and 2021.",
           family = "Freight Text Pro", size = 6,
           hjust = 0, fontface = "plain", colour = "black") +
  annotate("curve", x = as_date("2021-06-10"), xend = as_date("2021-04-30"),
           y = 450, yend = 330, curvature = 0.2,
           arrow = arrow(length = unit(2, "mm")), colour = "black") +
  labs(title = "Northern Ireland has sustained periods of deaths above the past five-year average.",
       subtitle = str_wrap("Weekly registered deaths in the NI General Registrar's Office Registration System, between 4th January 2020 and 11th February 2022. Public holidays can affect delays between death and registration. Registered deaths are split by whether Covid-19 was mentioned on the certificate.",
                           120),
       x = "Week end date (Friday)",
       y = "Weekly registered deaths",
       caption = nisra_caption)

## Saving the graph
ggsave(file = "/cloud/project/code/00-UPD/fig_11_2_gg_nisra.jpeg",
       plot = fig_11_2_gg_nisra,
       device = "jpeg",
       height = 800/96, width = 1600/96, dpi = 96)
