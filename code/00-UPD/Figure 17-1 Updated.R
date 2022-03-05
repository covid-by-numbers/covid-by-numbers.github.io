## Packages and Themes
library(tidyverse)
library(curl)
library(readODS)
library(lubridate)
library(scales)
library(patchwork)

# Next, we load the plotting theme
source("/cloud/project/code/00-UPD/CBN Theme.R")

## Drawing the data
# We draw the data from the online weekly flu and Covid-19 report file
ukhsa_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1057083/Weekly_Influenza_and_COVID19_report_data_w8.ods"
ukhsa_2021_w_end_date <- as_date("2021-02-21")
ukhsa_2022_w_end_date <- as_date("2022-02-20")
ukhsa_analysis_date <- as_date("2021-07-04")
ukhsa_fig33_range <- "B8:E61"
ukhsa_fig39_range <- "B8:D60"

# These lines change aspects of the final graph
ukhsa_graph_title <- "Flu was suppressed during winter again, with Covid-19 admission rates in England rising until Jan-22."
ukhsa_graph_subtitle <- "RCGP consultant rates and hospital admissions rates per 100,000 people in England, between 28 June 2021 and 20 Febraury 2022."
ukhsa_graph_caption <- "Source: UKHSA Weekly national Influenza and COVID-19 surveillance report, Week 8 2022; PHE national flu report: 18 April 2019 (week 16)."
ukhsa_graph_pos_date <- as_date("2021-10-03")

phe_url <- "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/796520/Weekly_national_influenza_report_data_week_16_2019.ods"
phe_range <- "C9:F42"

temp <- tempfile()
temp <- curl_download(url = ukhsa_url, destfile = temp,
                      quiet = TRUE, mode = "wb")

ukhsa_fig33_tbl <- read_ods(temp, sheet = "Figure_33&34__Primary_care",
                            range = ukhsa_fig33_range) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  dplyr::mutate(week_end_date = seq(ukhsa_2021_w_end_date,
                                    ukhsa_2022_w_end_date, by = "7 days")) %>%
  filter(week_end_date != ukhsa_2021_w_end_date)

ukhsa_fig39_tbl <- read_ods(temp, sheet = "Figure_39__SARI_Watch-hospital",
                            range = ukhsa_fig39_range) %>%
  janitor::clean_names() %>%
  as_tibble()

# Next, draw past influenza-like illness consultation rates from a PHE file
temp <- curl_download(url = phe_url, destfile = temp,
                      quiet = TRUE, mode = "wb")

phe_rcgp_tbl <- read_ods(temp, sheet = "RCGP",
                         range = phe_range) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  dplyr::select(1, 4) %>%
  dplyr::rename(week_number = 1, ili_rate_2018_19 = 2)

# We create our data frame
ukhsa_flucovid_df <- dplyr::full_join(ukhsa_fig33_tbl, ukhsa_fig39_tbl,
                                      by = "week_number") %>%
  dplyr::filter(week_end_date >= ukhsa_analysis_date) %>%
  dplyr::left_join(phe_rcgp_tbl, by = "week_number") %>%
  dplyr::select(week_number, week_end_date, covid_19_like_indicator_rate,
                ili_rate, ili_rate_2018_19,
                covid_19_hospital_admission_rate, influenza_hospital_admission_rate) %>%
  tidyr::pivot_longer(cols = 3:7, names_to = "measures",
                      values_to = "rates") %>%
  tidyr::drop_na()

# We set the breaks for the graphs
ukhsa_flucovid_breaks <- seq(as_date("2021-07-11"), ukhsa_2022_w_end_date,
                             by = "8 weeks")

## Making the graphs
ukhsa_flucovid_gg1 <- ukhsa_flucovid_df %>%
  dplyr::filter(measures %in% c("covid_19_like_indicator_rate", "ili_rate", "ili_rate_2018_19")) %>%
  ggplot(aes(x = week_end_date, y = rates, group = measures)) +
  geom_line(aes(colour = measures), size = 1.5) +
  scale_x_date(breaks = ukhsa_flucovid_breaks,
               date_labels = "%d %b\n%Y") +
  scale_y_continuous(labels = scales::comma_format(),
                     expand = c(0,0),
                     limits = c(0, 1300)) +
  scale_colour_manual(guide = "none",
                      values = c("#15c6d4", "#ec6752", "#B8AB00")) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "RCGP consultation rates (per 100,000 people)",
       x = "Week end date (Sunday)", y = "") +
  annotate("text", x = ukhsa_graph_pos_date, y = 1200,
           label = "Covid-19-like indicator", hjust = 0,
           family = "Freight Text Pro", size = 6,
           fontface = "bold", colour = "#15c6d4") +
  annotate("text", x = ukhsa_graph_pos_date, y = 100,
           label = "Influenza-like illness", hjust = 0,
           family = "Freight Text Pro", size = 6,
           fontface = "bold", colour = "#ec6752") +
  annotate("text", x = ukhsa_graph_pos_date, y = 200,
           label = "Influenza-like illness (2018-19)", hjust = 0,
           family = "Freight Text Pro", size = 6,
           fontface = "bold", colour = "#B8AB00")

ukhsa_flucovid_gg2 <- ukhsa_flucovid_df %>%
  filter(measures %in% c("covid_19_hospital_admission_rate", "influenza_hospital_admission_rate")) %>%
  ggplot(aes(x = week_end_date, y = rates, group = measures)) +
  geom_line(aes(colour = measures), size = 1.5) +
  scale_x_date(breaks = ukhsa_flucovid_breaks,
               date_labels = "%d %b\n%Y") +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, NA)) +
  scale_colour_manual(guide = "none",
                      values = c("#15c6d4", "#ec6752")) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Hospital admission rates (per 100,000 people)",
       x = "Week end date (Sunday)", y = "") +
  annotate("text", x = ukhsa_graph_pos_date, y = 10,
           label = "Covid-19", hjust = 0,
           family = "Freight Text Pro", size = 6,
           fontface = "bold", colour = "#15c6d4") +
  annotate("text", x = ukhsa_graph_pos_date, y = 1,
           label = "Influenza", hjust = 0,
           family = "Freight Text Pro", size = 6,
           fontface = "bold", colour = "#ec6752")

fig_17_1_gg_upd <- ukhsa_flucovid_gg1 + ukhsa_flucovid_gg2 +
  plot_annotation(title = ukhsa_graph_title,
                  subtitle = ukhsa_graph_subtitle,
                  caption = ukhsa_graph_caption)

## Saving the graph
ggsave(file = "/cloud/project/code/00-UPD/fig_17_1_gg_upd.jpeg",
       plot = fig_17_1_gg_upd,
       device = "jpeg",
       height = 800/96, width = 1600/96, dpi = 96)

## Log scales version
ukhsa_graph_subtitle_log <- paste0(
  ukhsa_graph_subtitle,
  " The two graphs both have logarithmic scales, with zero values suppressed. The same distance means the same multiplicative factor.")

# First graph
ukhsa_flucovid_gg1_log <- ukhsa_flucovid_df %>%
  dplyr::filter(measures %in% c("covid_19_like_indicator_rate", "ili_rate", "ili_rate_2018_19")) %>%
  ggplot(aes(x = week_end_date, y = rates, group = measures)) +
  geom_line(aes(colour = measures), size = 1.5) +
  scale_x_date(breaks = ukhsa_flucovid_breaks,
               date_labels = "%d %b\n%Y") +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1),
                     expand = c(0,0),
                     limits = c(NA, 1500),
                     trans = "log10") +
  scale_colour_manual(guide = "none",
                      values = c("#15c6d4", "#ec6752", "#B8AB00")) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "RCGP consultation rates (per 100,000 people)",
       x = "Week end date (Sunday)", y = "") +
  annotate("text", x = ukhsa_graph_pos_date, y = 1300,
           label = "Covid-19-like indicator", hjust = 0,
           family = "Freight Text Pro", size = 6,
           fontface = "bold", colour = "#15c6d4") +
  annotate("text", x = ukhsa_graph_pos_date, y = 2,
           label = "Influenza-like illness", hjust = 0,
           family = "Freight Text Pro", size = 6,
           fontface = "bold", colour = "#ec6752") +
  annotate("text", x = ukhsa_graph_pos_date, y = 20,
           label = "Influenza-like illness\n(2018-19)", hjust = 0,
           family = "Freight Text Pro", size = 6,
           fontface = "bold", colour = "#B8AB00")

# Second graph
ukhsa_flucovid_gg2_log <- ukhsa_flucovid_df %>%
  filter(measures %in% c("covid_19_hospital_admission_rate", "influenza_hospital_admission_rate"),
         rates > 0) %>%
  ggplot(aes(x = week_end_date, y = rates, group = measures)) +
  geom_line(aes(colour = measures), size = 1.5) +
  scale_x_date(breaks = ukhsa_flucovid_breaks,
               date_labels = "%d %b\n%Y") +
  scale_y_continuous(expand = c(0,0),
                     trans = "log10") +
  scale_colour_manual(guide = "none",
                      values = c("#15c6d4", "#ec6752")) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Hospital admission rates (per 100,000 people)",
       x = "Week end date (Sunday)", y = "") +
  annotate("text", x = ukhsa_graph_pos_date, y = 12,
           label = "Covid-19", hjust = 0,
           family = "Freight Text Pro", size = 6,
           fontface = "bold", colour = "#15c6d4") +
  annotate("text", x = ukhsa_graph_pos_date, y = 0.4,
           label = "Influenza", hjust = 0,
           family = "Freight Text Pro", size = 6,
           fontface = "bold", colour = "#ec6752")

# Patchwork
fig_17_1_gg_upd_log <- ukhsa_flucovid_gg1_log + ukhsa_flucovid_gg2_log +
  plot_annotation(title = ukhsa_graph_title,
                  subtitle = str_wrap(ukhsa_graph_subtitle_log, width = 130),
                  caption = ukhsa_graph_caption)

# Save
ggsave(file = "/cloud/project/code/00-UPD/fig_17_1_gg_upd_log.jpeg",
       plot = fig_17_1_gg_upd_log,
       device = "jpeg",
       height = 900/96, width = 1800/96, dpi = 96)
