## Packages and Themes
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)
library(janitor)
library(curl)

# Next, we load the plotting theme
source("/cloud/project/code/00-UPD/CBN Theme.R")

## Drawing the data
# I draw these figures straight from the online file
ons_url <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/12735annualdeathsandmortalityrates1938to2020provisional/2020annualprovisionaladhoc2.xls"
ons_range <- "A15:E198"

temp <- tempfile()
temp <- curl_download(url = ons_url, destfile = temp,
                      quiet = TRUE, mode = "wb")

ons_deaths_df <- read_excel(temp, sheet = "Table",
                            range = ons_range) %>%
  janitor::clean_names() %>%
  filter(year >= 1900) %>%
  dplyr::rename(crude = 4, agest = 5) %>%
  dplyr::mutate(agest = as.numeric(agest))

ons_mortality_rates_df <- ons_deaths_df %>%
  select(year, crude, agest) %>%
  pivot_longer(cols = 2:3,
               names_to = "measures",
               values_to = "rate") %>%
  drop_na()

## Creating the graphs
ons_mortality_gg1 <- ons_deaths_df %>%
  ggplot(aes(x = year,
             y = number_of_deaths)) +
  geom_bar(stat = "identity",
           width = 1,
           aes(fill = number_of_deaths <= 600000)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,700000),
                     labels = label_comma(),
                     breaks = breaks_pretty(n = 7),
                     expand = c(0,0)) +
  scale_fill_manual(values = c("#222220", "#009fdb")) +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 20,
                                     face = "bold")) +
  labs(subtitle = "Number of death registrations in England and Wales",
       x = "Year",
       y = "") +
  annotate("text", x = 1925, y = 650000,
           size = 6, hjust = 0, family = "Freight Text Pro",
           label = "There are two years where death registrations exceeded 600,000:
  in 1918 (flu pandemic) and 2020 (COVID-19 pandemic).")

ons_mortality_gg2 <- ons_mortality_rates_df %>%
  ggplot(aes(x = year,
             y = rate,
             group = measures)) +
  geom_line(aes(color = measures),
            size = 1.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(limits = c(0,2500),
                     labels = label_comma(),
                     expand = c(0,0)) +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 20,
                                     face = "bold")) +
  labs(x = "Year",
       y = "",
       subtitle = "Mortality rates in England and Wales (per 100,000 people)") +
  scale_colour_manual(values = c("#15c6d4", "#ec6752")) +
  annotate("text", x = 1925, y = 1000,
           label = "Crude mortality rate",
           color = "#ec6752", fontface = "bold", size = 7,
           family = "Freight Text Pro") +
  annotate("text", x = 1985, y = 2200,
           label = "Age-standardised mortality rate",
           color = "#15c6d4", fontface = "bold", size = 7,
           family = "Freight Text Pro")

fig_15_2_gg_upd <- ons_mortality_gg1 + ons_mortality_gg2 +
  plot_annotation(title = "In 2020, there were over 608,000 death registrations in England and Wales.",
                  subtitle = "Deaths registrations of civilians from all causes in England and Wales, with age-standardised and crude mortality rates by year.",
                  caption = "Source: Office for National Statistics, 1900 to 2020 (provisional).")

## Saving the graph
ggsave(file = "/cloud/project/code/00-UPD/fig_15_2_gg_upd.jpeg",
       plot = fig_15_2_gg_upd,
       device = "tiff",
       height = 1000/96, width = 2000/96, dpi = 96)
