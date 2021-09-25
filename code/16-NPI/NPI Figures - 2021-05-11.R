## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(scales)
library(lubridate)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Drawing the data
# I draw data on the Oxford COVID-19 strigency index
# https://ourworldindata.org/covid-government-stringency-index
oxford_stringency_df <- read_excel("R/COVID By Numbers/NPI Figures/NPI Figures - 2021-05-11.xlsx", 
                                   sheet = "DATA-1") %>%
  filter(Code %in% c("SWE", "GBR", "USA", "NZL")) %>%
  mutate(Day = as_date(Day))

uk_visits_abroad_df <- read_excel("R/COVID By Numbers/NPI Figures/NPI Figures - 2021-05-11.xlsx", 
                                  sheet = "DATA-2") %>%
  mutate(month = as_date(month))

## Creating the graphs
oxford_stringency_gg <- oxford_stringency_df %>%
  ggplot(aes(x = Day,
             y = stringency_index,
             group = Entity)) +
  geom_line(size = 1.5,
            aes(colour = Entity)) +
  facet_wrap(~Entity) +
  scale_colour_manual(values = c("black", "grey25", "grey50", "grey75")) +
  guides(colour = "none") +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(limits = c(0,100),
                     expand = c(0,0)) +
  labs(title = "New Zealand initiated strict early action, whilst other nations maintained stringent government responses.",
       subtitle = str_wrap("This is a composite measure based on nine response indicators including school closures, workplace closures, and travel bans, rescaled to a value from 0 to 100 (100 = strictest). If policies vary at the subnational level, the index is shown as the response level of the strictest sub-region.",
                           width = 150),
       caption = "Source: Oxford Coronavirus Government Response Tracker project, via Our World in Data.",
       x = "Date",
       y = "Oxford Coronavirus government response stringency index")

uk_visits_abroad_gg <- uk_visits_abroad_df %>%
  ggplot(aes(x = month,
             y = uk_residents_visits_thousands/1000)) +
  geom_line(size = 1.5) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b\n%Y", expand = c(0.01, 0)) +
  scale_y_continuous(limits = c(0, 12),
                     expand = c(0,0),
                     labels = label_number_si(unit = "M")) +
  labs(title = "Estimated visits by UK residents abroad fell sharply in April 2020.",
       subtitle = "UK residents' visits abroad by month, non-seasonally adjusted, June 2017 to June 2020.",
       x = "Month",
       y = "Estimated visits abroad by month",
       caption = "Source: Office for National Statistics: Overseas travel and tourism, provisional: April to June 2020.")

## Saving the graphs
# Save the figures in the required dimensions
ggsave(file = "R/COVID By Numbers/NPI Figures/NPI_oxford_stringency_gg.jpeg",
       plot = oxford_stringency_gg,
       device = "jpeg",
       height = 10, width = 18)

ggsave(file = "R/COVID By Numbers/NPI Figures/NPI_uk_visits_abroad_gg.jpeg",
       plot = uk_visits_abroad_gg,
       device = "jpeg",
       height = 10, width = 15)