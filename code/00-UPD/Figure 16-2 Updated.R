## Packages and Themes
library(tidyverse)
library(readxl)
library(lubridate)

# Next, we load the plotting theme
source("/cloud/project/code/00-UPD/CBN Theme.R")

## Drawing the data
uk_visits_abroad_df <- read_excel("/cloud/project/code/16-NPI/NPI Figures - 2021-05-11.xlsx",
                                  sheet = "DATA-2") %>%
  mutate(month = as_date(month))

## Making the graph
fig_16_2_gg_upd <- uk_visits_abroad_df %>%
  ggplot(aes(x = month,
             y = uk_residents_visits_thousands/1000)) +
  geom_line(size = 1.5) +
  scale_x_date(date_breaks = "3 months",
               date_labels = "%b\n%Y") +
  scale_y_continuous(limits = c(0, 12),
                     expand = c(0,0),
                     labels = label_number_si(unit = "M")) +
  labs(title = "Estimated visits by UK residents abroad fell sharply in April 2020.",
       subtitle = "UK residents' visits abroad by month, non-seasonally adjusted, June 2017 to June 2020.",
       x = "Month",
       y = "Estimated visits abroad by month",
       caption = "Source: Office for National Statistics: Overseas travel and tourism, provisional: April to June 2020.")

## Saving the graph
ggsave(file = "/cloud/project/code/00-UPD/fig_16_2_gg_upd.jpeg",
       plot = fig_16_2_gg_upd,
       device = "jpeg",
       height = 750/96, width = 1500/96, dpi = 96)
