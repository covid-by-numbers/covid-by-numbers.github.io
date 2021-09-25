## Packages and themes
# Install the packages we need
library(tidyverse)
library(rgdal)
library(broom)
library(readxl)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")
theme_cbn_clean <- theme_cbn +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
theme_set(theme_cbn_clean)

## Drawing data and the shape file
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/articles/comparisonsofallcausemortalitybetweeneuropeancountriesandregions/2020
rASMR_map_UK_df <- read_excel("R/COVID By Numbers/SPR Figures/rASMR map UK  - 2021-04-17.xlsx", 
                                      sheet = "DATA-1")

rASMR_map_tidy_df <- rASMR_map_UK_df %>%
  filter(str_detect(nuts_code, "UK")) %>%
  select(nuts_code, w16, w17)

# You need to replace this line with the relevant file path for your session
# https://geoportal.statistics.gov.uk/datasets/473aefdcee19418da7e5dbfdeacf7b90_1?geometry=-44.245%2C51.101%2C39.383%2C59.782
NUTS_Level_3_2018_01 <- readOGR(dsn = "R/COVID By Numbers/NUTS_Level3_2018_01",
                                layer = "NUTS_Level_3_(January_2018)_Boundaries")


# https://www.marmoe.xyz/2018/09/04/shapefiles-in-r-with-ggplot2-rgdal/
NUTS_Level_3_tidy <- broom::tidy(NUTS_Level_3_2018_01)

NUTS_Level_3_2018_01$id <- row.names(NUTS_Level_3_2018_01)
NUTS_Level_3_tidy <- left_join(NUTS_Level_3_tidy,
                               NUTS_Level_3_2018_01@data,
                               by = "id")
NUTS_L3_tidy <- left_join(NUTS_Level_3_tidy,
                          rASMR_map_tidy_df,
                          by = c("nuts318cd" = "nuts_code")) %>%
  rename(long = long.x, lat = lat.x)

## Making the map
rASMR_map_gg <- NUTS_L3_tidy %>% ggplot(aes(x = long, y = lat, group = group, fill = w16)) +
  geom_polygon(colour = "black", size = 0.1) +
  coord_equal() + 
  guides(x = "none", y = "none") +
  scale_fill_steps(name = "rASMR [%]",
                   low = "white", high = "black",
                   limits = c(-100, 400),
                   show.limits = TRUE,
                   breaks = c(0, 30, 60, 90, 150),
                   guide = guide_coloursteps(even.steps = FALSE,
                                             barheight = 20,
                                             direction = "vertical",
                                             label.position = "right")) +
  theme(legend.position = "right") +
  labs(title = "Increased deaths were across the UK.",
       subtitle = str_wrap("Relative age-standardised mortality rate [%] by NUTS3 level region, for the United Kingdom. This is the week ending 17th April 2020 (week 16).",
                           width = 50),
       x = "", y = "",
       caption = str_wrap("Source: Office for National Statistics: Comparisons of all-cause mortality between European countries and regions: 2020.",
                          width = 50))