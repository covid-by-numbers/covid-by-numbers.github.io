## Packages and Theme
library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(patchwork)

# Next, we load the plotting theme
source("/cloud/project/code/00-UPD/CBN Theme.R")

## Drawing the data
# I draw the data from a prepared file
dot_incidents_df <- read_excel("/cloud/project/code/17-COL/COL Figures - 2021-04-05.xlsx",
                                sheet = "DATA-1") %>%
  mutate(month = as_date(month))

dot_incidents_df_1 <- dot_incidents_df %>%
  select(1:6) %>%
  pivot_longer(cols = 2:6,
               names_to = "road_user_type",
               values_to = "road_casualties")

dot_incidents_df_2 <- dot_incidents_df %>%
  select(1, 7:11) %>%
  pivot_longer(cols = 2:6,
               names_to = "road_user_type",
               values_to = "change") %>%
  mutate(road_user_type = str_replace(road_user_type, "_change", ""))

dot_incidents_df <- full_join(dot_incidents_df_1,
                              dot_incidents_df_2)

## Creating the graph
cbn_colours <- c("#1c1d1a", "#15c6d4", "#ec6752")
dot_incidents_lbl <- tribble(~road_user_type, ~road_user_label, ~month, ~casualties,
                             "all_road_users", "All road users", "2020-01-10", 13000,
                             "car_users", "Car users", "2020-01-10", 8000,
                             "pedal_cyclists", "Pedal cyclists", "2020-01-10", 2000) %>%
  mutate(month = as_date(month))

# The first graph is of road incident levels
dot_incidents_gg1 <- dot_incidents_df %>%
  filter(road_user_type %in% c("all_road_users", "car_users", "pedal_cyclists")) %>%
  ggplot(aes(x = month, y = road_casualties, group = road_user_type)) +
  geom_line(aes(colour = road_user_type), size = 1.5) +
  scale_colour_manual(values = cbn_colours, guide = "none") +
  scale_x_date(date_labels = "%b\n%Y",
               expand = c(0.02,0)) +
  scale_y_continuous(breaks = pretty_breaks(),
                     labels = label_comma(),
                     expand = c(0,0),
                     limits = c(0, 14000)) +
  geom_text(data = dot_incidents_lbl,
             aes(x = month, y = casualties, label = road_user_label,
                 colour = road_user_type), size = 7, fontface = "bold",
             hjust = 0, family = "Freight Text Pro") +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "Provisional road casualty estimates",
       x = "Month", y = "Number of road casualties")

# The second graph is of the change relative to the previous year
dot_incidents_gg2 <- dot_incidents_df %>%
  filter(road_user_type %in% c("all_road_users", "car_users", "pedal_cyclists")) %>%
  ggplot(aes(x = month, y = change, group = road_user_type)) +
  geom_line(aes(colour = road_user_type), size = 1.5) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  scale_colour_manual(values = cbn_colours, guide = "none") +
  scale_x_date(date_labels = "%b\n%Y",
               expand = c(0.02,0)) +
  scale_y_continuous(breaks = pretty_breaks(),
                     limits = c(-80, 20)) +
  theme(plot.subtitle = element_text(size = 20, face = "bold"),
        legend.position = "none") +
  labs(subtitle = "Change [%] compared to 2019",
       x = "Month", y = "Change [in %]")

# Those two smaller graphs are put in a patchwork layout
fig_17_2_gg_upd <- dot_incidents_gg1 + dot_incidents_gg2 +
  plot_annotation(title = "Provisional estimates in Great Britain suggest a large fall in road casualties in April to June 2020.",
                  subtitle = "Numbers and annual change [%] of reported road casualties by different types of road user, in January to June 2020.",
                  caption = "Source: Department for Transport: Reported road casualties by road user type, first half 2020.")

## Saving the graph
ggsave(file = "/cloud/project/code/00-UPD/fig_17_2_gg_upd.jpeg",
       plot = fig_17_2_gg_upd,
       device = "jpeg",
       height = 800/96, width = 1800/96, dpi = 96)
