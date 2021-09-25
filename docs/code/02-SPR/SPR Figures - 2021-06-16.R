## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)
library(patchwork)
library(scales)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Drawing the data
# I draw the data from a prepared file
# This was sent following correspondence with the authors at the University of Oxford
# https://science.sciencemag.org/content/371/6530/708/tab-figures-data
importation_events_df <- read_excel("R/COVID By Numbers/SPR Figures/SPR Figures - 2021-04-16.xlsx", 
                                    col_types = c("date", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric"))

importation_tidy_df <- importation_events_df %>%
  mutate(date = as_date(date),
         other_countries = Ireland + Switzerland + US + Other) %>%
  select(date, China, Italy, Spain, France, Netherlands, other_countries) %>%
  rename(Other = other_countries) %>%
  pivot_longer(cols = 2:7,
               names_to = "country",
               values_to = "assigned_events")

importation_tidy_df$country <- factor(importation_tidy_df$country,
                                      levels = c("China", "France", "Italy", "Spain", "Netherlands", "Other"))

importation_labels_df <- importation_tidy_df %>%
  group_by(country) %>%
  summarise(total_assigned_events = sum(assigned_events)) %>%
  mutate(label = paste0("Total estimate: ",
                        round(total_assigned_events, digits = 0)))

# The cases and deaths data is in a prepared file
# https://api.coronavirus.data.gov.uk/v2/data?areaType=overview&metric=cumCasesByPublishDate&metric=newCasesByPublishDate&metric=newDeathsByPublishDate&metric=cumDeathsByPublishDate&format=csv
phe_casesdeaths_df <- read_excel("R/COVID By Numbers/SPR Figures/SPR Figures - 2021-04-16.xlsx", 
                                 sheet = "DATA-2") %>%
  mutate(date = as_date(date)) %>%
  filter(date < "2020-04-01")

phe_casesdeaths_labels <- tribble(
  ~date, ~label,
  "2020-02-26", "1",
  "2020-03-09", "2",
  "2020-03-12", "3",
  "2020-03-16", "4",
  "2020-03-18", "5",
  "2020-03-23", "6") %>%
  mutate(date = as_date(date))

# The model estimates of overall infections are drawn from two data frames
# https://ourworldindata.org/covid-models#imperial-college-london-icl
infection_est_df_lshtm <- read_excel("R/COVID By Numbers/SPR Figures/SPR Figures - 2021-04-16.xlsx", 
                                     sheet = "DATA-3A") %>%
  mutate(date = as_date(date))

infection_est_df_icl <- read_excel("R/COVID By Numbers/SPR Figures/SPR Figures - 2021-04-16.xlsx", 
                                     sheet = "DATA-3B") %>%
  mutate(date = as_date(date))

infection_est_df <- bind_rows(infection_est_df_lshtm,
                              infection_est_df_icl) %>%
  filter(code == "GBR",
         date >= as_date("2020-03-01"),
         date <= as_date("2020-05-31"))

## Creating the graphs
# This is the graph of estimated importation events
importation_events_gg_base <- importation_tidy_df %>%
  ggplot(aes(x = date, y = assigned_events,
             group = fct_rev(country))) +
  geom_col(aes(fill = country),
           position = "identity",
           width = 1) +
  geom_text(data = importation_labels_df,
            aes(x = as_date("2020-01-05"),
                y = 28,
                label = label,
                colour = country,
                group = country),
            size = 7, fontface = "bold", hjust = 0) +
  facet_wrap(~country) +
  scale_x_date(expand = c(0,0),
               date_breaks = "4 weeks",
               date_labels = "%d-%b\n%Y") +
  scale_y_continuous(limits = c(0, 30),
                     expand = c(0,0)) +
  guides(fill = "none", colour = "none") +
  labs(title = "About 6 in 10 imported lineages came from France, Italy, and Spain.",
       subtitle = str_wrap("Estimated virus lineage importations with observed onward transmission per day (imputed) from the lag model. Statistical assignment to each country is a point estimate: there is uncertainty around each of these estimates. Total estimates are rounded.",
                           width = 130),
       x = "Imputed date of import event",
       y = "Importation events leading to observed onward transmission",
       caption = "Source: Establishment and lineage dynamics of the SARS-CoV-2 epidemic in the UK (Science, 2021).")

importation_events_gg <- importation_events_gg_base +
  scale_fill_grey(start = 0.1, end = 0.9) +
  scale_colour_grey(start = 0.1, end = 0.9)

importation_events_gg_col <- importation_events_gg_base +
  scale_fill_manual(values = c("#008080", "#800000", "#8FD694", "#FFC857", "#88498F", "grey50")) +
  scale_colour_manual(values = c("#008080", "#800000", "#8FD694", "#FFC857", "#88498F", "grey50"))

# The patchwork graph depicts the early part of the UK pandemic
phe_casesdeaths_gg1 <- phe_casesdeaths_df %>%
  ggplot(aes(x = date, y = newCasesByPublishDate)) +
  geom_line(size = 1.5) +
  scale_x_date(date_labels = "%d-%b",
               date_breaks = "2 weeks") +
  scale_y_continuous(labels = label_comma(),
                     expand = c(0,0)) +
  geom_vline(xintercept = phe_casesdeaths_labels$date,
             linetype = "dashed") +
  annotate("text", x = phe_casesdeaths_labels$date, y = 3000,
           label = phe_casesdeaths_labels$label, size = 7,
           hjust = -0.2) +
  geom_label(x = as_date("2020-02-03"), y = 2000, fill = "white",
  label =
  "    1. Contain, Delay, Research, and Mitigate plan published.
    2. PM holds first press conference on COVID-19.
    3. New self-isolation rules for those with symptoms.
    4. Daily press conferences and social distancing starts.
    5. Schools in England close for most children.
    6. PM issues stay-at-home order (lockdown begins).",
  size = 5, hjust = 0) +
  theme(plot.subtitle = element_text(size = 20, face = "plain")) +
  labs(title = "New reported lab-confirmed cases",
       x = "Reporting date",
       y = "New cases")

# The right-hand graph is for new reported deaths
phe_casesdeaths_gg2 <- phe_casesdeaths_df %>%
  ggplot(aes(x = date, y = newDeathsByPublishDate)) +
  geom_line(size = 1.5, na.rm = TRUE) +
  scale_x_date(date_labels = "%d-%b",
               date_breaks = "2 weeks") +
  scale_y_continuous(labels = label_comma(),
                     expand = c(0,0)) +
  geom_vline(xintercept = phe_casesdeaths_labels$date,
             linetype = "dashed") +
  annotate("text", x = phe_casesdeaths_labels$date, y = 350,
           label = phe_casesdeaths_labels$label, size = 7,
           hjust = -0.2) + theme(plot.subtitle = element_text(size = 20, face = "plain")) +
  labs(title = "New reported COVID-19 surveillance deaths",
       x = "Reporting date",
       y = "New COVID-19-related deaths")

# Patchwork then puts the two together
phe_casesdeaths_gg <- phe_casesdeaths_gg1 / phe_casesdeaths_gg2 +
  plot_annotation(title = "The first reported UK COVID-19 death was on 6th March 2020.",
                  subtitle = "New reported lab-confirmed SARS-CoV-2 cases and deaths (from any cause after a positive test), by reporting date in the UK.",
                  caption = "Source: Public Health England COVID-19 dashboard data download.")

## This is the model estimates against 7-day rolling average of confirmed cases
infection_est_gg <- infection_est_df %>%
  ggplot(aes(x = date, y = new_infections_est,
             ymin = new_infections_lower, ymax = new_infections_upper,
             group = model)) +
  geom_ribbon(alpha = 0.1,
              aes(fill = model)) +
  geom_line(aes(colour = model),
            size = 1.5) +
  geom_line(data = filter(infection_est_df, model == "ICL"),
            aes(y = new_confirmed_cases_7davg, linetype = code),
            size = 1.5) +
  scale_x_date(date_labels = "%d-%b",
               date_breaks = "1 month") +
  scale_y_continuous(expand = c(0,0),
                     labels = label_comma()) +
  scale_colour_manual(name = "",
                      aesthetics = c("colour", "fill"),
                      values = c("grey40", "grey70")) +
  scale_linetype_manual(name = "", values = "solid",
                        labels = "7-day rolling average of confirmed cases") +
  labs(title = "Model estimates suggest there were many more infections than captured by testing.",
       subtitle = str_wrap("Imperial College London and London School of Hygiene & Tropical Medicine estimates (with 95% intervals) of new infections by day, and 7-day rolling average of confirmed cases. This is for the United Kingdom between 1st March and 31st May 2020.",
                           width = 130),
       x = "Date",
       y = "Estimated new infections",
       caption = "Source: Our World in Data: How epidemiological models of COVID-19 help us estimate the true number of infections.")

## Saving the graphs
# Saving these graphs in the required dimensions
ggsave(file = "R/COVID By Numbers/SPR Figures/SPR_phe_casesdeaths_gg.jpeg",
       plot = phe_casesdeaths_gg,
       device = "jpeg",
       height = 15, width = 15)

ggsave(file = "R/COVID By Numbers/SPR Figures/SPR_infection_est_gg.jpeg",
       plot = infection_est_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/SPR Figures/SPR_importation_events_gg.jpeg",
       plot = importation_events_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/SPR Figures/SPR_importation_events_gg_col.jpeg",
       plot = importation_events_gg_col,
       device = "jpeg",
       height = 10, width = 15)