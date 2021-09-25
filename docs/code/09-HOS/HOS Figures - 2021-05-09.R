## Packages and themes
# First, I install the packages we need
library(tidyverse)
library(readxl)
library(lubridate)
library(ggrepel)
library(patchwork)
library(scales)
library(zoo)

# Next, we set the plotting theme from the helper file
source("R/COVID By Numbers/COVID By Numbers Theme.R")

## Draw the data
# First, I draw statistics on COVID-19 hospital admissions from the prepared file
# https://ourworldindata.org/grapher/weekly-hospital-admissions-covid-per-million?country=GBR~USA~ESP~ITA
owid_admissions_df <- read_excel("R/COVID By Numbers/HOS Figures/HOS Figures - 2021-05-09.xlsx", 
                                 sheet = "DATA-1") %>%
  mutate(day = as_date(day),
         weekly_new_admissions_per_100000 = weekly_new_hospital_admissions_per_million/10) %>%
  filter(code %in% c("GBR", "USA", "ESP", "ITA"))

owid_admissions_end <- owid_admissions_df %>%
  filter(day == as_date("2021-05-02"))

# Second, I draw statistics for hospital admissions by PHE region
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/983708/Weekly_Influenza_and_COVID19_report_data_w18.xlsx
phe_admissions_df <- read_excel("R/COVID By Numbers/HOS Figures/HOS Figures - 2021-05-09.xlsx", 
                                sheet = "DATA-2",
                                col_types = c("numeric", "numeric", "date", "date", "text",
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric", "numeric",
                                              "numeric", "numeric", "numeric"))

phe_admissions_tidy_df <- phe_admissions_df %>%
  mutate(week_start_date = as_date(week_start_date),
         week_end_date = as_date(week_end_date)) %>%
  pivot_longer(cols = 6:14,
               names_to = "phe_region",
               values_to = "weekly_admission_rate")

# Third. I draw statistics for NHS England admission statistics
# https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/Weekly-covid-admissions-and-beds-publication-210506.xlsx
nhs_admissions_df <- read_excel("R/COVID By Numbers/HOS Figures/HOS Figures - 2021-05-09.xlsx", 
                                sheet = "DATA-3",
                                col_types = c("date", "numeric", "numeric")) %>%
  mutate(date = as_date(date)) %>%
  arrange(date) %>%
  mutate(estimated_diagnoses_after_seven_days = estimated_new_hospital_cases - estimated_new_admissions_from_the_community,
         estimated_diagnoses_after_seven_days_share = estimated_diagnoses_after_seven_days / estimated_new_hospital_cases,
         estimated_new_hospital_cases_7d_sum = rollsum(estimated_new_hospital_cases,
                                                       k = 7, fill = NA, align = "right"),
         estimated_diagnoses_after_seven_days_7d_sum = rollsum(estimated_diagnoses_after_seven_days,
                                                               k = 7, fill = NA, align = "right"),
         estimated_diagnoses_after_seven_days_share_7da = estimated_diagnoses_after_seven_days_7d_sum / estimated_new_hospital_cases_7d_sum)

nhs_admissions_tidy_df <- nhs_admissions_df %>%
  select(date, 3, 4) %>%
  pivot_longer(cols = 2:3,
               names_to = "nhs_measure",
               values_to = "admissions_count")

nhs_admissions_tidy_df$nhs_measure <- factor(nhs_admissions_tidy_df$nhs_measure,
                                             levels = c("estimated_new_admissions_from_the_community",
                                                        "estimated_diagnoses_after_seven_days"))

nhs_admissions_share_df <- nhs_admissions_df %>%
  select(date, 5, 8) %>%
  pivot_longer(cols = 2:3,
               names_to = "measure",
               values_to = "share") %>%
  drop_na()

# The fourth graph is from an ONS-produced pre-print analysis
# https://www.bmj.com/content/372/bmj.n693/related
ons_adverse_events_df <- read_excel("R/COVID By Numbers/HOS Figures/HOS Figures - 2021-05-09.xlsx", 
                                    sheet = "DATA-4")

ons_adverse_events_df$group <- factor(ons_adverse_events_df$group,
                                      levels = c("Male", "Female",
                                                 "69 years and under", "70 years and over",
                                                 "White", "Non-White"))

## Create the graphs
owid_admissions_gg <- owid_admissions_df %>%
  ggplot(aes(x = day,
             y = weekly_new_admissions_per_100000,
             colour = entity)) +
  geom_line(size = 1.5) +
  geom_point(data = owid_admissions_end,
             aes(colour = entity),
             size = 3) +
  geom_text_repel(data = owid_admissions_end,
                  aes(label = code,
                      colour = entity),
                  size = 5, nudge_x = 15) +
  theme(legend.position = "none") +
  scale_colour_manual(values = c("grey30", "grey60", "grey90", "black")) +
  scale_x_date(date_labels = "%d-%b\n%Y") +
  labs(title = "In these four countries, weekly COVID-19 admissions rose in the winter of 2020.",
       subtitle = str_wrap("Weekly new COVID-19 admissions per estimated 100,000 people, by country. This is for Italy (ITA), Spain (ESP), the United Kingdom (GBR) and the United States (USA).", width = 100),
       x = "Week end date",
       y = "Weekly new COVID-19 admissions per 100,000 people",
       caption = "Source: Our World in Data, using ECDC for EU countries and government sources elsewhere.")

phe_admissions_gg <- phe_admissions_tidy_df %>%
  ggplot(aes(x = week_start_date,
             y = weekly_admission_rate,
             colour = disease)) +
  geom_line(size = 1.5) +
  facet_wrap(~phe_region) +
  scale_x_date(date_breaks = "2 months",
               date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_colour_manual(name = "Disease",
                      values = c("black", "grey50")) +
  labs(title = "COVID-19 admissions had two peaks in most regions, whilst influenza was suppressed.",
       subtitle = "Weekly hospital admissions per 100,000 people in PHE centres. 29th June 2020 to 2nd May 2021.",
       x = "Week start date",
       y = "Weekly hospital admissions per 100,000 people",
       caption = "Source: Public Health England: Weekly national Influenza and COVID-19 surveillance report, Week 18 report (SARI Watch).")

# This is a patchwork graph
nhs_date_breaks <- c("2020-09-01", "2020-11-01", "2021-01-01", "2021-03-01") %>%
  as_date()

nhs_admissions_gg1 <- nhs_admissions_tidy_df %>%
  ggplot(aes(x = date,
             y = admissions_count,
             group = nhs_measure)) +
  geom_col(aes(fill = nhs_measure),
           width = 1,
           position = position_stack(reverse = TRUE)) +
  scale_x_date(breaks = nhs_date_breaks,
               date_labels = "%d-%b\n%Y",
               expand = c(0,0)) +
  scale_y_continuous(labels = label_comma(),
                     expand = c(0,0)) +
  scale_fill_manual(name = "",
                    labels = c("Admissions or diagnoses within seven days", "Diagnoses after seven days"),
                    values = c("grey30", "grey60")) +
  theme(plot.subtitle = element_text(size = 20, face = "bold")) +
  labs(subtitle = "NHS England new hospital cases by diagnosis time",
       x = "Date", y = "New admissions")

nhs_admissions_gg2 <- nhs_admissions_share_df %>%
  ggplot(aes(x = date, y = 100*share, group = measure)) +
  geom_line(aes(colour = measure),
            size = 1.5) +
  scale_colour_manual(values = c("grey90", "grey60")) +
  scale_x_date(breaks = nhs_date_breaks,
               date_labels = "%d-%b\n%Y",
               expand =c(0,0)) +
  scale_y_continuous(limits = c(0,30),
                     expand = c(0,0)) +
  geom_text(x = as_date("2021-02-01"), y = 15,
            label = "Weighted 7-day\nrolling average",
            hjust = 0, vjust = -1, colour = "grey60",
            fontface = "bold", size = 7) +
  guides(colour = "none") +
  theme(plot.subtitle = element_text(size = 20, face = "bold", colour = "grey60")) +
  labs(subtitle = "Estimated diagnoses after seven days from admission [%]",
       x = "Date",
       y = "")

# Putting the two parts together
nhs_admissions_gg <- nhs_admissions_gg1 + nhs_admissions_gg2 +
  plot_annotation(title = "In England, the share of diagnoses seven days or more after admission rose to about 1 in 4 in December 2020.",
                  subtitle = "New hospital cases in England are patients admitted in previous 24 hours for the first time with COVID-19 plus patients diagnosed in hospital in previous 24 hours.",
                  caption = "Source: NHS England: Weekly admissions and beds up to 6th April 2021.")

ons_adverse_events_gg <- ons_adverse_events_df %>%
  filter(adverse_event %in% c("Death", "Readmission")) %>%
  ggplot(aes(x = rate_ratio_estimate,
             xmin = rate_ratio_lower,
             xmax = rate_ratio_upper,
             y = group)) +
  geom_pointrange(size  = 1) +
  scale_x_continuous(limits = c(0,22),
                     expand = c(0,0)) +
  facet_wrap(~adverse_event) +
  labs(title =  "People with Covid-19 discharged from hospitals are at higher risk of death.",
       subtitle = str_wrap("Rate ratios comparing individuals with Covid-19 in England discharged from hospital by 31st August 2020 with matched controls, stratified by personal factors.",
                           width = 100),
       x = "Rate ratio (compared to matched controls)",
       y = "Demographic groups",
       caption = "Source: Post-covid syndrome in individuals admitted to hospital with covid-19: retrospective cohort study (BMJ, 2021).")

## Savings the graphs
# Saving the figures in the required dimensions
ggsave(file = "R/COVID By Numbers/HOS Figures/HOS_owid_admissions_gg.jpeg",
       plot = owid_admissions_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/HOS Figures/HOS_phe_admissions_gg.jpeg",
       plot = phe_admissions_gg,
       device = "jpeg",
       height = 10, width = 15)

ggsave(file = "R/COVID By Numbers/HOS Figures/HOS_nhs_admissions_gg.jpeg",
       plot = nhs_admissions_gg,
       device = "jpeg",
       height = 13, width = 20)

ggsave(file = "R/COVID By Numbers/HOS Figures/HOS_ons_adverse_events_gg.jpeg",
       plot = ons_adverse_events_gg,
       device = "jpeg",
       height = 10, width = 15)