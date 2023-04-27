library(tidyverse)
library(data.table)
library(magrittr)
library(here)
library(lubridate)
library(arrow)
library(glue)

source(here::here("analysis/functions/redaction.R"))

shielding_cohort <- arrow::read_parquet(file = here::here("output/data_edited.gz.parquet"),
                                        compression = "gzip", compression_level = 5)
output_dir_plot <- here("output/figures")
fs::dir_create(output_dir_plot)

shielding_cohort %>% 
  group_by(shielding) %>% 
  summarise(hosps = sum(all_covid_hosp, na.rm = T)) %>% 
  print()

# Add week number and year for first hospitalisation
shielding_hosp <- shielding_cohort %>% 
  dplyr::select(patient_id,
                dplyr::starts_with("pt_"),
                shielding,
                dplyr::contains("hosp_admitted")) %>% 
  pivot_longer(cols = dplyr::contains("hosp_admitted"), 
               names_pattern = "covid_hosp_admitted_(.)",
               names_to = "covid_admission",
               values_to = "admission_date") %>% 
  drop_na()

# create a couple of numeric week and year variables to group on
shielding_hosp <- shielding_hosp %>% 
  mutate(hosp_week = lubridate::week(admission_date),
         hosp_year = lubridate::year(admission_date))

shielding_hosp_summ <- shielding_hosp %>% 
  group_by(hosp_year, hosp_week, shielding) %>% 
  summarise(weekly_admissions = n()) %>% 
  ungroup()

mindate <- min(shielding_hosp$admission_date)
print(mindate)
shielding_hosp_summ <- shielding_hosp_summ %>% 
  mutate(plot_date = mindate + weeks(hosp_week - week(mindate)) + years(hosp_year - year(mindate))) 

pdf(here::here("output/figures/covid_hosp_over_time.pdf"), width = 8, height = 6)
ggplot(shielding_hosp_summ, aes(x = plot_date, y = weekly_admissions, col = shielding)) +
  geom_line() + 
  geom_point(size = 1.2, pch = 1) +
  facet_wrap(~shielding, ncol = 1, scales = "free_y") +
  labs(x = "Date", y = "Weekly admissions") + 
  theme_bw()
dev.off()

hirisk_hosp_cumsum <- shielding_hosp_summ %>% 
  group_by(shielding) %>% 
  mutate(cumsum_admissions = cumsum(weekly_admissions))

pdf(here::here("output/figures/covid_hosp_over_time_cumsum.pdf"), width = 8, height = 6)
ggplot(hirisk_hosp_cumsum, aes(x = plot_date, y = cumsum_admissions, col = shielding, fill = shielding)) +
  geom_col() + 
  facet_wrap(~shielding, ncol = 1, scales = "free_y") +
  labs(x = "Date", y = "Total admissions") + 
  theme_bw()
dev.off()

