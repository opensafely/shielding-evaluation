library(tidyverse)
library(data.table)
library(magrittr)
library(here)
library(lubridate)
library(arrow)
library(glue)

source(here::here("analysis/functions/redaction.R"))

output_dir_plot <- here("output/figures")
fs::dir_create(output_dir_plot)

shielding_cohort <- arrow::read_parquet(file = here::here("output/data_edited.gz.parquet"),
                                        compression = "gzip", compression_level = 5)
										
shielding_cohort <- shielding_cohort[which(shielding_cohort$practice_nuts=="East Midlands"),]
										
shielding_cohort_s <- shielding_cohort
shielding_cohort_a <- shielding_cohort

shielding_cohort_s %>% group_by(shielding) %>% #by shielding
  summarise(hosps = sum(all_covid_hosp, na.rm = T)) %>% print()

shielding_cohort_a %>% group_by(age_cat) %>% #by age_cat
  summarise(hosps = sum(all_covid_hosp, na.rm = T)) %>% print()


# Add week number and year for first hospitalisation
shielding_hosp <- shielding_cohort %>% 
  dplyr::select(patient_id,
                dplyr::starts_with("pt_"),
                dplyr::contains("hosp_admitted")) %>% 
  pivot_longer(cols = dplyr::contains("hosp_admitted"), 
               names_pattern = "covid_hosp_admitted_(.)",
               names_to = "covid_admission",
               values_to = "admission_date") %>% 
  drop_na()
  
shielding_hosp_s <- shielding_cohort_s %>% 
  dplyr::select(patient_id,
                dplyr::starts_with("pt_"),
                shielding,
                dplyr::contains("hosp_admitted")) %>% 
  pivot_longer(cols = dplyr::contains("hosp_admitted"), 
               names_pattern = "covid_hosp_admitted_(.)",
               names_to = "covid_admission",
               values_to = "admission_date") %>% 
  drop_na()

shielding_hosp_a <- shielding_cohort_a %>% 
  dplyr::select(patient_id,
                dplyr::starts_with("pt_"),
                age_cat,
                dplyr::contains("hosp_admitted")) %>% 
  pivot_longer(cols = dplyr::contains("hosp_admitted"), 
               names_pattern = "covid_hosp_admitted_(.)",
               names_to = "covid_admission",
               values_to = "admission_date") %>% 
  drop_na()

# create numeric week and year variables to group on
shielding_hosp <- shielding_hosp %>%
  mutate(hosp_week = lubridate::week(admission_date),
         hosp_year = lubridate::year(admission_date))
		 
shielding_hosp_s <- shielding_hosp_s %>%
  mutate(hosp_week = lubridate::week(admission_date),
         hosp_year = lubridate::year(admission_date))

shielding_hosp_a <- shielding_hosp_a %>%
  mutate(hosp_week = lubridate::week(admission_date),
         hosp_year = lubridate::year(admission_date))

#WEEKLY
shielding_hosp_summ <- shielding_hosp %>% 
  group_by(hosp_year, hosp_week) %>%
  summarise(weekly_admissions = n()) %>% 
  ungroup()
  
  shielding_hosp_summ_s <- shielding_hosp_s %>% 
  group_by(hosp_year, hosp_week, shielding) %>%
  summarise(weekly_admissions = n()) %>% 
  ungroup()

shielding_hosp_summ_a <- shielding_hosp_a %>% 
  group_by(hosp_year, hosp_week, age_cat) %>%
  summarise(weekly_admissions = n()) %>% 
  ungroup()

mindate <- min(shielding_hosp$admission_date)
print(mindate)
shielding_hosp_summ <- shielding_hosp_summ %>% 
  mutate(plot_date = mindate + weeks(hosp_week - week(mindate)) + years(hosp_year - year(mindate))) %>% 
  mutate(total_admissions = cumsum(weekly_admissions)) #CUMULATIVE

mindate <- min(shielding_hosp_s$admission_date)
shielding_hosp_summ_s <- shielding_hosp_summ_s %>% 
  mutate(plot_date = mindate + weeks(hosp_week - week(mindate)) + years(hosp_year - year(mindate))) %>% 
  mutate(total_admissions = cumsum(weekly_admissions)) #CUMULATIVE

shielding_hosp_summ_a <- shielding_hosp_summ_a %>% 
  mutate(plot_date = mindate + weeks(hosp_week - week(mindate)) + years(hosp_year - year(mindate))) %>% 
  mutate(total_admissions = cumsum(weekly_admissions)) #CUMULATIVE


##WEEKLY
pdf(here::here(paste0("output/figures/covid_hosp_over_time2","_EastMid",".pdf")), width = 8, height = 6)
##All
ggplot(shielding_hosp_summ, aes(x = plot_date, y = weekly_admissions)) +
  geom_line() + 
  geom_point(size = 1.2, pch = 1) +
  labs(x = "Date", y = "Weekly admissions") + 
  theme_bw()
#by shielding
ggplot(shielding_hosp_summ_s, aes(x = plot_date, y = weekly_admissions, col = shielding)) +
  geom_line() + 
  geom_point(size = 1.2, pch = 1) +
  facet_wrap(~shielding, ncol = 1, scales = "free_y") +
  labs(x = "Date", y = "Weekly admissions") + 
  theme_bw()
#by age_cat
ggplot(shielding_hosp_summ_a, aes(x = plot_date, y = weekly_admissions, col = age_cat)) +
  geom_line() + 
  geom_point(size = 1.2, pch = 1) +
  facet_wrap(~age_cat, ncol = 1, scales = "free_y") +
  labs(x = "Date", y = "Weekly admissions") + 
  theme_bw()
dev.off()


#CUMULATIVE
pdf(here::here(paste0("output/figures/covid_hosp_over_time_cumsum2","_EastMid",".pdf")), width = 8, height = 6)
##All
ggplot(shielding_hosp_summ, aes(x = plot_date, y = total_admissions)) +
  geom_line() + 
  geom_point(size = 1.2, pch = 1) +
  labs(x = "Date", y = "Weekly admissions") + 
  theme_bw()
#by shielding
ggplot(shielding_hosp_summ_s, aes(x = plot_date, y = total_admissions, col = shielding)) +
  geom_line() + 
  geom_point(size = 1.2, pch = 1) +
  facet_wrap(~shielding, ncol = 1, scales = "free_y") +
  labs(x = "Date", y = "Weekly admissions") + 
  theme_bw() 
#by age_cat
ggplot(shielding_hosp_summ_a, aes(x = plot_date, y = total_admissions, col = age_cat)) +
  geom_line() + 
  geom_point(size = 1.2, pch = 1) +
  facet_wrap(~age_cat, ncol = 1, scales = "free_y") +
  labs(x = "Date", y = "Weekly admissions") + 
  theme_bw()
dev.off()



