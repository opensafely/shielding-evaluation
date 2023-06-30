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
	
shielding_cohort <- shielding_cohort[which(shielding_cohort$ons_death_date>="2020-01-01"),]
shielding_cohort <- shielding_cohort[which(is.element(shielding_cohort$ons_underlying_cause,c("U071","U072"))),]	
shielding_cohort <- shielding_cohort[which(shielding_cohort$practice_nuts=="East"),]

shielding_cohort_s <- shielding_cohort
shielding_cohort_a <- shielding_cohort

shielding_cohort_s %>% group_by(shielding) %>% #by shielding
  summarise(deaths = sum(has_died, na.rm = T)) %>% print()

shielding_cohort_a %>% group_by(age_cat) %>% #by age_cat
  summarise(deaths = sum(has_died, na.rm = T)) %>% print()

#Add week number and year for deaths
shielding_death   <- shielding_cohort %>% 
  dplyr::select(patient_id, death_date) %>% 
  drop_na()

shielding_death_s <- shielding_cohort_s %>% 
  dplyr::select(patient_id, death_date, shielding) %>% 
  drop_na()  

shielding_death_a <- shielding_cohort_a %>% 
  dplyr::select(patient_id, death_date, age_cat) %>% 
  drop_na()
  
#create numeric week and year variables to group on
shielding_death <- shielding_death %>%
  mutate(death_week = lubridate::week(death_date),
         death_year = lubridate::year(death_date))

shielding_death_s <- shielding_death_s %>%
  mutate(death_week = lubridate::week(death_date),
         death_year = lubridate::year(death_date))

shielding_death_a <- shielding_death_a %>%
  mutate(death_week = lubridate::week(death_date),
         death_year = lubridate::year(death_date))

#WEEKLY
shielding_death_summ <- shielding_death %>% 
  group_by(death_year, death_week) %>%
  summarise(weekly_deaths = n()) %>% 
  ungroup()		
  
shielding_death_summ_s <- shielding_death_s %>% 
  group_by(death_year, death_week, shielding) %>%
  summarise(weekly_deaths = n()) %>% 
  ungroup()

shielding_death_summ_a <- shielding_death_a %>% 
  group_by(death_year, death_week, age_cat) %>%
  summarise(weekly_deaths = n()) %>% 
  ungroup()

mindate <- min(shielding_death$death_date)
maxdate <- max(shielding_death$death_date)
print(mindate)
shielding_death_summ <- shielding_death_summ %>% 
  mutate(plot_date = mindate + weeks(death_week - week(mindate)) + years(death_year - year(mindate))) %>% 
  mutate(total_deaths = cumsum(weekly_deaths)) #CUMULATIVE

mindate <- min(shielding_death_s$death_date)
maxdate <- max(shielding_death_s$death_date)
shielding_death_summ_s <- shielding_death_summ_s %>% 
  mutate(plot_date = mindate + weeks(death_week - week(mindate)) + years(death_year - year(mindate))) %>% 
  mutate(total_deaths = cumsum(weekly_deaths)) #CUMULATIVE

shielding_death_summ_a <- shielding_death_summ_a %>% 
  mutate(plot_date = mindate + weeks(death_week - week(mindate)) + years(death_year - year(mindate))) %>% 
  mutate(total_deaths = cumsum(weekly_deaths)) #CUMULATIVE


#WEEKLY
pdf(here::here(paste0("output/figures/covid_deaths_over_time2","_East",".pdf")), width = 8, height = 6)

##All
ggplot(shielding_death_summ, aes(x = plot_date, y = weekly_deaths)) +
  geom_line() + 
  geom_point(size = 1.2, pch = 1) +
  labs(x = "Date", y = "Weekly deaths") + 
  #xlim(2020, 2023) +
  theme_bw()
#by shielding
ggplot(shielding_death_summ_s, aes(x = plot_date, y = weekly_deaths, col = shielding)) +
  geom_line() + 
  geom_point(size = 1.2, pch = 1) +
  facet_wrap(~shielding, ncol = 1, scales = "free_y") +
  labs(x = "Date", y = "Weekly deaths") + 
  theme_bw()
#by age_cat
ggplot(shielding_death_summ_a, aes(x = plot_date, y = weekly_deaths, col = age_cat)) +
  geom_line() + 
  geom_point(size = 1.2, pch = 1) +
  facet_wrap(~age_cat, ncol = 1, scales = "free_y") +
  labs(x = "Date", y = "Weekly deaths") + 
  theme_bw()
dev.off()


#CUMULATIVE
pdf(here::here(paste0("output/figures/covid_deaths_over_time_cumsum2","_East",".pdf")), width = 8, height = 6)
##All
ggplot(shielding_death_summ, aes(x = plot_date, y = total_deaths)) +
  geom_line() + 
  geom_point(size = 1.2, pch = 1) +
  labs(x = "Date", y = "Total deaths") + 
  theme_bw()
#by shielding
ggplot(shielding_death_summ_s, aes(x = plot_date, y = total_deaths, col = shielding)) +
  geom_line() + 
  geom_point(size = 1.2, pch = 1) +
  facet_wrap(~shielding, ncol = 1, scales = "free_y") +
  labs(x = "Date", y = "Total deaths") + 
  theme_bw() 
#by age_cat
ggplot(shielding_death_summ_a, aes(x = plot_date, y = total_deaths, col = age_cat)) +
  geom_line() + 
  geom_point(size = 1.2, pch = 1) +
  facet_wrap(~age_cat, ncol = 1, scales = "free_y") +
  labs(x = "Date", y = "Total deaths") + 
  theme_bw()
dev.off()



