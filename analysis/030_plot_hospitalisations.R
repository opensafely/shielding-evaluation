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

output_dir <- here("output/data_properties")
fs::dir_create(output_dir)
output_dir_plot <- here("output/figures")
fs::dir_create(output_dir_plot)

capture.output(
  file = paste0(output_dir, "/different_shielding.txt"),
  shielding_cohort %>% 
    group_by(shielding) %>% 
    summarise(denom = n(), hosps = sum(all_covid_hosp, na.rm = T)) %>% 
    mutate(proportion = round(hosps/denom, 3)) %>% 
    print()
  ,
  shielding_cohort %>% 
    group_by(shielding_v1_binary) %>% 
    summarise(denom = n(), hosps = sum(all_covid_hosp, na.rm = T)) %>% 
    mutate(proportion = round(hosps/denom, 3)) %>% 
    print()
  ,
  shielding_cohort %>% 
    group_by(shielding_v2_binary) %>% 
    summarise(denom = n(), hosps = sum(all_covid_hosp, na.rm = T)) %>% 
    mutate(proportion = round(hosps/denom, 3)) %>% 
    print()
  ,
  shielding_cohort %>% 
    group_by(hirisk_shield_count) %>% 
    summarise(denom = n(), hosps = sum(all_covid_hosp, na.rm = T)) %>% 
    mutate(proportion = round(hosps/denom, 3)) %>% 
    print()
)

# version 1: hi-risk code only  -------------------------------------------
# Add week number and year for first hospitalisation
shielding_hosp_v1 <- shielding_cohort %>% 
  dplyr::select(patient_id,
                dplyr::starts_with("pt_"),
                shielding_v1_startdate,
                dplyr::contains("hosp_admitted")) %>% 
  pivot_longer(cols = dplyr::contains("hosp_admitted"), 
               names_pattern = "covid_hosp_admitted_(.)",
               names_to = "covid_admission",
               values_to = "admission_date") %>% 
  filter(!is.na(admission_date)) %>% 
  # create binary variable for whether the patient was shielding 
  # at the date of hospital admission:
  mutate(shielding = as.numeric(admission_date > shielding_v1_startdate)) %>% 
  # there will be a lot of NAs (people who never shielded have NA `hirisk_only`)
  # so replace these with 0
  mutate(shielding = replace_na(0)) %>% 
  ungroup()

# version 2: hi-risk until lo-risk code  ----------------------------------
shielding_hosp_v2 <- shielding_cohort %>% 
  dplyr::select(patient_id,
                dplyr::starts_with("pt_"),
                shielding_v2_startdate, shielding_v2_enddate,
                dplyr::contains("hosp_admitted")) %>% 
  pivot_longer(cols = dplyr::contains("hosp_admitted"), 
               names_pattern = "covid_hosp_admitted_(.)",
               names_to = "covid_admission",
               values_to = "admission_date") %>% 
  filter(!is.na(admission_date)) %>% 
  # create binary variable for whether the patient was shielding 
  # at the date of hospital admission:
  # To do so, because of a lot of NA values, replace these with an 
  # artificially futuristic date value 
  mutate_at(vars(shielding_v2_startdate, shielding_v2_enddate), 
            ~replace_na(., as.Date("3023-01-01"))) %>% 
  mutate(shielding = as.numeric(admission_date >= shielding_v2_startdate & 
                                  admission_date <= shielding_v2_enddate)) %>% 
  ungroup()

# plot the results --------------------------------------------------------
plot_shielding_hosp <- function(input){
  # create a couple of numeric week and year variables to group on
  shielding_hosp <- input %>% 
    mutate(shielding = factor(shielding), 
           hosp_week = lubridate::week(admission_date),
           hosp_year = lubridate::year(admission_date)) %>% 
    group_by(hosp_year, hosp_week, shielding) %>% 
    summarise(weekly_admissions = n(), .groups = "keep") 
  
  mindate <- min(input$admission_date)
  print(mindate)
  shielding_hosp <- shielding_hosp %>% 
    mutate(plot_date = mindate + weeks(hosp_week - week(mindate)) + years(hosp_year - year(mindate))) 
  
  p1 <- ggplot(shielding_hosp, aes(x = plot_date, y = weekly_admissions, col = shielding)) +
    geom_line() + 
    geom_point(size = 1.2, pch = 1) +
    facet_wrap(~shielding, ncol = 1, scales = "free_y") +
    ylim(c(0, NA)) +
    labs(x = "Date", y = "Weekly admissions") + 
    theme_bw()
  
  hirisk_hosp_cumsum <- shielding_hosp %>% 
    group_by(shielding) %>% 
    mutate(cumsum_admissions = cumsum(weekly_admissions))
  
  p2 <- ggplot(hirisk_hosp_cumsum, aes(x = plot_date, y = cumsum_admissions, col = shielding, fill = shielding)) +
    geom_col() + 
    facet_wrap(~shielding, ncol = 1, scales = "free_y") +
    labs(x = "Date", y = "Total admissions") + 
    ylim(c(0, NA)) +
    theme_bw()
  
  return(list(p1=p1, p2=p2))
}

shieldplots_v1 <- plot_shielding_hosp(shielding_hosp_v1)
shieldplots_v2 <- plot_shielding_hosp(shielding_hosp_v2)

# Version 1 of shielding definition
pdf(here::here("output/figures/covid_hosp_over_time_v1.pdf"), width = 8, height = 6); shieldplots_v1$p1; dev.off()
pdf(here::here("output/figures/covid_hosp_over_time_cumsum_v1.pdf"), width = 8, height = 6); shieldplots_v1$p2; dev.off()
# Version 2 of shielding definition
pdf(here::here("output/figures/covid_hosp_over_time_v2.pdf"), width = 8, height = 6); shieldplots_v2$p1; dev.off()
pdf(here::here("output/figures/covid_hosp_over_time_cumsum_v2.pdf"), width = 8, height = 6); shieldplots_v2$p2; dev.off()

