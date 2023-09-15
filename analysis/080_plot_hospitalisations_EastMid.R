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
dim_sc = dim(shielding_cohort)
if (dim_sc[1]>1000){ #only operates on real data, as dummy data has 500 or 1000 rows
  shielding_cohort <- shielding_cohort[which(shielding_cohort$practice_nuts=="East Midlands"),] #limit to East practices, but not in dummy data as too few data
}

# Add week number and year for first hospitalisation
shielding_hosp <- shielding_cohort %>% 
  dplyr::select(patient_id,		
                dplyr::starts_with("pt_"),
                shielding, age_cat,					
                dplyr::contains("hosp_admitted")) %>% 
  pivot_longer(cols = dplyr::contains("hosp_admitted"), 
               names_pattern = "covid_hosp_admitted_(.)",
               names_to = "covid_admission",
               values_to = "admission_date") %>% 
  drop_na()

# create week and year variables
mindate <- min(shielding_hosp$admission_date, na.rm = TRUE)
print(mindate)
  
shielding_hosp <- shielding_hosp %>%
  mutate(hosp_week = lubridate::week(admission_date),
         hosp_year = lubridate::year(admission_date),
         plot_date = mindate + weeks(hosp_week - week(mindate)) + years(hosp_year - year(mindate)))

#Week density and cumulative
By_week <- function(data, vars){
  data %>% 
     group_by(across({{vars}}))      %>%
     mutate(weekly_admissions = n()) %>%
     ungroup(hosp_year, hosp_week)   %>%
     arrange(plot_date)              %>%
     mutate(total_admissions = cumsum(weekly_admissions)) %>% #CUMULATIVE                                  
     ungroup()
}

#overall
shielding_hosp_summ   <- shielding_hosp %>% By_week(c(hosp_year, hosp_week))
# by shielding
shielding_hosp_summ_s <- shielding_hosp %>% By_week(c(hosp_year, hosp_week, shielding))
# by age
shielding_hosp_summ_a <- shielding_hosp %>% By_week(c(hosp_year, hosp_week, age_cat))                         


#Figures
fig <- function(data, x , y, col, facets, xname='Date', yname='Weekly admissions') {
  ggplot(data, aes(x = {{x}}, y = {{y}}, col = {{col}})) +
    geom_line() + 
    geom_point(size = 1.2, pch = 1) +
    facet_wrap(facets, ncol = 1, scales = 'free_y') +
    labs(x = xname, y = yname) +
	ylim(c(0, NA)) +
    theme_bw()
}

##PLOTS WEEKLY
pdf(here::here(paste0("output/figures/covid_hosp_over_time2","_EastMid",".pdf")), width = 8, height = 6)
##All
fig(shielding_hosp_summ,   plot_date, weekly_admissions,                      facets = 'NULL',          xname = 'Date',  yname = 'Weekly admissions')
#by shielding
fig(shielding_hosp_summ_s, plot_date, weekly_admissions, col = shielding,     facets = "shielding",     xname = 'Date',  yname = 'Weekly admissions')
#by age_cat
fig(shielding_hosp_summ_a, plot_date, weekly_admissions, col = age_cat,       facets = "age_cat",       xname = 'Date',  yname = 'Weekly admissions')

dev.off()


##PLOTS CUMULATIVE
pdf(here::here(paste0("output/figures/covid_hosp_over_time_cumsum2","_EastMid",".pdf")), width = 8, height = 6)
##All
fig(shielding_hosp_summ,   plot_date, total_admissions,                      facets = 'NULL',          xname = 'Date',  yname = 'Total admissions')
#by shielding
fig(shielding_hosp_summ_s, plot_date, total_admissions, col = shielding,     facets = "shielding",     xname = 'Date',  yname = 'Total admissions')
#by age_cat
fig(shielding_hosp_summ_a, plot_date, total_admissions, col = age_cat,       facets = "age_cat",       xname = 'Date',  yname = 'Total admissions')

dev.off()
