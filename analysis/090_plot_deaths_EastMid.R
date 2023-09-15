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
  shielding_cohort <- shielding_cohort[which(shielding_cohort$ons_death_date>="2020-01-01"),]                      #remove deaths prior to 2020 but registered from 2020
  shielding_cohort <- shielding_cohort[which(is.element(shielding_cohort$ons_underlying_cause,c("U071","U072"))),] #remove deaths not caused by covid_deaths_over_time2
shielding_cohort <- shielding_cohort[which(shielding_cohort$practice_nuts=="East Midlands"),]
}

#Add week number and year for deaths
shielding_death <- shielding_cohort %>% 
  dplyr::select(patient_id, death_date, ons_death_date, shielding, age_cat, practice_nuts) %>% #keep "death_date" for now
  drop_na()
  
#create week and year variables
mindate <- min(shielding_death$ons_death_date, na.rm = TRUE)
maxdate <- max(shielding_death$ons_death_date, na.rm = TRUE)
print(mindate)

shielding_death <- shielding_death %>%
  mutate(death_week = lubridate::week(ons_death_date),
         death_year = lubridate::year(ons_death_date),
         plot_date = mindate + weeks(death_week - week(mindate)) + years(death_year - year(mindate)))
  
#Week density and cumulative
By_week <- function(data, vars){
  data %>% 
     group_by(across({{vars}}))      %>%
     mutate(weekly_deaths = n())     %>%
     ungroup(death_year, death_week) %>%
     arrange(plot_date)              %>%
     mutate(total_deaths = cumsum(weekly_deaths)) %>% #CUMULATIVE                                  
     ungroup()
}
#overall
shielding_death_summ   <- shielding_death %>% By_week(c(death_year, death_week))
# by shielding
shielding_death_summ_s <- shielding_death %>% By_week(c(death_year, death_week, shielding))
# by age
shielding_death_summ_a <- shielding_death %>% By_week(c(death_year, death_week, age_cat))


#Figures
fig <- function(data, x , y, col, facets, xname='Date', yname='Weekly deaths') {
  ggplot(data, aes(x = {{x}}, y = {{y}}, col = {{col}})) +
    geom_line() + 
    geom_point(size = 1.2, pch = 1) +
    facet_wrap(facets, ncol = 1, scales = 'free_y') +
    labs(x = xname, y = yname) + 
    ylim(c(0, NA)) +
    theme_bw()
}
##PLOTS WEEKLY
pdf(here::here(paste0("output/figures/covid_deaths_over_time2","_EastMid",".pdf")), width = 8, height = 6)
##All
fig(shielding_death_summ,   plot_date, weekly_deaths,                      facets = 'NULL',          xname = 'Date',  yname = 'Weekly deaths')
#by shielding
fig(shielding_death_summ_s, plot_date, weekly_deaths, col = shielding,     facets = "shielding",     xname = 'Date',  yname = 'Weekly deaths')
#by age_cat
fig(shielding_death_summ_a, plot_date, weekly_deaths, col = age_cat,       facets = "age_cat",       xname = 'Date',  yname = 'Weekly deaths')

dev.off()

##PLOTS CUMULATIVE
pdf(here::here(paste0("output/figures/covid_deaths_over_time_cumsum2","_EastMid",".pdf")), width = 8, height = 6)
##All
fig(shielding_death_summ,   plot_date, total_deaths,                      facets = 'NULL',          xname = 'Date',  yname = 'Total deaths')
#by shielding
fig(shielding_death_summ_s, plot_date, total_deaths, col = shielding,     facets = "shielding",     xname = 'Date',  yname = 'Total deaths')
#by age_cat
fig(shielding_death_summ_a, plot_date, total_deaths, col = age_cat,       facets = "age_cat",       xname = 'Date',  yname = 'Total deaths')

dev.off()


