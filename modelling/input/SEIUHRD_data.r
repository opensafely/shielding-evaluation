## 040 hospital admissions
## 050 deaths
library(data.table)
library(magrittr)
library(arrow)
library(glue)


source(here::here("analysis/functions/redaction.R"))

output_plot <- here("output/figures")
fs::dir_create(output_plot)

#H
shielding_cohort  <- arrow::read_parquet(file = here::here("output/data_edited.gz.parquet"),
                                          compression = "gzip", compression_level = 5)
#D
shielding_cohortD <- shielding_cohort
dim_sc = dim(shielding_cohort)
if (dim_sc[1]>1000){ #only operates on real data, as dummy data has 500 or 1000 rows
  shielding_cohortD <- shielding_cohortD[which(shielding_cohortD$ons_death_date>="2020-01-01"),]                      #remove deaths prior to 2020 but registered from 2020
  shielding_cohortD <- shielding_cohortD[which(is.element(shielding_cohortD$ons_underlying_cause,c("U071","U072"))),] #remove deaths not caused by covid_deaths_over_time2
}

#H week and year for hospitalisations
shielding_hosp <- shielding_cohort                         %>% 
  dplyr::select(patient_id,
                dplyr::starts_with("pt_"),
                shielding, 
                age_cat,
                practice_nuts,
                dplyr::contains("hosp_admitted"))          %>% 
  pivot_longer(cols = dplyr::contains("hosp_admitted"), 
                names_pattern = "covid_hosp_admitted_(.)",
                names_to = "covid_admission",
                values_to = "admission_date")              %>% 
  drop_na()

#D week and year for deaths
shielding_death <- shielding_cohortD                       %>% 
  dplyr::select(patient_id, 
                death_date, ons_death_date,                #keep "death_date" for now
                shielding, 
                age_cat, 
                practice_nuts)                             %>% 
  drop_na()

#H week, year, date-by-week variables
StartDateH <- min(shielding_hosp$admission_date, na.rm = TRUE)

shielding_hosp <- shielding_hosp %>%
  mutate(hosp_week = lubridate::week(admission_date), #non-unique id across years
         hosp_year = lubridate::year(admission_date),
         plot_date = StartDateH + weeks(hosp_week - week(StartDateH)) + years(hosp_year - year(StartDateH))) %>%
  arrange(hosp_week)              

#D week, year, date-by-week variables
StartDateD <- min(shielding_death$ons_death_date, na.rm = TRUE)

shielding_death <- shielding_death  %>%
  mutate(death_week = lubridate::week(ons_death_date),
         death_year = lubridate::year(ons_death_date),
         plot_date  = StartDateD + weeks(death_week - week(StartDateD)) + years(death_year - year(StartDateD))) %>%
  arrange(death_week)

#H Week density and cumulative
By_weekH <- function(data, vars){
  data %>% 
    group_by(across({{vars}}))      %>%
    mutate(weekly_admissions = n()) %>%
    #ungroup(hosp_year, hosp_week)   %>%
    #arrange(plot_date)              %>%
    #mutate(total_admissions = cumsum(weekly_admissions)) #%>% #CUMULATIVE                                  
  ungroup()
}
#D Week density and cumulative
By_weekD <- function(data, vars){
  data %>% 
    group_by(across({{vars}}))      %>%
    mutate(weekly_deaths = n())     %>%
    #ungroup(death_year, death_week) %>%
    #arrange(plot_date)              %>%
    #mutate(total_deaths = cumsum(weekly_deaths)) #%>% #CUMULATIVE                                  
  ungroup()
}

#H overall
shielding_hosp_summ   <- shielding_hosp %>% By_weekH(c(hosp_year, hosp_week))
# by age
shielding_hosp_summ_a <- shielding_hosp %>% By_weekH(c(hosp_year, hosp_week, age_cat))                         

#D overall
shielding_death_summ   <- shielding_death %>% By_weekD(c(death_year, death_week))
# by age
shielding_death_summ_a <- shielding_death %>% By_weekD(c(death_year, death_week, age_cat))

#Date range of Data
#plot_date (weeks) is the date identifier because counts are aggregated in weeks
#H
Week1OfDataH = min(shielding_hosp_summ$plot_date)
Week2OfDataH = max(shielding_hosp_summ$plot_date)
#D
Week1OfDataD = min(shielding_death_summ$plot_date)
Week2OfDataD = max(shielding_death_summ$plot_date)
#All
Week1OfData  = max(Week1OfDataH, Week1OfDataD)
Week2OfData  = min(Week2OfDataH, Week2OfDataD)
print(paste0("Hospit data - date by week, range  ", Week1OfDataH, ", ", Week2OfDataH))
print(paste0("Deaths data - date by week, range  ", Week1OfDataD, ", ", Week2OfDataD))
print(paste0("All data    - date by week, range  ", Week1OfData,  ", ", Week2OfData))
#H data used - H happens before D
ih0 = which( shielding_hosp_summ$plot_date <=Week2OfData & shielding_hosp_summ$plot_date >=Week1OfData)
#D data used - discard earlier D - Dummy: disregard being same date in H and D, provided range overlaps
id  = which( shielding_death_summ$plot_date<=Week2OfData & shielding_death_summ$plot_date>=Week1OfData)
ih  = ih0[1:length(id)]
nD  = length(ih)
print(paste0("length of datasets = ",nD," weeks"))

#Figures H, D - overlapping groups
fig0 <- function(data, x , y, col, xname='Date', yname='Weekly admissions') { #yname='Weekly deaths'
  ggplot(data, aes(x = {{x}}, y = {{y}}, col = {{col}})) +
    geom_line() + 
    geom_point(size = 1.2, pch = 1) +
    labs(x = xname, y = yname) +
    ylim(c(0, NA)) +
    theme_bw() }

#Figures H, D - panels for groups
fig <- function(data, x , y, col, facets, xname='Date', yname='Weekly admissions') { #yname='Weekly deaths'
  p <- ggplot(data, aes(x = {{x}}, y = {{y}}, col = {{col}})) +
    geom_line() + 
    geom_point(size = 1.2, pch = 1) +
    facet_wrap(facets, ncol = 1, scales = 'free_y') +
    labs(x = xname, y = yname) +
    ylim(c(0, NA)) +
    theme_bw() 
  print(p)} #for when sourcing


##PLOTS WEEKLY
#H
pdf(file = paste0(output_plot,"/",pset$File_data_hosp), width = 8, height = 6)
  ##All
  fig(shielding_hosp_summ, plot_date, weekly_admissions, facets ='NULL', xname ='Date', yname ='Weekly admissions')
dev.off()
#D
pdf(file = paste0(output_plot,"/",pset$File_data_deaths), width = 8, height = 6)
  ##All
  fig(shielding_death_summ, plot_date, weekly_deaths, facets = 'NULL', xname = 'Date',  yname = 'Weekly deaths')
dev.off()

#H
#by age_cat
  #fig0(shielding_hosp_summ_a, plot_date, weekly_admissions, col = age_cat,                    xname = 'Date',  yname = 'Weekly admissions')
  #by age_cat - panels
  #fig(shielding_hosp_summ_a, plot_date, weekly_admissions, col = age_cat, facets = "age_cat", xname = 'Date',  yname = 'Weekly admissions')
#D
#by age_cat
  #fig0(shielding_death_summ_a, plot_date, weekly_deaths, col = age_cat,                          xname = 'Date',  yname = 'Weekly deaths')
  #by age_cat - panels
  #fig(shielding_death_summ_a, plot_date, weekly_deaths, col = age_cat, facets = "age_cat",       xname = 'Date',  yname = 'Weekly deaths')
##Data - merging H & D

datD <- tibble(Weeks       = shielding_hosp_summ$hosp_week[ih], 
              Dataz        = shielding_hosp_summ$weekly_admissions[ih],
              DatesH       = shielding_hosp_summ$plot_date[ih],
              Dataw        = shielding_death_summ$weekly_deaths[id],
              DatesD       = shielding_death_summ$plot_date[id],
              Week1OfData  = Week1OfData,
              Week2OfData  = Week2OfData)
#H
p1 <- ggplot(datD, aes(x = Weeks)) + #Dates))
      geom_point(aes(y = Dataz)) +
      labs(x = 'Weeks', y = 'Weekly admissions') 
#D
p2 <- ggplot(datD, aes(x = Weeks)) + #Dates))
      geom_point(aes(y = Dataw)) +
      labs(x = 'Weeks', y = 'Weekly deaths')

if (pset$iplatform==1) { 
      gridExtra::grid.arrange(p1, p2, nrow = 2) }

pdf(file = paste0(output_dir,"/",pset$File_data_plots))
      gridExtra::grid.arrange(p1, p2, nrow = 2)
dev.off()



