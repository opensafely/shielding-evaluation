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
dim_sc = dim(shielding_cohortD)
if (dim_sc[1]>1000){ #only operates on real data, as dummy data has 500 or 1000 rows
  shielding_cohortD <- shielding_cohortD[           which(shielding_cohortD$ons_death_date>="2020-01-01"),]                      #remove deaths prior to 2020 but registered from 2020
  shielding_cohortD <- shielding_cohortD[which(is.element(shielding_cohortD$ons_underlying_cause,c("U071","U072"))),] #remove deaths not caused by covid_deaths_over_time2
}

#H week and year of hospitalisations
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
  drop_na()   #dummy example: drops to dim 11 x 8 from 500 x 89

#D week and year of deaths
shielding_death <- shielding_cohortD                       %>% 
  dplyr::select(patient_id, 
                ons_death_date,
                shielding, 
                age_cat, 
                practice_nuts)                             %>% 
  drop_na()   #dummy example: drops to dim 16 x 5 from 500 x 89

#H week, year, date-by-week variables
StartDateH <- min(shielding_hosp$admission_date, na.rm = TRUE)
  print(paste0("H start date: ", StartDateH))

shielding_hosp <- shielding_hosp %>%
  mutate( week = ceiling(1/7+as.numeric(difftime(admission_date, "2020-01-01", units = "weeks"))), #day 1-7 = w1 #unique id across years
          date = StartDateH + weeks(week - week(StartDateH)) ) %>%              
  arrange(date)  #dim 11 x11

#D week, year, date-by-week variables
StartDateD <- min(shielding_death$ons_death_date, na.rm = TRUE)
print(paste0("D start date: ", StartDateD))

shielding_death <- shielding_death  %>%
  mutate( week = ceiling(1/7+as.numeric(difftime(ons_death_date, "2020-01-01", units = "weeks"))), #day 1-7 = w1
          date = StartDateD + weeks(week - week(StartDateD)) ) %>%
  arrange(date)  #dim 16 x6

#H, D Week frequency
By_week <- function(data, vars){
  data %>% 
    group_by(across({{vars}}))    %>%
    mutate(week_freq = n())    }

#overall
shielding_hosp_summ    <- shielding_hosp  %>% By_week(c(week))
shielding_death_summ   <- shielding_death %>% By_week(c(week))
# by age
shielding_hosp_summ_a  <- shielding_hosp  %>% By_week(c(week, age_cat))                         
shielding_death_summ_a <- shielding_death %>% By_week(c(week, age_cat))

#Date range, week number from start of range, and number or week records
#(date is week id because counts are aggregated in weeks)
#H
Week1OfDataH     = min(shielding_hosp_summ$date)
Week2OfDataH     = max(shielding_hosp_summ$date)
WeekWeek1OfDataH = min(shielding_hosp_summ$week)
WeekWeek2OfDataH = max(shielding_hosp_summ$week)
nH               = length(shielding_hosp_summ$date)
#D
Week1OfDataD     = min(shielding_death_summ$date)
Week2OfDataD     = max(shielding_death_summ$date)
WeekWeek1OfDataD = min(shielding_death_summ$week)
WeekWeek2OfDataD = max(shielding_death_summ$week)
nD               = length(shielding_death_summ$date)
#H, D
print(paste0("H data - date of week, range  ", Week1OfDataH, ", ", Week2OfDataH))
print(paste0("D data - date of week, range  ", Week1OfDataD, ", ", Week2OfDataD))
print(paste0("H data - week count,   range  ", WeekWeek1OfDataH, ", ", WeekWeek2OfDataH))
print(paste0("D data - week count,   range  ", WeekWeek1OfDataD, ", ", WeekWeek2OfDataD))
print(paste0("H data - number of week records = ", nH))
print(paste0("D data - number of week records = ", nD))


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
  print(p)} #for when sourcing the code


##PLOTS WEEKLY
#H
pdf(file = paste0(output_plot,"/",pset$File_data_hosp), width = 8, height = 6)
  ##All
  fig(shielding_hosp_summ, date,  week_freq, facets ='NULL', xname ='Date', yname ='Weekly admissions')
  #by age_cat
    #fig0(shielding_hosp_summ_a, date, week_freq, col = age_cat,                     xname = 'Date',  yname = 'Weekly admissions')
  #by age_cat - panels
    #fig(shielding_hosp_summ_a,  date, week_freq, col = age_cat, facets = "age_cat", xname = 'Date',  yname = 'Weekly admissions')
  dev.off()
#D
pdf(file = paste0(output_plot,"/",pset$File_data_deaths), width = 8, height = 6)
  ##All
  fig(shielding_death_summ, date, week_freq, facets = 'NULL', xname = 'Date',  yname = 'Weekly deaths')
dev.off()

#Datasets to be passed
#(H and D have own set of dates)
datDH <- shielding_hosp_summ            %>% 
         select(week, week_freq, date)  %>%  
         distinct                       %>%
         mutate(Week1OfData = min(shielding_hosp_summ$date), 
                Week2OfData = max(shielding_hosp_summ$date)) %>% 
         rename(Weeks=week, Dataz=week_freq, Dates=date)

datDD <- shielding_death_summ           %>% 
         select(week, week_freq, date)  %>%  
         distinct                       %>%
         mutate(Week1OfData = min(shielding_death_summ$date), 
                Week2OfData = max(shielding_death_summ$date)) %>% 
        rename(Weeks=week, Dataw=week_freq, Dates=date)

#H
p1 <- ggplot(datDH, aes(x = Weeks)) +
      geom_point(aes(y = Dataz)) +
      labs(x = 'Weeks', y = 'Weekly admissions') 
#D
p2 <- ggplot(datDD, aes(x = Weeks)) +
      geom_point(aes(y = Dataw)) +
      labs(x = 'Weeks', y = 'Weekly deaths')

if (pset$iplatform==1) { 
      gridExtra::grid.arrange(p1, p2, nrow = 2) }

pdf(file = paste0(output_dir,"/",pset$File_data_plots))
      gridExtra::grid.arrange(p1, p2, nrow = 2)
dev.off()



