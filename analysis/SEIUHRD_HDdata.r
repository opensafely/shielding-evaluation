## Data subsets, plots, dat files
## All, H, DH, DO
## Aggregation by week, ageg, shielding
## (see HDdata_test.R for tests on simulated data)

library(arrow)
library(data.table)
library(ggplot2)
library(glue)
library(gridExtra)
library(here)
library(lubridate)
library(magrittr)
library(tidyverse)

jobno = "JDat6_"

######## Functions #############################################################
######### Data rounding/truncation
pc2 <- function(x)  { return(round(100*x,2))}
rd2 <- function(x)  { return(round(x,2))}
rd3 <- function(x)  { return(round(x,3))}
tr  <- function(x)  {x = if(x<=7) "<=7" else x; return(x)}

######## Week frequencies
frqs <- function(data, vars, name){
  data %>% 
    group_by(across({{vars}}))    %>%
    mutate({{name}} := n())  %>% #     }
    ungroup() }  #.group = "keep" or "drop" ?

###### Plots weekly
#Figures - overlapping groups
fig0 <- function(data, x , y, col, xname='Week', yname='Weekly admissions') { #yname='Weekly deaths'
  p0 <- ggplot(data, aes(x = {{x}}, y = {{y}}, col = {{col}})) +
    geom_line() +     geom_point(size = 1.2, pch = 1) +    
    labs(x = xname, y = yname) +  ylim(c(0, NA)) +   theme_bw() 
  print(p0)} #for when sourcing the code
#Figures - panels for groups
fig <- function(data, x , y, col, facets, xname='Date', yname='Weekly admissions') { #yname='Weekly deaths'
  p <- ggplot(data, aes(x = {{x}}, y = {{y}}, col = {{col}})) +
    geom_line() +     geom_point(size = 1.2, pch = 1) +    
    facet_wrap(facets, ncol = 1, scales = 'free_y') +
    labs(x = xname, y = yname) +  ylim(c(0, NA)) +  theme_bw() 
  print(p)}
##NOTE: fig0() (overlap) - for now - better than panels - unless for H & D


######## Get Source Data #######################################################
source(here::here("analysis/functions/redaction.R"))

output_dir <- here("output/HDsynthesis")
fs::dir_create(output_dir)

DAT  <- arrow::read_parquet(file = here::here("output/data_edited.gz.parquet"),
                                   compression = "gzip", compression_level = 5)
#### Study date range
Date1="2020-01-01"
Date2="2020-12-01"

#D
dim_sc = dim(DAT) #500, 89
if (dim_sc[1]>1000){ #only operates on real data, as dummy data has 500 to 1000 rows
  DAT <- DAT[           which(DAT$ons_death_date>="2020-01-01") | is.na(DAT$ons_death_date),]                      #remove deaths prior to 2020 but registered from 2020
  DAT <- DAT[which(is.element(DAT$ons_underlying_cause,c("U071","U072")) | is.na(DAT$ons_underlying_cause)),] #remove deaths not caused by covid_deaths_over_time2
}


### First filtering - keeping carehomes and multiple hopsitalisations ##########
DAT <- DAT                                                %>%
  ###patient id/age, care home, shielding, hosp/death 
  dplyr::select(patient_id,
                age_cat,                                       # factor:  0-4 5-11 12-17 18-29 30-39 40-49 50-59 60-69 70+
                care_home, care_home_nursing,                  # logical: T/F - resident in care home
                all_covid_hosp,                                # number:  0:n, hopitalisation count for patient
                #covid_hosp_cat,                               # factor:  0:2, 3+  <= "all_covid_hosp" #dataset.all_covid_hosp = all_covid_hosp \ .count_for_patient()
                dplyr::contains("hosp_admitted"),              # date,    "covid_hosp_admitted_{n}",
                dplyr::contains("hosp_discharge"),             # date,    "covid_hosp_admitted_{n}",
                ons_death_date,                                # date,    covid death
                shielding,                                     # factor:  High Risk, Low/Moderate risk, No shielding
                shielding_v1_startdate,                        # date,    High Risk before 2020-04-21 - NA if binary=0
                shielding_v1_binary,                           # logical: T/F,  High Risk before 2020-04-21
                dplyr::contains("hirisk_codedate"),            # date,    when received 1st-6th high risk flag
                hirisk_shield_count)                       %>% # number:  0:n, high risk flags received per patient
  ###carehome flag
  mutate(carehome = as.factor(ifelse(care_home==TRUE | care_home_nursing==TRUE, 1, 0)) ) %>% 
  mutate(carehome = replace(carehome, is.na(carehome), as.factor(0))) %>% # number: 0:1 - resident in care home with or without nursing
  ###age groups
  filter(!is.na(age_cat))                                  %>% # remove patients with missing 'age_cat'
  ###study period up to 2020-12-01
  filter(ons_death_date         <= "2020-12-01" | is.na(ons_death_date ))        %>%
  filter(covid_hosp_admitted_1  <= "2020-12-01" | is.na(covid_hosp_admitted_1 )) %>%
  filter(covid_hosp_admitted_2  <= "2020-12-01" | is.na(covid_hosp_admitted_2 )) %>%
  filter(covid_hosp_admitted_3  <= "2020-12-01" | is.na(covid_hosp_admitted_3 )) %>%
  filter(covid_hosp_admitted_4  <= "2020-12-01" | is.na(covid_hosp_admitted_4 )) %>%
  filter(covid_hosp_admitted_5  <= "2020-12-01" | is.na(covid_hosp_admitted_5 )) %>%
  filter(covid_hosp_admitted_6  <= "2020-12-01" | is.na(covid_hosp_admitted_6 )) %>%
  filter(shielding_v1_startdate <= "2020-12-01" | is.na(shielding_v1_startdate )) %>%
  #Adding variables:
  mutate(ageg     = as.integer(factor(age_cat)))           %>% # factor: 1:9 <> 0-4 5-11 12-17 18-29 30-39 40-49 50-59 60-69 70+ 
  ###Shielding flag
  mutate(shield   = as.factor(ifelse(shielding=="High Risk",1,0)))  %>% # factor: 0:1 - ever shielded (had high risk flag)
  mutate(shield   = replace(shield, is.na(shield), as.factor(0)))   %>% # TODO: test shield_date < admission_date & < death_date
  ###Shielding flag, but prior 2020-04-21
  mutate(shield1  = ifelse(shielding_v1_binary==TRUE,1,0)) %>% # number: 0:1 - hHigh Risk before 2020-04-21
  mutate(shield1  = ifelse(is.na(shield1), 0, shield1))    %>% #
  mutate(shield1_date = shielding_v1_startdate)            %>% # date, or NA if shield1=0
  #mutate(shield1 = as.numeric( min(c(admission_date, ons_death_date), na.rm = T) > shielding_v1_startdate) ) %>% #Limit shielding by date
  ###Restrict to deaths or hospitalisations, not neither
  filter(!is.na(ons_death_date) | all_covid_hosp>0)        %>%   # Tried "| !is.na(admission_date))": too few
  ###Refer admissions to first admission (NB: removed pivot_longer, hence one row per patient)
  mutate(admission_date = covid_hosp_admitted_1)           %>%
  ###Refer discharge to first discharge 
  mutate(discharge_date = covid_hosp_discharge_1)          %>%
  ###(Discarded) Refer discharge to last discharge
  ###Vars replaced by numeric/factor flags
  select(-c(dplyr::contains("hirisk"), shielding, 
            shielding_v1_binary, shielding_v1_startdate))    %>%   # no longer need 
  ###For now, drop thes
  select(-c(shield1,shield1_date))

names1 = names(DAT)
print(paste0("names1"))
print(paste0(names1))


### Aggregation
#H D week, year, dates-by-week
StartDateH <- min(DAT$admission_date, na.rm = T)
StartDateD <- min(DAT$ons_death_date, na.rm = T)
print(paste0("H start date: ", StartDateH))
print(paste0("D start date: ", StartDateD))

DAT <- DAT %>%
  mutate( weekH = ceiling(1/7+as.numeric(difftime(admission_date, "2020-01-01", units = "weeks"))), #day 1-7 = w1 #unique id across years
          dateH = StartDateH + weeks(weekH - week(StartDateH)) ) %>%              
  mutate( weekD = ceiling(1/7+as.numeric(difftime(ons_death_date, "2020-01-01", units = "weeks"))), #day 1-7 = w1
          dateD = StartDateD + weeks(weekD - week(StartDateD)) ) #%>%

names1 = names(DAT)
print(paste0("names1"))
print(paste0(names1))


### Carehomes plots and data
pdf(file = paste0(output_dir,"/",jobno,"Carehomes_included_H,D.pdf"), width = 8, height = 6)

## PLot Hospitalisations by carehome use
  dat <- DAT %>% filter(!is.na(weekH) | all_covid_hosp>0) %>% 
                 frqs(c(weekH, carehome), "freqHch") %>% 
                 select(weekH, dateH, freqHch, carehome) %>%
                 group_by(carehome) %>%
                 distinct(weekH, .keep_all = TRUE) %>%
                 arrange(carehome, weekH)
  fig0(dat, x=weekH, y=freqHch, col=carehome, xname ='Week', yname ='Hospital admissions')
  ### save data - truncated
  dati <- dat %>% mutate(freqHch = ifelse(freqHch<=7,"<=7",freqHch)) %>%
                  write.csv(file=paste0(output_dir,"/",jobno,"Carehomes_included_H.csv")) 
  
## PLot Deaths anywhere by carehome use
  dat <- DAT %>% filter(!is.na(weekD) | !is.na(ons_death_date)) %>% 
                 frqs(c(weekD, carehome), "freqDch") %>% 
                 select(weekD, dateD, freqDch, carehome) %>%
                 group_by(carehome) %>%
                 distinct(weekD, .keep_all = TRUE) %>%
                 arrange(carehome, weekD)
  fig0(dat, x=weekD, y=freqDch, col=carehome, xname ='Week', yname ='Deaths anywhere')
  ### save data - truncated
  dati <- dat %>% mutate(freqDch = ifelse(freqDch<=7,"<=7",freqDch)) %>%
                  write.csv(file=paste0(output_dir,"/",jobno,"Carehomes_included_D.csv")) 
  
## PLot Deaths in Hospital by carehome use
  dat <- DAT %>% filter((!is.na(weekD) | !is.na(ons_death_date)) & (!is.na(weekH) | all_covid_hosp>0)) %>% 
                 frqs(c(weekD, carehome), "freqDch") %>% 
                 select(weekD, dateD, freqDch, carehome) %>%
                 group_by(carehome) %>%
                 distinct(weekD, .keep_all = TRUE) %>%
                 arrange(carehome, weekD)
  fig0(dat, x=weekD, y=freqDch, col=carehome, xname ='Week', yname ='Deaths in Hospital')
  ### save data - truncated
  dati <- dat %>% mutate(freqDch = ifelse(freqDch<=7,"<=7",freqDch)) %>%
                  write.csv(file=paste0(output_dir,"/",jobno,"Carehomes_included_DH.csv"))

## PLot Deaths outside Hospital by carehome use
  dat <- DAT %>% filter((!is.na(weekD) | !is.na(ons_death_date)) & (is.na(weekH) | all_covid_hosp==0)) %>% 
                 frqs(c(weekD, carehome), "freqDch") %>% 
                 select(weekD, dateD, freqDch, carehome) %>%
                 group_by(carehome) %>%
                 distinct(weekD, .keep_all = TRUE) %>%
                 arrange(carehome, weekD)
  fig0(dat, x=weekD, y=freqDch, col=carehome, xname ='Week', yname ='Deaths outside Hospital')
  ### save data - truncated
  dati <- dat %>% mutate(freqDch = ifelse(freqDch<=7,"<=7",freqDch)) %>%
                  write.csv(file=paste0(output_dir,"/",jobno,"Carehomes_included_DO.csv"))
dev.off()


### Hospital re-admissions plots and data
### TODO: PLot H: all, first, and subsequent hospitalisations



### Second filtering - remove carehomes & subsequent hospitalisations
###                  - remove patient_id, change age_cat
DAT <- DAT                                                 %>%
  ###Remove carehomes & multiple hospitalisations
  filter( (care_home_nursing==FALSE | is.na(care_home_nursing)) & 
          (care_home==FALSE | is.na(care_home)) )          %>%
  ###for excel writing
  #mutate(age_cat  = paste0("'", age_cat))                  %>% #Not writing age_cat but ageg in files
  ###No longer need or variable replaced with numeric/factor flags
  select(-c(care_home_nursing, care_home, carehome,
            patient_id, #all_covid_hosp, 
            dplyr::contains("hosp_admitted"),
            dplyr::contains("hosp_discharge")) )           %>%
  #drop_na()    #Not yet for joint H and D and shielding
  ungroup()

names2 = names(DAT)
print(paste0("names2"))
print(paste0(names2))



### For fitting - TIME SERIES DATA #############################################
### H & D events linked at patient level
#TODO: check weekH < weekD (hospitalisations and deaths in hospital)
#TODO: shield1 rather than shield


pdf(file = paste0(output_dir,"/",jobno,"HDdata_Timeseries_H,HR,DH,DO.pdf"), width = 8, height = 6)

### DATA Hospitalisations (first admission) by week, age, and shielding ########
  dat <- DAT %>% filter(!is.na(weekH) | all_covid_hosp>0) %>%
                 frqs(c(weekH, age_cat, shield), "freqHas") %>% 
                 select(weekH, dateH, freqHas, age_cat, ageg, shield)
  ### plot Hospitalisations
  nH = sum(dat$freqHas,na.rm=T)
  fig0(dat, x=weekH, y=freqHas, col=NULL,    xname ='Week', yname =paste0('Admissions (tot ', nH,')') )
  fig0(dat, x=weekH, y=freqHas, col=age_cat, xname ='Week', yname =paste0('Admissions (tot ', nH,')') )
  fig0(dat, x=weekH, y=freqHas, col=shield,  xname ='Week', yname =paste0('Admissions (tot ', nH,')') )
  ### plot by shielding
  dat0 <- dat %>% filter(shield==0)
  dat1 <- dat %>% filter(shield==1)
  nH0 = sum(dat0$freqHas,na.rm=T)
  nH1 = sum(dat1$freqHas,na.rm=T)
  fig0(dat0, x=weekH, y=freqHas, col=age_cat, xname ='Week', yname =paste0('Admissions not shielding (tot ', nH0,')') )
  fig0(dat1, x=weekH, y=freqHas, col=age_cat, xname ='Week', yname =paste0('Admissions shielding (tot ',     nH1,')') )
  ### save data - truncated
  dat  <- dat %>% select(weekH, dateH, freqHas, ageg, shield) %>%
                  group_by(ageg, shield) %>%
                  distinct(weekH, .keep_all = TRUE) %>%
                  arrange(ageg, shield, weekH)
  datH <- dat %>% rename(WeeksH=weekH, Dataz=freqHas, DatesH=dateH)           #fit data
  dat  <- dat %>% mutate(freqHas = ifelse(freqHas<=7,"<=7",freqHas)) %>%
                  write.csv(file=paste0(output_dir,"/",jobno,"Hdata.csv"))    #show data
  ## save data by age and shielding - truncated
  #  for (i in 1:9){ for(s in 0:1){
  #  dati <- dat %>% filter(ageg==i & shield==s) %>% select(weekH, freqHas) %>% 
  #                  mutate(freqHas = ifelse(freqHas<=7,"<=7",freqHas)) %>% arrange(weekH) %>%
  #                  write.csv(file=paste0(output_dir,"/","Hdata_a",i,"_s",s,".csv")) }}

  ### DATA Hospitalisations with recovery by week, age, and shielding ########
  dat <- DAT %>% filter( (!is.na(weekH) | all_covid_hosp>0) & (is.na(weekD) | is.na(ons_death_date)) ) %>%
                 frqs(c(weekH, age_cat, shield), "freqHas") %>% 
                 select(weekH, dateH, freqHas, age_cat, ageg, shield)
  ### plot Hospitalisations with recovery
  nHR = sum(dat$freqHas,na.rm=T)
  fig0(dat, x=weekH, y=freqHas, col=NULL,    xname ='Week', yname =paste0('Admissions & recovery (tot ', nHR,')') )
  fig0(dat, x=weekH, y=freqHas, col=age_cat, xname ='Week', yname =paste0('Admissions & recovery (tot ', nHR,')') )
  fig0(dat, x=weekH, y=freqHas, col=shield,  xname ='Week', yname =paste0('Admissions & recovery (tot ', nHR,')') )
  ### plot by shielding
  dat0 <- dat %>% filter(shield==0)
  dat1 <- dat %>% filter(shield==1)
  nHR0 = sum(dat0$freqHas,na.rm=T)
  nHR1 = sum(dat1$freqHas,na.rm=T)
  fig0(dat0, x=weekH, y=freqHas, col=age_cat, xname ='Week', yname =paste0('Admissions & recovery not shielding (tot ', nHR0,')') )
  fig0(dat1, x=weekH, y=freqHas, col=age_cat, xname ='Week', yname =paste0('Admissions & recovery shielding (tot ',     nHR1,')') )
  ### save data - truncated
  dat   <- dat %>% select(weekH, dateH, freqHas, ageg, shield) %>%
                   group_by(ageg, shield) %>%
                   distinct(weekH, .keep_all = TRUE) %>%
                   arrange(ageg, shield, weekH)
  datHR <- dat %>% rename(WeeksH=weekH, Dataz=freqHas, DatesH=dateH)          #fit data
  dat   <- dat %>% mutate(freqHas = ifelse(freqHas<=7,"<=7",freqHas)) %>%
                   write.csv(file=paste0(output_dir,"/",jobno,"HRdata.csv"))  #show data
  
  ### DATA Deaths in Hospital by week, age, and shielding ########################
  #TODO: & weekH < weekD)
  dat <- DAT %>% filter((!is.na(weekD) | !is.na(ons_death_date)) & (!is.na(weekH) | all_covid_hosp>0)) %>% 
                 frqs(c(weekD, age_cat, shield), "freqDas") %>%
                 select(weekD, dateD, freqDas, age_cat, ageg, shield)
  ### plot Deaths in Hospital
  nDH = sum(dat$freqDas,na.rm=T)
  fig0(dat, x=weekD, y=freqDas, col=NULL,    xname ='Week', yname =paste0('Deaths in Hospital (tot ', nDH,')') )
  fig0(dat, x=weekD, y=freqDas, col=age_cat, xname ='Week', yname =paste0('Deaths in Hospital (tot ', nDH,')') )
  fig0(dat, x=weekD, y=freqDas, col=shield,  xname ='Week', yname =paste0('Deaths in Hospital (tot ', nDH,')') )
  ### plot by shielding
  dat0 <- dat %>% filter(shield==0)
  dat1 <- dat %>% filter(shield==1)
  nDH0 = sum(dat0$freqDas,na.rm=T)
  nDH1 = sum(dat1$freqDas,na.rm=T)
  fig0(dat0, x=weekD, y=freqDas, col=age_cat, xname ='Week', yname =paste0('Deaths in Hospital not shielding (tot ', nDH0,')') )
  fig0(dat1, x=weekD, y=freqDas, col=age_cat, xname ='Week', yname =paste0('Deaths in Hospital shielding (tot ',     nDH1,')') )
  ### save data - truncated
  dat   <- dat %>% select(weekD, dateD, freqDas, ageg, shield) %>%
                   group_by(ageg, shield) %>%
                   distinct(weekD, .keep_all = TRUE) %>%
                   arrange(ageg, shield, weekD)
  datDH <- dat %>% rename(WeeksD=weekD, Dataw=freqDas, DatesD=dateD)          #fit data
  dat   <- dat %>% mutate(freqDas = ifelse(freqDas<=7,"<=7",freqDas)) %>%
                   write.csv(file=paste0(output_dir,"/",jobno,"DHdata.csv"))  #show data
  
### DATA Deaths outside Hospital by week, age, and shielding ###################
  dat <- DAT %>% filter((!is.na(weekD) | !is.na(ons_death_date)) & (is.na(weekH) | all_covid_hosp==0)) %>% 
                 frqs(c(weekD, age_cat, shield), "freqDas") %>% 
                 select(weekD, dateD, freqDas, age_cat, ageg, shield)
  ### plot Deaths outside Hospital
  nDO = sum(dat$freqDas,na.rm=T)
  fig0(dat, x=weekD, y=freqDas, col=NULL,    xname ='Week', yname =paste0('Deaths outside Hospital (tot ', nDO,')') )
  fig0(dat, x=weekD, y=freqDas, col=age_cat, xname ='Week', yname =paste0('Deaths outside Hospital (tot ', nDO,')') )
  fig0(dat, x=weekD, y=freqDas, col=shield,  xname ='Week', yname =paste0('Deaths outside Hospital (tot ', nDO,')') )
  ### plot Deaths outside Hospital by shielding
  dat0 <- dat %>% filter(shield==0)
  dat1 <- dat %>% filter(shield==1)
  nDO0 = sum(dat0$freqDas,na.rm=T)
  nDO1 = sum(dat1$freqDas,na.rm=T)
  fig0(dat0, x=weekD, y=freqDas, col=age_cat, xname ='Week', yname =paste0('Deaths outside Hospital not shielding (total ', nDO0,')') )
  fig0(dat1, x=weekD, y=freqDas, col=age_cat, xname ='Week', yname =paste0('Deaths outside Hospital shielding (total ',     nDO1,')') )
  ### save data - truncated
  dat   <- dat %>% select(weekD, dateD, freqDas, ageg, shield) %>%
                   group_by(ageg, shield) %>%
                   distinct(weekD, .keep_all = TRUE) %>%
                   arrange(ageg, shield, weekD)
  datDO <- dat %>% rename(WeeksD=weekD, Dataw=freqDas, DatesD=dateD)          #fit data
  dat   <- dat %>% mutate(freqDas = ifelse(freqDas<=7,"<=7",freqDas)) %>% 
                   write.csv(file=paste0(output_dir,"/",jobno,"DOdata.csv"))  #show data
dev.off()



### DATA plots overall #########################################################
### DATA Hospitalisations (first admission) ####################################
  dat <- DAT %>% filter(!is.na(weekH) | all_covid_hosp>0) %>%
                 frqs(c(weekH), "freqH") %>% select(weekH, freqH)
  nH = sum(dat$freqH,na.rm=T)
  p1 <- ggplot(dat, aes(x = weekH)) +
        geom_point(aes(y = freqH))  + labs(x = 'Week', y = paste0('Admissions (tot ', nH,')'))
### DATA Hospitalisations with recovery ########################################
  dat <- DAT %>% filter( (!is.na(weekH) | all_covid_hosp>0) & (is.na(weekD) | is.na(ons_death_date)) ) %>%
                 frqs(c(weekH), "freqH") %>% select(weekH, freqH)
  nHR = sum(dat$freqH,na.rm=T)
  p2 <- ggplot(dat, aes(x = weekH)) +
        geom_point(aes(y = freqH))  + labs(x = 'Week', y =paste0('Admissions & recovery (tot ', nHR,')')) 
### DATA Deaths in Hospital ####################################################
  dat <- DAT %>% filter( (!is.na(weekD) | !is.na(ons_death_date)) & (!is.na(weekH) | all_covid_hosp>0)) %>% 
                 frqs(c(weekD), "freqD") %>% select(weekD, freqD)
  nDH = sum(dat$freqD,na.rm=T)
  p3 <- ggplot(dat, aes(x = weekD)) +
        geom_point(aes(y = freqD))  + labs(x = 'Week', y =paste0('Deaths in Hospital (tot ', nDH,')'))

### DATA Deaths outside Hospital ###############################################
  dat <- DAT %>% filter((!is.na(weekD) | !is.na(ons_death_date)) & (is.na(weekH) | all_covid_hosp==0)) %>% 
                 frqs(c(weekD), "freqD") %>% select(weekD, freqD)
  nDO = sum(dat$freqD,na.rm=T)
  p4 <- ggplot(dat, aes(x = weekD)) +
        geom_point(aes(y = freqD))  + labs(x = 'Week', y =paste0('Deaths outside Hospital (tot ', nDO,')') )

pdf(file = paste0(output_dir,"/",jobno,"HDdata_Incidence_H,HR,DH,DO.pdf"))
  gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol=2)
dev.off()




#### Summary of statistics and parameter estimates
##### Data & metadata for _fit
sink(file = paste0(output_dir,"/",jobno,"HDdata_summary.txt"),append=F,split=F)

#TODO: assess date/week range of each TS - use it or set missing data to zero?

print(paste0("Date range: ", Date1," to ", Date2))

cat("Time series date range \n")
#TODO; use weekH or weekD rather than date?
##H
WeekLim_H   = range(datH$DatesH)
WeekLim_H1  = range(datH$DatesH[which(datH$ageg==1)])
WeekLim_H2  = range(datH$DatesH[which(datH$ageg==2)],na.rm=T)
WeekLim_H3  = range(datH$DatesH[which(datH$ageg==3)],na.rm=T)
WeekLim_H4  = range(datH$DatesH[which(datH$ageg==4)],na.rm=T)
WeekLim_H5  = range(datH$DatesH[which(datH$ageg==5)],na.rm=T)
WeekLim_H6  = range(datH$DatesH[which(datH$ageg==6)])
WeekLim_H7  = range(datH$DatesH[which(datH$ageg==7)])
WeekLim_H8  = range(datH$DatesH[which(datH$ageg==8)])
WeekLim_H9  = range(datH$DatesH[which(datH$ageg==9)])
WeekLim_H10 = range(datH$DatesH[which(datH$ageg==1 & datH$shield=="0")])
WeekLim_H20 = range(datH$DatesH[which(datH$ageg==2 & datH$shield=="0")])
WeekLim_H30 = range(datH$DatesH[which(datH$ageg==3 & datH$shield=="0")])
WeekLim_H40 = range(datH$DatesH[which(datH$ageg==4 & datH$shield=="0")])
WeekLim_H50 = range(datH$DatesH[which(datH$ageg==5 & datH$shield=="0")])
WeekLim_H60 = range(datH$DatesH[which(datH$ageg==6 & datH$shield=="0")])
WeekLim_H70 = range(datH$DatesH[which(datH$ageg==7 & datH$shield=="0")])
WeekLim_H80 = range(datH$DatesH[which(datH$ageg==8 & datH$shield=="0")])
WeekLim_H90 = range(datH$DatesH[which(datH$ageg==9 & datH$shield=="0")])
WeekLim_H11 = range(datH$DatesH[which(datH$ageg==1 & datH$shield=="1")])
WeekLim_H21 = range(datH$DatesH[which(datH$ageg==2 & datH$shield=="1")])
WeekLim_H31 = range(datH$DatesH[which(datH$ageg==3 & datH$shield=="1")])
WeekLim_H41 = range(datH$DatesH[which(datH$ageg==4 & datH$shield=="1")])
WeekLim_H51 = range(datH$DatesH[which(datH$ageg==5 & datH$shield=="1")])
WeekLim_H61 = range(datH$DatesH[which(datH$ageg==6 & datH$shield=="1")])
WeekLim_H71 = range(datH$DatesH[which(datH$ageg==7 & datH$shield=="1")])
WeekLim_H81 = range(datH$DatesH[which(datH$ageg==8 & datH$shield=="1")])
WeekLim_H91 = range(datH$DatesH[which(datH$ageg==9 & datH$shield=="1")])

##DH
WeekLim_DH   = range(datDH$DatesD)
WeekLim_DH1  = range(datDH$DatesD[which(datDH$ageg==1)],na.rm=T)
WeekLim_DH2  = range(datDH$DatesD[which(datDH$ageg==2)],na.rm=T)
WeekLim_DH3  = range(datDH$DatesD[which(datDH$ageg==3)],na.rm=T)
WeekLim_DH4  = range(datDH$DatesD[which(datDH$ageg==4)],na.rm=T)
WeekLim_DH5  = range(datDH$DatesD[which(datDH$ageg==5)],na.rm=T)
WeekLim_DH6  = range(datDH$DatesD[which(datDH$ageg==6)])
WeekLim_DH7  = range(datDH$DatesD[which(datDH$ageg==7)])
WeekLim_DH8  = range(datDH$DatesD[which(datDH$ageg==8)])
WeekLim_DH9  = range(datDH$DatesD[which(datDH$ageg==9)])
WeekLim_DH10 = range(datDH$DatesD[which(datDH$ageg==1 & datDH$shield=="0")],na.rm=T)
WeekLim_DH20 = range(datDH$DatesD[which(datDH$ageg==2 & datDH$shield=="0")],na.rm=T)
WeekLim_DH30 = range(datDH$DatesD[which(datDH$ageg==3 & datDH$shield=="0")],na.rm=T)
WeekLim_DH40 = range(datDH$DatesD[which(datDH$ageg==4 & datDH$shield=="0")],na.rm=T)
WeekLim_DH50 = range(datDH$DatesD[which(datDH$ageg==5 & datDH$shield=="0")],na.rm=T)
WeekLim_DH60 = range(datDH$DatesD[which(datDH$ageg==6 & datDH$shield=="0")])
WeekLim_DH70 = range(datDH$DatesD[which(datDH$ageg==7 & datDH$shield=="0")])
WeekLim_DH80 = range(datDH$DatesD[which(datDH$ageg==8 & datDH$shield=="0")])
WeekLim_DH90 = range(datDH$DatesD[which(datDH$ageg==9 & datDH$shield=="0")])
WeekLim_DH11 = range(datDH$DatesD[which(datDH$ageg==1 & datDH$shield=="1")],na.rm=T)
WeekLim_DH21 = range(datDH$DatesD[which(datDH$ageg==2 & datDH$shield=="1")],na.rm=T)
WeekLim_DH31 = range(datDH$DatesD[which(datDH$ageg==3 & datDH$shield=="1")],na.rm=T)
WeekLim_DH41 = range(datDH$DatesD[which(datDH$ageg==4 & datDH$shield=="1")],na.rm=T)
WeekLim_DH51 = range(datDH$DatesD[which(datDH$ageg==5 & datDH$shield=="1")],na.rm=T)
WeekLim_DH61 = range(datDH$DatesD[which(datDH$ageg==6 & datDH$shield=="1")])
WeekLim_DH71 = range(datDH$DatesD[which(datDH$ageg==7 & datDH$shield=="1")])
WeekLim_DH81 = range(datDH$DatesD[which(datDH$ageg==8 & datDH$shield=="1")])
WeekLim_DH91 = range(datDH$DatesD[which(datDH$ageg==9 & datDH$shield=="1")])

##DO
WeekLim_DO   = range(datDO$DatesD)
WeekLim_DO1  = range(datDO$DatesD[which(datDO$ageg==1)],na.rm=T)
WeekLim_DO2  = range(datDO$DatesD[which(datDO$ageg==2)],na.rm=T)
WeekLim_DO3  = range(datDO$DatesD[which(datDO$ageg==3)],na.rm=T)
WeekLim_DO4  = range(datDO$DatesD[which(datDO$ageg==4)],na.rm=T)
WeekLim_DO5  = range(datDO$DatesD[which(datDO$ageg==5)],na.rm=T)
WeekLim_DO6  = range(datDO$DatesD[which(datDO$ageg==6)])
WeekLim_DO7  = range(datDO$DatesD[which(datDO$ageg==7)])
WeekLim_DO8  = range(datDO$DatesD[which(datDO$ageg==8)])
WeekLim_DO9  = range(datDO$DatesD[which(datDO$ageg==9)])
WeekLim_DO10 = range(datDO$DatesD[which(datDO$ageg==1 & datDO$shield=="0")],na.rm=T)
WeekLim_DO20 = range(datDO$DatesD[which(datDO$ageg==2 & datDO$shield=="0")],na.rm=T)
WeekLim_DO30 = range(datDO$DatesD[which(datDO$ageg==3 & datDO$shield=="0")],na.rm=T)
WeekLim_DO40 = range(datDO$DatesD[which(datDO$ageg==4 & datDO$shield=="0")],na.rm=T)
WeekLim_DO50 = range(datDO$DatesD[which(datDO$ageg==5 & datDO$shield=="0")],na.rm=T)
WeekLim_DO60 = range(datDO$DatesD[which(datDO$ageg==6 & datDO$shield=="0")])
WeekLim_DO70 = range(datDO$DatesD[which(datDO$ageg==7 & datDO$shield=="0")])
WeekLim_DO80 = range(datDO$DatesD[which(datDO$ageg==8 & datDO$shield=="0")])
WeekLim_DO90 = range(datDO$DatesD[which(datDO$ageg==9 & datDO$shield=="0")])
WeekLim_DO11 = range(datDO$DatesD[which(datDO$ageg==1 & datDO$shield=="1")],na.rm=T)
WeekLim_DO21 = range(datDO$DatesD[which(datDO$ageg==2 & datDO$shield=="1")],na.rm=T)
WeekLim_DO31 = range(datDO$DatesD[which(datDO$ageg==3 & datDO$shield=="1")],na.rm=T)
WeekLim_DO41 = range(datDO$DatesD[which(datDO$ageg==4 & datDO$shield=="1")],na.rm=T)
WeekLim_DO51 = range(datDO$DatesD[which(datDO$ageg==5 & datDO$shield=="1")],na.rm=T)
WeekLim_DO61 = range(datDO$DatesD[which(datDO$ageg==6 & datDO$shield=="1")])
WeekLim_DO71 = range(datDO$DatesD[which(datDO$ageg==7 & datDO$shield=="1")])
WeekLim_DO81 = range(datDO$DatesD[which(datDO$ageg==8 & datDO$shield=="1")])
WeekLim_DO91 = range(datDO$DatesD[which(datDO$ageg==9 & datDO$shield=="1")])
cat("\n")
print(paste0("H data"))
print(paste0("Week range Hosp:                  ", WeekLim_H[1],   ", ", WeekLim_H[2] ))
print(paste0("Week range Hosp ageg 1:           ", WeekLim_H1[1],  ", ", WeekLim_H1[2] ))
print(paste0("Week range Hosp ageg 2:           ", WeekLim_H2[1],  ", ", WeekLim_H2[2] ))
print(paste0("Week range Hosp ageg 3:           ", WeekLim_H3[1],  ", ", WeekLim_H3[2] ))
print(paste0("Week range Hosp ageg 4:           ", WeekLim_H4[1],  ", ", WeekLim_H4[2] ))
print(paste0("Week range Hosp ageg 5:           ", WeekLim_H5[1],  ", ", WeekLim_H5[2] ))
print(paste0("Week range Hosp ageg 6:           ", WeekLim_H6[1],  ", ", WeekLim_H6[2] ))
print(paste0("Week range Hosp ageg 7:           ", WeekLim_H7[1],  ", ", WeekLim_H7[2] ))
print(paste0("Week range Hosp ageg 8:           ", WeekLim_H8[1],  ", ", WeekLim_H8[2] ))
print(paste0("Week range Hosp ageg 9:           ", WeekLim_H9[1],  ", ", WeekLim_H9[2] ))
print(paste0("Week range Hosp ageg 1 no shield: ", WeekLim_H10[1], ", ", WeekLim_H10[2] ))
print(paste0("Week range Hosp ageg 2 no shield: ", WeekLim_H20[1], ", ", WeekLim_H20[2] ))
print(paste0("Week range Hosp ageg 3 no shield: ", WeekLim_H30[1], ", ", WeekLim_H30[2] ))
print(paste0("Week range Hosp ageg 4 no shield: ", WeekLim_H40[1], ", ", WeekLim_H40[2] ))
print(paste0("Week range Hosp ageg 5 no shield: ", WeekLim_H50[1], ", ", WeekLim_H50[2] ))
print(paste0("Week range Hosp ageg 6 no shield: ", WeekLim_H60[1], ", ", WeekLim_H60[2] ))
print(paste0("Week range Hosp ageg 7 no shield: ", WeekLim_H70[1], ", ", WeekLim_H70[2] ))
print(paste0("Week range Hosp ageg 8 no shield: ", WeekLim_H80[1], ", ", WeekLim_H80[2] ))
print(paste0("Week range Hosp ageg 9 no shield: ", WeekLim_H90[1], ", ", WeekLim_H90[2] ))
print(paste0("Week range Hosp ageg 1 shielding: ", WeekLim_H11[1], ", ", WeekLim_H11[2] ))
print(paste0("Week range Hosp ageg 2 shielding: ", WeekLim_H21[1], ", ", WeekLim_H21[2] ))
print(paste0("Week range Hosp ageg 3 shielding: ", WeekLim_H31[1], ", ", WeekLim_H31[2] ))
print(paste0("Week range Hosp ageg 4 shielding: ", WeekLim_H41[1], ", ", WeekLim_H41[2] ))
print(paste0("Week range Hosp ageg 5 shielding: ", WeekLim_H51[1], ", ", WeekLim_H51[2] ))
print(paste0("Week range Hosp ageg 6 shielding: ", WeekLim_H61[1], ", ", WeekLim_H61[2] ))
print(paste0("Week range Hosp ageg 7 shielding: ", WeekLim_H71[1], ", ", WeekLim_H71[2] ))
print(paste0("Week range Hosp ageg 8 shielding: ", WeekLim_H81[1], ", ", WeekLim_H81[2] ))
print(paste0("Week range Hosp ageg 9 shielding: ", WeekLim_H91[1], ", ", WeekLim_H91[2] ))
cat("\n")
print(paste0("DH data"))
print(paste0("Week range deaths in Hosp:                  ", WeekLim_DH[1],   ", ", WeekLim_DH[2] ))
print(paste0("Week range deaths in Hosp ageg 1:           ", WeekLim_DH1[1],  ", ", WeekLim_DH1[2] ))
print(paste0("Week range deaths in Hosp ageg 2:           ", WeekLim_DH2[1],  ", ", WeekLim_DH2[2] ))
print(paste0("Week range deaths in Hosp ageg 3:           ", WeekLim_DH3[1],  ", ", WeekLim_DH3[2] ))
print(paste0("Week range deaths in Hosp ageg 4:           ", WeekLim_DH4[1],  ", ", WeekLim_DH4[2] ))
print(paste0("Week range deaths in Hosp ageg 5:           ", WeekLim_DH5[1],  ", ", WeekLim_DH5[2] ))
print(paste0("Week range deaths in Hosp ageg 6:           ", WeekLim_DH6[1],  ", ", WeekLim_DH6[2] ))
print(paste0("Week range deaths in Hosp ageg 7:           ", WeekLim_DH7[1],  ", ", WeekLim_DH7[2] ))
print(paste0("Week range deaths in Hosp ageg 8:           ", WeekLim_DH8[1],  ", ", WeekLim_DH8[2] ))
print(paste0("Week range deaths in Hosp ageg 9:           ", WeekLim_DH9[1],  ", ", WeekLim_DH9[2] ))
print(paste0("Week range deaths in Hosp ageg 1 no shield: ", WeekLim_DH10[1], ", ", WeekLim_DH10[2] ))
print(paste0("Week range deaths in Hosp ageg 2 no shield: ", WeekLim_DH20[1], ", ", WeekLim_DH20[2] ))
print(paste0("Week range deaths in Hosp ageg 3 no shield: ", WeekLim_DH30[1], ", ", WeekLim_DH30[2] ))
print(paste0("Week range deaths in Hosp ageg 4 no shield: ", WeekLim_DH40[1], ", ", WeekLim_DH40[2] ))
print(paste0("Week range deaths in Hosp ageg 5 no shield: ", WeekLim_DH50[1], ", ", WeekLim_DH50[2] ))
print(paste0("Week range deaths in Hosp ageg 6 no shield: ", WeekLim_DH60[1], ", ", WeekLim_DH60[2] ))
print(paste0("Week range deaths in Hosp ageg 7 no shield: ", WeekLim_DH70[1], ", ", WeekLim_DH70[2] ))
print(paste0("Week range deaths in Hosp ageg 8 no shield: ", WeekLim_DH80[1], ", ", WeekLim_DH80[2] ))
print(paste0("Week range deaths in Hosp ageg 9 no shield: ", WeekLim_DH90[1], ", ", WeekLim_DH90[2] ))
print(paste0("Week range deaths in Hosp ageg 1 shielding: ", WeekLim_DH11[1], ", ", WeekLim_DH11[2] ))
print(paste0("Week range deaths in Hosp ageg 2 shielding: ", WeekLim_DH21[1], ", ", WeekLim_DH21[2] ))
print(paste0("Week range deaths in Hosp ageg 3 shielding: ", WeekLim_DH31[1], ", ", WeekLim_DH31[2] ))
print(paste0("Week range deaths in Hosp ageg 4 shielding: ", WeekLim_DH41[1], ", ", WeekLim_DH41[2] ))
print(paste0("Week range deaths in Hosp ageg 5 shielding: ", WeekLim_DH51[1], ", ", WeekLim_DH51[2] ))
print(paste0("Week range deaths in Hosp ageg 6 shielding: ", WeekLim_DH61[1], ", ", WeekLim_DH61[2] ))
print(paste0("Week range deaths in Hosp ageg 7 shielding: ", WeekLim_DH71[1], ", ", WeekLim_DH71[2] ))
print(paste0("Week range deaths in Hosp ageg 8 shielding: ", WeekLim_DH81[1], ", ", WeekLim_DH81[2] ))
print(paste0("Week range deaths in Hosp ageg 9 shielding: ", WeekLim_DH91[1], ", ", WeekLim_DH91[2] ))
cat("\n")
print(paste0("DO data"))
print(paste0("Week range deaths outside Hosp:                  ", WeekLim_DO[1],   ", ", WeekLim_DO[2] ))
print(paste0("Week range deaths outside Hosp ageg 1:           ", WeekLim_DO1[1],  ", ", WeekLim_DO1[2] ))
print(paste0("Week range deaths outside Hosp ageg 2:           ", WeekLim_DO2[1],  ", ", WeekLim_DO2[2] ))
print(paste0("Week range deaths outside Hosp ageg 3:           ", WeekLim_DO3[1],  ", ", WeekLim_DO3[2] ))
print(paste0("Week range deaths outside Hosp ageg 4:           ", WeekLim_DO4[1],  ", ", WeekLim_DO4[2] ))
print(paste0("Week range deaths outside Hosp ageg 5:           ", WeekLim_DO5[1],  ", ", WeekLim_DO5[2] ))
print(paste0("Week range deaths outside Hosp ageg 6:           ", WeekLim_DO6[1],  ", ", WeekLim_DO6[2] ))
print(paste0("Week range deaths outside Hosp ageg 7:           ", WeekLim_DO7[1],  ", ", WeekLim_DO7[2] ))
print(paste0("Week range deaths outside Hosp ageg 8:           ", WeekLim_DO8[1],  ", ", WeekLim_DO8[2] ))
print(paste0("Week range deaths outside Hosp ageg 9:           ", WeekLim_DO9[1],  ", ", WeekLim_DO9[2] ))
print(paste0("Week range deaths outside Hosp ageg 1 no shield: ", WeekLim_DO10[1], ", ", WeekLim_DO10[2] ))
print(paste0("Week range deaths outside Hosp ageg 2 no shield: ", WeekLim_DO20[1], ", ", WeekLim_DO20[2] ))
print(paste0("Week range deaths outside Hosp ageg 3 no shield: ", WeekLim_DO30[1], ", ", WeekLim_DO30[2] ))
print(paste0("Week range deaths outside Hosp ageg 4 no shield: ", WeekLim_DO40[1], ", ", WeekLim_DO40[2] ))
print(paste0("Week range deaths outside Hosp ageg 5 no shield: ", WeekLim_DO50[1], ", ", WeekLim_DO50[2] ))
print(paste0("Week range deaths outside Hosp ageg 6 no shield: ", WeekLim_DO60[1], ", ", WeekLim_DO60[2] ))
print(paste0("Week range deaths outside Hosp ageg 7 no shield: ", WeekLim_DO70[1], ", ", WeekLim_DO70[2] ))
print(paste0("Week range deaths outside Hosp ageg 8 no shield: ", WeekLim_DO80[1], ", ", WeekLim_DO80[2] ))
print(paste0("Week range deaths outside Hosp ageg 9 no shield: ", WeekLim_DO90[1], ", ", WeekLim_DO90[2] ))
print(paste0("Week range deaths outside Hosp ageg 1 shielding: ", WeekLim_DO11[1], ", ", WeekLim_DO11[2] ))
print(paste0("Week range deaths outside Hosp ageg 2 shielding: ", WeekLim_DO21[1], ", ", WeekLim_DO21[2] ))
print(paste0("Week range deaths outside Hosp ageg 3 shielding: ", WeekLim_DO31[1], ", ", WeekLim_DO31[2] ))
print(paste0("Week range deaths outside Hosp ageg 4 shielding: ", WeekLim_DO41[1], ", ", WeekLim_DO41[2] ))
print(paste0("Week range deaths outside Hosp ageg 5 shielding: ", WeekLim_DO51[1], ", ", WeekLim_DO51[2] ))
print(paste0("Week range deaths outside Hosp ageg 6 shielding: ", WeekLim_DO61[1], ", ", WeekLim_DO61[2] ))
print(paste0("Week range deaths outside Hosp ageg 7 shielding: ", WeekLim_DO71[1], ", ", WeekLim_DO71[2] ))
print(paste0("Week range deaths outside Hosp ageg 8 shielding: ", WeekLim_DO81[1], ", ", WeekLim_DO81[2] ))
print(paste0("Week range deaths outside Hosp ageg 9 shielding: ", WeekLim_DO91[1], ", ", WeekLim_DO91[2] ))

cat("\n")
cat("Data points \n")
##H
np_H    = length(datH$DatesH)
np_H1   = length(datH$DatesH[which(datH$ageg==1)])
np_H2   = length(datH$DatesH[which(datH$ageg==2)])
np_H3   = length(datH$DatesH[which(datH$ageg==3)])
np_H4   = length(datH$DatesH[which(datH$ageg==4)])
np_H5   = length(datH$DatesH[which(datH$ageg==5)])
np_H6   = length(datH$DatesH[which(datH$ageg==6)])
np_H7   = length(datH$DatesH[which(datH$ageg==7)])
np_H8   = length(datH$DatesH[which(datH$ageg==8)])
np_H9   = length(datH$DatesH[which(datH$ageg==9)])
np_H10  = length(datH$DatesH[which(datH$ageg==1 & datH$shield=="0")])
np_H20  = length(datH$DatesH[which(datH$ageg==2 & datH$shield=="0")])
np_H30  = length(datH$DatesH[which(datH$ageg==3 & datH$shield=="0")])
np_H40  = length(datH$DatesH[which(datH$ageg==4 & datH$shield=="0")])
np_H50  = length(datH$DatesH[which(datH$ageg==5 & datH$shield=="0")])
np_H60  = length(datH$DatesH[which(datH$ageg==6 & datH$shield=="0")])
np_H70  = length(datH$DatesH[which(datH$ageg==7 & datH$shield=="0")])
np_H80  = length(datH$DatesH[which(datH$ageg==8 & datH$shield=="0")])
np_H90  = length(datH$DatesH[which(datH$ageg==9 & datH$shield=="0")])
np_H11  = length(datH$DatesH[which(datH$ageg==1 & datH$shield=="1")])
np_H21  = length(datH$DatesH[which(datH$ageg==2 & datH$shield=="1")])
np_H31  = length(datH$DatesH[which(datH$ageg==3 & datH$shield=="1")])
np_H41  = length(datH$DatesH[which(datH$ageg==4 & datH$shield=="1")])
np_H51  = length(datH$DatesH[which(datH$ageg==5 & datH$shield=="1")])
np_H61  = length(datH$DatesH[which(datH$ageg==6 & datH$shield=="1")])
np_H71  = length(datH$DatesH[which(datH$ageg==7 & datH$shield=="1")])
np_H81  = length(datH$DatesH[which(datH$ageg==8 & datH$shield=="1")])
np_H91  = length(datH$DatesH[which(datH$ageg==9 & datH$shield=="1")])
##DH
np_DH    = length(datDH$DatesD)
np_DH1   = length(datDH$DatesD[which(datDH$ageg==1)])
np_DH2   = length(datDH$DatesD[which(datDH$ageg==2)])
np_DH3   = length(datDH$DatesD[which(datDH$ageg==3)])
np_DH4   = length(datDH$DatesD[which(datDH$ageg==4)])
np_DH5   = length(datDH$DatesD[which(datDH$ageg==5)])
np_DH6   = length(datDH$DatesD[which(datDH$ageg==6)])
np_DH7   = length(datDH$DatesD[which(datDH$ageg==7)])
np_DH8   = length(datDH$DatesD[which(datDH$ageg==8)])
np_DH9   = length(datDH$DatesD[which(datDH$ageg==9)])
np_DH10  = length(datDH$DatesD[which(datDH$ageg==1 & datDH$shield=="0")])
np_DH20  = length(datDH$DatesD[which(datDH$ageg==2 & datDH$shield=="0")])
np_DH30  = length(datDH$DatesD[which(datDH$ageg==3 & datDH$shield=="0")])
np_DH40  = length(datDH$DatesD[which(datDH$ageg==4 & datDH$shield=="0")])
np_DH50  = length(datDH$DatesD[which(datDH$ageg==5 & datDH$shield=="0")])
np_DH60  = length(datDH$DatesD[which(datDH$ageg==6 & datDH$shield=="0")])
np_DH70  = length(datDH$DatesD[which(datDH$ageg==7 & datDH$shield=="0")])
np_DH80  = length(datDH$DatesD[which(datDH$ageg==8 & datDH$shield=="0")])
np_DH90  = length(datDH$DatesD[which(datDH$ageg==9 & datDH$shield=="0")])
np_DH11  = length(datDH$DatesD[which(datDH$ageg==1 & datDH$shield=="1")])
np_DH21  = length(datDH$DatesD[which(datDH$ageg==2 & datDH$shield=="1")])
np_DH31  = length(datDH$DatesD[which(datDH$ageg==3 & datDH$shield=="1")])
np_DH41  = length(datDH$DatesD[which(datDH$ageg==4 & datDH$shield=="1")])
np_DH51  = length(datDH$DatesD[which(datDH$ageg==5 & datDH$shield=="1")])
np_DH61  = length(datDH$DatesD[which(datDH$ageg==6 & datDH$shield=="1")])
np_DH71  = length(datDH$DatesD[which(datDH$ageg==7 & datDH$shield=="1")])
np_DH81  = length(datDH$DatesD[which(datDH$ageg==8 & datDH$shield=="1")])
np_DH91  = length(datDH$DatesD[which(datDH$ageg==9 & datDH$shield=="1")])
##DO
np_DO    = length(datDO$DatesD)
np_DO1   = length(datDO$DatesD[which(datDO$ageg==1)])
np_DO2   = length(datDO$DatesD[which(datDO$ageg==2)])
np_DO3   = length(datDO$DatesD[which(datDO$ageg==3)])
np_DO4   = length(datDO$DatesD[which(datDO$ageg==4)])
np_DO5   = length(datDO$DatesD[which(datDO$ageg==5)])
np_DO6   = length(datDO$DatesD[which(datDO$ageg==6)])
np_DO7   = length(datDO$DatesD[which(datDO$ageg==7)])
np_DO8   = length(datDO$DatesD[which(datDO$ageg==8)])
np_DO9   = length(datDO$DatesD[which(datDO$ageg==9)])
np_DO10  = length(datDO$DatesD[which(datDO$ageg==1 & datDO$shield=="0")])
np_DO20  = length(datDO$DatesD[which(datDO$ageg==2 & datDO$shield=="0")])
np_DO30  = length(datDO$DatesD[which(datDO$ageg==3 & datDO$shield=="0")])
np_DO40  = length(datDO$DatesD[which(datDO$ageg==4 & datDO$shield=="0")])
np_DO50  = length(datDO$DatesD[which(datDO$ageg==5 & datDO$shield=="0")])
np_DO60  = length(datDO$DatesD[which(datDO$ageg==6 & datDO$shield=="0")])
np_DO70  = length(datDO$DatesD[which(datDO$ageg==7 & datDO$shield=="0")])
np_DO80  = length(datDO$DatesD[which(datDO$ageg==8 & datDO$shield=="0")])
np_DO90  = length(datDO$DatesD[which(datDO$ageg==9 & datDO$shield=="0")])
np_DO11  = length(datDO$DatesD[which(datDO$ageg==1 & datDO$shield=="1")])
np_DO21  = length(datDO$DatesD[which(datDO$ageg==2 & datDO$shield=="1")])
np_DO31  = length(datDO$DatesD[which(datDO$ageg==3 & datDO$shield=="1")])
np_DO41  = length(datDO$DatesD[which(datDO$ageg==4 & datDO$shield=="1")])
np_DO51  = length(datDO$DatesD[which(datDO$ageg==5 & datDO$shield=="1")])
np_DO61  = length(datDO$DatesD[which(datDO$ageg==6 & datDO$shield=="1")])
np_DO71  = length(datDO$DatesD[which(datDO$ageg==7 & datDO$shield=="1")])
np_DO81  = length(datDO$DatesD[which(datDO$ageg==8 & datDO$shield=="1")])
np_DO91  = length(datDO$DatesD[which(datDO$ageg==9 & datDO$shield=="1")])

cat("\n")
##H
print(paste0("H data"))
print(paste0("Data points Hosp:                  ", np_H[1],   ", ", np_H[2] ))
print(paste0("Data points Hosp ageg 1:           ", np_H1[1],  ", ", np_H1[2] ))
print(paste0("Data points Hosp ageg 2:           ", np_H2[1],  ", ", np_H2[2] ))
print(paste0("Data points Hosp ageg 3:           ", np_H3[1],  ", ", np_H3[2] ))
print(paste0("Data points Hosp ageg 4:           ", np_H4[1],  ", ", np_H4[2] ))
print(paste0("Data points Hosp ageg 5:           ", np_H5[1],  ", ", np_H5[2] ))
print(paste0("Data points Hosp ageg 6:           ", np_H6[1],  ", ", np_H6[2] ))
print(paste0("Data points Hosp ageg 7:           ", np_H7[1],  ", ", np_H7[2] ))
print(paste0("Data points Hosp ageg 8:           ", np_H8[1],  ", ", np_H8[2] ))
print(paste0("Data points Hosp ageg 9:           ", np_H9[1],  ", ", np_H9[2] ))
print(paste0("Data points Hosp ageg 1 no shield: ", np_H10[1], ", ", np_H10[2] ))
print(paste0("Data points Hosp ageg 2 no shield: ", np_H20[1], ", ", np_H20[2] ))
print(paste0("Data points Hosp ageg 3 no shield: ", np_H30[1], ", ", np_H30[2] ))
print(paste0("Data points Hosp ageg 4 no shield: ", np_H40[1], ", ", np_H40[2] ))
print(paste0("Data points Hosp ageg 5 no shield: ", np_H50[1], ", ", np_H50[2] ))
print(paste0("Data points Hosp ageg 6 no shield: ", np_H60[1], ", ", np_H60[2] ))
print(paste0("Data points Hosp ageg 7 no shield: ", np_H70[1], ", ", np_H70[2] ))
print(paste0("Data points Hosp ageg 8 no shield: ", np_H80[1], ", ", np_H80[2] ))
print(paste0("Data points Hosp ageg 9 no shield: ", np_H90[1], ", ", np_H90[2] ))
print(paste0("Data points Hosp ageg 1 shielding: ", np_H11[1], ", ", np_H11[2] ))
print(paste0("Data points Hosp ageg 2 shielding: ", np_H21[1], ", ", np_H21[2] ))
print(paste0("Data points Hosp ageg 3 shielding: ", np_H31[1], ", ", np_H31[2] ))
print(paste0("Data points Hosp ageg 4 shielding: ", np_H41[1], ", ", np_H41[2] ))
print(paste0("Data points Hosp ageg 5 shielding: ", np_H51[1], ", ", np_H51[2] ))
print(paste0("Data points Hosp ageg 6 shielding: ", np_H61[1], ", ", np_H61[2] ))
print(paste0("Data points Hosp ageg 7 shielding: ", np_H71[1], ", ", np_H71[2] ))
print(paste0("Data points Hosp ageg 8 shielding: ", np_H81[1], ", ", np_H81[2] ))
print(paste0("Data points Hosp ageg 9 shielding: ", np_H91[1], ", ", np_H91[2] ))

##DH
print(paste0("DH data"))
print(paste0("Data points deaths in Hosp:                  ", np_DH[1],   ", ", np_DH[2] ))
print(paste0("Data points deaths in Hosp ageg 1:           ", np_DH1[1],  ", ", np_DH1[2] ))
print(paste0("Data points deaths in Hosp ageg 2:           ", np_DH2[1],  ", ", np_DH2[2] ))
print(paste0("Data points deaths in Hosp ageg 3:           ", np_DH3[1],  ", ", np_DH3[2] ))
print(paste0("Data points deaths in Hosp ageg 4:           ", np_DH4[1],  ", ", np_DH4[2] ))
print(paste0("Data points deaths in Hosp ageg 5:           ", np_DH5[1],  ", ", np_DH5[2] ))
print(paste0("Data points deaths in Hosp ageg 6:           ", np_DH6[1],  ", ", np_DH6[2] ))
print(paste0("Data points deaths in Hosp ageg 7:           ", np_DH7[1],  ", ", np_DH7[2] ))
print(paste0("Data points deaths in Hosp ageg 8:           ", np_DH8[1],  ", ", np_DH8[2] ))
print(paste0("Data points deaths in Hosp ageg 9:           ", np_DH9[1],  ", ", np_DH9[2] ))
print(paste0("Data points deaths in Hosp ageg 1 no shield: ", np_DH10[1], ", ", np_DH10[2] ))
print(paste0("Data points deaths in Hosp ageg 2 no shield: ", np_DH20[1], ", ", np_DH20[2] ))
print(paste0("Data points deaths in Hosp ageg 3 no shield: ", np_DH30[1], ", ", np_DH30[2] ))
print(paste0("Data points deaths in Hosp ageg 4 no shield: ", np_DH40[1], ", ", np_DH40[2] ))
print(paste0("Data points deaths in Hosp ageg 5 no shield: ", np_DH50[1], ", ", np_DH50[2] ))
print(paste0("Data points deaths in Hosp ageg 6 no shield: ", np_DH60[1], ", ", np_DH60[2] ))
print(paste0("Data points deaths in Hosp ageg 7 no shield: ", np_DH70[1], ", ", np_DH70[2] ))
print(paste0("Data points deaths in Hosp ageg 8 no shield: ", np_DH80[1], ", ", np_DH80[2] ))
print(paste0("Data points deaths in Hosp ageg 9 no shield: ", np_DH90[1], ", ", np_DH90[2] ))
print(paste0("Data points deaths in Hosp ageg 1 shielding: ", np_DH11[1], ", ", np_DH11[2] ))
print(paste0("Data points deaths in Hosp ageg 2 shielding: ", np_DH21[1], ", ", np_DH21[2] ))
print(paste0("Data points deaths in Hosp ageg 3 shielding: ", np_DH31[1], ", ", np_DH31[2] ))
print(paste0("Data points deaths in Hosp ageg 4 shielding: ", np_DH41[1], ", ", np_DH41[2] ))
print(paste0("Data points deaths in Hosp ageg 5 shielding: ", np_DH51[1], ", ", np_DH51[2] ))
print(paste0("Data points deaths in Hosp ageg 6 shielding: ", np_DH61[1], ", ", np_DH61[2] ))
print(paste0("Data points deaths in Hosp ageg 7 shielding: ", np_DH71[1], ", ", np_DH71[2] ))
print(paste0("Data points deaths in Hosp ageg 8 shielding: ", np_DH81[1], ", ", np_DH81[2] ))
print(paste0("Data points deaths in Hosp ageg 9 shielding: ", np_DH91[1], ", ", np_DH91[2] ))

##DO
print(paste0("DO data"))
print(paste0("Data points deaths outside Hosp:                  ", np_DO[1],   ", ", np_DO[2] ))
print(paste0("Data points deaths outside Hosp ageg 1:           ", np_DO1[1],  ", ", np_DO1[2] ))
print(paste0("Data points deaths outside Hosp ageg 2:           ", np_DO2[1],  ", ", np_DO2[2] ))
print(paste0("Data points deaths outside Hosp ageg 3:           ", np_DO3[1],  ", ", np_DO3[2] ))
print(paste0("Data points deaths outside Hosp ageg 4:           ", np_DO4[1],  ", ", np_DO4[2] ))
print(paste0("Data points deaths outside Hosp ageg 5:           ", np_DO5[1],  ", ", np_DO5[2] ))
print(paste0("Data points deaths outside Hosp ageg 6:           ", np_DO6[1],  ", ", np_DO6[2] ))
print(paste0("Data points deaths outside Hosp ageg 7:           ", np_DO7[1],  ", ", np_DO7[2] ))
print(paste0("Data points deaths outside Hosp ageg 8:           ", np_DO8[1],  ", ", np_DO8[2] ))
print(paste0("Data points deaths outside Hosp ageg 9:           ", np_DO9[1],  ", ", np_DO9[2] ))
print(paste0("Data points deaths outside Hosp ageg 1 no shield: ", np_DO10[1], ", ", np_DO10[2] ))
print(paste0("Data points deaths outside Hosp ageg 2 no shield: ", np_DO20[1], ", ", np_DO20[2] ))
print(paste0("Data points deaths outside Hosp ageg 3 no shield: ", np_DO30[1], ", ", np_DO30[2] ))
print(paste0("Data points deaths outside Hosp ageg 4 no shield: ", np_DO40[1], ", ", np_DO40[2] ))
print(paste0("Data points deaths outside Hosp ageg 5 no shield: ", np_DO50[1], ", ", np_DO50[2] ))
print(paste0("Data points deaths outside Hosp ageg 6 no shield: ", np_DO60[1], ", ", np_DO60[2] ))
print(paste0("Data points deaths outside Hosp ageg 7 no shield: ", np_DO70[1], ", ", np_DO70[2] ))
print(paste0("Data points deaths outside Hosp ageg 8 no shield: ", np_DO80[1], ", ", np_DO80[2] ))
print(paste0("Data points deaths outside Hosp ageg 9 no shield: ", np_DO90[1], ", ", np_DO90[2] ))
print(paste0("Data points deaths outside Hosp ageg 1 shielding: ", np_DO11[1], ", ", np_DO11[2] ))
print(paste0("Data points deaths outside Hosp ageg 2 shielding: ", np_DO21[1], ", ", np_DO21[2] ))
print(paste0("Data points deaths outside Hosp ageg 3 shielding: ", np_DO31[1], ", ", np_DO31[2] ))
print(paste0("Data points deaths outside Hosp ageg 4 shielding: ", np_DO41[1], ", ", np_DO41[2] ))
print(paste0("Data points deaths outside Hosp ageg 5 shielding: ", np_DO51[1], ", ", np_DO51[2] ))
print(paste0("Data points deaths outside Hosp ageg 6 shielding: ", np_DO61[1], ", ", np_DO61[2] ))
print(paste0("Data points deaths outside Hosp ageg 7 shielding: ", np_DO71[1], ", ", np_DO71[2] ))
print(paste0("Data points deaths outside Hosp ageg 8 shielding: ", np_DO81[1], ", ", np_DO81[2] ))
print(paste0("Data points deaths outside Hosp ageg 9 shielding: ", np_DO91[1], ", ", np_DO91[2] ))

#Patients
cat("\n")
cat("Patients \n")
nP        = sum(!is.na(DAT$ageg), na.rm = T)
nP_HorD   = sum(!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0)
nP_noHorD = sum( is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) )
print(paste0("Patient entries:     ", nP ))
print(paste0("Patients with hosp or death events:  ", nP_HorD,   ", %all patients: ", pc2(nP_HorD/nP) ))
print(paste0("Patients wout hosp or death events:  ", nP_noHorD, ", %all patients: ", pc2(nP_noHorD/nP) ))
cat("\n")
cat("Patients by age \n")
nP_00 = sum(DAT$ageg==1, na.rm = T)
nP_05 = sum(DAT$ageg==2, na.rm = T)
nP_12 = sum(DAT$ageg==3, na.rm = T)
nP_18 = sum(DAT$ageg==4, na.rm = T)
nP_30 = sum(DAT$ageg==5, na.rm = T)
nP_40 = sum(DAT$ageg==6, na.rm = T)
nP_50 = sum(DAT$ageg==7, na.rm = T)
nP_60 = sum(DAT$ageg==8, na.rm = T)
nP_70 = sum(DAT$ageg==9, na.rm = T)
nP_sum = (nP_00+nP_05+nP_12+nP_18+nP_30+nP_40+nP_50+nP_60+nP_70)
print(paste0("Patient % age 0-4:   ", pc2(nP_00/nP) )); 
print(paste0("Patient % age 5-11:  ", pc2(nP_05/nP) ));
print(paste0("Patient % age 12-17: ", pc2(nP_12/nP) ));
print(paste0("Patient % age 18-29: ", pc2(nP_18/nP) ));
print(paste0("Patient % age 30-39: ", pc2(nP_30/nP) ));
print(paste0("Patient % age 40-49: ", pc2(nP_40/nP) ));
print(paste0("Patient % age 50-59: ", pc2(nP_50/nP) ));
print(paste0("Patient % age 60-69: ", pc2(nP_60/nP) ));
print(paste0("Patient % age 70+:   ", pc2(nP_70/nP) ));
print(paste0("Patient % all:       ", pc2(nP_sum/nP) ));

#Hospitalisations
cat("\n")
cat("Hospitalised patients/Hospitalisations (1st admission) \n")
nP_Hosp     = length( which(!is.na(DAT$weekH) | DAT$all_covid_hosp>0) )
print(paste0("Ever hospitalised (>=once):  ", nP_Hosp, ", or ", nH)) #nH = sum(dat$freqHas,na.rm=T)

cat("\n")
cat("Hospitalised patients by age \n") 
nP_Hosp_00  = length( which(DAT$all_covid_hosp>0 & DAT$ageg==1) ) #NB: 'admitted' matters here, no.admissions doens't
nP_Hosp_05  = length( which(DAT$all_covid_hosp>0 & DAT$ageg==2) )
nP_Hosp_12  = length( which(DAT$all_covid_hosp>0 & DAT$ageg==3) )
nP_Hosp_18  = length( which(DAT$all_covid_hosp>0 & DAT$ageg==4) )
nP_Hosp_30  = length( which(DAT$all_covid_hosp>0 & DAT$ageg==5) )
nP_Hosp_40  = length( which(DAT$all_covid_hosp>0 & DAT$ageg==6) )
nP_Hosp_50  = length( which(DAT$all_covid_hosp>0 & DAT$ageg==7) )
nP_Hosp_60  = length( which(DAT$all_covid_hosp>0 & DAT$ageg==8) )
nP_Hosp_70  = length( which(DAT$all_covid_hosp>0 & DAT$ageg==9) )
nP_Hosp_sum = nP_Hosp_00+nP_Hosp_05+nP_Hosp_12+nP_Hosp_18+nP_Hosp_30+nP_Hosp_40+nP_Hosp_50+nP_Hosp_60+nP_Hosp_70
print(paste0("Patients hospitalised age 0-4:   ", tr(nP_Hosp_00), ", %patients hosp: ", pc2(nP_Hosp_00/nP_Hosp) ))
print(paste0("Patients hospitalised age 5-11:  ", tr(nP_Hosp_05), ", %patients hosp: ", pc2(nP_Hosp_05/nP_Hosp) ))
print(paste0("Patients hospitalised age 12-17: ", tr(nP_Hosp_12), ", %patients hosp: ", pc2(nP_Hosp_12/nP_Hosp) ))
print(paste0("Patients hospitalised age 18-29: ", tr(nP_Hosp_18), ", %patients hosp: ", pc2(nP_Hosp_18/nP_Hosp) ))
print(paste0("Patients hospitalised age 30-39: ", tr(nP_Hosp_30), ", %patients hosp: ", pc2(nP_Hosp_30/nP_Hosp) ))
print(paste0("Patients hospitalised age 40-49: ", tr(nP_Hosp_40), ", %patients hosp: ", pc2(nP_Hosp_40/nP_Hosp) ))
print(paste0("Patients hospitalised age 50-59: ", tr(nP_Hosp_50), ", %patients hosp: ", pc2(nP_Hosp_50/nP_Hosp) ))
print(paste0("Patients hospitalised age 60-69: ", tr(nP_Hosp_60), ", %patients hosp: ", pc2(nP_Hosp_60/nP_Hosp) ))
print(paste0("Patients hospitalised age 70+:   ", tr(nP_Hosp_70), ", %patients hosp: ", pc2(nP_Hosp_70/nP_Hosp) ))
print(paste0("Patients hospitalised total %:   ", pc2(nP_Hosp_sum/nP_Hosp) ))


#Deaths
cat("\n")
cat("Deaths \n")
nP_D   = sum(!is.na(DAT$ons_death_date))
print(paste0("Patients that died:        ", nP_D))

cat("\n")
cat("Deaths in hospital \n")
nP_DH  = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0, na.rm = T)
nP_RH  = nP_Hosp - nP_DH
print(paste0("Patients died in hospital: ", nP_DH,     ", mfraction     ", rd3(nP_DH/nP_Hosp), ", %all deaths: ", pc2(nP_DH/nP_D) ))
print(paste0("Patients recovered:        ", nP_RH,     ", 1-mfraction   ", rd3(nP_RH/nP_Hosp) ))

cat("\n")
cat("Deaths in hospital by age \n")
nP_DH_00 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==1, na.rm = T)
nP_DH_05 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==2, na.rm = T)
nP_DH_12 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==3, na.rm = T)
nP_DH_18 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==4, na.rm = T)
nP_DH_30 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==5, na.rm = T)
nP_DH_40 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==6, na.rm = T)
nP_DH_50 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==7, na.rm = T)
nP_DH_60 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==8, na.rm = T)
nP_DH_70 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==9, na.rm = T)
nP_DH_sum = (nP_DH_00+nP_DH_05+nP_DH_12+nP_DH_18+nP_DH_30+nP_DH_40+nP_DH_50+nP_DH_60+nP_DH_70)
print(paste0("Patients died in hopital age 0-4:   ", tr(nP_DH_00), ", %hosp deaths: ", pc2(nP_DH_00/nP_DH) ))
print(paste0("Patients died in hopital age 5-11:  ", tr(nP_DH_05), ", %hosp deaths: ", pc2(nP_DH_05/nP_DH) ))
print(paste0("Patients died in hopital age 12-17: ", tr(nP_DH_12), ", %hosp deaths: ", pc2(nP_DH_12/nP_DH) ))
print(paste0("Patients died in hopital age 18-29: ", tr(nP_DH_18), ", %hosp deaths: ", pc2(nP_DH_18/nP_DH) ))
print(paste0("Patients died in hopital age 30-39: ", tr(nP_DH_30), ", %hosp deaths: ", pc2(nP_DH_30/nP_DH) ))
print(paste0("Patients died in hopital age 40-49: ", tr(nP_DH_40), ", %hosp deaths: ", pc2(nP_DH_40/nP_DH) ))
print(paste0("Patients died in hopital age 50-59: ", tr(nP_DH_50), ", %hosp deaths: ", pc2(nP_DH_50/nP_DH) ))
print(paste0("Patients died in hopital age 60-69: ", tr(nP_DH_60), ", %hosp deaths: ", pc2(nP_DH_60/nP_DH) ))
print(paste0("Patients died in hopital age 70+:   ", tr(nP_DH_70), ", %hosp deaths: ", pc2(nP_DH_70/nP_DH) ))
print(paste0("Patients died in hopital total:     ", nP_DH_sum,    ", %hosp deaths: ", pc2(nP_DH_sum/nP_DH) ))
cat("Mortality fraction \n")
mfraction_00 = rd3(nP_DH_00/nP_Hosp_00)
mfraction_05 = rd3(nP_DH_05/nP_Hosp_05)
mfraction_12 = rd3(nP_DH_12/nP_Hosp_12)
mfraction_18 = rd3(nP_DH_18/nP_Hosp_18)
mfraction_30 = rd3(nP_DH_30/nP_Hosp_30)
mfraction_40 = rd3(nP_DH_40/nP_Hosp_40)
mfraction_50 = rd3(nP_DH_50/nP_Hosp_50)
mfraction_60 = rd3(nP_DH_60/nP_Hosp_60)
mfraction_70 = rd3(nP_DH_70/nP_Hosp_70)
print(paste0("Hopital mfraction age 0-4:     ", mfraction_00 ))
print(paste0("Hopital mfraction age 5-11:    ", mfraction_05 ))
print(paste0("Hopital mfraction age 12-17:   ", mfraction_12 ))
print(paste0("Hopital mfraction age 18-29:   ", mfraction_18 ))
print(paste0("Hopital mfraction age 30-39:   ", mfraction_30 ))
print(paste0("Hopital mfraction age 40-49:   ", mfraction_40 ))
print(paste0("Hopital mfraction age 50-59:   ", mfraction_50 ))
print(paste0("Hopital mfraction age 60-69:   ", mfraction_60 ))
print(paste0("Hopital mfraction age 70+:     ", mfraction_70 ))

cat("\n")
cat("Deaths outside hospital \n")
nP_DO = sum(!is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)), na.rm = T)
print(paste0("Patients died outside hosp:   ", nP_DO, ", %all deaths:   ", pc2(nP_DO/nP_D) ))

cat("\n")
cat("Deaths in hospital - average time to death since 1st admission (the admission_date) \n")
cat("                   - assumption: merging time in any subsequent admissions \n")
###Across all ages - as the mortality fraction is age specific
men_time_to_death = rd3(as.numeric(   mean(DAT$ons_death_date[which(DAT$all_covid_hosp>0)] - DAT$admission_date[which(DAT$all_covid_hosp>0)], na.rm =T)) )
med_time_to_death = rd3(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp>0)] - DAT$admission_date[which(DAT$all_covid_hosp>0)], na.rm =T)) )
print(paste0("Mean (median) time to death since 1st admission: ", men_time_to_death, " (", med_time_to_death, ")" ))

cat("\n")
cat("Recovery in hospital - average time to 1st discharge \n") 
cat("                     - assumption: discarding time in subsequent admissions \n")
###Across all ages - as the fraction of mortality is age specific
men_time_to_recover  = rd3(as.numeric(   mean(DAT$discharge_date  - DAT$admission_date, na.rm =T)) )
med_time_to_recover  = rd3(as.numeric( median(DAT$discharge_date  - DAT$admission_date, na.rm =T)) )
print(paste0("Mean (median) time from 1st admission to 1st discharge:  ", men_time_to_recover, " (", med_time_to_recover, ")" ))


#Shielding
cat("\n")
cat("Shielding/Not \n")
##TODO: use shield or shield1
nP_s1        = sum(DAT$shield=="1", na.rm = T)
nP_s0        = sum(DAT$shield=="0", na.rm = T)
nP_HorD_s1   = sum(DAT$shield=="1" & (!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0))
nP_HorD_s0   = sum(DAT$shield=="0" & (!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0))
nP_noHorD_s1 = sum(DAT$shield=="1" &  (is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) ))
nP_noHorD_s0 = sum(DAT$shield=="0" &  (is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) ))
print(paste0("Patients shielding:                         ", nP_s1,        ", %patients:     ", pc2(nP_s1/nP) ))
print(paste0("Patients not shield:                        ", nP_s0,        ", %patients:     ", pc2(nP_s0/nP) ))
print(paste0("Patients shielding with hosp/death events:  ", nP_HorD_s1,   ", %sh patients:  ", pc2(nP_HorD_s1/nP_s1) ))
print(paste0("Patients shielding wout hosp/death events:  ", nP_noHorD_s1, ", %sh patients:  ", pc2(nP_noHorD_s1/nP_s1) ))
print(paste0("Patients not shield with hosp/death events: ", nP_HorD_s0,   ", %nsh patients: ", pc2(nP_HorD_s0/nP_s0) ))
print(paste0("Patients not shield wout hosp/death events: ", nP_noHorD_s0, ", %nsh patients: ", pc2(nP_noHorD_s0/nP_s0) ))

cat("\n")
cat("Shielding by age \n")
nP_00_s1 = sum(DAT$shield=="1" & DAT$ageg==1, na.rm = T)
nP_05_s1 = sum(DAT$shield=="1" & DAT$ageg==2, na.rm = T)
nP_12_s1 = sum(DAT$shield=="1" & DAT$ageg==3, na.rm = T)
nP_18_s1 = sum(DAT$shield=="1" & DAT$ageg==4, na.rm = T)
nP_30_s1 = sum(DAT$shield=="1" & DAT$ageg==5, na.rm = T)
nP_40_s1 = sum(DAT$shield=="1" & DAT$ageg==6, na.rm = T)
nP_50_s1 = sum(DAT$shield=="1" & DAT$ageg==7, na.rm = T)
nP_60_s1 = sum(DAT$shield=="1" & DAT$ageg==8, na.rm = T)
nP_70_s1 = sum(DAT$shield=="1" & DAT$ageg==9, na.rm = T)
nP_sum_s1 = nP_00_s1 + nP_05_s1 + nP_12_s1 + nP_18_s1 + nP_30_s1 + nP_40_s1 + nP_50_s1 + nP_60_s1 + nP_70_s1
print(paste0("Patient age 0-4   % shielding: ", pc2(nP_00_s1/nP_00) ))
print(paste0("Patient age 5-11  % shielding: ", pc2(nP_05_s1/nP_05) ))
print(paste0("Patient age 12-17 % shielding: ", pc2(nP_12_s1/nP_12) ))
print(paste0("Patient age 18-29 % shielding: ", pc2(nP_18_s1/nP_18) ))
print(paste0("Patient age 30-39 % shielding: ", pc2(nP_30_s1/nP_30) ))
print(paste0("Patient age 40-49 % shielding: ", pc2(nP_40_s1/nP_40) ))
print(paste0("Patient age 50-59 % shielding: ", pc2(nP_50_s1/nP_50) ))
print(paste0("Patient age 60-69 % shielding: ", pc2(nP_60_s1/nP_60) ))
print(paste0("Patient age 70+   % shielding: ", pc2(nP_70_s1/nP_70) ))
cat("\n")
print(paste0("Patient shielding % age 0-4:   ", pc2(nP_00_s1/nP_s1) ))
print(paste0("Patient shielding % age 5-11:  ", pc2(nP_05_s1/nP_s1) ))
print(paste0("Patient shielding % age 12-17: ", pc2(nP_12_s1/nP_s1) ))
print(paste0("Patient shielding % age 18-29: ", pc2(nP_18_s1/nP_s1) ))
print(paste0("Patient shielding % age 30-39: ", pc2(nP_30_s1/nP_s1) ))
print(paste0("Patient shielding % age 40-49: ", pc2(nP_40_s1/nP_s1) ))
print(paste0("Patient shielding % age 50-59: ", pc2(nP_50_s1/nP_s1) ))
print(paste0("Patient shielding % age 60-69: ", pc2(nP_60_s1/nP_s1) ))
print(paste0("Patient shielding % age 70+:   ", pc2(nP_70_s1/nP_s1) ))
print(paste0("Patient shielding % all ages:  ", pc2(nP_sum_s1/nP_s1) ))

cat("\n")
cat("Shielding/Not patients in hospital \n")
nP_Hosp_s1 = length( which(DAT$all_covid_hosp>0 & DAT$shield=="1"))
nP_Hosp_s0 = nP_Hosp - nP_Hosp_s1
nP_RH_s1   = length( which( is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shield=="1"))
nP_RH_s0   = nP_RH - nP_RH_s1
nP_DH_s1   = length( which(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shield=="1"))
nP_DH_s0   = nP_DH - nP_DH_s1
print(paste0("Shielding patients in hospital:             ", nP_Hosp_s1, ", %patients hosp:   ", pc2(nP_Hosp_s1/nP_Hosp) ))
print(paste0("Shielding patients in hosp recovered:       ", nP_RH_s1,   ", %sh patnts hosp:  ", pc2(nP_RH_s1/nP_Hosp_s1) ))
print(paste0("Shielding patients in hospital died:        ", nP_DH_s1,   ", %sh patnts hosp:  ", pc2(nP_DH_s1/nP_Hosp_s1) ))
print(paste0("Non-shield patients in hospital:            ", nP_Hosp_s0, ", %patients hosp:   ", pc2(nP_Hosp_s0/nP_Hosp) ))
print(paste0("Non-shield patients in hosp recovered:      ", nP_RH_s0,   ", %nsh patnts hosp: ", pc2(nP_RH_s0/nP_Hosp_s0) ))
print(paste0("Non-shield patients in hospital died:       ", nP_DH_s0,   ", %nsh patnts hosp: ", pc2(nP_DH_s0/nP_Hosp_s0) ))

cat("\n")
cat("Shielding/Not patients in hospital by age\n")
nP_Hosp_00_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$ageg==1) )
nP_Hosp_05_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$ageg==2) )
nP_Hosp_12_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$ageg==3) )
nP_Hosp_18_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$ageg==4) )
nP_Hosp_30_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$ageg==5) )
nP_Hosp_40_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$ageg==6) )
nP_Hosp_50_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$ageg==7) )
nP_Hosp_60_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$ageg==8) )
nP_Hosp_70_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$ageg==9) )
nP_Hosp_00_s0 = nP_Hosp_00 - nP_Hosp_00_s1
nP_Hosp_05_s0 = nP_Hosp_05 - nP_Hosp_05_s1
nP_Hosp_12_s0 = nP_Hosp_12 - nP_Hosp_12_s1
nP_Hosp_18_s0 = nP_Hosp_18 - nP_Hosp_18_s1
nP_Hosp_30_s0 = nP_Hosp_30 - nP_Hosp_30_s1
nP_Hosp_40_s0 = nP_Hosp_40 - nP_Hosp_40_s1
nP_Hosp_50_s0 = nP_Hosp_50 - nP_Hosp_50_s1
nP_Hosp_60_s0 = nP_Hosp_60 - nP_Hosp_60_s1
nP_Hosp_70_s0 = nP_Hosp_70 - nP_Hosp_70_s1

cat("\n")
cat("Shielding/Not deaths outside hospital \n")
nP_DO_s1 = length( which(!is.na(DAT$ons_death_date) 
           & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) & DAT$shield=="1" ))
nP_DO_s0 = nP_DO - nP_DO_s1
print(paste0("Shielding patients died outside hospital:   ", nP_DO_s1,  ", %deaths out hosp: ", pc2(nP_DO_s1/nP_DO) ))
print(paste0("Non-shield patients died outside hospital:  ", nP_DO_s0,  ", %deaths out hosp: ", pc2(nP_DO_s0/nP_DO) ))

cat("\n")
cat("Shielding/Not deaths in hospital by age \n")
nP_DH_00_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==1 ))
nP_DH_05_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==2 ))
nP_DH_12_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==3 ))
nP_DH_18_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==4 ))
nP_DH_30_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==5 ))
nP_DH_40_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==6 ))
nP_DH_50_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==7 ))
nP_DH_60_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==8 ))
nP_DH_70_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$ageg==9 ))
nP_DH_00_s0   = nP_DH_00 - nP_DH_00_s1
nP_DH_05_s0   = nP_DH_05 - nP_DH_05_s1
nP_DH_12_s0   = nP_DH_12 - nP_DH_12_s1
nP_DH_18_s0   = nP_DH_18 - nP_DH_18_s1
nP_DH_30_s0   = nP_DH_30 - nP_DH_30_s1
nP_DH_40_s0   = nP_DH_40 - nP_DH_40_s1
nP_DH_50_s0   = nP_DH_50 - nP_DH_50_s1
nP_DH_60_s0   = nP_DH_60 - nP_DH_60_s1
nP_DH_70_s0   = nP_DH_70 - nP_DH_70_s1

cat("\n")
cat("Shielding/Not deaths in hospital - mortality fraction by age \n")
mfraction_00_s1 = rd3(nP_DH_00_s1/nP_Hosp_00_s1)
mfraction_05_s1 = rd3(nP_DH_05_s1/nP_Hosp_05_s1)
mfraction_12_s1 = rd3(nP_DH_12_s1/nP_Hosp_12_s1)
mfraction_18_s1 = rd3(nP_DH_18_s1/nP_Hosp_18_s1)
mfraction_30_s1 = rd3(nP_DH_30_s1/nP_Hosp_30_s1)
mfraction_40_s1 = rd3(nP_DH_40_s1/nP_Hosp_40_s1)
mfraction_50_s1 = rd3(nP_DH_50_s1/nP_Hosp_50_s1)
mfraction_60_s1 = rd3(nP_DH_60_s1/nP_Hosp_60_s1)
mfraction_70_s1 = rd3(nP_DH_70_s1/nP_Hosp_70_s1)
#
mfraction_00_s0 = rd3(nP_DH_00_s0/nP_Hosp_00_s0)
mfraction_05_s0 = rd3(nP_DH_05_s0/nP_Hosp_05_s0)
mfraction_12_s0 = rd3(nP_DH_12_s0/nP_Hosp_12_s0)
mfraction_18_s0 = rd3(nP_DH_18_s0/nP_Hosp_18_s0)
mfraction_30_s0 = rd3(nP_DH_30_s0/nP_Hosp_30_s0)
mfraction_40_s0 = rd3(nP_DH_40_s0/nP_Hosp_40_s0)
mfraction_50_s0 = rd3(nP_DH_50_s0/nP_Hosp_50_s0)
mfraction_60_s0 = rd3(nP_DH_60_s0/nP_Hosp_60_s0)
mfraction_70_s0 = rd3(nP_DH_70_s0/nP_Hosp_70_s0)
#
print(paste0("Shiedling hosp mfraction age 0-4:    ", mfraction_00_s1 ))
print(paste0("Shiedling hosp mfraction age 5-11:   ", mfraction_05_s1 ))
print(paste0("Shiedling hosp mfraction age 12-17:  ", mfraction_12_s1 ))
print(paste0("Shiedling hosp mfraction age 18-29:  ", mfraction_18_s1 ))
print(paste0("Shiedling hosp mfraction age 30-39:  ", mfraction_30_s1 ))
print(paste0("Shiedling hosp mfraction age 40-49:  ", mfraction_40_s1 ))
print(paste0("Shiedling hosp mfraction age 50-59:  ", mfraction_50_s1 ))
print(paste0("Shiedling hosp mfraction age 60-69:  ", mfraction_60_s1 ))
print(paste0("Shiedling hosp mfraction age 70+:    ", mfraction_70_s1 ))
cat("\n")
print(paste0("Non-shield hosp mfraction age 0-4:   ", mfraction_00_s0 ))
print(paste0("Non-shield hosp mfraction age 5-11:  ", mfraction_05_s0 ))
print(paste0("Non-shield hosp mfraction age 12-17: ", mfraction_12_s0 ))
print(paste0("Non-shield hosp mfraction age 18-29: ", mfraction_18_s0 ))
print(paste0("Non-shield hosp mfraction age 30-39: ", mfraction_30_s0 ))
print(paste0("Non-shield hosp mfraction age 40-49: ", mfraction_40_s0 ))
print(paste0("Non-shield hosp mfraction age 50-59: ", mfraction_50_s0 ))
print(paste0("Non-shield hosp mfraction age 60-69: ", mfraction_60_s0 ))
print(paste0("Non-shield hosp mfraction age 70+:   ", mfraction_70_s0 ))

cat("\n")

print(paste0("H start date: ", StartDateH))
print(paste0("D start date: ", StartDateD))

print(paste0("names1"))
print(paste0(names1))

print(paste0("names2"))
print(paste0(names2))

sink()



