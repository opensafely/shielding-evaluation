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

jobno = "JDat5_"

######## Functions #############################################################
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
  ###Refer discharge to last discharge 
  mutate(discharge_date2 = covid_hosp_discharge_1)         %>%
  mutate(discharge_date2 = ifelse(!is.na(covid_hosp_discharge_2),covid_hosp_discharge_2,covid_hosp_discharge_1) ) %>%
  mutate(discharge_date2 = ifelse(!is.na(covid_hosp_discharge_3),covid_hosp_discharge_3,discharge_date2) ) %>%
  mutate(discharge_date2 = ifelse(!is.na(covid_hosp_discharge_4),covid_hosp_discharge_4,discharge_date2) ) %>%
  mutate(discharge_date2 = ifelse(!is.na(covid_hosp_discharge_5),covid_hosp_discharge_5,discharge_date2) ) %>%
  mutate(discharge_date2 = ifelse(!is.na(covid_hosp_discharge_6),covid_hosp_discharge_6,discharge_date2) ) %>%
  #mutate(discharge_date2 = as.Date(discharge_date2, format = "%Y-%m-%d")) %>%
  ###Vars replaced by numeric/factor flags
  select(-c(dplyr::contains("hirisk"), shielding, 
            shielding_v1_binary, shielding_v1_startdate))    %>%   # no longer need 
  ###For now, drop thes
  select(-c(shield1,shield1_date))

names1 = names(DAT)
print(paste0("names1"))
print(paste0(names1))


### Aggregation
#H D week, year, date-by-week variables
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
pdf(file = paste0(output_dir,"/",jobno,"HDdata_H,D_Carehomes.pdf"), width = 8, height = 6)

## PLot Hospitalisations by carehome
  dat <- DAT %>% filter(!is.na(weekH) | all_covid_hosp>0) %>% 
                 frqs(c(weekH, carehome), "freqHch") %>% select(weekH, dateH, freqHch, carehome) 
  fig0(dat, x=weekH, y=freqHch, col=carehome, xname ='Week', yname ='Weekly admissions')
  ### save data - truncated
  dati <- dat %>% mutate(freqHch = ifelse(freqHch<7,"<7",freqHch)) %>% arrange(weekH) %>%
                  write.csv(file=paste0(output_dir,"/",jobno,"HDdata_H_Carehomes.csv")) 
  
## PLot Deaths anywhere by carehome
  dat <- DAT %>% filter(!is.na(weekD) | !is.na(ons_death_date)) %>% 
                 frqs(c(weekD, carehome), "freqDch") %>% select(weekD, dateD, freqDch, carehome)
  fig0(dat, x=weekD, y=freqDch, col=carehome, xname ='Week', yname ='Weekly deaths anywhere')
  ### save data - truncated
  dati <- dat %>% mutate(freqDch = ifelse(freqDch<7,"<7",freqDch)) %>% arrange(weekD) %>%
                  write.csv(file=paste0(output_dir,"/",jobno,"HDdata_D_Carehomes.csv")) 
  
## PLot Deaths in Hospital by carehome
  dat <- DAT %>% filter((!is.na(weekD) | !is.na(ons_death_date)) & (!is.na(weekH) | all_covid_hosp>0)) %>% 
                 frqs(c(weekD, carehome), "freqDch") %>% select(weekD, dateD, freqDch, carehome)
  fig0(dat, x=weekD, y=freqDch, col=carehome, xname ='Week', yname ='Weekly deaths in Hospital')
  ### save data - truncated
  dati <- dat %>% mutate(freqDch = ifelse(freqDch<7,"<7",freqDch)) %>% arrange(weekD) %>%
                  write.csv(file=paste0(output_dir,"/",jobno,"HDdata_DH_Carehomes.csv"))
  
## PLot Deaths outside Hospital by carehome
  dat <- DAT %>% filter((!is.na(weekD) | !is.na(ons_death_date)) & (is.na(weekH) | all_covid_hosp==0)) %>% 
                 frqs(c(weekD, carehome), "freqDch") %>% select(weekD, dateD, freqDch, carehome)
  fig0(dat, x=weekD, y=freqDch, col=carehome, xname ='Week', yname ='Weekly deaths outside Hospital')
  ### save data - truncated
  dati <- dat %>% mutate(freqDch = ifelse(freqDch<7,"<7",freqDch)) %>% arrange(weekD) %>%
                  write.csv(file=paste0(output_dir,"/",jobno,"HDdata_DO_Carehomes.csv"))
dev.off()



### Hospital re-admissions plots and data
### TODO: PLot H: all, first, and subsequent hospitalisations



### Second filtering - remove carehomes & subsequent hospitalisations
DAT <- DAT                                                 %>%
  ###Remove carehomes & multiple hospitalisations
  filter( (care_home_nursing==FALSE | is.na(care_home_nursing)) & 
          (care_home==FALSE | is.na(care_home)) )          %>%
  ###for excel writing
  mutate(age_cat  = paste0("'", age_cat))                  %>%
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
#TODO: write if ageg contains data - project.yaml may not work on dummy data
#TODO: check weekH < weekD (hospitalisations and deaths in hospital)
#TODO: shield1


pdf(file = paste0(output_dir,"/",jobno,"HDdata_H,HR,DH,DO.pdf"), width = 8, height = 6)

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
  dat  <- dat %>% select(weekH, dateH, freqHas, ageg, shield)
  datH <- dat %>% rename(WeeksH=weekH, Dataz=freqHas, DatesH=dateH) #fit data
  dat  <- dat %>% mutate(freqHas = ifelse(freqHas<7,"<7",freqHas)) %>% arrange(ageg, shield, weekH) %>%
                  write.csv(file=paste0(output_dir,"/",jobno,"Hdata.csv"))
  ## save data by age and shielding - truncated
  #  for (i in 1:9){ for(s in 0:1){
  #  dati <- dat %>% filter(ageg==i & shield==s) %>% select(weekH, freqHas) %>% 
  #                  mutate(freqHas = ifelse(freqHas<7,"<7",freqHas)) %>% arrange(weekH) %>%
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
  dat   <- dat %>% select(weekH, dateH, freqHas, ageg, shield) 
  datHR <- dat %>% rename(WeeksH=weekH, Dataz=freqHas, DatesH=dateH) #fit data
  dat   <- dat %>% mutate(freqHas = ifelse(freqHas<7,"<7",freqHas)) %>% arrange(ageg, shield, weekH) %>%
                   write.csv(file=paste0(output_dir,"/",jobno,"HRdata.csv"))
   
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
  dat   <- dat %>% select(weekD, dateD, freqDas, ageg, shield) 
  datDH <- dat %>% rename(WeeksD=weekD, Dataw=freqDas, DatesD=dateD) #fit data
  dat   <- dat %>% mutate(freqDas = ifelse(freqDas<7,"<7",freqDas)) %>% arrange(ageg, shield, weekD) %>%
                   write.csv(file=paste0(output_dir,"/",jobno,"DHdata.csv"))

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
  dat   <- dat %>% select(weekD, dateD, freqDas, ageg, shield)
  datDO <- dat %>% rename(WeeksD=weekD, Dataw=freqDas, DatesD=dateD) #fit data
  dat   <- dat %>% mutate(freqDas = ifelse(freqDas<7,"<7",freqDas)) %>% arrange(ageg, shield, weekD) %>%
                   write.csv(file=paste0(output_dir,"/",jobno,"DOdata.csv"))
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
#file = paste0(output_dir,"/","HDdata_Incidence.pdf"
pdf(file = paste0(output_dir,"/",jobno,"HDdata_Incidence.pdf"))
  gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2, ncol=2)
dev.off()




#### Summary of statistics and parameter estimates
##### Data & metadata for _fit
sink(file = paste0(output_dir,"/",jobno,"HDdata_summary.txt"),append=F,split=F)

#TODO: assess date/week range of each TS - use it or set missing to zero?

cat("\n")
cat("Data date range \n")
##H
WeekLim_H   = range(datH$DatesH)
WeekLim_H1  = range(datH$DatesH[which(datH$ageg==1)])
WeekLim_H2  = range(datH$DatesH[which(datH$ageg==2)],na.rm=T)
WeekLim_H3  = range(datH$DatesH[which(datH$ageg==3)])
WeekLim_H4  = range(datH$DatesH[which(datH$ageg==4)])
WeekLim_H5  = range(datH$DatesH[which(datH$ageg==5)])
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
WeekLim_DH   = range(datH$DatesD)
WeekLim_DH1  = range(datDH$DatesD[which(datDH$ageg==1)])
WeekLim_DH2  = range(datDH$DatesD[which(datDH$ageg==2)])
WeekLim_DH3  = range(datDH$DatesD[which(datDH$ageg==3)])
WeekLim_DH4  = range(datDH$DatesD[which(datDH$ageg==4)])
WeekLim_DH5  = range(datDH$DatesD[which(datDH$ageg==5)])
WeekLim_DH6  = range(datDH$DatesD[which(datDH$ageg==6)])
WeekLim_DH7  = range(datDH$DatesD[which(datDH$ageg==7)])
WeekLim_DH8  = range(datDH$DatesD[which(datDH$ageg==8)])
WeekLim_DH9  = range(datDH$DatesD[which(datDH$ageg==9)])
WeekLim_DH10 = range(datDH$DatesD[which(datDH$ageg==1 & datDH$shield=="0")])
WeekLim_DH20 = range(datDH$DatesD[which(datDH$ageg==2 & datDH$shield=="0")])
WeekLim_DH30 = range(datDH$DatesD[which(datDH$ageg==3 & datDH$shield=="0")])
WeekLim_DH40 = range(datDH$DatesD[which(datDH$ageg==4 & datDH$shield=="0")])
WeekLim_DH50 = range(datDH$DatesD[which(datDH$ageg==5 & datDH$shield=="0")])
WeekLim_DH60 = range(datDH$DatesD[which(datDH$ageg==6 & datDH$shield=="0")])
WeekLim_DH70 = range(datDH$DatesD[which(datDH$ageg==7 & datDH$shield=="0")])
WeekLim_DH80 = range(datDH$DatesD[which(datDH$ageg==8 & datDH$shield=="0")])
WeekLim_DH90 = range(datDH$DatesD[which(datDH$ageg==9 & datDH$shield=="0")])
WeekLim_DH11 = range(datDH$DatesD[which(datDH$ageg==1 & datDH$shield=="1")])
WeekLim_DH21 = range(datDH$DatesD[which(datDH$ageg==2 & datDH$shield=="1")])
WeekLim_DH31 = range(datDH$DatesD[which(datDH$ageg==3 & datDH$shield=="1")])
WeekLim_DH41 = range(datDH$DatesD[which(datDH$ageg==4 & datDH$shield=="1")])
WeekLim_DH51 = range(datDH$DatesD[which(datDH$ageg==5 & datDH$shield=="1")])
WeekLim_DH61 = range(datDH$DatesD[which(datDH$ageg==6 & datDH$shield=="1")])
WeekLim_DH71 = range(datDH$DatesD[which(datDH$ageg==7 & datDH$shield=="1")])
WeekLim_DH81 = range(datDH$DatesD[which(datDH$ageg==8 & datDH$shield=="1")])
WeekLim_DH91 = range(datDH$DatesD[which(datDH$ageg==9 & datDH$shield=="1")])

##DH
WeekLim_DO   = range(datDO$DatesD)
WeekLim_DO1  = range(datDO$DatesD[which(datDO$ageg==1)])
WeekLim_DO2  = range(datDO$DatesD[which(datDO$ageg==2)])
WeekLim_DO3  = range(datDO$DatesD[which(datDO$ageg==3)])
WeekLim_DO4  = range(datDO$DatesD[which(datDO$ageg==4)])
WeekLim_DO5  = range(datDO$DatesD[which(datDO$ageg==5)])
WeekLim_DO6  = range(datDO$DatesD[which(datDO$ageg==6)])
WeekLim_DO7  = range(datDO$DatesD[which(datDO$ageg==7)])
WeekLim_DO8  = range(datDO$DatesD[which(datDO$ageg==8)])
WeekLim_DO9  = range(datDO$DatesD[which(datDO$ageg==9)])
WeekLim_DO10 = range(datDO$DatesD[which(datDO$ageg==1 & datDO$shield=="0")])
WeekLim_DO20 = range(datDO$DatesD[which(datDO$ageg==2 & datDO$shield=="0")])
WeekLim_DO30 = range(datDO$DatesD[which(datDO$ageg==3 & datDO$shield=="0")])
WeekLim_DO40 = range(datDO$DatesD[which(datDO$ageg==4 & datDO$shield=="0")])
WeekLim_DO50 = range(datDO$DatesD[which(datDO$ageg==5 & datDO$shield=="0")])
WeekLim_DO60 = range(datDO$DatesD[which(datDO$ageg==6 & datDO$shield=="0")])
WeekLim_DO70 = range(datDO$DatesD[which(datDO$ageg==7 & datDO$shield=="0")])
WeekLim_DO80 = range(datDO$DatesD[which(datDO$ageg==8 & datDO$shield=="0")])
WeekLim_DO90 = range(datDO$DatesD[which(datDO$ageg==9 & datDO$shield=="0")])
WeekLim_DO11 = range(datDO$DatesD[which(datDO$ageg==1 & datDO$shield=="1")])
WeekLim_DO21 = range(datDO$DatesD[which(datDO$ageg==2 & datDO$shield=="1")])
WeekLim_DO31 = range(datDO$DatesD[which(datDO$ageg==3 & datDO$shield=="1")])
WeekLim_DO41 = range(datDO$DatesD[which(datDO$ageg==4 & datDO$shield=="1")])
WeekLim_DO51 = range(datDO$DatesD[which(datDO$ageg==5 & datDO$shield=="1")])
WeekLim_DO61 = range(datDO$DatesD[which(datDO$ageg==6 & datDO$shield=="1")])
WeekLim_DO71 = range(datDO$DatesD[which(datDO$ageg==7 & datDO$shield=="1")])
WeekLim_DO81 = range(datDO$DatesD[which(datDO$ageg==8 & datDO$shield=="1")])
WeekLim_DO91 = range(datDO$DatesD[which(datDO$ageg==9 & datDO$shield=="1")])

print(paste0("Week range Hosp:                  ", WeekLim_H))
print(paste0("Week range Hosp ageg 1:           ", WeekLim_H1))
print(paste0("Week range Hosp ageg 2:           ", WeekLim_H2))
print(paste0("Week range Hosp ageg 3:           ", WeekLim_H3))
print(paste0("Week range Hosp ageg 4:           ", WeekLim_H4))
print(paste0("Week range Hosp ageg 5:           ", WeekLim_H5))
print(paste0("Week range Hosp ageg 6:           ", WeekLim_H6))
print(paste0("Week range Hosp ageg 7:           ", WeekLim_H7))
print(paste0("Week range Hosp ageg 8:           ", WeekLim_H8))
print(paste0("Week range Hosp ageg 9:           ", WeekLim_H9))
print(paste0("Week range Hosp ageg 1 no shield: ", WeekLim_H10))
print(paste0("Week range Hosp ageg 2 no shield: ", WeekLim_H20))
print(paste0("Week range Hosp ageg 3 no shield: ", WeekLim_H30))
print(paste0("Week range Hosp ageg 4 no shield: ", WeekLim_H40))
print(paste0("Week range Hosp ageg 5 no shield: ", WeekLim_H50))
print(paste0("Week range Hosp ageg 6 no shield: ", WeekLim_H60))
print(paste0("Week range Hosp ageg 7 no shield: ", WeekLim_H70))
print(paste0("Week range Hosp ageg 8 no shield: ", WeekLim_H80))
print(paste0("Week range Hosp ageg 9 no shield: ", WeekLim_H90))
print(paste0("Week range Hosp ageg 1 shielding: ", WeekLim_H11))
print(paste0("Week range Hosp ageg 1 shielding: ", WeekLim_H21))
print(paste0("Week range Hosp ageg 1 shielding: ", WeekLim_H31))
print(paste0("Week range Hosp ageg 1 shielding: ", WeekLim_H41))
print(paste0("Week range Hosp ageg 1 shielding: ", WeekLim_H51))
print(paste0("Week range Hosp ageg 1 shielding: ", WeekLim_H61))
print(paste0("Week range Hosp ageg 1 shielding: ", WeekLim_H71))
print(paste0("Week range Hosp ageg 1 shielding: ", WeekLim_H81))
print(paste0("Week range Hosp ageg 1 shielding: ", WeekLim_H91))

print(paste0("Week range deaths in Hosp:                  ", WeekLim_DH))
print(paste0("Week range deaths in Hosp ageg 1:           ", WeekLim_DH1))
print(paste0("Week range deaths in Hosp ageg 2:           ", WeekLim_DH2))
print(paste0("Week range deaths in Hosp ageg 3:           ", WeekLim_DH3))
print(paste0("Week range deaths in Hosp ageg 4:           ", WeekLim_DH4))
print(paste0("Week range deaths in Hosp ageg 5:           ", WeekLim_DH5))
print(paste0("Week range deaths in Hosp ageg 6:           ", WeekLim_DH6))
print(paste0("Week range deaths in Hosp ageg 7:           ", WeekLim_DH7))
print(paste0("Week range deaths in Hosp ageg 8:           ", WeekLim_DH8))
print(paste0("Week range deaths in Hosp ageg 9:           ", WeekLim_DH9))
print(paste0("Week range deaths in Hosp ageg 1 no shield: ", WeekLim_DH10))
print(paste0("Week range deaths in Hosp ageg 2 no shield: ", WeekLim_DH20))
print(paste0("Week range deaths in Hosp ageg 3 no shield: ", WeekLim_DH30))
print(paste0("Week range deaths in Hosp ageg 4 no shield: ", WeekLim_DH40))
print(paste0("Week range deaths in Hosp ageg 5 no shield: ", WeekLim_DH50))
print(paste0("Week range deaths in Hosp ageg 6 no shield: ", WeekLim_DH60))
print(paste0("Week range deaths in Hosp ageg 7 no shield: ", WeekLim_DH70))
print(paste0("Week range deaths in Hosp ageg 8 no shield: ", WeekLim_DH80))
print(paste0("Week range deaths in Hosp ageg 9 no shield: ", WeekLim_DH90))
print(paste0("Week range deaths in Hosp ageg 1 shielding: ", WeekLim_DH11))
print(paste0("Week range deaths in Hosp ageg 1 shielding: ", WeekLim_DH21))
print(paste0("Week range deaths in Hosp ageg 1 shielding: ", WeekLim_DH31))
print(paste0("Week range deaths in Hosp ageg 1 shielding: ", WeekLim_DH41))
print(paste0("Week range deaths in Hosp ageg 1 shielding: ", WeekLim_DH51))
print(paste0("Week range deaths in Hosp ageg 1 shielding: ", WeekLim_DH61))
print(paste0("Week range deaths in Hosp ageg 1 shielding: ", WeekLim_DH71))
print(paste0("Week range deaths in Hosp ageg 1 shielding: ", WeekLim_DH81))
print(paste0("Week range deaths in Hosp ageg 1 shielding: ", WeekLim_DH91))

print(paste0("Week range deaths outside Hosp:                  ", WeekLim_DO))
print(paste0("Week range deaths outside Hosp ageg 1:           ", WeekLim_DO1))
print(paste0("Week range deaths outside Hosp ageg 2:           ", WeekLim_DO2))
print(paste0("Week range deaths outside Hosp ageg 3:           ", WeekLim_DO3))
print(paste0("Week range deaths outside Hosp ageg 4:           ", WeekLim_DO4))
print(paste0("Week range deaths outside Hosp ageg 5:           ", WeekLim_DO5))
print(paste0("Week range deaths outside Hosp ageg 6:           ", WeekLim_DO6))
print(paste0("Week range deaths outside Hosp ageg 7:           ", WeekLim_DO7))
print(paste0("Week range deaths outside Hosp ageg 8:           ", WeekLim_DO8))
print(paste0("Week range deaths outside Hosp ageg 9:           ", WeekLim_DO9))
print(paste0("Week range deaths outside Hosp ageg 1 no shield: ", WeekLim_DO10))
print(paste0("Week range deaths outside Hosp ageg 2 no shield: ", WeekLim_DO20))
print(paste0("Week range deaths outside Hosp ageg 3 no shield: ", WeekLim_DO30))
print(paste0("Week range deaths outside Hosp ageg 4 no shield: ", WeekLim_DO40))
print(paste0("Week range deaths outside Hosp ageg 5 no shield: ", WeekLim_DO50))
print(paste0("Week range deaths outside Hosp ageg 6 no shield: ", WeekLim_DO60))
print(paste0("Week range deaths outside Hosp ageg 7 no shield: ", WeekLim_DO70))
print(paste0("Week range deaths outside Hosp ageg 8 no shield: ", WeekLim_DO80))
print(paste0("Week range deaths outside Hosp ageg 9 no shield: ", WeekLim_DO90))
print(paste0("Week range deaths outside Hosp ageg 1 shielding: ", WeekLim_DO11))
print(paste0("Week range deaths outside Hosp ageg 1 shielding: ", WeekLim_DO21))
print(paste0("Week range deaths outside Hosp ageg 1 shielding: ", WeekLim_DO31))
print(paste0("Week range deaths outside Hosp ageg 1 shielding: ", WeekLim_DO41))
print(paste0("Week range deaths outside Hosp ageg 1 shielding: ", WeekLim_DO51))
print(paste0("Week range deaths outside Hosp ageg 1 shielding: ", WeekLim_DO61))
print(paste0("Week range deaths outside Hosp ageg 1 shielding: ", WeekLim_DO71))
print(paste0("Week range deaths outside Hosp ageg 1 shielding: ", WeekLim_DO81))
print(paste0("Week range deaths outside Hosp ageg 1 shielding: ", WeekLim_DO91))

cat("\n")
cat("Data points \n")
##H
np_H    = length(datH$DatesH)
np_H1   = length(datH$DatesH[which(datH$ageg==1)])
np_H10  = length(datH$DatesH[which(datH$ageg==1 & datH$shield=="0")])
np_H11  = length(datH$DatesH[which(datH$ageg==1 & datH$shield=="1")])

np_DH   = length(datDH$DatesD)
np_DH1  = length(datDH$DatesD[which(datDH$ageg==1)])
np_DH10 = length(datDH$DatesD[which(datDH$ageg==1 & datDH$shield=="0")])
np_DH11 = length(datDH$DatesD[which(datDH$ageg==1 & datDH$shield=="1")])

np_DO   = length(datDO$DatesD)
np_DO1  = length(datDO$DatesD[which(datDO$ageg==1)])
np_DO10 = length(datDO$DatesD[which(datDO$ageg==1 & datDO$shield=="0")])
np_DO11 = length(datDO$DatesD[which(datDO$ageg==1 & datDO$shield=="1")])

print(paste0("Data points Hosp:                  ", np_H))
print(paste0("Data points Hosp ageg 1:           ", np_H1))
print(paste0("Data points Hosp ageg 1 no shield: ", np_H10))
print(paste0("Data points Hosp ageg 1 shielding: ", np_H11))

print(paste0("Data points deaths in Hosp:                  ", np_DH))
print(paste0("Data points deaths in Hosp ageg 1:           ", np_DH1))
print(paste0("Data points deaths in Hosp ageg 1 no shield: ", np_DH10))
print(paste0("Data points deaths in Hosp ageg 1 shielding: ", np_DH11))

print(paste0("Data points deaths outside Hosp:                  ", np_DO))
print(paste0("Data points deaths outside Hosp ageg 1:           ", np_DO1))
print(paste0("Data points deaths outside Hosp ageg 1 no shield: ", np_DO10))
print(paste0("Data points deaths outside Hosp ageg 1 shielding: ", np_DO11))

#TODO: finish data points

#Patients
cat("\n")
cat("Patients \n")
nP = sum(!is.na(DAT$age_cat), na.rm = T)
print(paste0("Patient entries ", nP ))
print(paste0("Unique patients (if not as above) ", length(DAT$age_cat) )) #=> row <> one patient
#Age
nP_00 = sum(DAT$age_cat=="0-4", na.rm = T)
nP_05 = sum(DAT$age_cat=="5-11", na.rm = T)
nP_12 = sum(DAT$age_cat=="12-17", na.rm = T)
nP_18 = sum(DAT$age_cat=="18-29", na.rm = T)
nP_30 = sum(DAT$age_cat=="30-39", na.rm = T)
nP_40 = sum(DAT$age_cat=="40-49", na.rm = T)
nP_50 = sum(DAT$age_cat=="50-59", na.rm = T)
nP_60 = sum(DAT$age_cat=="60-69", na.rm = T)
nP_70 = sum(DAT$age_cat=="70+", na.rm = T)
print(paste0("Patients proportion age 0-4:   ", round(nP_00/nP,3) )); 
print(paste0("Patients proportion age 5-11:  ", round(nP_05/nP,3) ));
print(paste0("Patients proportion age 12-17: ", round(nP_12/nP,3) ));
print(paste0("Patients proportion age 18-29: ", round(nP_18/nP,3) ));
print(paste0("Patients proportion age 30-39: ", round(nP_30/nP,3) ));
print(paste0("Patients proportion age 40-49: ", round(nP_40/nP,3) ));
print(paste0("Patients proportion age 50-59: ", round(nP_50/nP,3) ));
print(paste0("Patients proportion age 60-69: ", round(nP_60/nP,3) ));
print(paste0("Patients proportion age 70+:   ", round(nP_70/nP,3) ));
#Hospitalisations
cat("\n")
cat("Hospitalisations (excluding re-admissions) \n")
nP_Hosp     = length( which(!is.na(DAT$weekH) | DAT$all_covid_hosp>0) )
nP_Hosp_00  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
print(paste0("Patients hospitalised:           ", nH ))
print(paste0("Patients hospitalised age 0-4:   ", nP_Hosp_00))
print(paste0("Patients hospitalised age 5-11:  ", nP_Hosp_05))
print(paste0("Patients hospitalised age 12-17: ", nP_Hosp_12))
print(paste0("Patients hospitalised age 18-29: ", nP_Hosp_18))
print(paste0("Patients hospitalised age 30-39: ", nP_Hosp_30))
print(paste0("Patients hospitalised age 40-49: ", nP_Hosp_40))
print(paste0("Patients hospitalised age 50-59: ", nP_Hosp_50))
print(paste0("Patients hospitalised age 60-69: ", nP_Hosp_60))
print(paste0("Patients hospitalised age 70+:   ", nP_Hosp_70))

#Deaths
cat("\n")
cat("Deaths \n")
nP_D   = sum(!is.na(DAT$ons_death_date))
print(paste0("Patients that died:        ", nP_D))
cat("Deaths in hospital \n")
nP_DH  = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0, na.rm = T)
nP_RH  = nP_Hosp - nP_DH
print(paste0("Patients that died:        ", nP_DH, ", mfraction   ", round(nP_DH/nP_Hosp,3) ))
print(paste0("Patients that recovered:   ", nP_RH, ", 1-mfraction ", round(nP_RH/nP_Hosp,3) ))
cat("\n")
cat("Deaths in hospital by age \n")
nP_DH_00 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4", na.rm = T)
nP_DH_05 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11", na.rm = T)
nP_DH_12 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17", na.rm = T)
nP_DH_18 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29", na.rm = T)
nP_DH_30 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39", na.rm = T)
nP_DH_40 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49", na.rm = T)
nP_DH_50 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59", na.rm = T)
nP_DH_60 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69", na.rm = T)
nP_DH_70 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="70+", na.rm = T)
###Mortality fraction 
mfraction_00 = round(nP_DH_00/nP_Hosp_00, 3)
mfraction_05 = round(nP_DH_05/nP_Hosp_05, 3)
mfraction_12 = round(nP_DH_12/nP_Hosp_12, 3)
mfraction_18 = round(nP_DH_18/nP_Hosp_18, 3)
mfraction_30 = round(nP_DH_30/nP_Hosp_30, 3)
mfraction_40 = round(nP_DH_40/nP_Hosp_40, 3)
mfraction_50 = round(nP_DH_50/nP_Hosp_50, 3)
mfraction_60 = round(nP_DH_60/nP_Hosp_60, 3)
mfraction_70 = round(nP_DH_70/nP_Hosp_70, 3)
print(paste0("Patients that died in hopital age 0-4:     ", nP_DH_00, ", mfraction  ", mfraction_00 ))
print(paste0("Patients that died in hopital age 5-11:    ", nP_DH_05, ", mfraction  ", mfraction_05 ))
print(paste0("Patients that died in hopital age 12-17:   ", nP_DH_12, ", mfraction  ", mfraction_12 ))
print(paste0("Patients that died in hopital age 18-29:   ", nP_DH_18, ", mfraction  ", mfraction_18 ))
print(paste0("Patients that died in hopital age 30-39:   ", nP_DH_30, ", mfraction  ", mfraction_30 ))
print(paste0("Patients that died in hopital age 40-49:   ", nP_DH_40, ", mfraction  ", mfraction_40 ))
print(paste0("Patients that died in hopital age 50-59:   ", nP_DH_50, ", mfraction  ", mfraction_50 ))
print(paste0("Patients that died in hopital age 60-69:   ", nP_DH_60, ", mfraction  ", mfraction_60 ))
print(paste0("Patients that died in hopital age 70+:     ", nP_DH_70, ", mfraction  ", mfraction_70 ))

cat("\n")
cat("Deaths outside hospital \n")
nP_DO = sum(!is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)), na.rm = T)
print(paste0("Patients that died outside hospital (not in carehomes):     ", nP_DO))
print(paste0("Proportion of deaths outside hospital:                      ", nP_DO/nP_D))

cat("\n")
cat("Deaths in hospital - average time to death from 1st admission - Discarding subsequent admissions \n")
###Across all ages - as the fraction of mortality is by age
men_time_to_death = round(as.numeric(   mean(DAT$ons_death_date[which(DAT$all_covid_hosp>0)] - DAT$admission_date[which(DAT$all_covid_hosp>0)], na.rm =T)),3)
med_time_to_death = round(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp>0)] - DAT$admission_date[which(DAT$all_covid_hosp>0)], na.rm =T)),3)
print(paste0("Mean (median) time in hospital from 1st admission to death:   ", men_time_to_death, " (", med_time_to_death, ")" ))

cat("\n")
cat("Recovery in hospital - average time to recovery \n")
###Across all ages - as the fraction of mortality is by age
#TODO: sort out discharge_date2 - Date subtraction
men_time_to_recover  = round(as.numeric(   mean(DAT$discharge_date  - DAT$admission_date, na.rm =T)),3)
med_time_to_recover  = round(as.numeric( median(DAT$discharge_date  - DAT$admission_date, na.rm =T)),3)
#men_time_to_recover2 = round(as.numeric(   mean(DAT$discharge_date2 - DAT$admission_date, na.rm =T)),3)
#med_time_to_recover2 = round(as.numeric( median(DAT$discharge_date2 - DAT$admission_date, na.rm =T)),3)
print(paste0("Mean (median) recovery time in hospital, 1st admission to 1st discharge:  ", men_time_to_recover, " (", med_time_to_recover, ")" ))
#print(paste0("Mean (median) recovery time in hospital, 1st admission to last discharge: ", men_time_to_recover2, " (", med_time_to_recover2, ")" ))

cat("\n")
cat("Shielding \n")
nP_s1 = sum(DAT$shield=="1", na.rm = T)
nP_s0 = sum(DAT$shield=="0", na.rm = T)
print(paste0("Patients shielding:          ", nP_s1))
print(paste0("Patients not shielding:      ", nP_s0))
print(paste0("Check: shielding + not - nP: ", nP_s1+nP_s0-nP))
nP_00_s1 = sum(DAT$shield=="1" & DAT$age_cat=="0-4", na.rm = T)
nP_05_s1 = sum(DAT$shield=="1" & DAT$age_cat=="5-11", na.rm = T)
nP_12_s1 = sum(DAT$shield=="1" & DAT$age_cat=="12-17", na.rm = T)
nP_18_s1 = sum(DAT$shield=="1" & DAT$age_cat=="18-29", na.rm = T)
nP_30_s1 = sum(DAT$shield=="1" & DAT$age_cat=="30-39", na.rm = T)
nP_40_s1 = sum(DAT$shield=="1" & DAT$age_cat=="40-49", na.rm = T)
nP_50_s1 = sum(DAT$shield=="1" & DAT$age_cat=="50-59", na.rm = T)
nP_60_s1 = sum(DAT$shield=="1" & DAT$age_cat=="60-69", na.rm = T)
nP_70_s1 = sum(DAT$shield=="1" & DAT$age_cat=="70+", na.rm = T)
print(paste0("Patient fraction shielding:           ", round(nP_s1/nP,3) ))
print(paste0("Patient age 0-4   fraction shielding: ", round(nP_00_s1/nP_00,3) ))
print(paste0("Patient age 5-11  fraction shielding: ", round(nP_05_s1/nP_05,3) ))
print(paste0("Patient age 12-17 fraction shielding: ", round(nP_12_s1/nP_12,3) ))
print(paste0("Patient age 18-29 fraction shielding: ", round(nP_18_s1/nP_18,3) ))
print(paste0("Patient age 30-39 fraction shielding: ", round(nP_30_s1/nP_30,3) ))
print(paste0("Patient age 40-49 fraction shielding: ", round(nP_30_s1/nP_40,3) ))
print(paste0("Patient age 50-59 fraction shielding: ", round(nP_30_s1/nP_50,3) ))
print(paste0("Patient age 60-69 fraction shielding: ", round(nP_30_s1/nP_60,3) ))
print(paste0("Patient age 70+   fraction shielding: ", round(nP_30_s1/nP_70,3) ))

cat("\n")
cat("Shielding patients in hospital \n")
nP_Hosp_s1 = length( which(DAT$all_covid_hosp>0 & DAT$shield=="1"))
nP_Hosp_s0 = nP_Hosp - nP_Hosp_s1
nP_DH_s1   = length( which(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shield=="1"))
nP_DH_s0   = nP_DH - nP_DH_s1
print(paste0("Patients shielding in hospital:                   ", nP_Hosp_s1))
print(paste0("Patients shielding that died in hospital:         ", nP_DH_s1 ))
print(paste0("Shielding proportion in hospital that died:       ", nP_DH_s1/nP_Hosp_s1 ))

cat("\n")
cat("Shielding patients outside hospital \n")
nP_DO_s1    = length( which(!is.na(DAT$ons_death_date) 
                            & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) & DAT$shield=="1" ))
print(paste0("Patients shielding that died outside hospital:    ", nP_Hosp_s1))

cat("\n")
cat("Shielding in hospital - mortality fraction by age (ever admitted) \n")
nP_Hosp_00_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70_s1 = length( which(DAT$shield=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
nP_Hosp_00_s0 = nP_Hosp_00 - nP_Hosp_00_s1
nP_Hosp_05_s0 = nP_Hosp_05 - nP_Hosp_05_s1
nP_Hosp_12_s0 = nP_Hosp_12 - nP_Hosp_12_s1
nP_Hosp_18_s0 = nP_Hosp_18 - nP_Hosp_18_s1
nP_Hosp_30_s0 = nP_Hosp_30 - nP_Hosp_30_s1
nP_Hosp_40_s0 = nP_Hosp_40 - nP_Hosp_40_s1
nP_Hosp_50_s0 = nP_Hosp_50 - nP_Hosp_50_s1
nP_Hosp_60_s0 = nP_Hosp_60 - nP_Hosp_60_s1
nP_Hosp_70_s0 = nP_Hosp_70 - nP_Hosp_70_s1

nP_DH_00_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4" ))
nP_DH_05_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11" ))
nP_DH_12_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17" ))
nP_DH_18_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29" ))
nP_DH_30_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39" ))
nP_DH_40_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49" ))
nP_DH_50_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59" ))
nP_DH_60_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69" ))
nP_DH_70_s1   = length( which(DAT$shield=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="70+" ))
nP_DH_00_s0   = nP_DH_00 - nP_DH_00_s1
nP_DH_05_s0   = nP_DH_05 - nP_DH_05_s1
nP_DH_12_s0   = nP_DH_12 - nP_DH_12_s1
nP_DH_18_s0   = nP_DH_18 - nP_DH_18_s1
nP_DH_30_s0   = nP_DH_30 - nP_DH_30_s1
nP_DH_40_s0   = nP_DH_40 - nP_DH_40_s1
nP_DH_50_s0   = nP_DH_50 - nP_DH_50_s1
nP_DH_60_s0   = nP_DH_60 - nP_DH_60_s1
nP_DH_70_s0   = nP_DH_70 - nP_DH_70_s1

mfraction_00_s1 = round(nP_DH_00_s1/nP_Hosp_00_s1, 3)
mfraction_05_s1 = round(nP_DH_05_s1/nP_Hosp_05_s1, 3)
mfraction_12_s1 = round(nP_DH_12_s1/nP_Hosp_12_s1, 3)
mfraction_18_s1 = round(nP_DH_18_s1/nP_Hosp_18_s1, 3)
mfraction_30_s1 = round(nP_DH_30_s1/nP_Hosp_30_s1, 3)
mfraction_40_s1 = round(nP_DH_40_s1/nP_Hosp_40_s1, 3)
mfraction_50_s1 = round(nP_DH_50_s1/nP_Hosp_50_s1, 3)
mfraction_60_s1 = round(nP_DH_60_s1/nP_Hosp_60_s1, 3)
mfraction_70_s1 = round(nP_DH_70_s1/nP_Hosp_70_s1, 3)
mfraction_00_s0 = round(nP_DH_00_s0/nP_Hosp_00_s0, 3)
mfraction_05_s0 = round(nP_DH_05_s0/nP_Hosp_05_s0, 3)
mfraction_12_s0 = round(nP_DH_12_s0/nP_Hosp_12_s0, 3)
mfraction_18_s0 = round(nP_DH_18_s0/nP_Hosp_18_s0, 3)
mfraction_30_s0 = round(nP_DH_30_s0/nP_Hosp_30_s0, 3)
mfraction_40_s0 = round(nP_DH_40_s0/nP_Hosp_40_s0, 3)
mfraction_50_s0 = round(nP_DH_50_s0/nP_Hosp_50_s0, 3)
mfraction_60_s0 = round(nP_DH_60_s0/nP_Hosp_60_s0, 3)
mfraction_70_s0 = round(nP_DH_70_s0/nP_Hosp_70_s0, 3)

print(paste0("Shiedling patients died in hopital age 0-4:    ", nP_DH_00_s1, ", mfraction  ", mfraction_00_s1 ))
print(paste0("Shiedling patients died in hopital age 5-11:   ", nP_DH_05_s1, ", mfraction  ", mfraction_05_s1 ))
print(paste0("Shiedling patients died in hopital age 12-17:  ", nP_DH_12_s1, ", mfraction  ", mfraction_12_s1 ))
print(paste0("Shiedling patients died in hopital age 18-29:  ", nP_DH_18_s1, ", mfraction  ", mfraction_18_s1 ))
print(paste0("Shiedling patients died in hopital age 30-39:  ", nP_DH_30_s1, ", mfraction  ", mfraction_30_s1 ))
print(paste0("Shiedling patients died in hopital age 40-49:  ", nP_DH_40_s1, ", mfraction  ", mfraction_40_s1 ))
print(paste0("Shiedling patients died in hopital age 50-59:  ", nP_DH_50_s1, ", mfraction  ", mfraction_50_s1 ))
print(paste0("Shiedling patients died in hopital age 60-69:  ", nP_DH_60_s1, ", mfraction  ", mfraction_60_s1 ))
print(paste0("Shiedling patients died in hopital age 70+:    ", nP_DH_70_s1, ", mfraction  ", mfraction_70_s1 ))
print(paste0("Non-shield patients died in hopital age 0-4:   ", nP_DH_00_s1, ", mfraction  ", mfraction_00_s0 ))
print(paste0("Non-shield patients died in hopital age 5-11:  ", nP_DH_05_s0, ", mfraction  ", mfraction_05_s0 ))
print(paste0("Non-shield patients died in hopital age 12-17: ", nP_DH_12_s0, ", mfraction  ", mfraction_12_s0 ))
print(paste0("Non-shield patients died in hopital age 18-29: ", nP_DH_18_s0, ", mfraction  ", mfraction_18_s0 ))
print(paste0("Non-shield patients died in hopital age 30-39: ", nP_DH_30_s0, ", mfraction  ", mfraction_30_s0 ))
print(paste0("Non-shield patients died in hopital age 40-49: ", nP_DH_40_s0, ", mfraction  ", mfraction_40_s0 ))
print(paste0("Non-shield patients died in hopital age 50-59: ", nP_DH_50_s0, ", mfraction  ", mfraction_50_s0 ))
print(paste0("Non-shield patients died in hopital age 60-69: ", nP_DH_60_s0, ", mfraction  ", mfraction_60_s0 ))
print(paste0("Non-shield patients died in hopital age 70+:   ", nP_DH_70_s0, ", mfraction  ", mfraction_70_s0 ))
cat("\n")

print(paste0("H start date: ", StartDateH))
print(paste0("D start date: ", StartDateD))

print(paste0("names1"))
print(paste0(names1))

print(paste0("names2"))
print(paste0(names2))

sink()



