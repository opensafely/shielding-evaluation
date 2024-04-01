# 10feb24 - add dataframes dataH_l, datHD_l, datDO_l
# 05feb24 - folder path revision; Option_Writedfs=1; Options_ moved up
# 02feb24 - cvs - if Options_
# 30jan24 - R vrs
# Removed: Carehome plots, Hospital re-admissions (start), Plots 5 (DATA overall), Summary of statistics 
# 10jan24 - Rmarkdown vrs
# H & D events linked at patient level
# TODO: check weekH < weekD (hospitalisations and deaths in hospital)
# TODO: shield1 rather than shield

library(arrow)
library(data.table)
library(ggplot2)
library(glue)
library(gridExtra)
library(here)
library(lubridate)
library(magrittr)
library(svglite)
library(tidyverse)


Option_Main=1; #0, 1 #sourced from main...r
if (Option_Main==0){ jobno = "J5c_" } else { jobno = pset$Job }

output_dir_HD <- paste0("./output/HDsynthesis") #paste0(getwd(),"/output/HDsynthesis") #here::here("output/HDsynthesis")
fs::dir_create(output_dir_HD)
#print(list.files(path = "."))


### Replacement for low count ##################################################
Freq_cutoff = 8 #"[REDACTED]" #replaces 0:7
### Print and save plots #######################################################
Option_Printout=0 #0, 1
### Save dataframes ############################################################
Option_Writeout=0 #0, 1 #dataframes of figures
Option_Writedfs=1#0 #0, 1 #dataframes fitted to

######## Functions #############################################################
######### Data rounding/truncation
pc1 <- function(x)  { return(round(100*x,1))}
pc2 <- function(x)  { return(round(100*x,2))}
rd2 <- function(x)  { return(round(x,2))}
rd3 <- function(x)  { return(round(x,3))}
tr  <- function(x)  {x = if(x<8) "<8" else x; return(x)}
pct1 <- function(n,N) {
   x = if(n<8 & N>7) as.character(paste0("<",eval(round(100*(8/N),1)),"%")) else if(n<8 & N<8) as.character(paste0("NA")) else paste0(pc1(n/N),'%'); return(x)}
frt2 <- function(n,N) {
   x = if(n<8 & N>7) as.character(paste0("<",eval(round((8/N),1)))) else if(n<8 & N<8) as.character(paste0("NA")) else paste0(rd2(n/N)); return(x)}

######## Week frequencies
frqs <- function(data, vars, name){
  data %>% 
    group_by(across({{vars}}))    %>%
    mutate({{name}} := n())       %>% #     }
    ungroup() }  #.group = "keep" or "drop" ?

######## Plots
#Figures - overlapping groups
fig0 <- function(data, x , y, col, xname='Week', yname='Weekly admissions') { #yname='Weekly deaths'
  p0 <- ggplot(data, aes(x = {{x}}, y = {{y}}, col = {{col}})) +
    geom_line() +     geom_point(size = 1.2, pch = 1) +    
    labs(x = xname, y = yname) +  xlim(c(0, NA)) +  ylim(c(0, NA)) +   theme_bw() 
  print(p0)} #for when sourcing the code
#Figures - panels for groups
fig <- function(data, x , y, col, facets, xname='Date', yname='Weekly admissions') { #yname='Weekly deaths'
  p <- ggplot(data, aes(x = {{x}}, y = {{y}}, col = {{col}})) +
    geom_line() +     geom_point(size = 1.2, pch = 1) +    
    facet_wrap(facets, ncol = 1, scales = 'free_y') +
    labs(x = xname, y = yname) +  xlim(c(0, NA)) +  ylim(c(0, NA)) +   theme_bw() 
  print(p)}


######## Get Source Data #######################################################
source(here::here("analysis/functions/redaction.R"))

#DAT  <- arrow::read_parquet(file = here::here("output/data_edited.gz.parquet"),
DAT  <- arrow::read_parquet(file = paste0(getwd(),"/output/data_edited.gz.parquet"),
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

### Aggregation
#H D week, year, dates-by-week
StartDateH <- min(DAT$admission_date, na.rm = T)
StartDateD <- min(DAT$ons_death_date, na.rm = T)
print(paste0("H start date: ", StartDateH))
print(paste0("D start date: ", StartDateD))

DAT <- DAT %>% #NB: ceiling(1/7 + nweeks) = floor(nweeks) + 1
  mutate( weekH = ceiling(1/7+as.numeric(difftime(admission_date, "2020-01-01", units = "weeks"))), #day 1-7 = w1 #unique id across years
          dateH = StartDateH + weeks(weekH - week(StartDateH)) ) %>%              
  mutate( weekD = ceiling(1/7+as.numeric(difftime(ons_death_date, "2020-01-01", units = "weeks"))), #day 1-7 = w1
          dateD = StartDateD + weeks(weekD - week(StartDateD)) ) #%>%

names1 = names(DAT)
#print(paste0("names1")); print(paste0(names1))


### Second filtering - remove carehomes & subsequent hospitalisations  #########
###                  - remove patient_id, change age_cat               #########
DAT <- DAT                                                 %>%
  ###Remove carehomes & multiple hospitalisations
  filter( (care_home_nursing==FALSE | is.na(care_home_nursing)) & 
          (care_home==FALSE | is.na(care_home)) )          %>%
  ###No longer need or variable replaced with numeric/factor flags
  select(-c(care_home_nursing, care_home, carehome,
            patient_id, #all_covid_hosp, 
            dplyr::contains("hosp_admitted"),
            dplyr::contains("hosp_discharge")) )           %>%
  ungroup()

names2 = names(DAT)
#print(paste0("names2")); print(paste0(names2))



### TIME SERIES DATA for fitting & plotting ####################################
#TODO: check weekH < weekD (hospitalisations and deaths in hospital)
#TODO: shield1 rather than shield


### FUNCTIONS
## Function to insert dataframe (data rows only) into long-pivot dataframe (data & missing data rows)
Include_dat_a <- function(fX,fdat,WHAT,MERGE){
  if (WHAT=="H") {
    X   <- fX    %>% rename(week=weekH, freq=freqHa, date=dateH) 
    dat <- fdat  %>% rename(week=weekH, freq=freqHa, date=dateH) }
  if (WHAT=="D") {
    X   <- fX    %>% rename(week=weekD, freq=freqDa, date=dateD)
    dat <- fdat  %>% rename(week=weekD, freq=freqDa, date=dateD) } 
  if (MERGE=="NOMERGE")  { dat <- dat %>% rename(ageg2=ageg)}
  uages = unique(dat$ageg2)
for (ja in seq_along(uages)) {
  dage   = uages[ja]
  dweeks = dat$week[which(dat$ageg2==dage)]
for (jw in seq_along(dweeks)) { 
            dweek = dweeks[jw]          #row's id: age, week=> date, freq
            ddate = dat$date[which(dat$ageg2==dage & dat$week==dweek)]      #row's date
            dfreq = dat$freq[which(dat$ageg2==dage & dat$week==dweek)]      #row's freq
                       X$date[which( X$ageg2==dage &   X$week==dweek)] = ddate #pass date to x
                       X$freq[which( X$ageg2==dage &   X$week==dweek)] = dfreq #pass freq to x
}};
if (WHAT=="H") X <- X  %>% rename(weekH=week, freqHa=freq, dateH=date) 
if (WHAT=="D") X <- X  %>% rename(weekD=week, freqDa=freq, dateD=date) 
return(X)
} #data; print(X[which(X$freqHa>0),]) 

Include_dat <- function(fX,fdat,WHAT){
  if (WHAT=="H") {
    X   <- fX    %>% rename(week=weekH, freq=freqH, date=dateH) 
    dat <- fdat  %>% rename(week=weekH, freq=freqH, date=dateH) }
  if (WHAT=="D") {
    X   <- fX    %>% rename(week=weekD, freq=freqD, date=dateD)
    dat <- fdat  %>% rename(week=weekD, freq=freqD, date=dateD) } 
  uweeks = unique(dat$week)
  for (jw in seq_along(uweeks)) { 
    dweek = uweeks[jw]                            #row's id: week=> date, freq
    ddate = dat$date[which(dat$week==dweek)]      #row's date
    dfreq = dat$freq[which(dat$week==dweek)]      #row's freq
    X$date[which( X$week==dweek)] = ddate         #pass date to x
    X$freq[which( X$week==dweek)] = dfreq         #pass freq to x
  };
  if (WHAT=="H") X <- X  %>% rename(weekH=week, freqH=freq, dateH=date) 
  if (WHAT=="D") X <- X  %>% rename(weekD=week, freqD=freq, dateD=date) 
  return(X)
} #data; print(X[which(X$freqHa>0),]) 

## Function for Long-pivot including the data and 0s (where data missing, no reporting)
Longdf_a <- function(lageg,nageg,nweek,WHAT){
  ageg  = rep(lageg:9,each=nweek)
  week  = rep(1:nweek,times=nageg)
  freq  = rep(0,times=nageg*nweek) #No reporting occurred
  date  = as.Date(rep(NA,times=nageg*nweek))
  if (WHAT=="H") X <- tibble(ageg2=ageg, weekH=week,dateH=date,freqHa=freq)
  if (WHAT=="D") X <- tibble(ageg2=ageg, weekD=week,dateD=date,freqDa=freq)
  return(X) }
Longdf <- function(nweek,WHAT){
  week  = rep(1:nweek,times=1)
  freq  = rep(0,times=nweek) #No reporting occurred
  date  = as.Date(rep(NA,times=nweek))
  if (WHAT=="H") X <- tibble(weekH=week,dateH=date,freqH=freq)
  if (WHAT=="D") X <- tibble(weekD=week,dateD=date,freqD=freq)
  return(X) }

### DATA 1 Hospitalisations (first admission) by week, age, and shielding ######
filename = "Hdata"; filepart = paste0(output_dir_HD,"/",jobno,filename)
dat1 <- DAT %>% filter(!is.na(weekH) | all_covid_hosp>0)
### Plots 1.1-1.3 Hospitalisations
#all
dat <- dat1 %>% frqs(c(weekH), "freqH") %>% 
                select(weekH, dateH, freqH) %>% 
                distinct(weekH, .keep_all = TRUE) %>%
                arrange(weekH)
if (Option_Printout==1){
  yname = paste0('Admissions (tot ', sum(dat$freqH,na.rm=T),')')
  p1 <- fig0(dat, x=weekH, y=freqH, col=NULL, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_all.svg")); print(p1); invisible(dev.off())       }
datH <- dat %>% rename(Week=weekH, Date=dateH, Freq=freqH)                #(no longer) fit data
if (Option_Writeout==1){ #TODO: make Option_Writedfs???
datw <- dat %>% mutate(freqH = ifelse(freqH<8,Freq_cutoff,freqH)) %>%
                write.csv(file=paste0(filepart,"_all.csv"))             } #show data
#Long-pivot FULL including the data and 0s (where data missing, no reporting) - no ageg merging
nweek = max(range(c(range(DAT$weekH,na.rm=T),range(DAT$weekD,na.rm=T))))
X <- Longdf(nweek,"H") %>%
     Include_dat(., dat, "H") #dat; print(X[which(X$freqH>0),]) 
datH_l <- X  %>% rename(Week=weekH, Date=dateH, Freq=freqH)               #fit data
if (Option_Writedfs==1){
X      <- X  %>% rename(Week=weekH, Date=dateH, Freq=freqH) %>%
                 write.csv(.,file=paste0(filepart,"_all_long.csv"))  }    #show data
#by_age
dat <- dat1 %>% frqs(c(weekH, age_cat), "freqHa") %>% 
                select(ageg, weekH, dateH, freqHa, age_cat) %>% 
                group_by(ageg) %>%
                distinct(weekH, .keep_all = TRUE) %>%
                arrange(ageg, weekH)
if (Option_Printout==1){
  yname = paste0('Admissions (tot ', sum(dat$freqHa,na.rm=T),')')
  p2 <- fig0(dat, x=weekH, y=freqHa, col=age_cat, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_by_age.svg")); print(p2); invisible(dev.off())     }
datHa<- dat %>% rename(Week=weekH, Date=dateH, Freq=freqHa)                #fit data
if (Option_Writedfs==1){
datw <- dat %>% mutate(freqHa = ifelse(freqHa<8,Freq_cutoff,freqHa)) %>%
                write.csv(file=paste0(filepart,"_by_age.csv"))           } #show data
#Long-pivot FULL including the data and 0s (where data missing, no reporting) - no ageg merging
X <- Longdf_a(1,9,nweek,"H") %>%
     Include_dat_a(., dat, "H", "NOMERGE")  #dat; print(X[which(X$freqHa>0),]) 
datHa_l <- X  %>% rename(Week=weekH, Date=dateH, Freq=freqHa, ageg=ageg2)  #fit data
if (Option_Writedfs==1){
X       <- X  %>% rename(Week=weekH, Date=dateH, Freq=freqHa, ageg=ageg2) %>%
                  write.csv(.,file=paste0(filepart,"_by_age_long.csv"))  } #show data
#MERGE ageg 1-4, save long-pivot data frame (all ages x all weeks (x shield))
lageg = 4; lage_cat="0-29"
nageg = 9-lageg+1
dat  <- dat1 %>% mutate(age_cat2= age_cat)                               %>%
                 mutate(age_cat2=as.factor(ifelse(ageg<=lageg,lage_cat,levels(age_cat2)[age_cat2] ))) %>%
                 mutate(ageg2   = ifelse(ageg<=lageg,lageg, ageg))       %>%
                 frqs(c(weekH, ageg2), "freqHa")                %>% 
                 select(ageg2, weekH, dateH, freqHa, age_cat2)  %>% 
                 group_by(ageg2)                                %>%
                 distinct(weekH, .keep_all = TRUE)              %>%
                 arrange(ageg2, weekH)
if (Option_Printout==1){
  yname = paste0('Admissions, 6 strata (tot ', sum(dat$freqHa,na.rm=T),')')
  p2 <- fig0(dat, x=weekH, y=freqHa, col=age_cat2, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_by_age_merged.svg")); print(p2); invisible(dev.off()) }
#Long-pivot MERGED including the data and 0s (where data missing, no reporting - merged agegs
X <- Longdf_a(lageg,nageg,nweek,"H") %>%
     Include_dat_a(., dat, "H", "MERGE")    #dat; print(X[which(X$freqHa>0),]) 
datHa_m_l<- X  %>% rename(Week=weekH, Date=dateH, Freq=freqHa)             #fit data
if (Option_Writedfs==1){
X        <- X  %>% rename(Week=weekH, Date=dateH, Freq=freqHa) %>%
                   write.csv(.,file=paste0(filepart,"_by_age_merged_long.csv"))  } #show data
if (Option_Writeout==1){				   
dat      <-dat %>% mutate(freqHa = ifelse(freqHa<8,Freq_cutoff,freqHa))      %>%
                   write.csv(.,file=paste0(filepart,"_by_age_merged.csv"))       } #show data
#by_shield
dat <- dat1 %>% frqs(c(weekH, shield), "freqHs") %>% 
                select(shield, weekH, dateH, freqHs) %>% 
                group_by(shield) %>%
                distinct(weekH, .keep_all = TRUE) %>%
                arrange(shield, weekH)
if (Option_Printout==1){
  yname = paste0('Admissions (tot ', sum(dat$freqHs,na.rm=T),')')
  p3 <- fig0(dat, x=weekH, y=freqHs, col=shield, xname ='Week', yname = yname)#
  svglite(paste0(filepart,"_by_shi.svg")); print(p3); invisible(dev.off())    }
datHs<- dat %>% rename(Week=weekH, Date=dateH, Freq=freqHs)                #fit data
if (Option_Writedfs==1){
dat  <- dat %>% mutate(freqHs = ifelse(freqHs<8,Freq_cutoff,freqHs)) %>%
                write.csv(file=paste0(filepart,"_by_shi.csv"))           } #show data
### Plots 1.4, 1.5 by shielding cohort
#shi_by_age
dat <- dat1 %>% frqs(c(weekH, age_cat, shield), "freqHas") %>% 
                select(shield, ageg, weekH, dateH, freqHas, age_cat) %>% 
                group_by(ageg, shield) %>%
                distinct(weekH, .keep_all = TRUE) %>%
                arrange(ageg, shield, weekH)
dat0 <- dat %>% filter(shield==0)
dat1 <- dat %>% filter(shield==1)
if (Option_Printout==1){
  yname = paste0('Admissions not shielding (tot ', sum(dat0$freqHas,na.rm=T),')')
  p4 <- fig0(dat0, x=weekH, y=freqHas, col=age_cat, xname ='Week', yname = yname)
  yname = paste0('Admissions shielding (tot ',     sum(dat1$freqHas,na.rm=T),')') 
  p5 <- fig0(dat1, x=weekH, y=freqHas, col=age_cat, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_s0_by_age.svg")); print(p4); invisible(dev.off())
  svglite(paste0(filepart,"_s1_by_age.svg")); print(p5); invisible(dev.off())  }
dat   <- dat %>% select(-c(age_cat))
datHas<- dat %>% rename(Week=weekH, Date=dateH, Freq=freqHas)              #fit data
if (Option_Writedfs==1){
dat   <- dat %>% mutate(freqHas = ifelse(freqHas<8,Freq_cutoff,freqHas)) %>%
                 write.csv(file=paste0(filepart,"_s_by_age.csv"))        } #show data
#merge ageg 1-4 - shield age
#TODO...


### DATA 2 Hospitalisations with recovery by week, age, and shielding ########
filename = "HRdata"; filepart = paste0(output_dir_HD,"/",jobno,filename)
dat1 <- DAT %>% filter( (!is.na(weekH) | all_covid_hosp>0) & (is.na(weekD) | is.na(ons_death_date)) )
### Plots 2.1-2.3 Hospitalisations with recovery
#all
dat <- dat1 %>% frqs(c(weekH), "freqH") %>% 
                select(weekH, dateH, freqH) %>% 
                distinct(weekH, .keep_all = TRUE) %>%
                arrange(weekH)
if (Option_Printout==1){
  yname = paste0('Admissions & recovery (tot ', sum(dat$freqH,na.rm=T),')')
  p1 <- fig0(dat, x=weekH, y=freqH, col=NULL, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_all.svg")); print(p1); invisible(dev.off())       }
datHR<- dat %>% rename(Week=weekH, Date=dateH, Freq=freqH)                 #(not now) fit data
if (Option_Writeout==1){
dat  <- dat %>% mutate(freqH = ifelse(freqH<8,Freq_cutoff,freqH)) %>%
                write.csv(file=paste0(filepart,"_all.csv"))             }  #show data
#by_age
dat <- dat1 %>% frqs(c(weekH, age_cat), "freqHa") %>% 
                select(ageg, weekH, dateH, freqHa, age_cat) %>% 
                group_by(ageg) %>%
                distinct(weekH, .keep_all = TRUE) %>%
                arrange(ageg, weekH)
if (Option_Printout==1){
  yname = paste0('Admissions & recovery (tot ', sum(dat$freqHa,na.rm=T),')')
  p2 <- fig0(dat, x=weekH, y=freqHa, col=age_cat, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_by_age.svg")); print(p2); invisible(dev.off())    }
datHRa<-dat %>% rename(Week=weekH, Date=dateH, Freq=freqHa)                #(not now) fit data
if (Option_Writeout==1){
datw <- dat %>% mutate(freqHa = ifelse(freqHa<8,Freq_cutoff,freqHa)) %>%
                write.csv(file=paste0(filepart,"_by_age.csv"))           } #show data
#merge ageg 1-4
#TODO...
#by_shield
dat <- dat1 %>% frqs(c(weekH, shield), "freqHs") %>% 
                select(shield, weekH, dateH, freqHs) %>% 
                group_by(shield) %>%
                distinct(weekH, .keep_all = TRUE) %>%
                arrange(shield, weekH)
if (Option_Printout==1){
  yname = paste0('Admissions & recovery (tot ', sum(dat$freqHs,na.rm=T),')')
  p3 <- fig0(dat, x=weekH, y=freqHs, col=shield, xname ='Week', yname = yname)#
  svglite(paste0(filepart,"_by_shi.svg")); print(p3); invisible(dev.off())    }
datHRs<-dat %>% rename(Week=weekH, Date=dateH, Freq=freqHs)                #(not now) fit data
if (Option_Writeout==1){
dat  <- dat %>% mutate(freqHs = ifelse(freqHs<8,Freq_cutoff,freqHs)) %>%
                write.csv(file=paste0(filepart,"_by_shi.csv"))             } #show data
### Plots 2.4, 2.5 by shielding cohort
#shi_by_age
dat <- dat1 %>% frqs(c(weekH, age_cat, shield), "freqHas") %>% 
                select(shield, ageg, weekH, dateH, freqHas, age_cat) %>%
                group_by(ageg, shield) %>%
                distinct(weekH, .keep_all = TRUE) %>%
                arrange(ageg, shield, weekH)
dat0 <- dat %>% filter(shield==0)
dat1 <- dat %>% filter(shield==1)
if (Option_Printout==1){
  yname = paste0('Admissions & recovery not sh (tot ', sum(dat0$freqHas,na.rm=T),')')
  p4 <- fig0(dat0, x=weekH, y=freqHas, col=age_cat, xname ='Week', yname = yname)
  yname = paste0('Admissions & recovery shield (tot ',     sum(dat1$freqHas,na.rm=T),')') 
  p5 <- fig0(dat1, x=weekH, y=freqHas, col=age_cat, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_s0_by_age.svg")); print(p4); invisible(dev.off())
  svglite(paste0(filepart,"_s1_by_age.svg")); print(p5); invisible(dev.off())  }
dat    <- dat %>% select(-c(age_cat))
datHRas<- dat %>% rename(Week=weekH, Date=dateH, Freq=freqHas)             #(not now) fit data
if (Option_Writeout==1){
dat    <- dat %>% mutate(freqHas = ifelse(freqHas<8,Freq_cutoff,freqHas)) %>%
                  write.csv(file=paste0(filepart,"_s_by_age.csv"))       } #show data
#merge ageg 1-4
#TODO...

                 
### DATA 3 Deaths in Hospital by week, age, and shielding ####################
#TODO: & weekH < weekD)
dat1 <- DAT %>% filter((!is.na(weekD) | !is.na(ons_death_date)) & (!is.na(weekH) | all_covid_hosp>0))
filename = "DHdata"; filepart = paste0(output_dir_HD,"/",jobno,filename)
### Plots 3.1-3.3 Deaths in Hospital
#all
dat <- dat1 %>% frqs(c(weekD), "freqD") %>% 
                select(weekD, dateD, freqD) %>% 
                distinct(weekD, .keep_all = TRUE) %>%
                arrange(weekD)
if (Option_Printout==1){
  yname = paste0('Deaths in Hospital (tot ', sum(dat$freqD,na.rm=T),')')
  p1 <- fig0(dat, x=weekD, y=freqD, col=NULL, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_all.svg")); print(p1); invisible(dev.off())       }
datDH<- dat %>% rename(Week=weekD, Date=dateD, Freq=freqD)                 #(no longer) fit data
if (Option_Writeout==1){
dat  <- dat %>% mutate(freqD = ifelse(freqD<8,Freq_cutoff,freqD)) %>%
                write.csv(file=paste0(filepart,"_all.csv"))             } #show data
#Long-pivot FULL including the data and 0s (where data missing, no reporting) - no ageg merging
X <- Longdf(nweek,"D") %>%
     Include_dat(., dat, "D") #dat; print(X[which(X$freqD>0),]) 
datDH_l <- X  %>% rename(Week=weekD, Date=dateD, Freq=freqD)              #fit data
if (Option_Writedfs==1){
X      <- X  %>% rename(Week=weekD, Date=dateD, Freq=freqD) %>%
                 write.csv(.,file=paste0(filepart,"_all_long.csv"))  }    #show data
#by_age
dat <- dat1 %>% frqs(c(weekD, age_cat), "freqDa") %>% 
                select(ageg, weekD, dateD, freqDa, age_cat) %>% 
                group_by(ageg) %>%
                distinct(weekD, .keep_all = TRUE) %>%
                arrange(ageg, weekD)
if (Option_Printout==1){
  yname = paste0('Deaths in Hospital (tot ', sum(dat$freqDa,na.rm=T),')')
  p2 <- fig0(dat, x=weekD, y=freqDa, col=age_cat, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_by_age.svg")); print(p2); invisible(dev.off())    }
datDHa<-dat %>% rename(Week=weekD, Date=dateD, Freq=freqDa)                #fit data
if (Option_Writedfs==1){
datw  <-dat %>% mutate(freqDa = ifelse(freqDa<8,Freq_cutoff,freqDa)) %>%
                write.csv(file=paste0(filepart,"_by_age.csv"))           } #show data
#Long-pivot FULL including the data and 0s (where data missing, no reporting) - no ageg merging
X <- Longdf_a(1,9,nweek,"D") %>%
     Include_dat_a(., dat, "D", "NOMERGE")  #dat; print(X[which(X$freqDa>0),]) 
datDHa_l<- X  %>% rename(Week=weekD, Date=dateD, Freq=freqDa, ageg=ageg2)  #fit data
if (Option_Writedfs==1){
X       <- X  %>% rename(Week=weekD, Date=dateD, Freq=freqDa, ageg=ageg2) %>%
                  write.csv(.,file=paste0(filepart,"_by_age_long.csv"))  } #show data
#MERGE ageg 1-6, save long-pivot data frame (all ages x all weeks (x shield))
lageg = 6; lage_cat="0-49"
nageg = 9-lageg+1
dat  <- dat1 %>% mutate(age_cat2= age_cat)                               %>%
                 mutate(age_cat2=as.factor(ifelse(ageg<=lageg,lage_cat,levels(age_cat2)[age_cat2] ))) %>%
                 mutate(ageg2   = ifelse(ageg<=lageg,lageg, ageg))       %>%
                 frqs(c(weekD, ageg2), "freqDa")      %>%
                 select(ageg2, weekD, dateD, freqDa, age_cat2)  %>%
                 group_by(ageg2)                      %>%
                 distinct(weekD, .keep_all = TRUE)    %>%
                 arrange(ageg2, weekD)
if (Option_Printout==1){
  yname = paste0('Deaths in Hospital, 4 strata (tot ', sum(dat$freqDa,na.rm=T),')')
  p2 <- fig0(dat, x=weekD, y=freqDa, col=age_cat2, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_by_age_merged.svg")); print(p2); invisible(dev.off()) }
#Long-pivot MERGED including the data and 0s (where data missing, no reporting - merged agegs
X <- Longdf_a(lageg,nageg,nweek,"D") %>%
     Include_dat_a(., dat, "D", "MERGED")    #dat; print(X[which(X$freqHa>0),]) 
datDHa_m_l<- X  %>% rename(Week=weekD, Date=dateD, Freq=freqDa)            #fit data
if (Option_Writedfs==1){
X         <- X  %>% rename(Week=weekD, Date=dateD, Freq=freqDa) %>%
                    write.csv(.,file=paste0(filepart,"_by_age_merged_long.csv"))} #show data
if (Option_Writeout==1){					
dat       <-dat %>% mutate(freqDa = ifelse(freqDa<8,Freq_cutoff,freqDa))      %>%
                    write.csv(.,file=paste0(filepart,"_by_age_merged.csv"))}#show data
#by_shield
dat <- dat1 %>% frqs(c(weekD, shield), "freqDs") %>% 
                select(shield, weekD, dateD, freqDs) %>% 
                group_by(shield) %>%
                distinct(weekD, .keep_all = TRUE) %>%
                arrange(shield, weekD)
if (Option_Printout==1){
  yname = paste0('Deaths in Hospital (tot ', sum(dat$freqDs,na.rm=T),')')
  p3 <- fig0(dat, x=weekD, y=freqDs, col=shield, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_by_shi.svg")); print(p3); invisible(dev.off())    }
datDHs<-dat %>% rename(Week=weekD, Date=dateD, Freq=freqDs)                #fit data
if (Option_Writedfs==1){
dat  <- dat %>% mutate(freqDs = ifelse(freqDs<8,Freq_cutoff,freqDs)) %>%
                write.csv(file=paste0(filepart,"_by_shi.csv"))           } #show data
### Plot 3.4, 3.5 by shielding
#shi_by_age
dat <- dat1 %>% frqs(c(weekD, age_cat, shield), "freqDas") %>% 
                select(shield, ageg, weekD, dateD, freqDas, age_cat) %>% 
                group_by(ageg, shield) %>%
                distinct(weekD, .keep_all = TRUE) %>%
                arrange(ageg, shield, weekD)
dat0 <- dat %>% filter(shield==0)
dat1 <- dat %>% filter(shield==1)
if (Option_Printout==1){
  yname = paste0('Deaths in Hospital not shielding (tot ', sum(dat0$freqDas,na.rm=T),')') 
  p4 <- fig0(dat0, x=weekD, y=freqDas, col=age_cat, xname ='Week', yname = yname)
  yname = paste0('Deaths in Hospital shielding (tot ',     sum(dat1$freqDas,na.rm=T),')')
  p5 <- fig0(dat1, x=weekD, y=freqDas, col=age_cat, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_s0_by_age.svg")); print(p4); invisible(dev.off())
  svglite(paste0(filepart,"_s1_by_age.svg")); print(p5); invisible(dev.off())  }
dat   <- dat %>% select(-c(age_cat))
datDHas<-dat %>% rename(Week=weekD, Date=dateD, Freq=freqDas)              #fit data
if (Option_Writedfs==1){
dat   <- dat %>% mutate(freqDas = ifelse(freqDas<8,Freq_cutoff,freqDas)) %>%
                 write.csv(file=paste0(filepart,"_s_by_age.csv"))        } #show data
#merge ageg 1-6
#TODO...

               
### DATA 4 Deaths outside Hospital by week, age, and shielding #################
dat1 <- DAT %>% filter((!is.na(weekD) | !is.na(ons_death_date)) & (is.na(weekH) | all_covid_hosp==0))
filename = "DOdata"; filepart = paste0(output_dir_HD,"/",jobno,filename)
### Plots 4.1-4.3 Deaths outside Hospital
#all
dat <- dat1 %>% frqs(c(weekD), "freqD") %>% 
                select(weekD, dateD, freqD) %>% 
                distinct(weekD, .keep_all = TRUE) %>%
                arrange(weekD)
if (Option_Printout==1){
  yname = paste0('Deaths outside Hospital (tot ', sum(dat$freqD,na.rm=T),')')
  p1 <- fig0(dat, x=weekD, y=freqD, col=NULL, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_all.svg")); print(p1); invisible(dev.off())       }
datDO<- dat %>% rename(Week=weekD, Date=dateD, Freq=freqD)                 #(no longer) fit data
if (Option_Writeout==1){
dat  <- dat %>% mutate(freqD = ifelse(freqD<8,Freq_cutoff,freqD)) %>%
                write.csv(file=paste0(filepart,"_all.csv"))              } #show data
#Long-pivot FULL including the data and 0s (where data missing, no reporting) - no ageg merging
X <- Longdf(nweek,"D") %>%
     Include_dat(., dat, "D") #dat; print(X[which(X$freqD>0),]) 
datDO_l<- X  %>% rename(Week=weekD, Date=dateD, Freq=freqD)               #fit data
if (Option_Writedfs==1){
X      <- X  %>% rename(Week=weekD, Date=dateD, Freq=freqD) %>%
                 write.csv(.,file=paste0(filepart,"_all_long.csv"))  }    #show data
#by_age
dat <- dat1 %>% frqs(c(weekD, age_cat), "freqDa") %>% 
                select(ageg, weekD, dateD, freqDa, age_cat) %>% 
                group_by(ageg) %>%
                distinct(weekD, .keep_all = TRUE) %>%
                arrange(ageg, weekD)
if (Option_Printout==1){
  yname = paste0('Deaths outside Hospital (tot ', sum(dat$freqDa,na.rm=T),')')
  p2 <- fig0(dat, x=weekD, y=freqDa, col=age_cat, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_by_age.svg")); print(p2); invisible(dev.off())    }
datDOa<-dat %>% rename(Week=weekD, Date=dateD, Freq=freqDa)                #fit data
if (Option_Writedfs==1){
datw  <-dat %>% mutate(freqDa = ifelse(freqDa<8,Freq_cutoff,freqDa)) %>%
                write.csv(file=paste0(filepart,"_by_age.csv"))           } #show data
#Long-pivot FULL including the data and 0s (where data missing, no reporting) - no ageg merging
X <- Longdf_a(1,9,nweek,"D") %>%
     Include_dat_a(., dat, "D", "NOMERGE")  #dat; print(X[which(X$freqDa>0),]) 
datDOa_l<- X %>% rename(Week=weekD, Date=dateD, Freq=freqDa, ageg=ageg2)   #fit data
if (Option_Writedfs==1){
X       <- X  %>% rename(Week=weekD, Date=dateD, Freq=freqDa, ageg=ageg2) %>%
                  write.csv(.,file=paste0(filepart,"_by_age_long.csv"))  } #show data
#MERGE ageg 1-6, save long-pivot data frame (all ages x all weeks (x shield))
lageg = 6; lage_cat="0-49"
nageg = 9-lageg+1
dat  <- dat1 %>% mutate(age_cat2= age_cat)                               %>%
                 mutate(age_cat2=as.factor(ifelse(ageg<=lageg,lage_cat,levels(age_cat2)[age_cat2] ))) %>%
                 mutate(ageg2   = ifelse(ageg<=lageg,lageg, ageg))       %>%
                 frqs(c(weekD, ageg2), "freqDa")      %>%
                 select(ageg2, weekD, dateD, freqDa, age_cat2)  %>%
                 group_by(ageg2)                      %>%
                 distinct(weekD, .keep_all = TRUE)    %>%
                 arrange(ageg2, weekD)
if (Option_Printout==1){
  yname = paste0('Deaths outside Hospital, 4 strata (tot ', sum(dat$freqDa,na.rm=T),')')
  p2 <- fig0(dat, x=weekD, y=freqDa, col=age_cat2, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_by_age_merged.svg")); print(p2); invisible(dev.off())  }
#Long-pivot MERGED including the data and 0s (where data missing, no reporting - merged agegs
X <- Longdf_a(lageg,nageg,nweek,"D") %>%
     Include_dat_a(., dat, "D", "MERGED")    #dat; print(X[which(X$freqHa>0),]) 
datDOa_m_l<- X  %>% rename(Week=weekD, Date=dateD, Freq=freqDa)                   #fit data
if (Option_Writedfs==1){
X         <- X  %>% rename(Week=weekD, Date=dateD, Freq=freqDa) %>%
                    write.csv(.,file=paste0(filepart,"_by_age_merged_long.csv"))} #show data
if( Option_Writeout==1){
dat       <-dat %>% mutate(freqDa = ifelse(freqDa<8,Freq_cutoff,freqDa))      %>%
                    write.csv(.,file=paste0(filepart,"_by_age_merged.csv"))     } #show data
#by_shield
dat <- dat1 %>% frqs(c(weekD, shield), "freqDs") %>% 
                select(shield, weekD, dateD, freqDs) %>% 
                group_by(shield) %>%
                distinct(weekD, .keep_all = TRUE) %>%
                arrange(shield, weekD)
if (Option_Printout==1){
  yname = paste0('Deaths outside Hospital (tot ', sum(dat$freqDs,na.rm=T),')')
  p3 <- fig0(dat, x=weekD, y=freqDs, col=shield, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_by_shi.svg")); print(p3); invisible(dev.off())    }
datDOs<-dat %>% rename(Week=weekD, Date=dateD, Freq=freqDs)                #fit data
if (Option_Writedfs==1){
dat   <-dat %>% mutate(freqDs = ifelse(freqDs<8,Freq_cutoff,freqDs)) %>%
                write.csv(file=paste0(filepart,"_by_shi.csv"))           } #show data
### Plots 4.4, 4.5 Deaths outside Hospital by shielding
#shi_by_age
dat <- dat1 %>% frqs(c(weekD, age_cat, shield), "freqDas") %>% 
                select(shield, ageg, weekD, dateD, freqDas, age_cat) %>% 
                group_by(ageg, shield) %>%
                distinct(weekD, .keep_all = TRUE) %>%
                arrange(ageg, shield, weekD)
dat0 <- dat %>% filter(shield==0)
dat1 <- dat %>% filter(shield==1)
if (Option_Printout==1){
  yname = paste0('Deaths outside Hospital not shielding (tot ', sum(dat0$freqDas,na.rm=T),')') 
  p4 <- fig0(dat0, x=weekD, y=freqDas, col=age_cat, xname ='Week', yname = yname)
  yname = paste0('Deaths outside Hospital shielding (tot ',     sum(dat1$freqDas,na.rm=T),')')
  p5 <- fig0(dat1, x=weekD, y=freqDas, col=age_cat, xname ='Week', yname = yname)
  svglite(paste0(filepart,"_s0_by_age.svg")); print(p4); invisible(dev.off())
  svglite(paste0(filepart,"_s1_by_age.svg")); print(p5); invisible(dev.off())  }
dat   <- dat %>% select(-c(age_cat))
datDOas<-dat %>% rename(Week=weekD, Date=dateD, Freq=freqDas)             #fit data
if (Option_Writedfs==1){
dat   <- dat %>% mutate(freqDas = ifelse(freqDas<8,Freq_cutoff,freqDas)) %>%
                 write.csv(file=paste0(filepart,"_s_by_age.csv"))       } #show data
#merge ageg 1-6
#TODO...


### Plots 5 - DATA overall (no age or shield) ##################################


#### Summary of statistics and parameter estimates

