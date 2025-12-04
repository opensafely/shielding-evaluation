# Reads & writes rounded severe event data: H, DH, DO
# Dataframes for fitting: dataHas_l, datDHas_l, datDOas_l
# Output checked csv: rounding mid-6
# Shielding definition: flags up to 2020-12-01

library(arrow)
library(data.table)
library(ggplot2)
library(glue)
library(gridExtra)
library(here)
library(lubridate)
library(magrittr)
#library(svglite)
library(tidyverse)



### Replacement for low count ##################################################
Freq_cutoff = 8 #for "[REDACTED]" #replaces 0:7

### Save csv of long dataset (inc zeros for unreported events) #################
Option_WriteLong_all=0#1 #0, 1 #no strata
Option_WriteLong_age=0 #0, 1 #by age group
Option_WriteLong_shi=0#1 #0, 1 #by shielding
Option_WriteLong_sag=0#1 #0, 1 #by shielding and age
### Merge agegroups ############################################################
Option_Merge=0         #0, 1 #merge scarce-data age groups


######## Functions #############################################################
######### Data rounding or truncation
#roundmid_any <- function(x, to=6){
#  # like round_any, but centers on (integer) midpoint of the rounding points
#  ceiling(x/to)*to - (floor(to/2)*(x!=0)) }
rm6 <- function(x){ ceiling(x/6)*6 - (floor(6/2)*(x!=0))   }
r5  <- function(x){ return( round(x/5)*5)   }


######## Plots
#Figures - overlaps curves of groups, no panels
fig0 <- function(data, x , y, col, xname='Week', yname='Weekly admissions') {
  p0 <- ggplot(data, aes(x = {{x}}, y = {{y}}, col = {{col}})) +
    geom_line() +     geom_point(size = 1.2, pch = 1) +    
    labs(x = xname, y = yname) +  xlim(c(0, NA)) +  ylim(c(0, NA)) +   theme_bw() 
  print(p0)} #for when sourcing the code


#### Study date range
Date1="2020-01-01"
Date2="2020-12-01"


### TIME SERIES DATA for fitting & plotting ####################################


### FUNCTIONS
## Function to insert dataframe (data rows only) into long-pivot dataframe (data & missing data rows)
Include_dat_as <- function(fX,fdat,WHAT,MERGE){
  if (WHAT=="H") {
    X   <- fX    %>% rename(week=weekH, freq=freqHas, date=dateH, shei=shield) 
    dat <- fdat  %>% rename(week=weekH, freq=freqHas, date=dateH, shei=shield) }
  if (WHAT=="D") {
    X   <- fX    %>% rename(week=weekD, freq=freqDas, date=dateD, shei=shield)
    dat <- fdat  %>% rename(week=weekD, freq=freqDas, date=dateD, shei=shield) } 
  if (MERGE=="NOMERGE")  { dat <- dat %>% rename(ageg2=ageg)}
  ushie = unique(dat$shei)
  uages = unique(dat$ageg2)
  for (js in seq_along(ushie)) {
    dshi   = ushie[js]
  for (ja in seq_along(uages)) { #same for both values of ushie
    dage   = uages[ja]
    dweeks = dat$week[which(dat$ageg2==dage & dat$shei==dshi)]
    for (jw in seq_along(dweeks)) { 
      dweek = dweeks[jw]          #row's id: shie, age, week=> date, freq
      ddate = dat$date[which(dat$ageg2==dage & dat$shei==dshi & dat$week==dweek)]  #row's date
      dfreq = dat$freq[which(dat$ageg2==dage & dat$shei==dshi & dat$week==dweek)]  #row's freq
      X$date[which( X$ageg2==dage & X$shei==dshi & X$week==dweek)] = ddate         #pass date to x
      X$freq[which( X$ageg2==dage & X$shei==dshi & X$week==dweek)] = dfreq         #pass freq to x
    }}};
  if (WHAT=="H") X <- X  %>% rename(weekH=week, freqHas=freq, dateH=date, shield=shei) 
  if (WHAT=="D") X <- X  %>% rename(weekD=week, freqDas=freq, dateD=date, shield=shei) 
  return(X)
} #data; print(X[which(X$freqHas>0),]) 

Include_dat_s <- function(fX,fdat,WHAT,MERGE){
  if (WHAT=="H") {
    X   <- fX    %>% rename(week=weekH, freq=freqHs, date=dateH, shei=shield) 
    dat <- fdat  %>% rename(week=weekH, freq=freqHs, date=dateH, shei=shield) }
  if (WHAT=="D") {
    X   <- fX    %>% rename(week=weekD, freq=freqDs, date=dateD, shei=shield)
    dat <- fdat  %>% rename(week=weekD, freq=freqDs, date=dateD, shei=shield) } 
  ushie = unique(dat$shei)
  for (js in seq_along(ushie)) {
      dshi   = ushie[js]
      dweeks = dat$week[which(dat$shei==dshi)]
      for (jw in seq_along(dweeks)) { 
        dweek = dweeks[jw]          #row's id: shie => date, freq
        ddate = dat$date[which(dat$shei==dshi & dat$week==dweek)]     #row's date
        dfreq = dat$freq[which(dat$shei==dshi & dat$week==dweek)]     #row's freq
        X$date[which( X$shei==dshi & X$week==dweek)] = ddate          #pass date to x
        X$freq[which( X$shei==dshi & X$week==dweek)] = dfreq          #pass freq to x
      }};
  if (WHAT=="H") X <- X  %>% rename(weekH=week, freqHs=freq, dateH=date, shield=shei) 
  if (WHAT=="D") X <- X  %>% rename(weekD=week, freqDs=freq, dateD=date, shield=shei) 
  return(X)
} #data; print(X[which(X$freqHs>0),]) 

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
Longdf_as<- function(lageg,nageg,nweek,WHAT){
  shie  = rep(0:1,each=nweek*nageg)
  ageg  = rep(lageg:9,each=nweek,times=2)
  week  = rep(1:nweek,times=nageg*2)
  freq  = rep(0,times=nageg*nweek*2) #As default, no reporting occurred
  date  = as.Date(rep(NA,times=nageg*nweek*2))
  if (WHAT=="H") X <- tibble(ageg2=ageg, weekH=week,dateH=date,freqHas=freq, shield=shie)
  if (WHAT=="D") X <- tibble(ageg2=ageg, weekD=week,dateD=date,freqDas=freq, shield=shie)
  return(X) }
Longdf_s<- function(lageg,nageg,nweek,WHAT){
  shie  = rep(0:1,each=nweek)
  week  = rep(1:nweek,times=2)
  freq  = rep(0,times=nweek*2) #As default, no reporting occurred
  date  = as.Date(rep(NA,times=nweek*2))
  if (WHAT=="H") X <- tibble(weekH=week,dateH=date,freqHs=freq, shield=shie)
  if (WHAT=="D") X <- tibble(weekD=week,dateD=date,freqDs=freq, shield=shie)
  return(X) }
Longdf <- function(nweek,WHAT){
  week  = rep(1:nweek,times=1)
  freq  = rep(0,times=nweek)       #As default, no reporting occurred
  date  = as.Date(rep(NA,times=nweek))
  if (WHAT=="H") X <- tibble(weekH=week,dateH=date,freqH=freq)
  if (WHAT=="D") X <- tibble(weekD=week,dateD=date,freqD=freq)
  return(X) }



fileread = input_data


### DATA 1 Hospitalisations (first admission) by week, age, and shielding ######
filepart = "Hdata"
#all
dat      <- read.csv(file=paste0(fileread,"/table1_Hdata_all_ROUNDED.csv")) %>% 
            rename(weekH=Week, dateH=Date, freqH=Freq)                                           #}#output data
#Long-pivot, FULL including data and 0s (i.e. missing data, no reporting) - no ageg merging
nweek = max(range(c(48))) 
cat(paste0("... nweek = ", nweek), "\n")
datH_l   <- Longdf(nweek,"H") %>%                                                                #(no longer) fit data
            Include_dat(., dat, "H") %>% 
            rename(Week=weekH, Date=dateH, Freq=freqH)
if (Option_WriteLong_all==1){
  w0     <- datH_l   %>% write.csv(.,file=paste0(filepart,"_all_long.csv"))                     }#show data

#by_shield
dat      <- read.csv(file=paste0(fileread,"/table2_Hdata_by_shi_ROUNDED.csv")) %>% 
            rename(weekH=Week, dateH=Date, freqHs=Freq, shield=Shield)                          #}#output data
#Long-pivot, FULL including data and 0s (i.e. missing data, no reporting) - no ageg merging
datHs_l  <- Longdf_s(1,9,nweek,"H") %>%                                                          #fit data
            Include_dat_s(., dat, "H", "NOMERGE")  %>%  
            rename(Week=weekH, Date=dateH, Freq=freqHs, Shield=shield)
if (Option_WriteLong_shi==1){
  w0     <- datHs_l    %>% write.csv(.,file=paste0(filepart,"_by_shi_long.csv"))                }#show data

#shi_by_age
dat      <- read.csv(file=paste0(fileread,"/table3_Hdata_s_by_age_ROUNDED.csv")) %>% 
            rename(weekH=Week, dateH=Date, freqHas=Freq, ageg=Ageg, shield=Shield)               #}#output data
#Long-pivot, FULL including data and 0s (i.e. missing data, no reporting) - no ageg merging
datHas_l <- Longdf_as(1,9,nweek,"H") %>%                                                         #fit data
            Include_dat_as(., dat, "H", "NOMERGE")  %>%  
            rename(Week=weekH, Date=dateH, Freq=freqHas, Ageg=ageg2, Shield=shield)
if (Option_WriteLong_sag==1){
  w0     <- datHas_l   %>% write.csv(.,file=paste0(filepart,"_s_by_age_long.csv"))              }#show data


### DATA 3 Deaths in Hospital by week, age, and shielding ######################
filepart = "DHdata"
#all
dat     <- read.csv(file=paste0(fileread,"/table4_DHdata_all_ROUNDED.csv")) %>% 
           rename(weekD=Week, dateD=Date, freqD=Freq)                                          #}#output data
#Long-pivot, FULL including data and 0s (i.e. missing data, no reporting) - no ageg merging
datDH_l <- Longdf(nweek,"D") %>%
           Include_dat(., dat, "D") %>% 
           rename(Week=weekD, Date=dateD, Freq=freqD)                                            #fit data
if (Option_WriteLong_all==1){
  w0     <- datDH_l    %>% write.csv(.,file=paste0(filepart,"_all_long.csv"))                     }#show data

#by_shield
dat      <- read.csv(file=paste0(fileread,"/table5_DHdata_by_shi_ROUNDED.csv")) %>% 
            rename(weekD=Week, dateD=Date, freqDs=Freq, shield=Shield)                         #}#output data
#Long-pivot, FULL including data and 0s (i.e. missing data, no reporting) - no ageg merging
datDHs_l <- Longdf_s(1,9,nweek,"D") %>%                                                          #fit data
            Include_dat_s(., dat, "D", "NOMERGE")  %>% 
            rename(Week=weekD, Date=dateD, Freq=freqDs, Shield=shield)
if (Option_WriteLong_shi==1){
  w0     <- datDHs_l %>% write.csv(.,file=paste0(filepart,"_by_shi_long.csv"))                  }#show data

#shi_by_age
dat      <- read.csv(file=paste0(fileread,"/table6_DHdata_s_by_age_ROUNDED.csv")) %>% 
            rename(weekD=Week, dateD=Date, freqDas=Freq, ageg=Ageg, shield=Shield)               #}#output data
#Long-pivot, FULL including data and 0s (i.e. missing data, no reporting) - no ageg merging
datDHas_l<- Longdf_as(1,9,nweek,"D") %>%
            Include_dat_as(., dat, "D", "NOMERGE") %>%
            rename(Week=weekD, Date=dateD, Freq=freqDas, Ageg=ageg2, Shield=shield)               #fit data
if (Option_WriteLong_sag==1){
 w0      <-datDHas_l %>% write.csv(.,file=paste0(filepart,"_s_by_age_long.csv"))                  }#show data

               
### DATA 4 Deaths outside Hospital by week, age, and shielding #################
filepart = "DOdata"
#all
dat      <- read.csv(file=paste0(fileread,"/table7_DOdata_all_ROUNDED.csv")) %>% 
            rename(weekD=Week, dateD=Date, freqD=Freq)  
#Long-pivot, FULL including data and 0s (i.e. missing data, no reporting) - no ageg merging
datDO_l  <- Longdf(nweek,"D") %>%
            Include_dat(., dat, "D") %>%  
            rename(Week=weekD, Date=dateD, Freq=freqD)                                            #fit data
if (Option_WriteLong_all==1){
w0       <- datDO_l  %>% write.csv(.,file=paste0(filepart,"_all_long.csv"))                      }#show data

#by_shield
dat      <- read.csv(file=paste0(fileread,"/table8_DOdata_by_shi_ROUNDED.csv")) %>% 
            rename(weekD=Week, dateD=Date, freqDs=Freq, shield=Shield)                         #}#output data
#Long-pivot, FULL including data and 0s (i.e. missing data, no reporting) - no ageg merging
datDOs_l <- Longdf_s(1,9,nweek,"D") %>%
            Include_dat_s(., dat, "D", "NOMERGE") %>% 
            rename(Week=weekD, Date=dateD, Freq=freqDs, Shield=shield)
if (Option_WriteLong_shi==1){
  w0     <- datDOs_l %>% write.csv(.,file=paste0(filepart,"_by_shi_long.csv"))                  }#show data

#shi_by_age
dat      <- read.csv(file=paste0(fileread,"/table9_DOdata_s_by_age_ROUNDED.csv")) %>% 
            rename(weekD=Week, dateD=Date, freqDas=Freq, ageg=Ageg, shield=Shield)               #}#output data
#Long-pivot, FULL including data and 0s (i.e. missing data, no reporting) - no ageg merging
datDOas_l<- Longdf_as(1,9,nweek,"D") %>%
            Include_dat_as(., dat, "D", "NOMERGE") %>%
            rename(Week=weekD, Date=dateD, Freq=freqDas, Ageg=ageg2, Shield=shield)                    #fit data
if (Option_WriteLong_sag==1){
w0       <-datDOas_l %>% write.csv(.,file=paste0(filepart,"_s_by_age_long.csv"))                      }#show data

