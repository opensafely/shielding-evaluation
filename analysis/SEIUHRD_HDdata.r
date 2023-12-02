## 040 hospital admissions
## 050 deaths
## merging H, D, Sh for each ID
## see HDdata_test.R for tests on simulated data
##
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(here)
library(lubridate)

library(data.table)
library(magrittr)
library(arrow)
library(glue)

source(here::here("analysis/functions/redaction.R"))

output_dir <- here("output/HDsynthesis")
fs::dir_create(output_dir)

shielding_cohort  <- arrow::read_parquet(file = here::here("output/data_edited.gz.parquet"),
                                          compression = "gzip", compression_level = 5)
#D
dim_sc = dim(shielding_cohort) #500, 89
if (dim_sc[1]>1000){ #only operates on real data, as dummy data has 500 to 1000 rows
  shielding_cohort <- shielding_cohort[           which(shielding_cohort$ons_death_date>="2020-01-01"),]                      #remove deaths prior to 2020 but registered from 2020
  shielding_cohort <- shielding_cohort[which(is.element(shielding_cohort$ons_underlying_cause,c("U071","U072"))),] #remove deaths not caused by covid_deaths_over_time2
}

#H, D, Sh
DAT <- shielding_cohort                                    %>% 
  dplyr::select(patient_id,
                ons_death_date,                                # practice_nuts - dont need
                #shielding,                                    #=flags has had: high, low, none
                shielding_v1_startdate,                        #=date if high risk before 2010-04-21
                shielding_v1_binary,                           #=1    if high risk before 2010-04-21
                age_cat,
                dplyr::contains("hosp_admitted"))          %>%
  pivot_longer(cols = dplyr::contains("hosp_admitted"),        #: cols to pivot into longer format
                names_pattern = "covid_hosp_admitted_(.)",     #: If "names_to" has multiple values, controls how column name is broken up - names_pattern same spec as extract(), a regular expression containing matching groups
                names_to = "covid_admission",                  #: 1-6, new column (character vector) from info/data in cols
                values_to = "admission_date")              %>% #: name of new col from data in cell values. If names_to is a character containing ".value" sentinel, this value will be ignored, and ...
  filter(!(is.na(admission_date) & is.na(ons_death_date))) %>%
  filter(covid_admission %in% c(0,1)|is.na(covid_admission))  %>% # TODO: check is all non-hospital or 1st hospital
  select(-c(covid_admission))                              %>% # no longer need
  filter(!is.na(age_cat))                                  %>% # remove missing age_cat
  mutate(ageg =  as.integer(factor(age_cat)))              %>% # 1:9 <> 0-4 5-11 12-17 18-29 30-39 40-49 50-59 60-69 70+ 
  mutate(age_cat = paste0("'", age_cat))                   %>% #for excel writing
  mutate(shield1 = as.numeric(min(c(admission_date, ons_death_date), na.rm = T) > shielding_v1_startdate)) %>%
  mutate(shield1 = replace_na(0)) %>%
  #mutate(shield2 = as.numeric(shielding=="High Risk", na.rm = T)) %>%
  #mutate(shield2 = replace_na(0)) %>%
  #TODO: put back shield2 - when DAT can be bigger - see AH re 24/4/2020
  # drop_na()    #Not yet for H and D (can have in admission_date, ons_death, shielding_v1_startdate)
   ungroup()



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
  #arrange(dateH)  %>%  #arrange(dateD)



########Week frequencies
frqs <- function(data, vars, name){
  data %>% 
    group_by(across({{vars}}))    %>%
    mutate({{name}} := n())  %>% #     }
    ungroup() }
	#.group = "keep" or "drop" ?



########H & D aggregated separately - unlinked at patient level
HDaggregated=1;
if(HDaggregated==1){ #} & dim_sc[1]>1000){
#DATw   <- DAT %>% filter(!is.na(admission_date)) %>% frqs(c(weekH), "weekfreqH") %>% frqs(c(weekD), "weekfreqD") %>% ungroup()
#DATaw  <- DAT %>% filter(!is.na(admission_date)) %>% frqs(c(weekH, age_cat), "weekfreqHa") %>% frqs(c(weekD, age_cat), "weekfreqDa")
  
###Weekly by age and shielding #frqs=count unique pair of values (week,age, shield)
#DATasw <- DAT %>% filter(!is.na(admission_date)) %>% frqs(c(weekH, age_cat, shielding_v1_binary), "weekfreqHas") %>% 
#Aggregate H (NA D or not), and D (NA H or not)

  DAT %>% select(-c(patient_id, ons_death_date, shielding_v1_startdate)) %>% arrange(age_cat) %>% print(n=20)

DATasw <- DAT %>% frqs(c(weekH, age_cat, shielding_v1_binary), "weekfreqHas") %>% 
                  #ungroup() %>%
                  frqs(c(weekD, age_cat, shielding_v1_binary), "weekfreqDas") %>% 
                  #ungroup() %>%
                  select(-c(patient_id, ons_death_date, admission_date, shielding_v1_startdate)) %>%
                  relocate(shielding_v1_binary,.after=shield1) %>%
                  arrange(ageg)


#csv file (weekH, freqH, freqD, weekD) for each age group with at least one patient
#file=paste0(output_dir,"/","HDdata_a",i,".csv"))
for (i in 1:9){
##A) write if ageg contains data - project.yaml may not work on dummy data
#  DATa <- DATasw %>% group_by(ageg) %>% mutate(na=n()) %>% filter(ageg==i)
#  if (dim(DATa)[1]>0) write.csv(DATa, file=paste0(output_dir,"/","HDdata_a",i,".csv")) }
##B) write regardless of ageg containing data - project.yaml always works
  DATa <- DATasw %>% group_by(ageg) %>% mutate(na=n()) %>% filter(ageg==i) %>% 
                     write.csv(file=paste0(output_dir,"/","HDdata_a",i,".csv"))}

#csv file (weekH, freqH, freqD, weekD) - columns for every age group
#file=paste0(output_dir,"/","HDdata_a1to9.csv")
#TODO: stratify by shield
d2<-DATasw %>% group_by(ageg) %>% mutate(na=n()) #d2[,]<-NA
v=rep(NA,max(d2$na))
d2<-tibble(
  weekH1=v,freqH1=v, weekD1=v,freqD1=v,
  weekH2=v,freqH2=v, weekD2=v,freqD2=v,
  weekH3=v,freqH3=v, weekD3=v,freqD3=v,
  weekH4=v,freqH4=v, weekD4=v,freqD4=v,
  weekH5=v,freqH5=v, weekD5=v,freqD5=v,
  weekH6=v,freqH6=v, weekD6=v,freqD6=v,
  weekH7=v,freqH7=v, weekD7=v,freqD7=v,
  weekH8=v,freqH8=v, weekD8=v,freqD8=v,
  weekH9=v,freqH9=v, weekD9=v,freqD9=v)

#dwH = !is.na(DATasw$weekH)
#dwD = !is.na(DATasw$weekD)
#for (i in 1:9){
#  c4=4*i; c3=c4-1; c2=c4-2; c1=c4-3; 
#  dai = DATasw$ageg==i
#  ldai = sum(dai)
#  ldaidwH = sum(dai & dwH)
#  ldaidwD = sum(dai & dwD)
#  if(ldai>0){
#    if(ldaidwH>0){
#  d2[1:ldaidwH,c1] = DATasw$weekH      [dai & dwH]
#  d2[1:ldaidwH,c2] = DATasw$weekfreqHas[dai & dwH]}
#    if(ldaidwD>0){
#  d2[1:ldaidwD,c3] = DATasw$weekD      [dai & dwD]
#  d2[1:ldaidwD,c4] = DATasw$weekfreqDas[dai & dwD]}  } }

#v2
#dwH = !is.na(DATasw$weekH)
#dwD = !is.na(DATasw$weekD)
#for (i in 1:9){
#  c4=4*i; c3=c4-1; c2=c4-2; c1=c4-3; 
#  dai = DATasw$ageg==i
#  ldai = sum(dai,na.rm =T)
#  ldaidwH = sum(dai & dwH,na.rm =T)
#  ldaidwD = sum(dai & dwD,na.rm =T)
#  if(ldai>0){
#    if(ldaidwH>0){
#      d2[1:ldaidwH,c1] = DATasw$weekH      [dai & dwH]
#      d2[1:ldaidwH,c2] = DATasw$weekfreqHas[dai & dwH]}
#    if(ldaidwD>0){
#      d2[1:ldaidwD,c3] = DATasw$weekD      [dai & dwD]
#      d2[1:ldaidwD,c4] = DATasw$weekfreqDas[dai & dwD]}  } }
#
#
#write.csv(d2, file=paste0(output_dir,"/","HDdata_a1to9.csv"))
#
} #HDAggretated


########H & D branches: H0, HD, 0D subsets - linked at patient level
#=> no NA to remove in all vars needed, i.e.
#   -H in Dh; H,D in Dhd; D in Dd
#   -shield1, shild2 indices in DAT

Dh  <- subset(DAT,!is.na(weekH) &  is.na(weekD))                 %>% 
      select(-c("ons_death_date", "weekD","dateD"))
Dhd <- subset(DAT,!is.na(weekH) & !is.na(weekD) & weekH < weekD) 
Dd  <- subset(DAT, is.na(weekH) & !is.na(weekD))                 %>% 
      select(-c("admission_date", "weekH", "dateH"))


###Weekly overall
###don't need to use in plots
#Dh     <- Dh  %>% frqs(c(weekH), "weekfreqH")
#Dhd    <- Dhd %>% frqs(c(weekH), "weekfreqH") %>% frqs(c(weekD), "weekfreqD")
#Dd     <- Dd  %>% frqs(c(weekD), "weekfreqD")

###Weekly by age
###don't need to use in plots
#Dha     <- Dh  %>% frqs(c(weekH, age_cat), "weekfreqHa")
#Dhda    <- Dhd %>% frqs(c(weekH, age_cat), "weekfreqHa") %>% frqs(c(weekD, age_cat), "weekfreqDa")
#Dda     <- Dd  %>% frqs(c(weekD, age_cat), "weekfreqDa")

###Weekly by age and shielding
Dhas    <- Dh  %>% frqs(c(weekH, age_cat, shield1), "weekfreqHas") #%>% ungroup()
Dhdas   <- Dhd %>% frqs(c(weekH, age_cat, shield1), "weekfreqHas") %>% 
                   frqs(c(weekD, age_cat, shield1), "weekfreqDas") #%>% ungroup()
Ddas    <- Dd  %>% frqs(c(weekD, age_cat, shield1), "weekfreqDas") #%>% ungroup()


##PLOTS WEEKLY
#Figures H, D - overlapping groups
fig0 <- function(data, x , y, col, xname='Date', yname='Weekly admissions') { #yname='Weekly deaths'
  p0 <- ggplot(data, aes(x = {{x}}, y = {{y}}, col = {{col}})) +
    geom_line() +     geom_point(size = 1.2, pch = 1) +    labs(x = xname, y = yname) +    ylim(c(0, NA)) +
    theme_bw() 
  print(p0)} #for when sourcing the code
#Figures H, D - panels for groups
fig <- function(data, x , y, col, facets, xname='Date', yname='Weekly admissions') { #yname='Weekly deaths'
  p <- ggplot(data, aes(x = {{x}}, y = {{y}}, col = {{col}})) +
    geom_line() +     geom_point(size = 1.2, pch = 1) +    facet_wrap(facets, ncol = 1, scales = 'free_y') +
    labs(x = xname, y = yname) +    ylim(c(0, NA)) +    theme_bw() 
  print(p)} #for when sourcing the code


##NOTE: plots similar - Dh - Dha - Dhas - when "col = NULL" or absent
##NOTE: plots similar - Dh - Dha - Dhas - when "col = age_cat" or "shield"
##NOTE: fig0() (overlap) - for now - better than panels - unless for H & D
##TODO: plot by age for shiled1=0 or 1


#file = paste0(output_dir,"/","HDdata_subsets.pdf")
pdf(file = paste0(output_dir,"/","HDdata_subsets.pdf"), width = 8, height = 6)

##All
#fig(Dhas,  dateH, weekfreqH,   facets ='NULL', xname ='Date', yname ='Weekly admissions with recovery')
fig0(Dhas, dateH, weekfreqHas, col = NULL,     xname ='Date', yname ='Weekly admissions with recovery')
#by age_cat - overlap
fig0(Dhas, dateH, weekfreqHas, col = age_cat,  xname ='Date', yname ='Weekly admissions with recovery')
#by shield - overlap
fig0(Dhas, dateH, weekfreqHas, col = shield1,  xname ='Date', yname ='Weekly admissions with recovery')

#TODO: overlap H and D - extend function?
##All H
fig0(Dhdas, dateH, weekfreqHas, col = NULL,     xname ='Date', yname ='Weekly admissions')
#by age_cat - overlap
fig0(Dhdas, dateH, weekfreqHas, col = age_cat,  xname ='Date', yname ='Weekly admissions')
#by shield - overlap
fig0(Dhdas, dateH, weekfreqHas, col = shield1,  xname ='Date', yname ='Weekly admissions')
##All D
fig0(Dhdas, dateD, weekfreqDas, col = NULL,     xname ='Date', yname ='Weekly deaths after admission')
#by age_cat - overlap
fig0(Dhdas, dateD, weekfreqDas, col = age_cat,  xname ='Date', yname ='Weekly deaths after admission')
#by shield - overlap
fig0(Dhdas, dateD, weekfreqDas, col = shield1,  xname ='Date', yname ='Weekly deaths after admission')

##All
fig0(Ddas, dateD, weekfreqDas, col = NULL,     xname ='Date', yname ='Weekly deaths outside hospital')
#by age_cat - overlap
fig0(Ddas, dateD, weekfreqDas, col = age_cat,  xname ='Date', yname ='Weekly deaths outside hospital')
#by shield - overlap
fig0(Ddas, dateD, weekfreqDas, col = shield1,  xname ='Date', yname ='Weekly deaths outside hospital')

dev.off()



##### Data & metadata for _fit
#TODO: here or _fit: build age-spec vectors: weekX, weekfreqX, datX

#data limits
WeekLim_Dhd_H = range( Dhdas$dateH) #range(DATasw$dateH) #range(DATasw$weekH) 
WeekLim_Dhd_D = range( Dhdas$dateD) #range(DATasw$dateD) #range(DATasw$weekD)
WeekLim_Dh_H  = range(  Dhas$dateH)  
WeekLim_Dd_D  = range(  Ddas$dateD)
n_Dhd_H       = length(Dhdas$dateH) #length(DATasw$dateH)
n_Dhd_D       = length(Dhdas$dateD) #length(DATasw$dateD)
n_Dh_H        = length( Dhas$dateH) 
n_Dd_D        = length( Ddas$dateD) 

#print
print(paste0("Dhd H weeks: ", WeekLim_Dhd_H[1],", ", WeekLim_Dhd_H[2],", #pts ", n_Dhd_H))
print(paste0("Dhd D weeks: ", WeekLim_Dhd_D[1],", ", WeekLim_Dhd_D[2],", #pts ", n_Dhd_D))
print(paste0("Dh  H weeks: ", WeekLim_Dh_H[1], ", ", WeekLim_Dh_H[2],", #pts ", n_Dh_H))
print(paste0("Dd  D weeks: ", WeekLim_Dd_D[1], ", ", WeekLim_Dd_D[2],", #pts ", n_Dd_D))

#data.frames
datDhd<- Dhdas                               %>% 
  select(weekH, weekD, weekfreqHas, weekfreqDas, dateH, dateD, ageg, age_cat, shield1, shielding_v1_binary)  %>%  #age_cat bec grouped for plots
  distinct                                   %>%
  mutate(Week1OfDataH = WeekLim_Dhd_H[1], 
         Week2OfDataH = WeekLim_Dhd_H[2],
         Week1OfDataD = WeekLim_Dhd_D[1], 
         Week2OfDataD = WeekLim_Dhd_D[2])    %>% 
  rename(WeeksH=weekH, WeeksD=weekD, Dataz=weekfreqHas, Dataw=weekfreqDas, DatesH=dateH, DatesD=dateD)

datDh <- Dhas                                %>% 
  select(weekH, weekfreqHas, dateH, ageg, age_cat, shield1, shielding_v1_binary)  %>%   
  distinct                                   %>%
  mutate(Week1OfData = WeekLim_Dh_H[1], 
         Week2OfData = WeekLim_Dh_H[2])      %>% 
  rename(Weeks=weekH, Dataz=weekfreqHas, Dates=dateH)

datDd <- Ddas                                %>% 
  select(weekD, weekfreqDas, dateD, ageg, age_cat, shield1, shielding_v1_binary)  %>%  
  distinct                                   %>%
  mutate(Week1OfData = WeekLim_Dd_D[1],
         Week2OfData = WeekLim_Dd_D[2])      %>% 
         rename(Weeks=weekD, Dataw=weekfreqDas, Dates=dateD)

#csv files
#file=paste0(output_dir,"/","datDh.csv")
#file=paste0(output_dir,"/","datDhd.csv")
#file=paste0(output_dir,"/","datDd.csv")
datDh  %>% write.csv(file=paste0(output_dir,"/","datDh.csv"))
datDhd %>% write.csv(file=paste0(output_dir,"/","datDhd.csv"))
datDd  %>% write.csv(file=paste0(output_dir,"/","datDd.csv"))


#plots
#H
ph   <- ggplot(datDh, aes(x = Weeks)) +
        geom_point(aes(y = Dataz)) +
        labs(x = 'Weeks', y = 'Weekly admissions with recovery') 
#D
pd   <- ggplot(datDd, aes(x = Weeks)) +
        geom_point(aes(y = Dataw)) +
        labs(x = 'Weeks', y = 'Weekly deaths outside hospital')
#HD
phd1 <- ggplot(datDhd, aes(x = WeeksH)) +
        geom_point(aes(y = Dataz)) +
        labs(x = 'Weeks', y = 'Weekly admissions')
#HD
phd2 <- ggplot(datDhd, aes(x = WeeksD)) +
        geom_point(aes(y = Dataw)) +
        labs(x = 'Weeks', y = 'Weekly deaths after admission')

#file = paste0(output_dir,"/","HDdata_Incidence.pdf"
pdf(file = paste0(output_dir,"/","HDdata_Incidence.pdf"))
      gridExtra::grid.arrange(ph, pd, phd1, phd2, nrow = 2, ncol=2)
dev.off()

#if (pset$iplatform==1) { 
#      gridExtra::grid.arrange(ph, pd, phd1, phd2, nrow = 2, ncol=2) }



###Summary of Totals
#file = paste0(output_dir, "/HDdata_summary.txt")
#TODO: redo by age
#TODO: redo with shield2
#Total severe events
Total_H0 = sum(!is.na(Dh$dateH))
Total_HD = sum(!is.na(Dhd$dateH))
Total_0D = sum(!is.na(Dd$dateD))
m_fracHD = round(Total_HD/(Total_HD + Total_H0),3)
pmortout = round(Total_0D/(Total_HD + Total_0D),3)

Total_H0s = sum(!is.na(Dhas$dateH[which(Dhas$shield1==1)]))
Total_HDs = sum(!is.na(Dhdas$dateH[which(Dhdas$shield1==1)]))
Total_0Ds = sum(!is.na(Ddas$dateD[which(Ddas$shield1==1)]))
m_fracHDs = round(Total_HDs/(Total_HDs + Total_H0s),3)
pmortouts = round(Total_0Ds/(Total_HDs + Total_0Ds),3)

Total_H0n = sum(!is.na(Dhas$dateH[which(Dhas$shield1==0)]))
Total_HDn = sum(!is.na(Dhdas$dateH[which(Dhdas$shield1==0)]))
Total_0Dn = sum(!is.na(Ddas$dateD[which(Ddas$shield1==0)]))
m_fracHDn = round(Total_HDn/(Total_HDn + Total_H0n),3)
pmortoutn = round(Total_0Dn/(Total_HDn + Total_0Dn),3)

sink(file = paste0(output_dir, "/HDdata_summary.txt"),append=F,split=F)
cat("\n")
print(paste0("Dhd H weeks: ", WeekLim_Dhd_H[1],", ", WeekLim_Dhd_H[2],", #pts ", n_Dhd_H))
print(paste0("Dhd D weeks: ", WeekLim_Dhd_D[1],", ", WeekLim_Dhd_D[2],", #pts ", n_Dhd_D))
print(paste0("Dh  H weeks: ", WeekLim_Dh_H[1], ", ", WeekLim_Dh_H[2],", #pts ", n_Dh_H))
print(paste0("Dd  D weeks: ", WeekLim_Dd_D[1], ", ", WeekLim_Dd_D[2],", #pts ", n_Dd_D))

cat("\n")
print(paste0("Overall"))
print(paste0("Total_H0 = ", Total_H0 ))
print(paste0("Total_HD = ", Total_HD ))
print(paste0("Total_0D = ", Total_0D ))
print(paste0("Prob of mortality in hospital = ", m_fracHD ))
print(paste0("Fraction of deaths in community = ", pmortout ))
cat("\n")
print(paste0("Shielding"))
print(paste0("Total_H0s = ", Total_H0s))
print(paste0("Total_HDs = ", Total_HDs))
print(paste0("Total_0Ds = ", Total_0Ds))
print(paste0("Prob of mortality in hospital = ",   m_fracHDs))
print(paste0("Fraction of deaths in community = ", pmortouts))
cat("\n")
print(paste0("Not shielding"))
print(paste0("Total_H0n = ", Total_H0n))
print(paste0("Total_HDn = ", Total_HDn))
print(paste0("Total_0Dn = ", Total_0Dn))
print(paste0("Prob of mortality in hospital = ",   m_fracHDn))
print(paste0("Fraction of deaths in community = ", pmortoutn))
cat("\n")
sink()



