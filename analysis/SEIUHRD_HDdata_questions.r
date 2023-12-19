## Dataset general questions
## Merging H, D, Shielding for each ID
## see HDdata_test.R for tests on simulated data
##
library(arrow)
library(here)
library(lubridate)
library(gridExtra)


source(here::here("analysis/functions/redaction.R"))

output_dir <- here("output/HDsynthesis")
fs::dir_create(output_dir)

DAT0  <- arrow::read_parquet(file = here::here("output/data_edited.gz.parquet"),
                                          compression = "gzip", compression_level = 5)
#D
#Keep while testing?
dim_sc = dim(DAT0) #500, 89
if (dim_sc[1]>1000){ #only operates on real data, as dummy data has 500 to 1000 rows
  DAT0 <- DAT0[           which(DAT0$ons_death_date>="2020-01-01") | is.na(DAT0$ons_death_date),]                      #remove deaths prior to 2020 but registered from 2020
  DAT0 <- DAT0[which(is.element(DAT0$ons_underlying_cause,c("U071","U072")) | is.na(DAT0$ons_underlying_cause)),] #remove deaths not caused by covid_deaths_over_time2
}

#DAT0 <- dataset #No filtering yet

#TODO:
#up to 2020-12-01
#apply nec filters

FILTER=0
if(FILTER==1){
#First filtering
DAT0 <- DAT0                                                %>%
  ###patient id/age, care home, shielding, hosp/death 
  dplyr::select(patient_id,
                age_cat,                                       # factor:  0-4 5-11 12-17 18-29 30-39 40-49 50-59 60-69 70+
                care_home, care_home_nursing,                  # logical: T/F - resident in care home
                all_covid_hosp,                                # number:  0:n, hopitalisation count for patient
                covid_hosp_cat,                                # factor:  0:2, 3+  <= "all_covid_hosp" #dataset.all_covid_hosp = all_covid_hosp \ .count_for_patient()
                dplyr::contains("hosp_admitted"),              # date,    "covid_hosp_admitted_{n}",
                ons_death_date,                                # date,    covid death
                shielding,                                     # factor:  High Risk, Low/Moderate risk, No shielding
                shielding_v1_startdate,                        # date,    High Risk before 2020-04-21 - NA if binary=0
                shielding_v1_binary,                           # logical: T/F,  High Risk before 2020-04-21
                hirisk_shield_count)                       %>% # number:  0:n, high risk flags per patient
  ###age groups
  filter(!is.na(age_cat))                                      # remove patients with missing 'age_cat'
}

#use ?
#covid_hosp_cat ~ "COVID-19 hospitalisations per person (n)",


######## Answers in text
sink(file = paste0(output_dir, "/JDat3_HDdata_questions_answered.txt"),append=F,split=F)
cat("\n")
print(paste0("dataset rows ", dim(DAT0)[1], " and columns ", dim(DAT0)[2] ))
cat("\n")
print(paste0("Number of patient entries ", sum(!is.na(DAT0$patient_id)) ))
print(paste0("Number of unique patients (if not as above) ", length(unique(DAT0$patient_id)) ))
print(paste0("Number of missing patient id ", sum(is.na(DAT0$patient_id)) ))
cat("\n")
print(paste0("Number of patients ever hospitalised (i.e. at least once): "))
print(paste0( sum(!is.na(DAT0$covid_hosp_admitted_1)) ))
print(paste0( length(which(DAT0$all_covid_hosp>0)) ))
cat("\n")

print(paste0("Number of hospitalisations (inc re-admissions: "))
print(paste0( sum( !is.na(DAT0$covid_hosp_admitted_1) | !is.na(DAT0$covid_hosp_admitted_2) | 
                   !is.na(DAT0$covid_hosp_admitted_3) | !is.na(DAT0$covid_hosp_admitted_4) | 
                   !is.na(DAT0$covid_hosp_admitted_5) | !is.na(DAT0$covid_hosp_admitted_6) ) ))
print(paste0( sum(DAT0$all_covid_hosp,na.rm = T) ))
cat("\n")

print(paste0("Number of patients hospitalised more than once: "))
print(paste0( sum(!is.na(DAT0$covid_hosp_admitted_1) & !is.na(DAT0$covid_hosp_admitted_2)) ))
print(paste0( length(which(DAT0$all_covid_hosp>1)) ))
cat("\n")
print(paste0("Number of patients hospitalised 1x: "))
print(paste0( sum(!is.na(DAT0$covid_hosp_admitted_1) & is.na(DAT0$covid_hosp_admitted_2)) ))
print(paste0( length(which(DAT0$all_covid_hosp==1)) ))
print(paste0("Number of patients hospitalised 2x: ")) 
print(paste0( sum(!is.na(DAT0$covid_hosp_admitted_2) & is.na(DAT0$covid_hosp_admitted_3)) ))
print(paste0( length(which(DAT0$all_covid_hosp==2)) ))
print(paste0("Number of patients hospitalised 3x: ")) 
print(paste0( sum(!is.na(DAT0$covid_hosp_admitted_3) & is.na(DAT0$covid_hosp_admitted_4)) ))
print(paste0( length(which(DAT0$all_covid_hosp==3)) ))
print(paste0("Number of patients hospitalised 4x: ")) 
print(paste0( sum(!is.na(DAT0$covid_hosp_admitted_4) & is.na(DAT0$covid_hosp_admitted_5)) ))
print(paste0( length(which(DAT0$all_covid_hosp==4)) ))
print(paste0("Number of patients hospitalised 5x: "))
print(paste0( sum(!is.na(DAT0$covid_hosp_admitted_5) & is.na(DAT0$covid_hosp_admitted_6)) ))
print(paste0( length(which(DAT0$all_covid_hosp==5)) ))
print(paste0("Number of patients hospitalised 6x: "))
print(paste0( sum(!is.na(DAT0$covid_hosp_admitted_6)) ))
print(paste0( length(which(DAT0$all_covid_hosp==6)) ))
cat("\n")
print(paste0("Number of patients that died: "))
print(paste0( sum(!is.na(DAT0$ons_death_date)) ))
cat("\n")
print(paste0("Number of patients that died and were hospitalised: "))
print(paste0( sum(!is.na(DAT0$ons_death_date) & !is.na(DAT0$covid_hosp_admitted_1)) ))
print(paste0( sum(!is.na(DAT0$ons_death_date) & DAT0$all_covid_hosp>0) ))
cat("\n")
print(paste0("Number of patients that died outside hospital: "))
print(paste0( sum(  !is.na(DAT0$ons_death_date) & is.na(DAT0$covid_hosp_admitted_1)) ))
print(paste0( sum(!is.na(DAT0$ons_death_date) & (DAT0$all_covid_hosp==0 | is.na(DAT0$all_covid_hosp)) ) ))
cat("\n")
print(paste0("Number of patients that died outside hospital in carehomes: "))
print(paste0( sum(  !is.na(DAT0$ons_death_date) & is.na(DAT0$covid_hosp_admitted_1) 
  & (DAT0$care_home==TRUE | DAT0$care_home_nursing==TRUE)) ))
print(paste0( sum(  !is.na(DAT0$ons_death_date) & (DAT0$all_covid_hosp==0 | is.na(DAT0$all_covid_hosp)) 
  & (DAT0$care_home==TRUE | DAT0$care_home_nursing==TRUE)) ))
cat("\n")
print(paste0("Number of patients in carehomes: "))
print(paste0( sum(DAT0$care_home==TRUE | DAT0$care_home_nursing==TRUE, na.rm = T) ))
print(paste0("Number of patients not in carehomes: "))
print(paste0(sum(!(DAT0$care_home==TRUE | DAT0$care_home_nursing==TRUE)) ))
cat("\n")
print("High risk flag questions...")
print("Shielding questions...")
cat("\n")
sink()

######## Answers in pdf
pdf(file = paste0(output_dir,"/JDat3_HDdata_questions_answered.pdf")) #, height=)
txt=readLines(paste0(output_dir,"/JDat3_HDdata_questions_answered.txt"))
plot.new()
gridExtra::grid.table(txt, theme=ttheme_default(base_size = 7, padding = unit(c(1, 1),"mm") ))
dev.off()


