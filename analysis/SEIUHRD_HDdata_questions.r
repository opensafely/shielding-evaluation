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

DAT  <- arrow::read_parquet(file = here::here("output/data_edited.gz.parquet"),
                                          compression = "gzip", compression_level = 5)
#D
dim_sc = dim(DAT) #500, 89
if (dim_sc[1]>1000){ #only operates on real data, as dummy data has 500 to 1000 rows
  DAT <- DAT[           which(DAT$ons_death_date>="2020-01-01") | is.na(DAT$ons_death_date),]                      #remove deaths prior to 2020 but registered from 2020
  DAT <- DAT[which(is.element(DAT$ons_underlying_cause,c("U071","U072")) | is.na(DAT$ons_underlying_cause)),] #remove deaths not caused by covid_deaths_over_time2
}


### First filtering - keeping carehomes and multiple hopsitalisations############

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
  filter(shielding_v1_startdate <= "2020-12-01" | is.na(shielding_v1_startdate )) #%>%

####other: dates
#           covid_hosp_admitted_{n}    <= admission_date
#           covid_hosp_discharge_{n}   <= discharge_date
#           But, is enough the count 'all_covid_hosp' (total hospitalisations per patient)
#####other: comorbidities_factor       <= comorbid_count - number of comorbidities converted to factor (0,1,2+)
#           comorbid_count             - 0:9
#           But, High risk flags should be enough
#####other: total_primarycare_covid    - count for patient
#           covid_primary_cat          <= total_primarycare_covid - number of covid records as factor (0-5+)

names1 = names(DAT)

jobno = "JDat4_"
filename1a = "HDdata_questions_answered_Hospital_wCH"
filename2a = "HDdata_questions_answered_Deaths_wCH"
filename3a = "HDdata_questions_answered_Shielding_wCH"
filename1b = "HDdata_questions_answered_Hospital"
filename2b = "HDdata_questions_answered_Deaths"
filename3b = "HDdata_questions_answered_Shielding"


######## QUESTIONS with CH & multiple H ########################################

######## Answers in text
sink(file = paste0(output_dir, "/", jobno, filename1a, ".txt"),append=F,split=F)
cat("\n")
print(paste0("Dataset rows ", dim(DAT)[1], " and columns ", dim(DAT)[2] ))
nP_HorD    = sum(!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0)
nP_noHnoD  = sum( is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) )
nP_CH_HorD = sum( (!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0)
                    & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE))
nP_CH_noHnoD = sum( is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp))
                    & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE))
print(paste0("Patients with hospitalisation or death events: ",    nP_HorD))
print(paste0("Patients in CH with hospitalisation or death events: ", nP_CH_HorD))
print(paste0("Patients without hospitalisation or death events: ", nP_noHnoD))

cat("\n")
cat("Patients \n")
nP = sum(!is.na(DAT$patient_id), na.rm = T)
nP_00 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="0-4", na.rm = T), 3)
nP_05 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="5-11", na.rm = T), 3)
nP_12 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="12-17", na.rm = T), 3)
nP_18 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="18-29", na.rm = T), 3)
nP_30 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="30-39", na.rm = T), 3)
nP_40 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="40-49", na.rm = T), 3)
nP_50 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="50-59", na.rm = T), 3)
nP_60 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="60-69", na.rm = T), 3)
nP_70 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="70+", na.rm = T), 3)
print(paste0("Patient entries ", nP ))
print(paste0("Unique patients (if not as above) ", length(unique(DAT$patient_id)) )) #=> row <> one patient
print(paste0("Missing patient id ", sum(is.na(DAT$patient_id)) ))
print(paste0("Patients proportion age 0-4:   ", round(nP_00/nP,3) )); 
print(paste0("Patients proportion age 5-11:  ", round(nP_05/nP,3) ));
print(paste0("Patients proportion age 12-17: ", round(nP_12/nP,3) ));
print(paste0("Patients proportion age 18-29: ", round(nP_18/nP,3) ));
print(paste0("Patients proportion age 30-39: ", round(nP_30/nP,3) ));
print(paste0("Patients proportion age 40-49: ", round(nP_40/nP,3) ));
print(paste0("Patients proportion age 50-59: ", round(nP_50/nP,3) ));
print(paste0("Patients proportion age 60-69: ", round(nP_60/nP,3) ));
print(paste0("Patients proportion age 70+:   ", round(nP_70/nP,3) ));

cat("\n")
cat("Care home patients \n")
nP_CH  = sum( (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T)
nP_NCH = sum(!(DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T)
nP_CH60   = sum( DAT$age_cat=="60-69" &  (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T) 
nP_CH70   = sum( DAT$age_cat=="70+"   &  (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T) 
nP_NCH60  = sum( DAT$age_cat=="60-69" & !(DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T) 
nP_NCH70  = sum( DAT$age_cat=="70+"   & !(DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T) 
print(paste0("Patients in carehomes:               ", nP_CH)); 
print(paste0("Patients not in carehomes:           ", nP_NCH, " or ", nP-nP_CH))
print(paste0("Patients in carehomes age 70+:       ", nP_CH70)); 
print(paste0("Patients in carehomes age 60-69:     ", nP_CH60)); 
print(paste0("Patients not in carehomes age 60-69: ", nP_NCH60, " or ", nP_60-nP_CH60))
print(paste0("Patients not in carehomes age 70+:   ", nP_NCH70, " or ", nP_70-nP_CH70))

cat("\n")
cat("Hospitalised patients \n")
nP_Hosp    = length( which(DAT$all_covid_hosp>0))
nP_HospCH  = length( which(DAT$all_covid_hosp>0 & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE) )) 
nP_HospNCH = nP_Hosp - nP_HospCH
print(paste0("Ever hospitalised (at least once):   ", nP_Hosp))
print(paste0("Ever hospitalised while in carehome: ", nP_HospCH))
print(paste0("Odds hospitalised while in carehome (assuming same exposure): ", round( (nP_HospCH/nP_CH) / (nP_HospNCH/nP_NCH),2) ))

cat("\n")
cat("Hospitalisations (inc re-admissions) generally and in carehomes \n")
nH       = sum(DAT$all_covid_hosp, na.rm = T)
nH_CH    = sum(DAT$all_covid_hosp[which(DAT$care_home==TRUE | DAT$care_home_nursing==TRUE)], na.rm = T) 
nH_70    = sum(DAT$all_covid_hosp[which(DAT$age_cat=="70+")],na.rm=T)
nH_CH70  = sum(DAT$all_covid_hosp[which(DAT$age_cat=="70+" & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE))], na.rm = T) 
nH_NCH70 = nH_70 - nH_CH70
print(paste0("Hospitalisations:                      ", nH ))
print(paste0("Hospitalisations age +70:              ", nH_70 ))
print(paste0("Hospitalisations in carehomes:         ", nH_CH ))
print(paste0("Hospitalisations in carehomes age +70: ", nH_CH70 ))
print(paste0("Odds of hospitalisation in carehomes age 70+ (assuming same exposure): ", round( (nH_CH70/nP_CH70) / (nH_NCH70/nP_NCH70), 2) ))

cat("\n")
cat("Hospitalised patients, inc re-admitted, by age \n")
##TODO: restrict to 1st hospitalisation
nP_Hosp_00  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
print(paste0("Patients hospitalised age 0-4:   ", nP_Hosp_00))
print(paste0("Patients hospitalised age 5-11:  ", nP_Hosp_05))
print(paste0("Patients hospitalised age 12-17: ", nP_Hosp_12))
print(paste0("Patients hospitalised age 18-29: ", nP_Hosp_18))
print(paste0("Patients hospitalised age 30-39: ", nP_Hosp_30))
print(paste0("Patients hospitalised age 40-49: ", nP_Hosp_40))
print(paste0("Patients hospitalised age 50-59: ", nP_Hosp_50))
print(paste0("Patients hospitalised age 60-69: ", nP_Hosp_60))
print(paste0("Patients hospitalised age 70+:   ", nP_Hosp_70))

cat("\n")
cat("Hospitalised patients re-admitted \n")
nP_Hosp1  = length( which(DAT$all_covid_hosp==1))
nP_Hospgt1= length( which(DAT$all_covid_hosp>1))
nP_Hosp2  = length( which(DAT$all_covid_hosp==2))
nP_Hosp3  = length( which(DAT$all_covid_hosp==3))
nP_Hosp4  = length( which(DAT$all_covid_hosp==4))
nP_Hosp5  = length( which(DAT$all_covid_hosp==5))
nP_Hosp6  = length( which(DAT$all_covid_hosp==6))
print(paste0("Hospitalised with 1 admission:  ", nP_Hosp1 ))
print(paste0("Hospitalised more than once:    ", nP_Hospgt1 ))
print(paste0("Hospitalised with 2 admissions: ", nP_Hosp2 ))
print(paste0("Hospitalised with 3 admissions: ", nP_Hosp3 ))
print(paste0("Hospitalised with 4 admissions: ", nP_Hosp4 ))
print(paste0("Hospitalised with 5 admissions: ", nP_Hosp5 ))
print(paste0("Hospitalised with 6 admissions: ", nP_Hosp6 ))

cat("\n")
cat("Hospital re-admissions \n")
print(paste0("Hospitalisations, only 1 admission:  ", 
             sum(DAT$all_covid_hosp[which(DAT$all_covid_hosp==1)], na.rm = T) ))
print(paste0("All hospital re-admissions:          ", 
             sum(DAT$all_covid_hosp[which(DAT$all_covid_hosp>1)], na.rm = T) ))
print(paste0("Hospitalisations up to 2 admissions: ", 
             sum(DAT$all_covid_hosp[which(DAT$all_covid_hosp==2)], na.rm = T) ))
print(paste0("Hospitalisations up to 3 admissions: ", 
             sum(DAT$all_covid_hosp[which(DAT$all_covid_hosp==3)], na.rm = T) ))
print(paste0("Hospitalisations up to 4 admissions: ", 
             sum(DAT$all_covid_hosp[which(DAT$all_covid_hosp==4)], na.rm = T) ))
print(paste0("Hospitalisations up to 5 admissions: ", 
             sum(DAT$all_covid_hosp[which(DAT$all_covid_hosp==5)], na.rm = T) ))
print(paste0("Hospitalisations up to 6 admissions: ", 
             sum(DAT$all_covid_hosp[which(DAT$all_covid_hosp==6)], na.rm = T) ))

cat("\n")
cat("Hospitalised patients in carehomes and re-admitted,  \n")
nP_Hosp1CH  = length( which(DAT$all_covid_hosp==1 & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE) )) 
nP_Hosp2CH  = length( which(DAT$all_covid_hosp==2 & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE) )) 
nP_Hosp3CH  = length( which(DAT$all_covid_hosp==3 & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE) )) 
nP_Hosp4CH  = length( which(DAT$all_covid_hosp==4 & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE) )) 
nP_Hosp5CH  = length( which(DAT$all_covid_hosp==5 & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE) )) 
nP_Hosp6CH  = length( which(DAT$all_covid_hosp==6 & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE) )) 
nP_Hosp1NCH = nP_Hosp1 - nP_Hosp1CH
nP_Hosp2NCH = nP_Hosp2 - nP_Hosp2CH
nP_Hosp3NCH = nP_Hosp3 - nP_Hosp3CH
nP_Hosp4NCH = nP_Hosp4 - nP_Hosp4CH
nP_Hosp5NCH = nP_Hosp5 - nP_Hosp5CH
nP_Hosp6NCH = nP_Hosp6 - nP_Hosp6CH
print(paste0("Odds of 1st admission in carehomes (assuming same exposure): ", 
             round( (nP_Hosp1CH/nP_CH) / (nP_Hosp1NCH/nP_NCH), 2) ))
print(paste0("Odds of 2nd admission in carehomes (idem):                   ", 
             round( (nP_Hosp2CH/nP_CH) / (nP_Hosp2NCH/nP_NCH), 2) ))
print(paste0("Odds of 3rd admission in carehomes (idem):                   ", 
             round( (nP_Hosp3CH/nP_CH) / (nP_Hosp3NCH/nP_NCH), 2) ))
print(paste0("Odds of 4th admission in carehomes (idem):                   ",
             round( (nP_Hosp4CH/nP_CH) / (nP_Hosp4NCH/nP_NCH), 2) ))
print(paste0("Odds of 5th admission in carehomes (idem):                   ", 
             round( (nP_Hosp5CH/nP_CH) / (nP_Hosp5NCH/nP_NCH), 2) ))
print(paste0("Odds of 6th admission in carehomes (idem):                   ",
             round( (nP_Hosp6CH/nP_CH) / (nP_Hosp6NCH/nP_NCH), 2) ))

sink()



sink(file = paste0(output_dir, "/", jobno, filename2a, ".txt"),append=F,split=F)
cat("\n")
cat("Deaths \n")
nP_D   = sum(!is.na(DAT$ons_death_date))
print(paste0("Patients that died: ", nP_D))

cat("\n")
cat("Deaths in hospital \n")
nP_DH  = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0, na.rm = T)
nP_RH  = nP_Hosp - nP_DH
nP_DH1 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==1, na.rm = T)
nP_DH2 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==2, na.rm = T)
nP_DH3 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==3, na.rm = T)
nP_DH4 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==4, na.rm = T)
nP_DH5 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==5, na.rm = T)
nP_DH6 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==6, na.rm = T)
print(paste0("Patients that died:        ", nP_DH, ", mfraction   ", round(nP_DH/nP_Hosp,3) ))
print(paste0("Patients that recovered:   ", nP_RH, ", 1-mfraction ", round(nP_RH/nP_Hosp,3) ))
print(paste0("Deaths upon 1st admission: ", nP_DH1))
print(paste0("Deaths upon 2nd admission: ", nP_DH2))
print(paste0("Deaths upon 3rd admission: ", nP_DH3))
print(paste0("Deaths upon 4th admission: ", nP_DH4))
print(paste0("Deaths upon 5th admission: ", nP_DH5))
print(paste0("Deaths upon 6th admission: ", nP_DH6))

cat("\n")
cat("Deaths in hospital by age (inc re-admissions) \n")
##TODO: exclude carehomes ?
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
cat("Deaths in carehomes \n")
nP_DCH    = sum(!is.na(DAT$ons_death_date) & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T)
nP_DNCH   = nP_D -nP_DCH
nP_DCH70  = sum(!is.na(DAT$ons_death_date) & DAT$age_cat=="70+" &  (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T)
nP_DNCH70 = sum(!is.na(DAT$ons_death_date) & DAT$age_cat=="70+" & !(DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T)
print(paste0("Patients in carehomes that died:           ", nP_DCH))
print(paste0("Proportion in carehomes that died:         ", round(nP_DCH/nP_CH,3) ))
print(paste0("Proportion in carehomes age 70+ that died: ", round(nP_DCH70/nP_CH70,3) ))
print(paste0("Odds dying in carehomes age 70+ (assuming same exposure): ", round( (nP_DCH70/nP_CH70) / (nP_DNCH70/nP_NCH70),3) ))

cat("\n")
cat("Deaths outside hospital \n")
nP_DO      = sum(!is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)), na.rm = T)
nP_DOCH    = sum(!is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) 
                 & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T)
nP_DONCH   = nP_DO -nP_DOCH
nP_DOCH70  = sum(!is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp))
                 &  (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE) & DAT$age_cat=="70+", na.rm = T)
nP_DONCH70 = sum(!is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp))
                 & !(DAT$care_home==TRUE | DAT$care_home_nursing==TRUE) & DAT$age_cat=="70+", na.rm = T)
print(paste0("Patients that died outside hospital:                        ", nP_DO))
print(paste0("Patients in carehomes that died outside hospital:           ", nP_DOCH))
print(paste0("Patients in carehomes age 70+ that died outside hospital:   ", nP_DOCH70))
print(paste0("Proportion in carehomes age 70+ that died outside hospital: ", round(nP_DOCH70/nP_CH70,3) ))
print(paste0("Odds dying outside hospital in carehomes age 70+ (assuming same exposure): ", round( (nP_DOCH70/nP_CH70) / (nP_DONCH70/nP_NCH70),3) ))

cat("\n")
cat("Deaths in hospital - average time to death \n")
###Across all ages - the fraction of mortality is by age
men_time_to_death_1 = round(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==1)] - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==1)], na.rm =T)),3)
men_time_to_death_2 = round(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==2)] - DAT$covid_hosp_admitted_2[which(DAT$all_covid_hosp==2)], na.rm =T)),3)
men_time_to_death_3 = round(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==3)] - DAT$covid_hosp_admitted_3[which(DAT$all_covid_hosp==3)], na.rm =T)),3)
men_time_to_death_4 = round(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==4)] - DAT$covid_hosp_admitted_4[which(DAT$all_covid_hosp==4)], na.rm =T)),3)
men_time_to_death_5 = round(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==5)] - DAT$covid_hosp_admitted_5[which(DAT$all_covid_hosp==5)], na.rm =T)),3)
men_time_to_death_6 = round(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==6)] - DAT$covid_hosp_admitted_6[which(DAT$all_covid_hosp==6)], na.rm =T)),3)
med_time_to_death_1 = round(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==1)] - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==1)], na.rm =T)),3)
med_time_to_death_2 = round(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==2)] - DAT$covid_hosp_admitted_2[which(DAT$all_covid_hosp==2)], na.rm =T)),3)
med_time_to_death_3 = round(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==3)] - DAT$covid_hosp_admitted_3[which(DAT$all_covid_hosp==3)], na.rm =T)),3)
med_time_to_death_4 = round(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==4)] - DAT$covid_hosp_admitted_4[which(DAT$all_covid_hosp==4)], na.rm =T)),3)
med_time_to_death_5 = round(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==5)] - DAT$covid_hosp_admitted_5[which(DAT$all_covid_hosp==5)], na.rm =T)),3)
med_time_to_death_6 = round(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==6)] - DAT$covid_hosp_admitted_6[which(DAT$all_covid_hosp==6)], na.rm =T)),3)
print(paste0("Mean (median) time in hospital 1x, to death: ", men_time_to_death_1, " (", med_time_to_death_1, ")" ))
print(paste0("Mean (median) time in hospital 2x, to death: ", men_time_to_death_2, " (", med_time_to_death_2, ")" ))
print(paste0("Mean (median) time in hospital 3x, to death: ", men_time_to_death_3, " (", med_time_to_death_3, ")" ))
print(paste0("Mean (median) time in hospital 4x, to death: ", men_time_to_death_4, " (", med_time_to_death_4, ")" ))
print(paste0("Mean (median) time in hospital 5x, to death: ", men_time_to_death_5, " (", med_time_to_death_5, ")" ))
print(paste0("Mean (median) time in hospital 6x, to death: ", men_time_to_death_6, " (", med_time_to_death_6, ")" ))

cat("\n")
cat("Deaths in hospital - average time to recovery \n")
###Across all ages - the fraction of mortality is by age
men_time_to_recover_1 = round(as.numeric( mean(DAT$covid_hosp_discharge_1 - DAT$covid_hosp_admitted_1, na.rm =T)),3)
men_time_to_recover_2 = round(as.numeric( mean(DAT$covid_hosp_discharge_2 - DAT$covid_hosp_admitted_2, na.rm =T)),3)
men_time_to_recover_3 = round(as.numeric( mean(DAT$covid_hosp_discharge_3 - DAT$covid_hosp_admitted_3, na.rm =T)),3)
men_time_to_recover_4 = round(as.numeric( mean(DAT$covid_hosp_discharge_4 - DAT$covid_hosp_admitted_4, na.rm =T)),3)
men_time_to_recover_5 = round(as.numeric( mean(DAT$covid_hosp_discharge_5 - DAT$covid_hosp_admitted_5, na.rm =T)),3)
men_time_to_recover_6 = round(as.numeric( mean(DAT$covid_hosp_discharge_6 - DAT$covid_hosp_admitted_6, na.rm =T)),3)
med_time_to_recover_1 = round(as.numeric( mean(DAT$covid_hosp_discharge_1 - DAT$covid_hosp_admitted_1, na.rm =T)),3)
med_time_to_recover_2 = round(as.numeric( mean(DAT$covid_hosp_discharge_2 - DAT$covid_hosp_admitted_2, na.rm =T)),3)
med_time_to_recover_3 = round(as.numeric( mean(DAT$covid_hosp_discharge_3 - DAT$covid_hosp_admitted_3, na.rm =T)),3)
med_time_to_recover_4 = round(as.numeric( mean(DAT$covid_hosp_discharge_4 - DAT$covid_hosp_admitted_4, na.rm =T)),3)
med_time_to_recover_5 = round(as.numeric( mean(DAT$covid_hosp_discharge_5 - DAT$covid_hosp_admitted_5, na.rm =T)),3)
med_time_to_recover_6 = round(as.numeric( mean(DAT$covid_hosp_discharge_6 - DAT$covid_hosp_admitted_6, na.rm =T)),3)
print(paste0("Mean (median) time in hospital 1x, to recovery: ", men_time_to_recover_1, " (", med_time_to_recover_1, ")" ))
print(paste0("Mean (median) time in hospital 2x, to recovery: ", men_time_to_recover_2, " (", med_time_to_recover_2, ")" ))
print(paste0("Mean (median) time in hospital 3x, to recovery: ", men_time_to_recover_3, " (", med_time_to_recover_3, ")" ))
print(paste0("Mean (median) time in hospital 4x, to recovery: ", men_time_to_recover_4, " (", med_time_to_recover_4, ")" ))
print(paste0("Mean (median) time in hospital 5x, to recovery: ", men_time_to_recover_5, " (", med_time_to_recover_5, ")" ))
print(paste0("Mean (median) time in hospital 6x, to recovery: ", men_time_to_recover_6, " (", med_time_to_recover_6, ")" ))

sink()



sink(file = paste0(output_dir, "/", jobno, filename3a, ".txt"),append=F,split=F)
cat("\n")
cat("Shielding \n")
##TODO: use shield or shield1
nP_s1    = sum(DAT$shielding=="High Risk", na.rm=T)
nP_s0    = sum(DAT$shielding!="High Risk", na.rm=T)
nP_00_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="0-4", na.rm = T)
nP_05_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="5-11", na.rm = T)
nP_12_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="12-17", na.rm = T)
nP_18_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="18-29", na.rm = T)
nP_30_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="30-39", na.rm = T)
nP_40_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="40-49", na.rm = T)
nP_50_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="50-59", na.rm = T)
nP_60_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="60-69", na.rm = T)
nP_70_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="70+", na.rm = T)
print(paste0("Patients shielding:          ", nP_s1))
print(paste0("Patients not shielding:      ", nP_s0))
print(paste0("Check: shielding + not - nP: ", nP_s1+nP_s0-nP))
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
print(paste0("Patient shielding fraction age 0-4:   ", round(nP_00_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 5-11:  ", round(nP_05_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 12-17: ", round(nP_12_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 18-29: ", round(nP_18_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 30-39: ", round(nP_30_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 40-49: ", round(nP_30_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 50-59: ", round(nP_30_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 60-69: ", round(nP_30_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 70+:   ", round(nP_30_s1/nP_s1,3) ))

cat("\n")
cat("Shielding patients in hospital \n")
nP_Hosp_s1 = length( which(DAT$all_covid_hosp>0 & DAT$shielding=="High Risk"))
nP_Hosp_s0 = nP_Hosp - nP_Hosp_s1
nP_RH_s1   = length( which( is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shielding=="High Risk"))
nP_RH_s0   = nP_RH - nP_RH_s1
nP_DH_s1   = length( which(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shielding=="High Risk"))
nP_DH_s0   = nP_DH - nP_DH_s1
print(paste0("Patients shielding in hospital:                   ", nP_Hosp_s1))
print(paste0("Patients shielding that recovered in hospital:    ", nP_RH_s1))
print(paste0("Shielding proportion in hospital that recovered:  ", nP_RH_s1/nP_Hosp_s1))
print(paste0("Patients shielding that died in hospital:         ", nP_DH_s1 ))
print(paste0("Shielding proportion in hospital that died:       ", nP_DH_s1/nP_Hosp_s1 ))
print(paste0("Odds of shielders dying in hospital (assuming same exposure): ", (nP_DH_s1/nP_Hosp_s1) / (nP_DH_s0/nP_Hosp_s0) ))

cat("\n")
cat("Shielding patients outside hospital \n")
nP_DO_s1    = length( which(!is.na(DAT$ons_death_date) 
             & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) & DAT$shielding=="High Risk" ))
print(paste0("Patients shielding that died outside hospital:    ", nP_Hosp_s1))

cat("\n")
cat("Shielding in hospital - mortality fraction by age (inc re-admissions) \n")
##TODO: exclude carehomes ?
##TODO: calculate nmfraction = 1-mfraction instead?
##TODO: NOT NEC: men_time_to_death_1, men_time_to_recover_1 - assume shielding independent
nP_Hosp_00_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )

nP_DH_00_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4" ))
nP_DH_05_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11" ))
nP_DH_12_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17" ))
nP_DH_18_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29" ))
nP_DH_30_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39" ))
nP_DH_40_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49" ))
nP_DH_50_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59" ))
nP_DH_60_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69" ))
nP_DH_70_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="70+" ))

mfraction_00_s1 = round(nP_DH_00_s1/nP_Hosp_00_s1, 3)
mfraction_05_s1 = round(nP_DH_05_s1/nP_Hosp_05_s1, 3)
mfraction_12_s1 = round(nP_DH_12_s1/nP_Hosp_12_s1, 3)
mfraction_18_s1 = round(nP_DH_18_s1/nP_Hosp_18_s1, 3)
mfraction_30_s1 = round(nP_DH_30_s1/nP_Hosp_30_s1, 3)
mfraction_40_s1 = round(nP_DH_40_s1/nP_Hosp_40_s1, 3)
mfraction_50_s1 = round(nP_DH_50_s1/nP_Hosp_50_s1, 3)
mfraction_60_s1 = round(nP_DH_60_s1/nP_Hosp_60_s1, 3)
mfraction_70_s1 = round(nP_DH_70_s1/nP_Hosp_70_s1, 3)
print(paste0("Shiedling patients died in hopital age 0-4:     ", nP_DH_00_s1, ", mfraction  ", mfraction_00_s1 ))
print(paste0("Shiedling patients died in hopital age 5-11:    ", nP_DH_05_s1, ", mfraction  ", mfraction_05_s1 ))
print(paste0("Shiedling patients died in hopital age 12-17:   ", nP_DH_12_s1, ", mfraction  ", mfraction_12_s1 ))
print(paste0("Shiedling patients died in hopital age 18-29:   ", nP_DH_18_s1, ", mfraction  ", mfraction_18_s1 ))
print(paste0("Shiedling patients died in hopital age 30-39:   ", nP_DH_30_s1, ", mfraction  ", mfraction_30_s1 ))
print(paste0("Shiedling patients died in hopital age 40-49:   ", nP_DH_40_s1, ", mfraction  ", mfraction_40_s1 ))
print(paste0("Shiedling patients died in hopital age 50-59:   ", nP_DH_50_s1, ", mfraction  ", mfraction_50_s1 ))
print(paste0("Shiedling patients died in hopital age 60-69:   ", nP_DH_60_s1, ", mfraction  ", mfraction_60_s1 ))
print(paste0("Shiedling patients died in hopital age 70+:     ", nP_DH_70_s1, ", mfraction  ", mfraction_70_s1 ))

sink()


####NOT RELEVANT
####-Patients that neither died nor were hospitalised
####-covid_hosp_cat ~ "COVID-19 hospitalisations per person (n)" - it's in all_covid_hosp
####Relevant?
####-hirisk_codedate, hirisk_shield_count



######## Answers in pdf
pdf(file =    paste0(output_dir, "/", jobno, filename1a, ".pdf")) #, height=)
txt=readLines(paste0(output_dir, "/", jobno, filename1a, ".txt"))
plot.new()
gridExtra::grid.table(txt, theme=ttheme_default(base_size = 5, padding = unit(c(1, 1),"mm") ))
dev.off()

pdf(file =    paste0(output_dir, "/", jobno, filename2a, ".pdf")) #, height=)
txt=readLines(paste0(output_dir, "/", jobno, filename2a, ".txt"))
plot.new()
gridExtra::grid.table(txt, theme=ttheme_default(base_size = 7, padding = unit(c(1, 1),"mm") ))
dev.off()

pdf(file =    paste0(output_dir, "/", jobno, filename3a, ".pdf")) #, height=)
txt=readLines(paste0(output_dir, "/", jobno, filename3a, ".txt"))
plot.new()
gridExtra::grid.table(txt, theme=ttheme_default(base_size = 7, padding = unit(c(1, 1),"mm") ))
dev.off()




### Second filtering - remove carehomes & multiple hospitalisations

DAT <- DAT                                                 %>%
  ###Remove carehomes & multiple hospitalisations
  filter( (care_home==FALSE | is.na(care_home)) & 
          (care_home_nursing==FALSE | is.na(care_home_nursing)) ) %>%
  #Adding variables
  mutate(ageg     = as.integer(factor(age_cat)))           %>% # number: 1:9 <> 0-4 5-11 12-17 18-29 30-39 40-49 50-59 60-69 70+ 
  ###Shielding flag
  mutate(shield   = ifelse(shielding=="High Risk",1,0))    %>% # number: 0:1 - ever shielded (had high risk flag)
  mutate(shield   = replace(shield, is.na(shield), 0))     %>% # TODO: test shield_date < admission_date & < death_date
  ###Shielding flag, but prior 2020-04-21
  mutate(shield1  = ifelse(shielding_v1_binary==TRUE,1,0)) %>% # number: 0:1 - hHigh Risk before 2020-04-21
  mutate(shield1  = ifelse(is.na(shield1), 0, shield1))    %>% #
  mutate(shield1_date = shielding_v1_startdate)            %>% # date, or NA if shield1=0
  #mutate(shield1 = as.numeric( min(c(admission_date, ons_death_date), na.rm = T) > shielding_v1_startdate) ) %>% #Limit shielding by date
  ###Restrict to deaths or hospitalisations, not neither
  filter(!is.na(ons_death_date) | all_covid_hosp>0)          %>%   # Tried "| !is.na(admission_date))": too few
  ###Restrict admissions to first admissions (NB: removed pivot_longer, hence one row per patient)
  mutate(admission_date = covid_hosp_admitted_1)             %>%
  mutate(discharge_date = covid_hosp_discharge_1)            %>%
  ###Each patient has one row - other vars replaced by numeric flags
  select(-c(care_home_nursing, care_home, #shielding, #covid_admission, #patient_id, #all_covid_hosp, 
            dplyr::contains("hosp_admitted"),
            dplyr::contains("hosp_discharge"), 
            dplyr::contains("hirisk"), 
            shielding_v1_binary, shielding_v1_startdate))    %>%   # no longer need 
  ###For now, drop this
  select(-c(shield1,shield1_date))                           %>%
  #drop_na()    #Not yet for joint H and D and shielding
  ungroup()

names2 = names(DAT)

#NOTE RELEVANT: covid_hosp_admitted_i  (want admission_date=covid_hosp_admitted_1), 
#               covid_hosp_discharge_i (want discharge_date=covid_hospt_discharge_1)
#RELEVANT: all_covid_hosp (to filter in: 0, 1, and out: 2-6), 



######## QUESTIONS wo CH & wo multiple H #######################################

######## Answers in text
sink(file = paste0(output_dir, "/", jobno, filename1b, ".txt"),append=F,split=F)
cat("\n")
print(paste0("Dataset rows ", dim(DAT)[1], " and columns ", dim(DAT)[2] ))
nP_HorD   = sum(!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0)
nP_noHnoD = sum( is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) )
print(paste0("Patients with hospitalisation or death events: ",    nP_HorD))
print(paste0("Patients without hospitalisation or death events: ", nP_noHnoD))
cat("\n")

cat("Patients \n")
nP = sum(!is.na(DAT$patient_id), na.rm = T)
nP_00 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="0-4", na.rm = T), 3)
nP_05 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="5-11", na.rm = T), 3)
nP_12 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="12-17", na.rm = T), 3)
nP_18 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="18-29", na.rm = T), 3)
nP_30 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="30-39", na.rm = T), 3)
nP_40 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="40-49", na.rm = T), 3)
nP_50 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="50-59", na.rm = T), 3)
nP_60 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="60-69", na.rm = T), 3)
nP_70 = round(sum(!is.na(DAT$patient_id) & DAT$age_cat=="70+", na.rm = T), 3)
print(paste0("Patient entries ", nP ))
print(paste0("Unique patients (if not as above) ", length(unique(DAT$patient_id)) )) #=> row <> one patient
print(paste0("Missing patient id ", sum(is.na(DAT$patient_id)) ))
print(paste0("Patients proportion age 0-4:   ", round(nP_00/nP,3) )); 
print(paste0("Patients proportion age 5-11:  ", round(nP_05/nP,3) ));
print(paste0("Patients proportion age 12-17: ", round(nP_12/nP,3) ));
print(paste0("Patients proportion age 18-29: ", round(nP_18/nP,3) ));
print(paste0("Patients proportion age 30-39: ", round(nP_30/nP,3) ));
print(paste0("Patients proportion age 40-49: ", round(nP_40/nP,3) ));
print(paste0("Patients proportion age 50-59: ", round(nP_50/nP,3) ));
print(paste0("Patients proportion age 60-69: ", round(nP_60/nP,3) ));
print(paste0("Patients proportion age 70+:   ", round(nP_70/nP,3) ));

cat("\n")
cat("Hospitalised patients \n")
nP_Hosp    = length( which(DAT$all_covid_hosp>0))
print(paste0("Ever hospitalised (at least once):   ", nP_Hosp))

cat("\n")
cat("Hospitalisations (excluding re-admissions) \n")
nH       = sum(DAT$all_covid_hosp, na.rm = T)
nH_70    = sum(DAT$all_covid_hosp[which(DAT$age_cat=="70+")],na.rm=T)
print(paste0("Hospitalisations:                      ", nH ))
print(paste0("Hospitalisations age +70:              ", nH_70 ))

cat("\n")
cat("Patients hospitalised (at least once) by age \n")
##TODO: restrict to 1st hospitalisation
nP_Hosp_00  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
print(paste0("Patients hospitalised age 0-4:   ", nP_Hosp_00))
print(paste0("Patients hospitalised age 5-11:  ", nP_Hosp_05))
print(paste0("Patients hospitalised age 12-17: ", nP_Hosp_12))
print(paste0("Patients hospitalised age 18-29: ", nP_Hosp_18))
print(paste0("Patients hospitalised age 30-39: ", nP_Hosp_30))
print(paste0("Patients hospitalised age 40-49: ", nP_Hosp_40))
print(paste0("Patients hospitalised age 50-59: ", nP_Hosp_50))
print(paste0("Patients hospitalised age 60-69: ", nP_Hosp_60))
print(paste0("Patients hospitalised age 70+:   ", nP_Hosp_70))

cat("\n")
cat("Patients hospitalised (re-admitted) \n")
nP_Hosp1  = length( which(DAT$all_covid_hosp==1))
nP_Hospgt1= length( which(DAT$all_covid_hosp>1))
print(paste0("Hospitalised with only 1 admission:  ", nP_Hosp1 ))
print(paste0("Hospitalised more than once:         ", nP_Hospgt1 ))

cat("\n")
cat("Hospital re-admissions \n")
print(paste0("Hospitalisations, only 1 admission:       ", 
             sum(DAT$all_covid_hosp[which(DAT$all_covid_hosp==1)], na.rm = T) ))
print(paste0("All hospital re-admissions:               ", 
             sum(DAT$all_covid_hosp[which(DAT$all_covid_hosp>1)], na.rm = T) ))
print(paste0("Hospitalisations here (no re-admissions): ",
             sum(!is.na(DAT$admission_date)) ))
sink()



sink(file = paste0(output_dir, "/", jobno, filename2b, ".txt"),append=F,split=F)
cat("\n")
cat("Deaths \n")
nP_D   = sum(!is.na(DAT$ons_death_date))
print(paste0("Patients that died: ", nP_D))

cat("\n")
cat("Deaths in hospital \n")
nP_DH  = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0, na.rm = T)
nP_RH  = nP_Hosp - nP_DH
nP_DH1 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==1, na.rm = T)
nP_DH2 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==2, na.rm = T)
nP_DH3 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==3, na.rm = T)
nP_DH4 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==4, na.rm = T)
nP_DH5 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==5, na.rm = T)
nP_DH6 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==6, na.rm = T)
print(paste0("Patients that died:        ", nP_DH, ", mfraction   ", round(nP_DH/nP_Hosp,3) ))
print(paste0("Patients that recovered:   ", nP_RH, ", 1-mfraction ", round(nP_RH/nP_Hosp,3) ))
print(paste0("Deaths upon 1st admission: ", nP_DH1))
print(paste0("Deaths upon 2nd admission: ", nP_DH2))
print(paste0("Deaths upon 3rd admission: ", nP_DH3))
print(paste0("Deaths upon 4th admission: ", nP_DH4))
print(paste0("Deaths upon 5th admission: ", nP_DH5))
print(paste0("Deaths upon 6th admission: ", nP_DH6))

cat("\n")
cat("Deaths in hospital by age (inc re-admissions) \n")
##TODO: exclude carehomes ?
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
nP_DO      = sum(!is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)), na.rm = T)
nP_DOCH    = sum(!is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) 
                 & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T)
print(paste0("Patients that died outside hospital:                        ", nP_DO))
print(paste0("Patients in carehomes that died outside hospital:           ", nP_DOCH))


cat("\n")
cat("Deaths in hospital - average time to death from 1st admission - Discarding subsequent admissions \n")
###Across all ages - as the fraction of mortality is by age
men_time_to_death = round(as.numeric(   mean(DAT$ons_death_date[which(DAT$all_covid_hosp>0)] - DAT$admission_date[which(DAT$all_covid_hosp>0)], na.rm =T)),3)
med_time_to_death = round(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp>0)] - DAT$admission_date[which(DAT$all_covid_hosp>0)], na.rm =T)),3)
print(paste0("Mean (median) time in hospital from 1st admission to death:   ", men_time_to_death, " (", med_time_to_death, ")" ))

cat("\n")
cat("Deaths in hospital - average time to recovery \n")
###Across all ages - as the fraction of mortality is by age
men_time_to_recover = round(as.numeric(   mean(DAT$discharge_date - DAT$admission_date, na.rm =T)),3)
med_time_to_recover = round(as.numeric( median(DAT$discharge_date - DAT$admission_date, na.rm =T)),3)
print(paste0("Mean (median) time in hospital from 1st admission to recovery: ", men_time_to_recover, " (", med_time_to_recover, ")" ))

sink()




sink(file = paste0(output_dir, "/", jobno, filename3b, ".txt"),append=F,split=F)
cat("\n")
cat("Shielding \n")
##TODO: use shield or shield1
nP_s1    = sum(DAT$shielding=="High Risk", na.rm=T)
nP_s0    = sum(DAT$shielding!="High Risk", na.rm=T)
nP_00_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="0-4", na.rm = T)
nP_05_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="5-11", na.rm = T)
nP_12_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="12-17", na.rm = T)
nP_18_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="18-29", na.rm = T)
nP_30_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="30-39", na.rm = T)
nP_40_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="40-49", na.rm = T)
nP_50_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="50-59", na.rm = T)
nP_60_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="60-69", na.rm = T)
nP_70_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="70+", na.rm = T)
print(paste0("Patients shielding:          ", nP_s1))
print(paste0("Patients not shielding:      ", nP_s0))
print(paste0("Check: shielding + not - nP: ", nP_s1+nP_s0-nP))
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
print(paste0("Patient shielding fraction age 0-4:   ", round(nP_00_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 5-11:  ", round(nP_05_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 12-17: ", round(nP_12_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 18-29: ", round(nP_18_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 30-39: ", round(nP_30_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 40-49: ", round(nP_30_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 50-59: ", round(nP_30_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 60-69: ", round(nP_30_s1/nP_s1,3) ))
print(paste0("Patient shielding fraction age 70+:   ", round(nP_30_s1/nP_s1,3) ))

cat("\n")
cat("Shielding patients in hospital \n")
nP_Hosp_s1 = length( which(DAT$all_covid_hosp>0 & DAT$shielding=="High Risk"))
nP_Hosp_s0 = nP_Hosp - nP_Hosp_s1
nP_RH_s1   = length( which( is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shielding=="High Risk"))
nP_RH_s0   = nP_RH - nP_RH_s1
nP_DH_s1   = length( which(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shielding=="High Risk"))
nP_DH_s0   = nP_DH - nP_DH_s1
print(paste0("Patients shielding in hospital:                   ", nP_Hosp_s1))
print(paste0("Patients shielding that recovered in hospital:    ", nP_RH_s1))
print(paste0("Shielding proportion in hospital that recovered:  ", nP_RH_s1/nP_Hosp_s1))
print(paste0("Patients shielding that died in hospital:         ", nP_DH_s1 ))
print(paste0("Shielding proportion in hospital that died:       ", nP_DH_s1/nP_Hosp_s1 ))
print(paste0("Odds of shielders dying in hospital (assuming same exposure): ", (nP_DH_s1/nP_Hosp_s1) / (nP_DH_s0/nP_Hosp_s0) ))

cat("\n")
cat("Shielding patients outside hospital \n")
nP_DO_s1    = length( which(!is.na(DAT$ons_death_date) 
                            & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) & DAT$shielding=="High Risk" ))
print(paste0("Patients shielding that died outside hospital:    ", nP_Hosp_s1))

cat("\n")
cat("Shielding in hospital - mortality fraction by age (ever admitted) \n")
##TODO: calculate nmfraction = 1-mfraction instead?
##TODO: NOT NEC: men_time_to_death_1, men_time_to_recover_1 - assume shielding independent
nP_Hosp_00_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )

nP_DH_00_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4" ))
nP_DH_05_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11" ))
nP_DH_12_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17" ))
nP_DH_18_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29" ))
nP_DH_30_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39" ))
nP_DH_40_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49" ))
nP_DH_50_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59" ))
nP_DH_60_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69" ))
nP_DH_70_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="70+" ))

mfraction_00_s1 = round(nP_DH_00_s1/nP_Hosp_00_s1, 3)
mfraction_05_s1 = round(nP_DH_05_s1/nP_Hosp_05_s1, 3)
mfraction_12_s1 = round(nP_DH_12_s1/nP_Hosp_12_s1, 3)
mfraction_18_s1 = round(nP_DH_18_s1/nP_Hosp_18_s1, 3)
mfraction_30_s1 = round(nP_DH_30_s1/nP_Hosp_30_s1, 3)
mfraction_40_s1 = round(nP_DH_40_s1/nP_Hosp_40_s1, 3)
mfraction_50_s1 = round(nP_DH_50_s1/nP_Hosp_50_s1, 3)
mfraction_60_s1 = round(nP_DH_60_s1/nP_Hosp_60_s1, 3)
mfraction_70_s1 = round(nP_DH_70_s1/nP_Hosp_70_s1, 3)
print(paste0("Shiedling patients died in hopital age 0-4:     ", nP_DH_00_s1, ", mfraction  ", mfraction_00_s1 ))
print(paste0("Shiedling patients died in hopital age 5-11:    ", nP_DH_05_s1, ", mfraction  ", mfraction_05_s1 ))
print(paste0("Shiedling patients died in hopital age 12-17:   ", nP_DH_12_s1, ", mfraction  ", mfraction_12_s1 ))
print(paste0("Shiedling patients died in hopital age 18-29:   ", nP_DH_18_s1, ", mfraction  ", mfraction_18_s1 ))
print(paste0("Shiedling patients died in hopital age 30-39:   ", nP_DH_30_s1, ", mfraction  ", mfraction_30_s1 ))
print(paste0("Shiedling patients died in hopital age 40-49:   ", nP_DH_40_s1, ", mfraction  ", mfraction_40_s1 ))
print(paste0("Shiedling patients died in hopital age 50-59:   ", nP_DH_50_s1, ", mfraction  ", mfraction_50_s1 ))
print(paste0("Shiedling patients died in hopital age 60-69:   ", nP_DH_60_s1, ", mfraction  ", mfraction_60_s1 ))
print(paste0("Shiedling patients died in hopital age 70+:     ", nP_DH_70_s1, ", mfraction  ", mfraction_70_s1 ))

sink()



####Not relevant
####-Patients that neither died nor were hospitalised
####-covid_hosp_cat ~ "COVID-19 hospitalisations per person (n)" - it's in all_covid_hosp
####Relevant?
####-hirisk_codedate, hirisk_shield_count



######## Answers in pdf
pdf(file =    paste0(output_dir, "/", jobno, filename1b, ".pdf")) #, height=)
txt=readLines(paste0(output_dir, "/", jobno, filename1b, ".txt"))
plot.new()
gridExtra::grid.table(txt, theme=ttheme_default(base_size = 8, padding = unit(c(1, 1),"mm") ))
dev.off()

pdf(file =    paste0(output_dir, "/", jobno, filename2b, ".pdf")) #, height=)
txt=readLines(paste0(output_dir, "/", jobno, filename2b, ".txt"))
plot.new()
gridExtra::grid.table(txt, theme=ttheme_default(base_size = 8, padding = unit(c(1, 1),"mm") ))
dev.off()

pdf(file =    paste0(output_dir, "/", jobno, filename3b, ".pdf")) #, height=)
txt=readLines(paste0(output_dir, "/", jobno, filename3b, ".txt"))
plot.new()
gridExtra::grid.table(txt, theme=ttheme_default(base_size = 8, padding = unit(c(1, 1),"mm") ))
dev.off()



### Third filtering - remove patient_id, change age_cat

DAT <- DAT                                                 %>%
  ###for excel writing
  mutate(age_cat  = paste0("'", age_cat))                  %>%
  ###restrict to deaths or hospitalisations, not neither
  #filter(!is.na(ons_death_date) | all_covid_hosp>0)          %>%   # Tried "| !is.na(admission_date))": too few
  ###Remove patient id 
  select(-c(patient_id))
  #drop_na()    #Not yet for joint H and D and shielding
  ungroup()

names3 = names(DAT)


#Get
# time series - also, with and wo carehomes
# tables - data fitted - by age - later by shielding
