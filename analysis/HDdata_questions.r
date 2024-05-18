## Dataset general questions
## Linking H, D, and Shielding at ID level
## see HDdata_test.R for tests on simulated data
##
library(arrow)
library(here)
library(lubridate)
library(gridExtra)


#Dataset date range
Date01="2020-01-01" #study_start_date = datetime.date(2020, 1, 1)
Date02="2021-09-01" #study_end_date   = datetime.date(2021, 9, 1)

#Study date range -applied further below
Date1="2020-01-01"
Date2="2020-12-01"

#
jobno = "JDat15_"

#output names
filename1a = "HDdata_questions_answered_Hospital_wCH"
filename2a = "HDdata_questions_answered_Deaths_wCH"
filename3a = "HDdata_questions_answered_Shielding_wCH"
filename1b = "HDdata_questions_answered_Hospital"
filename2b = "HDdata_questions_answered_Deaths"
filename3b = "HDdata_questions_answered_Shielding"


#Read data
source(here::here("analysis/functions/redaction.R"))

output_dir <- here("output/HDsynthesis")
fs::dir_create(output_dir)

DAT  <- arrow::read_parquet(file = here::here("output/data_edited.gz.parquet"),
        compression = "gzip", compression_level = 5) 



######## TEXT OUTPUT STARTS ####################################################
sink(file = paste0(output_dir, "/", jobno, filename1a, ".txt"),append=F,split=F)


## Stats: raw data (no filters)
nP_tot_raw  = dim(DAT)[1]
nP_tot_raw_id  = sum(!is.na(DAT$patient_id), na.rm = T)
nP_tot_raw_age = sum(!is.na(DAT$age_cat), na.rm = T)
#
print(paste0("Raw data: "))
print(paste0("Raw data: rows:     ", nP_tot_raw))
print(paste0("Raw data: patients: ", nP_tot_raw_id))
print(paste0("Raw data: with age: ", nP_tot_raw_age))
cat("\n")



## Filter 0 (cohort0) - require age category exists 
## Define shielding status
DAT <- DAT %>%
        ###age groups  # remove patients with missing 'age_cat'
        filter(!is.na(DAT$age_cat))                                                   %>% 
        #shielding flag - up to 2020-12-01
        mutate(shieldA  = as.factor(ifelse(hirisk_codedate_1<"2020-12-02",1,0)))  %>% # date of first high risk flag
        mutate(shieldB  = as.factor(ifelse(highrisk_shield  <"2020-12-02",1,0)))  %>% # date of first high risk flag 
        mutate(shieldA  = replace(shieldA, is.na(shieldA), as.factor(0)))         %>% # TODO:test shield_date< admi_date &death_date
        mutate(shieldB  = replace(shieldB, is.na(shieldB), as.factor(0)))             # => B gave same results as A

## Stats: filter 0 (cohort0)
##- exc patients with missing age 
##- inc patients who died of non-covid causes  Jan-2020 - Sep-2021 (patients surviving are inc via NA)
##- inc patients with covid H, D events during Dec-2020 - Sep-2021
##- inc CH
nP_tot_f0     = dim(DAT)[1]
nP_tot_f0_age = sum(!is.na(DAT$age_cat), na.rm = T)
nP_tot_f0_id  = sum(!is.na(DAT$patient_id), na.rm = T)
nP_tot        = sum(!is.na(DAT$patient_id), na.rm = T)                               #used below with this name
#
nP_tot_s1     = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk", na.rm = T)  #used below with this name
nP_tot_s0     = nP_tot - nP_tot_s1
nP_tot_sA1    = sum(!is.na(DAT$patient_id) & DAT$shieldA=="1", na.rm = T)            #used below with this name
nP_tot_sA0    = nP_tot - nP_tot_sA1
#
print(paste0("Filter 0 - one exclusion (age category missing) - Cohort0"))
print(paste0("Filter 0: data rows:                                   ", nP_tot_f0))
print(paste0("Filter 0: has age:                                     ", nP_tot_f0_age))
print(paste0("Filter 0: has patient id:                              ", nP_tot_f0_id))
print(paste0("Filter 0: patient shielding (High Risk by 2021-09-01): ", nP_tot_s1))
print(paste0("Filter 0: patient not shielding:                       ", nP_tot_s0))
print(paste0("Filter 0: patient shieldA (High Risk by 2020-12-01):   ", nP_tot_sA1))
print(paste0("Filter 0: patient not shieldA:                         ", nP_tot_sA0))
cat("\n")

#cohort0
nP_00_tot = sum(!is.na(DAT$patient_id) & DAT$age_cat=="0-4", na.rm = T)
nP_05_tot = sum(!is.na(DAT$patient_id) & DAT$age_cat=="5-11", na.rm = T)
nP_12_tot = sum(!is.na(DAT$patient_id) & DAT$age_cat=="12-17", na.rm = T)
nP_18_tot = sum(!is.na(DAT$patient_id) & DAT$age_cat=="18-29", na.rm = T)
nP_30_tot = sum(!is.na(DAT$patient_id) & DAT$age_cat=="30-39", na.rm = T)
nP_40_tot = sum(!is.na(DAT$patient_id) & DAT$age_cat=="40-49", na.rm = T)
nP_50_tot = sum(!is.na(DAT$patient_id) & DAT$age_cat=="50-59", na.rm = T)
nP_60_tot = sum(!is.na(DAT$patient_id) & DAT$age_cat=="60-69", na.rm = T)
nP_70_tot = sum(!is.na(DAT$patient_id) & DAT$age_cat=="70+", na.rm = T)
nP_sum_tot = (nP_00_tot+nP_05_tot+nP_12_tot+nP_18_tot+nP_30_tot+nP_40_tot+nP_50_tot+nP_60_tot+nP_70_tot)
#cohort0 sheilding
nP_00_tot_s1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="0-4"   & DAT$shielding=="High Risk", na.rm = T)
nP_05_tot_s1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="5-11"  & DAT$shielding=="High Risk", na.rm = T)
nP_12_tot_s1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="12-17" & DAT$shielding=="High Risk", na.rm = T)
nP_18_tot_s1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="18-29" & DAT$shielding=="High Risk", na.rm = T)
nP_30_tot_s1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="30-39" & DAT$shielding=="High Risk", na.rm = T)
nP_40_tot_s1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="40-49" & DAT$shielding=="High Risk", na.rm = T)
nP_50_tot_s1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="50-59" & DAT$shielding=="High Risk", na.rm = T)
nP_60_tot_s1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="60-69" & DAT$shielding=="High Risk", na.rm = T)
nP_70_tot_s1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="70+"   & DAT$shielding=="High Risk", na.rm = T)
nP_sum_tot_s1 = (nP_00_tot_s1+nP_05_tot_s1+nP_12_tot_s1+nP_18_tot_s1+nP_30_tot_s1+nP_40_tot_s1+nP_50_tot_s1+nP_60_tot_s1+nP_70_tot_s1)
#cohort0 shieldA
nP_00_tot_sA1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="0-4"   & DAT$shieldA=="1", na.rm = T)
nP_05_tot_sA1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="5-11"  & DAT$shieldA=="1", na.rm = T)
nP_12_tot_sA1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="12-17" & DAT$shieldA=="1", na.rm = T)
nP_18_tot_sA1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="18-29" & DAT$shieldA=="1", na.rm = T)
nP_30_tot_sA1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="30-39" & DAT$shieldA=="1", na.rm = T)
nP_40_tot_sA1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="40-49" & DAT$shieldA=="1", na.rm = T)
nP_50_tot_sA1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="50-59" & DAT$shieldA=="1", na.rm = T)
nP_60_tot_sA1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="60-69" & DAT$shieldA=="1", na.rm = T)
nP_70_tot_sA1 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="70+"   & DAT$shieldA=="1", na.rm = T)
nP_sum_tot_sA1 = (nP_00_tot_sA1+nP_05_tot_sA1+nP_12_tot_sA1+nP_18_tot_sA1+nP_30_tot_sA1+nP_40_tot_sA1+nP_50_tot_sA1+nP_60_tot_sA1+nP_70_tot_sA1)



### Data rounding/truncation
pc1 <- function(x)  { return(round(100*x,1))}
pc2 <- function(x)  { return(round(100*x,2))}
rd2 <- function(x)  { return(round(x,2))}
rd3 <- function(x)  { return(round(x,3))}
tr  <- function(x)  {x = if(x<8) "<8" else x; return(x)}
pct1 <- function(n,N)
{x = if(n<8 & N>7) as.character(paste0("<",eval(round(100*(8/N),1)),"%")) else if(n<8 & N<8) as.character(paste0("NA")) else paste0(pc1(n/N),'%'); return(x)}
frt2 <- function(n,N)
{x = if(n<8 & N>7) as.character(paste0("<",eval(round((8/N),1)))) else if(n<8 & N<8) as.character(paste0("NA")) else paste0(rd2(n/N)); return(x)}



### Filter 1  (cohort1) - remove non-covid deaths - keeping carehomes and multiple hopspitalisations
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
				        ons_underlying_cause,                          # code,    covid death
                shielding,                                     # factor:  High Risk, Low/Moderate risk, No shielding
                shieldA, shieldB,                              # Defined above
                shielding_v1_startdate,                        # date,    High Risk before 2020-04-21 - NA if binary=0
                shielding_v1_binary,                           # logical: T/F,  High Risk before 2020-04-21
                dplyr::contains("hirisk_codedate"),            # date,    when received 1st-6th high risk flag
                hirisk_shield_count)                           # number:  0:n, high risk flags received per patient

#D - exclude non-covid deaths
dim_sc = dim(DAT) #500, 89
if (dim_sc[1]>1000){ #only operates on real data, as dummy data has 500 to 1000 rows
  DAT <- DAT[           which(DAT$ons_death_date>="2020-01-01") | is.na(DAT$ons_death_date),]  #remove deaths prior to 2020 but registered from 2020
  DAT <- DAT[which(is.element(DAT$ons_underlying_cause,c("U071","U072")) | is.na(DAT$ons_underlying_cause)),] #remove deaths not caused by covid
}

# Stats: filter 1 (cohort1)
##- exc patients with missing age 
##- exc patients who died of non-covid causes  Jan-2020 - Sep-2021
##-    NOTE: => could be excluding some relevant covid hospitalisations (up to Dec-2020) that did not lead to covid death
##- inc patients with covid H, D events during Dec-2020 - Sep-2021
##- inc CH 
nP_tot_f1       = dim(DAT)[1]
nP_tot_f1_id    = sum(!is.na(DAT$patient_id), na.rm = T)      #=nP_tot_f1 (id filter alreday applied)
#
nP_tot_f1_s1    = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk", na.rm = T)
nP_tot_f1_s0    = nP_tot_f1 - nP_tot_f1_s1
nP_tot_f1_sA1   = sum(!is.na(DAT$patient_id) & DAT$shieldA=="1", na.rm = T)
nP_tot_f1_sA0   = nP_tot_f1 - nP_tot_f1_sA1
#repeat from above, but used below with this name
nP_all_dates    = sum(!is.na(DAT$patient_id), na.rm = T)                                #=nP_tot_f1_id
nP_all_dates_s1 = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk", na.rm = T)   #=nP_tot_f1_s1
nP_all_dates_s0 = nP_all_dates - nP_all_dates_s1                                        #=nP_tot_f1_s0
nP_all_dates_sA1= sum(!is.na(DAT$patient_id) & DAT$shieldA=="1", na.rm = T)             #=nP_tot_f1_sA1
nP_all_dates_sA0= nP_all_dates - nP_all_dates_sA1                                       #=nP_tot_f1_sA0
#
print(paste0("Filter 1 - exclude patients with non-covid death - Cohort1"))
print(paste0("Filter 1: data rows:                                   ", nP_tot_f1))
print(paste0("Filter 1: has patient id:                              ", nP_tot_f1_id))
print(paste0("Filter 1: patient shielding (High Risk by 2021-09-01): ", nP_tot_f1_s1))
print(paste0("Filter 1: patient not shielding:                       ", nP_tot_f1_s0))
print(paste0("Filter 1: patient shieldA (High Risk by 2020-12-01):   ", nP_tot_f1_sA1))
print(paste0("Filter 1: patient not shieldA:                         ", nP_tot_f1_sA0))
cat("\n")

#cohort1
nP_00_all_dates = sum(!is.na(DAT$patient_id) & DAT$age_cat=="0-4", na.rm = T)
nP_05_all_dates = sum(!is.na(DAT$patient_id) & DAT$age_cat=="5-11", na.rm = T)
nP_12_all_dates = sum(!is.na(DAT$patient_id) & DAT$age_cat=="12-17", na.rm = T)
nP_18_all_dates = sum(!is.na(DAT$patient_id) & DAT$age_cat=="18-29", na.rm = T)
nP_30_all_dates = sum(!is.na(DAT$patient_id) & DAT$age_cat=="30-39", na.rm = T)
nP_40_all_dates = sum(!is.na(DAT$patient_id) & DAT$age_cat=="40-49", na.rm = T)
nP_50_all_dates = sum(!is.na(DAT$patient_id) & DAT$age_cat=="50-59", na.rm = T)
nP_60_all_dates = sum(!is.na(DAT$patient_id) & DAT$age_cat=="60-69", na.rm = T)
nP_70_all_dates = sum(!is.na(DAT$patient_id) & DAT$age_cat=="70+", na.rm = T)
nP_sum_all_dates = (nP_00_all_dates+nP_05_all_dates+nP_12_all_dates+nP_18_all_dates+
                    nP_30_all_dates+nP_40_all_dates+nP_50_all_dates+nP_60_all_dates+nP_70_all_dates)
#cohort1
nP_00_all_dates_s1 = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk" & DAT$age_cat=="0-4", na.rm = T)
nP_05_all_dates_s1 = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk" & DAT$age_cat=="5-11", na.rm = T)
nP_12_all_dates_s1 = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk" & DAT$age_cat=="12-17", na.rm = T)
nP_18_all_dates_s1 = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk" & DAT$age_cat=="18-29", na.rm = T)
nP_30_all_dates_s1 = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk" & DAT$age_cat=="30-39", na.rm = T)
nP_40_all_dates_s1 = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk" & DAT$age_cat=="40-49", na.rm = T)
nP_50_all_dates_s1 = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk" & DAT$age_cat=="50-59", na.rm = T)
nP_60_all_dates_s1 = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk" & DAT$age_cat=="60-69", na.rm = T)
nP_70_all_dates_s1 = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk" & DAT$age_cat=="70+", na.rm = T)
#
nP_00_all_dates_sA1 = sum(!is.na(DAT$patient_id) & DAT$shieldA=="1" & DAT$age_cat=="0-4", na.rm = T)
nP_05_all_dates_sA1 = sum(!is.na(DAT$patient_id) & DAT$shieldA=="1" & DAT$age_cat=="5-11", na.rm = T)
nP_12_all_dates_sA1 = sum(!is.na(DAT$patient_id) & DAT$shieldA=="1" & DAT$age_cat=="12-17", na.rm = T)
nP_18_all_dates_sA1 = sum(!is.na(DAT$patient_id) & DAT$shieldA=="1" & DAT$age_cat=="18-29", na.rm = T)
nP_30_all_dates_sA1 = sum(!is.na(DAT$patient_id) & DAT$shieldA=="1" & DAT$age_cat=="30-39", na.rm = T)
nP_40_all_dates_sA1 = sum(!is.na(DAT$patient_id) & DAT$shieldA=="1" & DAT$age_cat=="40-49", na.rm = T)
nP_50_all_dates_sA1 = sum(!is.na(DAT$patient_id) & DAT$shieldA=="1" & DAT$age_cat=="50-59", na.rm = T)
nP_60_all_dates_sA1 = sum(!is.na(DAT$patient_id) & DAT$shieldA=="1" & DAT$age_cat=="60-69", na.rm = T)
nP_70_all_dates_sA1 = sum(!is.na(DAT$patient_id) & DAT$shieldA=="1" & DAT$age_cat=="70+", na.rm = T)



### Filter 2  (cohort2) - study period up to 2020-12-01 - keeping carehomes and multiple hopspitalisations

#H,D - exclude covid hospitalisations and deaths after 2020-12-01

DAT <- DAT                                                                       %>%
       filter(ons_death_date         <= "2020-12-01" | is.na(ons_death_date ))        %>%
       filter(covid_hosp_admitted_1  <= "2020-12-01" | is.na(covid_hosp_admitted_1 )) %>%
       filter(covid_hosp_admitted_2  <= "2020-12-01" | is.na(covid_hosp_admitted_2 )) %>%
       filter(covid_hosp_admitted_3  <= "2020-12-01" | is.na(covid_hosp_admitted_3 )) %>%
       filter(covid_hosp_admitted_4  <= "2020-12-01" | is.na(covid_hosp_admitted_4 )) %>%
       filter(covid_hosp_admitted_5  <= "2020-12-01" | is.na(covid_hosp_admitted_5 )) %>%
       filter(covid_hosp_admitted_6  <= "2020-12-01" | is.na(covid_hosp_admitted_6 ))
   
# Stats: filter 2 (cohort2)
##- exc patients with missing age 
##- exc patients who died of non-covid causes  Jan-2020 - Sep-2021
##      NOTE: could include some covid hospitalisations that did not lead to covid death
##- exc patients with covid H, D events during Dec-2020 - Sep-2021
##- inc CH
nP_tot_f2       = dim(DAT)[1]
nP_tot_f2_id    = sum(!is.na(DAT$patient_id), na.rm = T)      #=nP_tot_f2 (id filter alreday applied)
#
nP_tot_f2_s1    = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk", na.rm = T)
nP_tot_f2_s0    = nP_tot_f2 - nP_tot_f2_s1
nP_tot_f2_sA1   = sum(!is.na(DAT$patient_id) & DAT$shieldA=="1", na.rm = T)
nP_tot_f2_sA0   = nP_tot_f2 - nP_tot_f2_sA1
#
print(paste0("Filter 2 - exclude covid deaths after Dec-20 - Cohort2"))
print(paste0("Filter 2: data rows:                                   ", nP_tot_f2))
print(paste0("Filter 2: has patient id:                              ", nP_tot_f2_id))
print(paste0("Filter 2: patient shielding (High Risk by 2021-09-01): ", nP_tot_f2_s1))
print(paste0("Filter 2: patient not shielding:                       ", nP_tot_f2_s0))
print(paste0("Filter 2: patient shieldA (High Risk by 2020-12-01):   ", nP_tot_f2_sA1))
print(paste0("Filter 2: patient not shieldA:                         ", nP_tot_f2_sA0))
cat("\n")

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
print(paste0("names1: ", names1))
cat("\n")



######## QUESTIONS with CH & multiple H ########################################
#sink(file = paste0(output_dir, "/", jobno, filename1a, ".txt"),append=F,split=F)

cat("Patients & Hopitalisations - wCH \n")
cat("\n")

print(paste0("Date range: ", Date1," to ", Date2))

cat("\n")
print(paste0("Dataset rows ", dim(DAT)[1], " and columns ", dim(DAT)[2] ))
cat("\n")
cat("Patients \n")
nP = sum(!is.na(DAT$patient_id), na.rm = T)
print(paste0("Patient entries:        ", nP ))
print(paste0("Unique patients:        ", length(unique(DAT$patient_id)) )) #=> row <> one patient
print(paste0("Missing patient id:     ", sum(is.na(DAT$patient_id)) ))
print(paste0("Cohort1 inc >2020-12-01 ", nP_all_dates, ", patient1 inc hosp/deaths Dec-2020 to Sep-2021" ))
print(paste0("Cohort0 inc other-death ", nP_tot,       ", patient0 inc also non-covid deaths Jan-20 to Sep-21" ))
nP_HorD    = sum(!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0)       #NOTE: excluded non-covid deaths (filter 1)
nP_noHorD  = sum( is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) )
nP_CH_HorD = sum( (!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0)
                  & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE))
nP_CH_noHorD = sum( is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp))
                    & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE))
print(paste0("Patients with covid hosp or death events: ", nP_HorD,   ", %patients:  ", rd2(100*nP_HorD/   nP) ))
print(paste0("Patients with covid hosp or death events: ", nP_HorD,   ", %cohort1:   ", rd2(100*nP_HorD/   nP_all_dates) ))
print(paste0("Patients with covid hosp or death events: ", nP_HorD,   ", %cohort0:   ", rd2(100*nP_HorD/   nP_tot) ))
print(paste0("Patients w cov hosp or death events in CH ", nP_CH_HorD,", %events:    ", rd2(100*nP_CH_HorD/nP_HorD) ))
print(paste0("Patients wout cov hosp or death events:   ", nP_noHorD, ", %patients:  ", rd2(100*nP_noHorD/ nP) ))
print(paste0("...wout... inc cohort0-1 other deaths:    ", nP_noHorD + nP_tot-nP, ", %cohort0: ", rd2(100*(nP_noHorD + nP_tot-nP)/ nP_tot) ))

cat("\n")
cat("Patients by age \n")
nP_00 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="0-4", na.rm = T)
nP_05 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="5-11", na.rm = T)
nP_12 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="12-17", na.rm = T)
nP_18 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="18-29", na.rm = T)
nP_30 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="30-39", na.rm = T)
nP_40 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="40-49", na.rm = T)
nP_50 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="50-59", na.rm = T)
nP_60 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="60-69", na.rm = T)
nP_70 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="70+", na.rm = T)
nP_sum = (nP_00+nP_05+nP_12+nP_18+nP_30+nP_40+nP_50+nP_60+nP_70)
print(paste0("Patient % age 0-4:   ", pct1(nP_00,nP) )); 
print(paste0("Patient % age 5-11:  ", pct1(nP_05,nP) ));
print(paste0("Patient % age 12-17: ", pct1(nP_12,nP) ));
print(paste0("Patient % age 18-29: ", pct1(nP_18,nP) ));
print(paste0("Patient % age 30-39: ", pct1(nP_30,nP) ));
print(paste0("Patient % age 40-49: ", pct1(nP_40,nP) ));
print(paste0("Patient % age 50-59: ", pct1(nP_50,nP) ));
print(paste0("Patient % age 60-69: ", pct1(nP_60,nP) ));
print(paste0("Patient % age 70+:   ", pct1(nP_70,nP) ));
print(paste0("Patient % all:       ", pct1(nP_sum,nP) ));

#cat("\n")
#cat("Cohort1 by age \n")
#print(paste0("Cohort1 % age 0-4:   ", pct1(nP_00_all_dates,nP_all_dates) )); 
#print(paste0("Cohort1 % age 5-11:  ", pct1(nP_05_all_dates,nP_all_dates) ));
#print(paste0("Cohort1 % age 12-17: ", pct1(nP_12_all_dates,nP_all_dates) ));
#print(paste0("Cohort1 % age 18-29: ", pct1(nP_18_all_dates,nP_all_dates) ));
#print(paste0("Cohort1 % age 30-39: ", pct1(nP_30_all_dates,nP_all_dates) ));
#print(paste0("Cohort1 % age 40-49: ", pct1(nP_40_all_dates,nP_all_dates) ));
#print(paste0("Cohort1 % age 50-59: ", pct1(nP_50_all_dates,nP_all_dates) ));
#print(paste0("Cohort1 % age 60-69: ", pct1(nP_60_all_dates,nP_all_dates) ));
#print(paste0("Cohort1 % age 70+:   ", pct1(nP_70_all_dates,nP_all_dates) ));
#print(paste0("Cohort1 % all:       ", pct1(nP_sum_all_dates,nP_all_dates) ));

cat("\n")
cat("Cohort0 by age \n")
print(paste0("Cohort0 % age 0-4:   ", pct1(nP_00_tot,nP_tot) )); 
print(paste0("Cohort0 % age 5-11:  ", pct1(nP_05_tot,nP_tot) ));
print(paste0("Cohort0 % age 12-17: ", pct1(nP_12_tot,nP_tot) ));
print(paste0("Cohort0 % age 18-29: ", pct1(nP_18_tot,nP_tot) ));
print(paste0("Cohort0 % age 30-39: ", pct1(nP_30_tot,nP_tot) ));
print(paste0("Cohort0 % age 40-49: ", pct1(nP_40_tot,nP_tot) ));
print(paste0("Cohort0 % age 50-59: ", pct1(nP_50_tot,nP_tot) ));
print(paste0("Cohort0 % age 60-69: ", pct1(nP_60_tot,nP_tot) ));
print(paste0("Cohort0 % age 70+:   ", pct1(nP_70_tot,nP_tot) ));
print(paste0("Cohort0 % all:       ", pct1(nP_sum_tot,nP_tot) ));

cat("\n")
cat("Carehome patients \n")
nP_CH  = sum( (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T)
nP_NCH = sum(!(DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T)
nP_CH60  = sum( DAT$age_cat=="60-69" &  (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T) 
nP_CH70  = sum( DAT$age_cat=="70+"   &  (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T) 
nP_NCH70 = sum( DAT$age_cat=="70+"   & !(DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T) 
print(paste0("Patients in CH:           ", nP_CH,   ", %patients:       ", pct1(nP_CH,nP) ))
print(paste0("Patients in + not - all:  ", nP_CH + nP_NCH - nP))
print(paste0("Patients in CH age 60-69: ", nP_CH60, ", %patients in CH: ", pct1(nP_CH60,nP_CH) ))
print(paste0("Patients in CH age 70+:   ", nP_CH70, ", %patients in CH: ", pct1(nP_CH70,nP_CH) )) 
print(paste0("Patients in CH with hosp or death events: ", nP_CH_HorD, ", %patients in CH: ", pct1(nP_CH_HorD,nP_CH) ))

cat("\n")
cat("Hospitalised patients \n")
nP_Hosp    = length( which(DAT$all_covid_hosp>0))
nP_HospCH  = length( which(DAT$all_covid_hosp>0 & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE) )) 
nP_HospNCH = nP_Hosp - nP_HospCH
print(paste0("Ever hospitalised (>=once):  ", nP_Hosp))
print(paste0("Ever hospitalised in CH:     ", nP_HospCH))
print(paste0("Ever hospitalised not in CH: ", nP_HospNCH))
print(paste0("Odds hospitalised in CH (assuming same exposure): ", rd2( (nP_HospCH/nP_CH) / (nP_HospNCH/nP_NCH) ) ))

cat("\n")
cat("Hospitalisations (inc re-admissions) overall and in carehomes \n")
nH       = sum(DAT$all_covid_hosp, na.rm = T)
nH_CH    = sum(DAT$all_covid_hosp[which(DAT$care_home==TRUE | DAT$care_home_nursing==TRUE)], na.rm = T) 
nH_70    = sum(DAT$all_covid_hosp[which(DAT$age_cat=="70+")],na.rm=T)
nH_CH70  = sum(DAT$all_covid_hosp[which(DAT$age_cat=="70+" & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE))], na.rm = T) 
nH_NCH70 = nH_70 - nH_CH70
print(paste0("Hospitalisations:               ", nH ))
print(paste0("Hospitalisations age +70:       ", nH_70 ))
print(paste0("Hospitalisations in CH:         ", nH_CH ))
print(paste0("Hospitalisations in CH age +70: ", nH_CH70 ))
print(paste0("Odds of hosp in CH age 70+ (assuming same exposure): ", rd2( (nH_CH70/nP_CH70) / (nH_NCH70/nP_NCH70) ) ))

cat("\n")
cat("Hospitalised patients (any no. admissions) by age \n")
nP_Hosp_00  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") ) #NB: 'admitted' matters here, no.admissions doens't
nP_Hosp_05  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
nP_Hosp_sum = nP_Hosp_00+nP_Hosp_05+nP_Hosp_12+nP_Hosp_18+nP_Hosp_30+nP_Hosp_40+nP_Hosp_50+nP_Hosp_60+nP_Hosp_70
print(paste0("Patients hospitalised age 0-4:   ", tr(nP_Hosp_00), ", %patients hosp: ", pct1(nP_Hosp_00,nP_Hosp) ))
print(paste0("Patients hospitalised age 5-11:  ", tr(nP_Hosp_05), ", %patients hosp: ", pct1(nP_Hosp_05,nP_Hosp) ))
print(paste0("Patients hospitalised age 12-17: ", tr(nP_Hosp_12), ", %patients hosp: ", pct1(nP_Hosp_12,nP_Hosp) ))
print(paste0("Patients hospitalised age 18-29: ", tr(nP_Hosp_18), ", %patients hosp: ", pct1(nP_Hosp_18,nP_Hosp) ))
print(paste0("Patients hospitalised age 30-39: ", tr(nP_Hosp_30), ", %patients hosp: ", pct1(nP_Hosp_30,nP_Hosp) ))
print(paste0("Patients hospitalised age 40-49: ", tr(nP_Hosp_40), ", %patients hosp: ", pct1(nP_Hosp_40,nP_Hosp) ))
print(paste0("Patients hospitalised age 50-59: ", tr(nP_Hosp_50), ", %patients hosp: ", pct1(nP_Hosp_50,nP_Hosp) ))
print(paste0("Patients hospitalised age 60-69: ", tr(nP_Hosp_60), ", %patients hosp: ", pct1(nP_Hosp_60,nP_Hosp) ))
print(paste0("Patients hospitalised age 70+:   ", tr(nP_Hosp_70), ", %patients hosp: ", pct1(nP_Hosp_70,nP_Hosp) ))
print(paste0("Patients hospitalised total %:   ", pct1(nP_Hosp_sum,nP_Hosp) ))

cat("\n")
cat("Hospitalised patients by number of admissions \n")
nP_Hosp1  = length( which(DAT$all_covid_hosp==1))
nP_Hospgt1= length( which(DAT$all_covid_hosp>1))
nP_Hosp2  = length( which(DAT$all_covid_hosp==2))
nP_Hosp3  = length( which(DAT$all_covid_hosp==3))
nP_Hospgt3= length( which(DAT$all_covid_hosp>3))
print(paste0("Hospitalised with 1 admission:  ", nP_Hosp1,   ", %patients hosp: ", pct1(nP_Hosp1,  nP_Hosp) ))
print(paste0("Hospitalised more than 1x:      ", nP_Hospgt1, ", %patients hosp: ", pct1(nP_Hospgt1,nP_Hosp) ))
print(paste0("Hospitalised once or more:      ", nP_Hosp1+nP_Hospgt1, ", %patients hosp: ", pct1((nP_Hosp1+nP_Hospgt1),nP_Hosp) ))
print(paste0("Hospitalised with 2 admissions: ", nP_Hosp2,   ", %patients hosp: ", pct1(nP_Hosp2  ,nP_Hosp) ))
print(paste0("Hospitalised with 3 admissions: ", nP_Hosp3,   ", %patients hosp: ", pct1(nP_Hosp3  ,nP_Hosp) ))
print(paste0("Hospitalised more than 3x:      ", nP_Hospgt3, ", %patients hosp: ", pct1(nP_Hospgt3,nP_Hosp) ))
nP_Hosp4  = length( which(DAT$all_covid_hosp==4))
nP_Hosp5  = length( which(DAT$all_covid_hosp==5))
nP_Hosp6  = length( which(DAT$all_covid_hosp==6))

cat("\n")
cat("Hospital re-admissions \n")
nH_all1    = sum(DAT$all_covid_hosp[which(DAT$all_covid_hosp==1)], na.rm = T)
nH_allgt1  = sum(DAT$all_covid_hosp[which(DAT$all_covid_hosp>1)], na.rm = T)    #all admissions of readmitted patients
nH_1st     = length(which(DAT$all_covid_hosp>1)) #first admissions in readmitted patients
nH_post1st = nH_allgt1 - nH_1st
print(paste0("Hospitalisations without re-admission:  ", nH_all1,    ", %all hosp: ", pct1(nH_all1,   nH) ))
print(paste0("Hospitalisations, 1st pre re-admission: ", nH_1st,     ", %all hosp: ", pct1(nH_1st,    nH) ))
print(paste0("All re-admissions (exc 1st admission):  ", nH_post1st, ", %all hosp: ", pct1(nH_post1st,nH) ))
print(paste0("Total of these three:                   ", nH_all1+nH_1st+nH_post1st, ", %all hosp: ", pct1((nH_all1+nH_1st+nH_post1st),nH) ))
nP_Hospgt2 = nP_Hospgt1 - nP_Hosp2
print(paste0("All 2nd re-admissions:  ", nP_Hospgt1, " = patients hosp >1x, %all hosp: ", pct1(nP_Hospgt1,nH) ))
print(paste0("All 3rd re-admissions:  ", nP_Hospgt2, " = patients hosp >2x, %all hosp: ", pct1(nP_Hospgt2,nH) ))
print(paste0("All post 3rd re-admis:  ", nP_Hospgt3, " = patients hosp >3x, %all hosp: ", pct1(nP_Hospgt3,nH) ))

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
             rd2( (nP_Hosp1CH/nP_CH) / (nP_Hosp1NCH/nP_NCH)) ))
print(paste0("Odds of 2nd admission in carehomes (idem):                   ", 
             rd2( (nP_Hosp2CH/nP_CH) / (nP_Hosp2NCH/nP_NCH)) ))
print(paste0("Odds of 3rd admission in carehomes (idem):                   ", 
             rd2( (nP_Hosp3CH/nP_CH) / (nP_Hosp3NCH/nP_NCH)) ))
print(paste0("Odds of 4th admission in carehomes (idem):                   ",
             rd2( (nP_Hosp4CH/nP_CH) / (nP_Hosp4NCH/nP_NCH)) ))
print(paste0("Odds of 5th admission in carehomes (idem):                   ", 
             rd2( (nP_Hosp5CH/nP_CH) / (nP_Hosp5NCH/nP_NCH)) ))
print(paste0("Odds of 6th admission in carehomes (idem):                   ",
             rd2( (nP_Hosp6CH/nP_CH) / (nP_Hosp6NCH/nP_NCH)) ))
sink()



sink(file = paste0(output_dir, "/", jobno, filename2a, ".txt"),append=F,split=F)

cat("Deaths - wCH \n")
cat("\n")

cat("Deaths \n")
nP_D = sum(!is.na(DAT$ons_death_date))
print(paste0("Patients that died: ", nP_D))

cat("\n")
cat("Deaths in hospital \n")
nP_DH  = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0, na.rm = T)
nP_RH  = nP_Hosp - nP_DH
nP_DH1 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==1, na.rm = T)
nP_DH2 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==2, na.rm = T)
nP_DH3 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==3, na.rm = T)
nP_DH4to6 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>=4, na.rm = T)
print(paste0("Patients died in hospital: ", nP_DH,     ", fatal prop    ", frt2(nP_DH, nP_Hosp), ", %all deaths: ", pct1(nP_DH,nP_D) ))
print(paste0("Patients recovered:        ", nP_RH,     ", 1-fatal prop  ", frt2(nP_RH, nP_Hosp) ))
print(paste0("Deaths upon 1st admission: ", nP_DH1,    ", %hosp deaths: ", pct1(nP_DH1,nP_DH) ))
print(paste0("Deaths upon 2nd admission: ", nP_DH2,    ", %hosp deaths: ", pct1(nP_DH2,nP_DH) ))
print(paste0("Deaths upon 3rd admission: ", nP_DH3,    ", %hosp deaths: ", pct1(nP_DH3,nP_DH) ))
print(paste0("Deaths upon >3 admissions: ", nP_DH4to6, ", %hosp deaths: ", pct1(nP_DH4to6,nP_DH) ))
print(paste0("Deaths total 1+ admission: ", nP_DH1+nP_DH2+nP_DH3+nP_DH4to6, ", %hosp deaths: ", pct1((nP_DH1+nP_DH2+nP_DH3+nP_DH4to6),nP_DH) ))

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
nP_DH_sum = (nP_DH_00+nP_DH_05+nP_DH_12+nP_DH_18+nP_DH_30+nP_DH_40+nP_DH_50+nP_DH_60+nP_DH_70)
print(paste0("Patients died in hopital age 0-4:   ", tr(nP_DH_00), ", %hosp deaths: ", pct1(nP_DH_00,nP_DH) ))
print(paste0("Patients died in hopital age 5-11:  ", tr(nP_DH_05), ", %hosp deaths: ", pct1(nP_DH_05,nP_DH) ))
print(paste0("Patients died in hopital age 12-17: ", tr(nP_DH_12), ", %hosp deaths: ", pct1(nP_DH_12,nP_DH) ))
print(paste0("Patients died in hopital age 18-29: ", tr(nP_DH_18), ", %hosp deaths: ", pct1(nP_DH_18,nP_DH) ))
print(paste0("Patients died in hopital age 30-39: ", tr(nP_DH_30), ", %hosp deaths: ", pct1(nP_DH_30,nP_DH) ))
print(paste0("Patients died in hopital age 40-49: ", tr(nP_DH_40), ", %hosp deaths: ", pct1(nP_DH_40,nP_DH) ))
print(paste0("Patients died in hopital age 50-59: ", tr(nP_DH_50), ", %hosp deaths: ", pct1(nP_DH_50,nP_DH) ))
print(paste0("Patients died in hopital age 60-69: ", tr(nP_DH_60), ", %hosp deaths: ", pct1(nP_DH_60,nP_DH) ))
print(paste0("Patients died in hopital age 70+:   ", tr(nP_DH_70), ", %hosp deaths: ", pct1(nP_DH_70,nP_DH) ))
print(paste0("Patients died in hopital total:     ", nP_DH_sum,    ", %hosp deaths: ", pct1(nP_DH_sum,nP_DH) ))
###Mortality fraction
mfraction_00 = rd3(nP_DH_00/nP_Hosp_00) #frt23(nP_DH_00,nP_Hosp_00)
mfraction_05 = rd3(nP_DH_05/nP_Hosp_05) # frt2(nP_DH_05,nP_Hosp_05)
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
cat("Deaths in carehomes \n")
nP_DCH    = sum(!is.na(DAT$ons_death_date) & (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T)
nP_DNCH   = nP_D -nP_DCH
nP_DCH70  = sum(!is.na(DAT$ons_death_date) & DAT$age_cat=="70+" &  (DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T)
nP_DNCH70 = sum(!is.na(DAT$ons_death_date) & DAT$age_cat=="70+" & !(DAT$care_home==TRUE | DAT$care_home_nursing==TRUE), na.rm = T)
print(paste0("Patients in CH died:         ", nP_DCH,   ", %patients CH:    ", pct1(nP_DCH,  nP_CH),   ", %all deaths: ", pct1(nP_DCH,  nP_D) ))
print(paste0("Patients in CH age 70+ died: ", nP_DCH70, ", %patients CH70+: ", pct1(nP_DCH70,nP_CH70), ", %CH deaths:  ", pct1(nP_DCH70,nP_DCH) ))
print(paste0("Odds age 70+ in CH dies in hosp (assuming same exposure): ", rd2( (nP_DCH70/nP_CH70) / (nP_DNCH70/nP_NCH70) ) ))


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
print(paste0("Patients died outside hosp:           ", nP_DO,    ", %all deaths:     ", pct1(nP_DO,    nP_D) ))
print(paste0("Patients in CH died out hosp:         ", nP_DOCH,  ", %patients CH:    ", pct1(nP_DOCH,  nP_CH) ))
print(paste0("Patients in CH age 70+ died out hosp: ", nP_DOCH70,", %patients CH70+: ", pct1(nP_DOCH70,nP_CH70) ))
print(paste0("Odds age 70+ in CH died out hosp (assuming same exposure): ", rd2( (nP_DOCH70/nP_CH70) / (nP_DONCH70/nP_NCH70)) ))


cat("\n")
cat("Deaths in hospital - average time to death since last admission \n")
###Across all ages - as the mortality fraction is age specific
men_time_to_death_1 = rd2(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==1)]   - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==1)], na.rm =T)) )
men_time_to_death_2 = rd2(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==2)]   - DAT$covid_hosp_admitted_2[which(DAT$all_covid_hosp==2)], na.rm =T)) )
men_time_to_death_3 = rd2(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==3)]   - DAT$covid_hosp_admitted_3[which(DAT$all_covid_hosp==3)], na.rm =T)) )
men_time_to_death_4 = rd2(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==4)]   - DAT$covid_hosp_admitted_4[which(DAT$all_covid_hosp==4)], na.rm =T)) )
men_time_to_death_5 = rd2(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==5)]   - DAT$covid_hosp_admitted_5[which(DAT$all_covid_hosp==5)], na.rm =T)) )
men_time_to_death_6 = rd2(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==6)]   - DAT$covid_hosp_admitted_6[which(DAT$all_covid_hosp==6)], na.rm =T)) )
med_time_to_death_1 = rd2(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==1)] - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==1)], na.rm =T)) )
med_time_to_death_2 = rd2(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==2)] - DAT$covid_hosp_admitted_2[which(DAT$all_covid_hosp==2)], na.rm =T)) )
med_time_to_death_3 = rd2(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==3)] - DAT$covid_hosp_admitted_3[which(DAT$all_covid_hosp==3)], na.rm =T)) )
med_time_to_death_4 = rd2(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==4)] - DAT$covid_hosp_admitted_4[which(DAT$all_covid_hosp==4)], na.rm =T)) )
med_time_to_death_5 = rd2(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==5)] - DAT$covid_hosp_admitted_5[which(DAT$all_covid_hosp==5)], na.rm =T)) )
med_time_to_death_6 = rd2(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==6)] - DAT$covid_hosp_admitted_6[which(DAT$all_covid_hosp==6)], na.rm =T)) )
print(paste0("Mean (median) time to death since last admission 1x: ", men_time_to_death_1, " (", med_time_to_death_1, ")" ))
print(paste0("Mean (median) time to death since last admission 2x: ", men_time_to_death_2, " (", med_time_to_death_2, ")" ))
print(paste0("Mean (median) time to death since last admission 3x: ", men_time_to_death_3, " (", med_time_to_death_3, ")" ))
print(paste0("Mean (median) time to death since last admission 4x: ", men_time_to_death_4, " (", med_time_to_death_4, ")" ))
print(paste0("Mean (median) time to death since last admission 5x: ", men_time_to_death_5, " (", med_time_to_death_5, ")" ))
print(paste0("Mean (median) time to death since last admission 6x: ", men_time_to_death_6, " (", med_time_to_death_6, ")" ))

cat("Deaths in hospital - average time to death since 1st admission \n")
cat("                   - consistist with what is feasible to model \n")
###Across all ages - as the mortality fraction is age specific
men_time_to_death_11 = rd2(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==1)]   - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==1)], na.rm =T)) )
men_time_to_death_21 = rd2(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==2)]   - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==2)], na.rm =T)) )
men_time_to_death_31 = rd2(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==3)]   - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==3)], na.rm =T)) )
men_time_to_death_41 = rd2(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==4)]   - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==4)], na.rm =T)) )
men_time_to_death_51 = rd2(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==5)]   - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==5)], na.rm =T)) )
men_time_to_death_61 = rd2(as.numeric( mean(DAT$ons_death_date[which(DAT$all_covid_hosp==6)]   - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==6)], na.rm =T)) )
med_time_to_death_11 = rd2(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==1)] - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==1)], na.rm =T)) )
med_time_to_death_21 = rd2(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==2)] - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==2)], na.rm =T)) )
med_time_to_death_31 = rd2(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==3)] - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==3)], na.rm =T)) )
med_time_to_death_41 = rd2(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==4)] - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==4)], na.rm =T)) )
med_time_to_death_51 = rd2(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==5)] - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==5)], na.rm =T)) )
med_time_to_death_61 = rd2(as.numeric( median(DAT$ons_death_date[which(DAT$all_covid_hosp==6)] - DAT$covid_hosp_admitted_1[which(DAT$all_covid_hosp==6)], na.rm =T)) )
print(paste0("Mean (median) time to death since 1st admission 1x: ", men_time_to_death_11, " (", med_time_to_death_11, ")" ))
print(paste0("Mean (median) time to death since 1st admission 2x: ", men_time_to_death_21, " (", med_time_to_death_21, ")" ))
print(paste0("Mean (median) time to death since 1st admission 3x: ", men_time_to_death_31, " (", med_time_to_death_31, ")" ))
print(paste0("Mean (median) time to death since 1st admission 4x: ", men_time_to_death_41, " (", med_time_to_death_41, ")" ))
print(paste0("Mean (median) time to death since 1st admission 5x: ", men_time_to_death_51, " (", med_time_to_death_51, ")" ))
print(paste0("Mean (median) time to death since 1st admission 6x: ", men_time_to_death_61, " (", med_time_to_death_61, ")" ))

cat("\n")
cat("Recovery in hospital - average time to recovery \n")
###Across all ages - as (1 - mortality fraction) is age specific
men_time_to_recover_1 = rd2(as.numeric( mean(DAT$covid_hosp_discharge_1 - DAT$covid_hosp_admitted_1, na.rm =T)) )
men_time_to_recover_2 = rd2(as.numeric( mean(DAT$covid_hosp_discharge_2 - DAT$covid_hosp_admitted_2, na.rm =T)) )
men_time_to_recover_3 = rd2(as.numeric( mean(DAT$covid_hosp_discharge_3 - DAT$covid_hosp_admitted_3, na.rm =T)) )
men_time_to_recover_4 = rd2(as.numeric( mean(DAT$covid_hosp_discharge_4 - DAT$covid_hosp_admitted_4, na.rm =T)) )
men_time_to_recover_5 = rd2(as.numeric( mean(DAT$covid_hosp_discharge_5 - DAT$covid_hosp_admitted_5, na.rm =T)) )
men_time_to_recover_6 = rd2(as.numeric( mean(DAT$covid_hosp_discharge_6 - DAT$covid_hosp_admitted_6, na.rm =T)) )
med_time_to_recover_1 = rd2(as.numeric( median(DAT$covid_hosp_discharge_1 - DAT$covid_hosp_admitted_1, na.rm =T)) )
med_time_to_recover_2 = rd2(as.numeric( median(DAT$covid_hosp_discharge_2 - DAT$covid_hosp_admitted_2, na.rm =T)) )
med_time_to_recover_3 = rd2(as.numeric( median(DAT$covid_hosp_discharge_3 - DAT$covid_hosp_admitted_3, na.rm =T)) )
med_time_to_recover_4 = rd2(as.numeric( median(DAT$covid_hosp_discharge_4 - DAT$covid_hosp_admitted_4, na.rm =T)) )
med_time_to_recover_5 = rd2(as.numeric( median(DAT$covid_hosp_discharge_5 - DAT$covid_hosp_admitted_5, na.rm =T)) )
med_time_to_recover_6 = rd2(as.numeric( median(DAT$covid_hosp_discharge_6 - DAT$covid_hosp_admitted_6, na.rm =T)) )
print(paste0("Mean (median) time to recovery in 1st admission: ", men_time_to_recover_1, " (", med_time_to_recover_1, ")" ))
print(paste0("Mean (median) time in hospital in 2nd admission: ", men_time_to_recover_2, " (", med_time_to_recover_2, ")" ))
print(paste0("Mean (median) time in hospital in 3rd admission: ", men_time_to_recover_3, " (", med_time_to_recover_3, ")" ))
print(paste0("Mean (median) time in hospital in 4th admission: ", men_time_to_recover_4, " (", med_time_to_recover_4, ")" ))
print(paste0("Mean (median) time in hospital in 5th admission: ", men_time_to_recover_5, " (", med_time_to_recover_5, ")" ))
print(paste0("Mean (median) time in hospital in 6th admission: ", men_time_to_recover_6, " (", med_time_to_recover_6, ")" ))

sink()



sink(file = paste0(output_dir, "/", jobno, filename3a, ".txt"),append=F,split=F)

cat("Shielding - wCH \n")
cat("\n")

cat("Shielding \n")
cat("shield:  inc High Risk flags up to 2021-09-01 \n")
cat("shieldA: inc High Risk flags up to 2020-12-01 \n")
#
nP_s1    = sum(DAT$shielding=="High Risk", na.rm=T)
nP_s0    = sum(DAT$shielding!="High Risk", na.rm=T)
nP_HorD_s1   = sum(DAT$shielding=="High Risk" & (!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0))
nP_noHorD_s1 = sum(DAT$shielding=="High Risk" &  (is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) ))
nP_HorD_s0   = sum(DAT$shielding!="High Risk" & (!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0))
nP_noHorD_s0 = sum(DAT$shielding!="High Risk" &  (is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) ))
#
nP_sA1    = sum(DAT$shieldA=="1", na.rm=T)
nP_sA0    = sum(DAT$shieldA!="1", na.rm=T)
nP_HorD_sA1   = sum(DAT$shieldA=="1" & (!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0))
nP_noHorD_sA1 = sum(DAT$shieldA=="1" &  (is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) ))
nP_HorD_sA0   = sum(DAT$shieldA!="1" & (!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0))
nP_noHorD_sA0 = sum(DAT$shieldA!="1" &  (is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) ))
#
print(paste0("Patients shield: ", nP_s1,           ", %patients: ", rd2(100*nP_s1/nP) ))
print(paste0("Patients not sh: ", nP_s0,           ", %patients: ", rd2(100*nP_s0/nP) ))
print(paste0("Patient0 shield: ", nP_tot_s1,       ", %cohort0:  ", rd2(100*nP_tot_s1/nP_tot) ))
print(paste0("Patient0 not sh: ", nP_tot_s0,       ", %cohort0:  ", rd2(100*nP_tot_s0/nP_tot) ))
print(paste0("Patient1 shield: ", nP_all_dates_s1, ", %cohort1:  ", rd2(100*nP_all_dates_s1/nP_all_dates) ))
print(paste0("Patient1 not sh: ", nP_all_dates_s0, ", %cohort1:  ", rd2(100*nP_all_dates_s0/nP_all_dates) ))
cat("\n")
print(paste0("Patients shieldA: ", nP_sA1,           ", %patients: ", rd2(100*nP_sA1/nP) ))
print(paste0("Patients not shA: ", nP_sA0,           ", %patients: ", rd2(100*nP_sA0/nP) ))
print(paste0("Patient0 shieldA: ", nP_tot_sA1,       ", %cohort0:  ", rd2(100*nP_tot_sA1/nP_tot) ))
print(paste0("Patient0 not shA: ", nP_tot_sA0,       ", %cohort0:  ", rd2(100*nP_tot_sA0/nP_tot) ))
print(paste0("Patient1 shieldA: ", nP_all_dates_sA1, ", %cohort1:  ", rd2(100*nP_all_dates_sA1/nP_all_dates) ))
print(paste0("Patient1 not shA: ", nP_all_dates_sA0, ", %cohort1:  ", rd2(100*nP_all_dates_sA0/nP_all_dates) ))
print(paste0("Patient0 inc CH, covid deaths Dec-2020-Sep-2021, non-covid deaths Jan-2020-Sep-2021"))
print(paste0("Patient1 inc CH, covid deaths Dec-2020-Sep-2021"))
cat("\n")
print(paste0("Patients shield with hosp/death events: ", nP_HorD_s1,   ", %sh patients:  ", rd2(100*nP_HorD_s1/   nP_s1) ))
print(paste0("Patients shield wout hosp/death events: ", nP_noHorD_s1, ", %sh patients:  ", rd2(100*nP_noHorD_s1/ nP_s1) ))
print(paste0("Patients not sh with hosp/death events: ", nP_HorD_s0,   ", %nsh patients: ", rd2(100*nP_HorD_s0/  nP_s0) ))
print(paste0("Patients not sh w/o hosp/death events:  ", nP_noHorD_s0, ", %nsh patients: ", rd2(100*nP_noHorD_s0/nP_s0) ))
print(paste0("not sh w/o... inc cohort0 other deaths: ", nP_noHorD_s0 + nP_tot_s0-nP_s0, ", %nsh cohort0:  ", rd2(100*(nP_noHorD_s0 + nP_tot_s0-nP_s0)/ nP_tot_s0) ))
cat("\n")
print(paste0("Patients shieldA with hosp/death events: ", nP_HorD_sA1,   ", %shA patients:  ", rd2(100*nP_HorD_sA1/   nP_sA1) ))
print(paste0("Patients shieldA wout hosp/death events: ", nP_noHorD_sA1, ", %shA patients:  ", rd2(100*nP_noHorD_sA1/ nP_sA1) ))
print(paste0("Patients not shA with hosp/death events: ", nP_HorD_sA0,   ", %nshA patients: ", rd2(100*nP_HorD_sA0/  nP_sA0) ))
print(paste0("Patients not shA w/o hosp/death events:  ", nP_noHorD_sA0, ", %nshA patients: ", rd2(100*nP_noHorD_sA0/nP_sA0) ))
print(paste0("not shA w/o... inc cohort0 other deaths: ", nP_noHorD_sA0 + nP_tot_sA0-nP_sA0, ", %nsh cohort0:  ", rd2(100*(nP_noHorD_sA0 + nP_tot_sA0-nP_sA0)/ nP_tot_s0) ))
cat("\n")
cat("Shielding by age \n")
nP_00_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="0-4", na.rm = T)
nP_05_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="5-11", na.rm = T)
nP_12_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="12-17", na.rm = T)
nP_18_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="18-29", na.rm = T)
nP_30_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="30-39", na.rm = T)
nP_40_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="40-49", na.rm = T)
nP_50_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="50-59", na.rm = T)
nP_60_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="60-69", na.rm = T)
nP_70_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="70+", na.rm = T)
nP_sum_s1 = nP_00_s1 + nP_05_s1 + nP_12_s1 + nP_18_s1 + nP_30_s1 + nP_40_s1 + nP_50_s1 + nP_60_s1 + nP_70_s1
print(paste0("Patient age 0-4   % shielding: ", pct1(nP_00_s1,nP_00) ))
print(paste0("Patient age 5-11  % shielding: ", pct1(nP_05_s1,nP_05) ))
print(paste0("Patient age 12-17 % shielding: ", pct1(nP_12_s1,nP_12) ))
print(paste0("Patient age 18-29 % shielding: ", pct1(nP_18_s1,nP_18) ))
print(paste0("Patient age 30-39 % shielding: ", pct1(nP_30_s1,nP_30) ))
print(paste0("Patient age 40-49 % shielding: ", pct1(nP_40_s1,nP_40) ))
print(paste0("Patient age 50-59 % shielding: ", pct1(nP_50_s1,nP_50) ))
print(paste0("Patient age 60-69 % shielding: ", pct1(nP_60_s1,nP_60) ))
print(paste0("Patient age 70+   % shielding: ", pct1(nP_70_s1,nP_70) ))
cat("\n")
print(paste0("Patient shielding % age 0-4:   ", pct1(nP_00_s1,nP_s1) ))
print(paste0("Patient shielding % age 5-11:  ", pct1(nP_05_s1,nP_s1) ))
print(paste0("Patient shielding % age 12-17: ", pct1(nP_12_s1,nP_s1) ))
print(paste0("Patient shielding % age 18-29: ", pct1(nP_18_s1,nP_s1) ))
print(paste0("Patient shielding % age 30-39: ", pct1(nP_30_s1,nP_s1) ))
print(paste0("Patient shielding % age 40-49: ", pct1(nP_40_s1,nP_s1) ))
print(paste0("Patient shielding % age 50-59: ", pct1(nP_50_s1,nP_s1) ))
print(paste0("Patient shielding % age 60-69: ", pct1(nP_60_s1,nP_s1) ))
print(paste0("Patient shielding % age 70+:   ", pct1(nP_70_s1,nP_s1) ))
print(paste0("Patient shielding % all ages:  ", pct1(nP_sum_s1,nP_s1) ))
#
cat("\n")
cat("ShieldA by age \n")
nP_00_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="0-4", na.rm = T)
nP_05_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="5-11", na.rm = T)
nP_12_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="12-17", na.rm = T)
nP_18_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="18-29", na.rm = T)
nP_30_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="30-39", na.rm = T)
nP_40_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="40-49", na.rm = T)
nP_50_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="50-59", na.rm = T)
nP_60_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="60-69", na.rm = T)
nP_70_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="70+", na.rm = T)
nP_sum_sA1 = nP_00_sA1 + nP_05_sA1 + nP_12_sA1 + nP_18_sA1 + nP_30_sA1 + nP_40_sA1 + nP_50_sA1 + nP_60_sA1 + nP_70_sA1
print(paste0("Patient age 0-4   % shieldA: ", pct1(nP_00_sA1,nP_00) ))
print(paste0("Patient age 5-11  % shieldA: ", pct1(nP_05_sA1,nP_05) ))
print(paste0("Patient age 12-17 % shieldA: ", pct1(nP_12_sA1,nP_12) ))
print(paste0("Patient age 18-29 % shieldA: ", pct1(nP_18_sA1,nP_18) ))
print(paste0("Patient age 30-39 % shieldA: ", pct1(nP_30_sA1,nP_30) ))
print(paste0("Patient age 40-49 % shieldA: ", pct1(nP_40_sA1,nP_40) ))
print(paste0("Patient age 50-59 % shieldA: ", pct1(nP_50_sA1,nP_50) ))
print(paste0("Patient age 60-69 % shieldA: ", pct1(nP_60_sA1,nP_60) ))
print(paste0("Patient age 70+   % shieldA: ", pct1(nP_70_sA1,nP_70) ))
cat("\n")
print(paste0("Patient shieldA % age 0-4:   ", pct1(nP_00_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 5-11:  ", pct1(nP_05_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 12-17: ", pct1(nP_12_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 18-29: ", pct1(nP_18_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 30-39: ", pct1(nP_30_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 40-49: ", pct1(nP_40_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 50-59: ", pct1(nP_50_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 60-69: ", pct1(nP_60_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 70+:   ", pct1(nP_70_sA1,nP_sA1) ))
print(paste0("Patient shieldA % all ages:  ", pct1(nP_sum_sA1,nP_sA1) ))

#cat("\n")
#cat("Shielding by age, in cohort1 (all dates) \n")
#print(paste0("Cohort1 age 0-4   % shielding: ", pct1(nP_00_all_dates_s1,nP_00_all_dates) ))
#print(paste0("Cohort1 age 5-11  % shielding: ", pct1(nP_05_all_dates_s1,nP_05_all_dates) ))
#print(paste0("Cohort1 age 12-17 % shielding: ", pct1(nP_12_all_dates_s1,nP_12_all_dates) ))
#print(paste0("Cohort1 age 18-29 % shielding: ", pct1(nP_18_all_dates_s1,nP_18_all_dates) ))
#print(paste0("Cohort1 age 30-39 % shielding: ", pct1(nP_30_all_dates_s1,nP_30_all_dates) ))
#print(paste0("Cohort1 age 40-49 % shielding: ", pct1(nP_40_all_dates_s1,nP_40_all_dates) ))
#print(paste0("Cohort1 age 50-59 % shielding: ", pct1(nP_50_all_dates_s1,nP_50_all_dates) ))
#print(paste0("Cohort1 age 60-69 % shielding: ", pct1(nP_60_all_dates_s1,nP_60_all_dates) ))
#print(paste0("Cohort1 age 70+   % shielding: ", pct1(nP_70_all_dates_s1,nP_70_all_dates) ))
#cat(" \n")
#cat("ShieldA by age, in cohort1 (all dates) \n")
#print(paste0("Cohort1 age 0-4   % shieldA: ", pct1(nP_00_all_dates_sA1,nP_00_all_dates) ))
#print(paste0("Cohort1 age 5-11  % shieldA: ", pct1(nP_05_all_dates_sA1,nP_05_all_dates) ))
#print(paste0("Cohort1 age 12-17 % shieldA: ", pct1(nP_12_all_dates_sA1,nP_12_all_dates) ))
#print(paste0("Cohort1 age 18-29 % shieldA: ", pct1(nP_18_all_dates_sA1,nP_18_all_dates) ))
#print(paste0("Cohort1 age 30-39 % shieldA: ", pct1(nP_30_all_dates_sA1,nP_30_all_dates) ))
#print(paste0("Cohort1 age 40-49 % shieldA: ", pct1(nP_40_all_dates_sA1,nP_40_all_dates) ))
#print(paste0("Cohort1 age 50-59 % shieldA: ", pct1(nP_50_all_dates_sA1,nP_50_all_dates) ))
#print(paste0("Cohort1 age 60-69 % shieldA: ", pct1(nP_60_all_dates_sA1,nP_60_all_dates) ))
#print(paste0("Cohort1 age 70+   % shieldA: ", pct1(nP_70_all_dates_sA1,nP_70_all_dates) ))
cat(" \n")
cat("Shielding by age, in cohort0 (all deaths, all dates) \n")
print(paste0("Patient age 0-4   % shielding: ", pct1(nP_00_tot_s1,nP_00_tot) ))
print(paste0("Patient age 5-11  % shielding: ", pct1(nP_05_tot_s1,nP_05_tot) ))
print(paste0("Patient age 12-17 % shielding: ", pct1(nP_12_tot_s1,nP_12_tot) ))
print(paste0("Patient age 18-29 % shielding: ", pct1(nP_18_tot_s1,nP_18_tot) ))
print(paste0("Patient age 30-39 % shielding: ", pct1(nP_30_tot_s1,nP_30_tot) ))
print(paste0("Patient age 40-49 % shielding: ", pct1(nP_40_tot_s1,nP_40_tot) ))
print(paste0("Patient age 50-59 % shielding: ", pct1(nP_50_tot_s1,nP_50_tot) ))
print(paste0("Patient age 60-69 % shielding: ", pct1(nP_60_tot_s1,nP_60_tot) ))
print(paste0("Patient age 70+   % shielding: ", pct1(nP_70_tot_s1,nP_70_tot) ))
cat(" \n")
cat("ShieldA by age, in cohort0 (all deaths, all dates) \n")
print(paste0("Patient age 0-4   % shieldA: ", pct1(nP_00_tot_sA1,nP_00_tot) ))
print(paste0("Patient age 5-11  % shieldA: ", pct1(nP_05_tot_sA1,nP_05_tot) ))
print(paste0("Patient age 12-17 % shieldA: ", pct1(nP_12_tot_sA1,nP_12_tot) ))
print(paste0("Patient age 18-29 % shieldA: ", pct1(nP_18_tot_sA1,nP_18_tot) ))
print(paste0("Patient age 30-39 % shieldA: ", pct1(nP_30_tot_sA1,nP_30_tot) ))
print(paste0("Patient age 40-49 % shieldA: ", pct1(nP_40_tot_sA1,nP_40_tot) ))
print(paste0("Patient age 50-59 % shieldA: ", pct1(nP_50_tot_sA1,nP_50_tot) ))
print(paste0("Patient age 60-69 % shieldA: ", pct1(nP_60_tot_sA1,nP_60_tot) ))
print(paste0("Patient age 70+   % shieldA: ", pct1(nP_70_tot_sA1,nP_70_tot) ))


cat("\n")
cat("Shielding patients in hospital \n")
nP_Hosp_s1 = length( which(DAT$all_covid_hosp>0 & DAT$shielding=="High Risk"))
nP_Hosp_s0 = nP_Hosp - nP_Hosp_s1
nP_RH_s1   = length( which( is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shielding=="High Risk"))
nP_RH_s0   = nP_RH - nP_RH_s1
nP_DH_s1   = length( which(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shielding=="High Risk"))
nP_DH_s0   = nP_DH - nP_DH_s1
print(paste0("Shielding patients in hospital:             ", nP_Hosp_s1, ", %patients hosp:  ", pct1(nP_Hosp_s1,nP_Hosp) ))
print(paste0("Shielding patients in hosp recovered:       ", nP_RH_s1,   ", %sh patnts hosp: ", pct1(nP_RH_s1,nP_Hosp_s1) ))
print(paste0("Shielding patients in hospital died:        ", nP_DH_s1,   ", %sh patnts hosp: ", pct1(nP_DH_s1,nP_Hosp_s1) ))
print(paste0("Odds shielder dies in hosp (assuming same exposure): ", rd2((nP_DH_s1/nP_Hosp_s1) / (nP_DH_s0/nP_Hosp_s0)) ))

cat("\n")
cat("ShieldA patients in hospital \n")
nP_Hosp_sA1 = length( which(DAT$all_covid_hosp>0 & DAT$shieldA=="1"))
nP_Hosp_sA0 = nP_Hosp - nP_Hosp_sA1
nP_RH_sA1   = length( which( is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shieldA=="1"))
nP_RH_sA0   = nP_RH - nP_RH_sA1
nP_DH_sA1   = length( which(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shieldA=="1"))
nP_DH_sA0   = nP_DH - nP_DH_sA1
print(paste0("ShieldA patients in hospital:             ", nP_Hosp_sA1, ", %patients hosp:  ",  pct1(nP_Hosp_sA1,nP_Hosp) ))
print(paste0("ShieldA patients in hosp recovered:       ", nP_RH_sA1,   ", %shA patnts hosp: ", pct1(nP_RH_sA1,nP_Hosp_sA1) ))
print(paste0("ShieldA patients in hospital died:        ", nP_DH_sA1,   ", %shA patnts hosp: ", pct1(nP_DH_sA1,nP_Hosp_sA1) ))
print(paste0("Odds shieldAer dies in hosp (assuming same exposure): ", rd2((nP_DH_sA1/nP_Hosp_sA1) / (nP_DH_sA0/nP_Hosp_sA0)) ))

cat("\n")
cat("Shielding patients in hospital by age\n")
nP_Hosp_00_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
nP_Hosp_sum_s1 = nP_Hosp_00_s1+nP_Hosp_05_s1+nP_Hosp_12_s1+nP_Hosp_18_s1+nP_Hosp_30_s1+nP_Hosp_40_s1+nP_Hosp_50_s1+nP_Hosp_60_s1+nP_Hosp_70_s1
print(paste0("Shielding patients in hosp age 0-4:   ", tr(nP_Hosp_00_s1), ", %pats hosp 0-4:   ", pct1(nP_Hosp_00_s1,nP_Hosp_00) ))
print(paste0("Shielding patients in hosp age 5-11:  ", tr(nP_Hosp_05_s1), ", %pats hosp 5-11:  ", pct1(nP_Hosp_05_s1,nP_Hosp_05) ))
print(paste0("Shielding patients in hosp age 12-17: ", tr(nP_Hosp_12_s1), ", %pats hosp 12-17: ", pct1(nP_Hosp_12_s1,nP_Hosp_12) ))
print(paste0("Shielding patients in hosp age 18-29: ", tr(nP_Hosp_18_s1), ", %pats hosp 18-29: ", pct1(nP_Hosp_18_s1,nP_Hosp_18) ))
print(paste0("Shielding patients in hosp age 30-39: ", tr(nP_Hosp_30_s1), ", %pats hosp 30-39: ", pct1(nP_Hosp_30_s1,nP_Hosp_30) ))
print(paste0("Shielding patients in hosp age 40-49: ", tr(nP_Hosp_40_s1), ", %pats hosp 40-49: ", pct1(nP_Hosp_40_s1,nP_Hosp_40) ))
print(paste0("Shielding patients in hosp age 50-59: ", tr(nP_Hosp_50_s1), ", %pats hosp 50-59: ", pct1(nP_Hosp_50_s1,nP_Hosp_50) ))
print(paste0("Shielding patients in hosp age 60-69: ", tr(nP_Hosp_60_s1), ", %pats hosp 60-69: ", pct1(nP_Hosp_60_s1,nP_Hosp_60) ))
print(paste0("Shielding patients in hosp age 70+:   ", tr(nP_Hosp_70_s1), ", %pats hosp 70+:   ", pct1(nP_Hosp_70_s1,nP_Hosp_70) ))
print(paste0("Shielding patients in hosp total:     ", nP_Hosp_sum_s1,    ", %pats hosp:       ", pct1(nP_Hosp_sum_s1,nP_Hosp) ))

cat("\n")
cat("ShieldA patients in hospital by age\n")
nP_Hosp_00_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
nP_Hosp_sum_sA1 = nP_Hosp_00_sA1+nP_Hosp_05_sA1+nP_Hosp_12_sA1+nP_Hosp_18_sA1+nP_Hosp_30_sA1+nP_Hosp_40_sA1+nP_Hosp_50_sA1+nP_Hosp_60_sA1+nP_Hosp_70_sA1
print(paste0("Shielding patients in hosp age 0-4:   ", tr(nP_Hosp_00_sA1), ", %pats hosp 0-4:   ", pct1(nP_Hosp_00_sA1,nP_Hosp_00) ))
print(paste0("Shielding patients in hosp age 5-11:  ", tr(nP_Hosp_05_sA1), ", %pats hosp 5-11:  ", pct1(nP_Hosp_05_sA1,nP_Hosp_05) ))
print(paste0("Shielding patients in hosp age 12-17: ", tr(nP_Hosp_12_sA1), ", %pats hosp 12-17: ", pct1(nP_Hosp_12_sA1,nP_Hosp_12) ))
print(paste0("Shielding patients in hosp age 18-29: ", tr(nP_Hosp_18_sA1), ", %pats hosp 18-29: ", pct1(nP_Hosp_18_sA1,nP_Hosp_18) ))
print(paste0("Shielding patients in hosp age 30-39: ", tr(nP_Hosp_30_sA1), ", %pats hosp 30-39: ", pct1(nP_Hosp_30_sA1,nP_Hosp_30) ))
print(paste0("Shielding patients in hosp age 40-49: ", tr(nP_Hosp_40_sA1), ", %pats hosp 40-49: ", pct1(nP_Hosp_40_sA1,nP_Hosp_40) ))
print(paste0("Shielding patients in hosp age 50-59: ", tr(nP_Hosp_50_sA1), ", %pats hosp 50-59: ", pct1(nP_Hosp_50_sA1,nP_Hosp_50) ))
print(paste0("Shielding patients in hosp age 60-69: ", tr(nP_Hosp_60_sA1), ", %pats hosp 60-69: ", pct1(nP_Hosp_60_sA1,nP_Hosp_60) ))
print(paste0("Shielding patients in hosp age 70+:   ", tr(nP_Hosp_70_sA1), ", %pats hosp 70+:   ", pct1(nP_Hosp_70_sA1,nP_Hosp_70) ))
print(paste0("Shielding patients in hosp total:     ", nP_Hosp_sum_sA1,    ", %pats hosp:       ", pct1(nP_Hosp_sum_sA1,nP_Hosp) ))

cat("\n")
cat("Shielding/Not deaths outside hospital \n")
nP_DO_s1 = length( which(!is.na(DAT$ons_death_date) 
           & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) & DAT$shielding=="High Risk" ))
nP_DO_s0 = nP_DO - nP_DO_s1
print(paste0("Shielding patients died outside hospital:    ", nP_DO_s1,  ", %deaths out hosp: ", pct1(nP_DO_s1,nP_DO) ))
print(paste0("Non-shield patients died outside hospital:   ", nP_DO_s0,  ", %deaths out hosp: ", pct1(nP_DO_s0,nP_DO) ))

cat("\n")
cat("ShieldA/Not deaths outside hospital \n")
nP_DO_sA1 = length( which(!is.na(DAT$ons_death_date) 
                         & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) & DAT$shieldA=="1" ))
nP_DO_sA0 = nP_DO - nP_DO_sA1
print(paste0("ShieldA patients died outside hospital:     ", nP_DO_sA1,  ", %deaths out hosp: ", pct1(nP_DO_sA1,nP_DO) ))
print(paste0("Non-shieldA patients died outside hospital: ", nP_DO_sA0,  ", %deaths out hosp: ", pct1(nP_DO_sA0,nP_DO) ))

cat("\n")
cat("Shielding deaths in hospital by age \n")
##NOTE: NOT NEC BY SHIELD: men_time_to_death_1, men_time_to_recover_1 (time to death since last admission 1x) - assume are shielding independent
nP_DH_00_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4" ))
nP_DH_05_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11" ))
nP_DH_12_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17" ))
nP_DH_18_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29" ))
nP_DH_30_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39" ))
nP_DH_40_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49" ))
nP_DH_50_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59" ))
nP_DH_60_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69" ))
nP_DH_70_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="70+" ))
nP_DH_sum_s1  = (nP_DH_00_s1+nP_DH_05_s1+nP_DH_12_s1+nP_DH_18_s1+nP_DH_30_s1+nP_DH_40_s1+nP_DH_50_s1+nP_DH_60_s1+nP_DH_70_s1)
print(paste0("Shielding patients died in hosp age 0-4:   ", tr(nP_DH_00_s1), ", %hosp sh deaths: ", pct1(nP_DH_00_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 5-11:  ", tr(nP_DH_05_s1), ", %hosp sh deaths: ", pct1(nP_DH_05_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 12-17: ", tr(nP_DH_12_s1), ", %hosp sh deaths: ", pct1(nP_DH_12_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 18-29: ", tr(nP_DH_18_s1), ", %hosp sh deaths: ", pct1(nP_DH_18_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 30-39: ", tr(nP_DH_30_s1), ", %hosp sh deaths: ", pct1(nP_DH_30_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 40-49: ", tr(nP_DH_40_s1), ", %hosp sh deaths: ", pct1(nP_DH_40_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 50-59: ", tr(nP_DH_50_s1), ", %hosp sh deaths: ", pct1(nP_DH_50_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 60-69: ", tr(nP_DH_60_s1), ", %hosp sh deaths: ", pct1(nP_DH_60_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 70+:   ", tr(nP_DH_70_s1), ", %hosp sh deaths: ", pct1(nP_DH_70_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp total:     ", nP_DH_sum_s1,    ", %hosp sh deaths: ", pct1(nP_DH_sum_s1,nP_DH_s1) ))

cat("\n")
cat("ShieldA deaths in hospital by age \n")
##NOTE: NOT NEC BY SHIELD: men_time_to_death_1, men_time_to_recover_1 (time to death since last admission 1x) - assume are shielding independent
nP_DH_00_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4" ))
nP_DH_05_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11" ))
nP_DH_12_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17" ))
nP_DH_18_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29" ))
nP_DH_30_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39" ))
nP_DH_40_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49" ))
nP_DH_50_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59" ))
nP_DH_60_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69" ))
nP_DH_70_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="70+" ))
nP_DH_sum_sA1  = (nP_DH_00_sA1+nP_DH_05_sA1+nP_DH_12_sA1+nP_DH_18_sA1+nP_DH_30_sA1+nP_DH_40_sA1+nP_DH_50_sA1+nP_DH_60_sA1+nP_DH_70_sA1)
print(paste0("ShieldA patients died in hosp age 0-4:   ", tr(nP_DH_00_sA1), ", %hosp sh deaths: ", pct1(nP_DH_00_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 5-11:  ", tr(nP_DH_05_sA1), ", %hosp sh deaths: ", pct1(nP_DH_05_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 12-17: ", tr(nP_DH_12_sA1), ", %hosp sh deaths: ", pct1(nP_DH_12_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 18-29: ", tr(nP_DH_18_sA1), ", %hosp sh deaths: ", pct1(nP_DH_18_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 30-39: ", tr(nP_DH_30_sA1), ", %hosp sh deaths: ", pct1(nP_DH_30_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 40-49: ", tr(nP_DH_40_sA1), ", %hosp sh deaths: ", pct1(nP_DH_40_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 50-59: ", tr(nP_DH_50_sA1), ", %hosp sh deaths: ", pct1(nP_DH_50_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 60-69: ", tr(nP_DH_60_sA1), ", %hosp sh deaths: ", pct1(nP_DH_60_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 70+:   ", tr(nP_DH_70_sA1), ", %hosp sh deaths: ", pct1(nP_DH_70_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp total:     ", nP_DH_sum_sA1,    ", %hosp sh deaths: ", pct1(nP_DH_sum_sA1,nP_DH_sA1) ))

cat("\n")
cat("Shielding deaths in hospital - fatality proportion by age \n")
mfraction_00_s1 = rd3(nP_DH_00_s1/nP_Hosp_00_s1) #frt2(nP_DH_00_s1,nP_Hosp_00_s1)
mfraction_05_s1 = rd3(nP_DH_05_s1/nP_Hosp_05_s1)
mfraction_12_s1 = rd3(nP_DH_12_s1/nP_Hosp_12_s1)
mfraction_18_s1 = rd3(nP_DH_18_s1/nP_Hosp_18_s1)
mfraction_30_s1 = rd3(nP_DH_30_s1/nP_Hosp_30_s1)
mfraction_40_s1 = rd3(nP_DH_40_s1/nP_Hosp_40_s1)
mfraction_50_s1 = rd3(nP_DH_50_s1/nP_Hosp_50_s1)
mfraction_60_s1 = rd3(nP_DH_60_s1/nP_Hosp_60_s1)
mfraction_70_s1 = rd3(nP_DH_70_s1/nP_Hosp_70_s1)
print(paste0("Shielding hosp mfraction age 0-4:     ", mfraction_00_s1 ))
print(paste0("Shielding hosp mfraction age 5-11:    ", mfraction_05_s1 ))
print(paste0("Shielding hosp mfraction age 12-17:   ", mfraction_12_s1 ))
print(paste0("Shielding hosp mfraction age 18-29:   ", mfraction_18_s1 ))
print(paste0("Shielding hosp mfraction age 30-39:   ", mfraction_30_s1 ))
print(paste0("Shielding hosp mfraction age 40-49:   ", mfraction_40_s1 ))
print(paste0("Shielding hosp mfraction age 50-59:   ", mfraction_50_s1 ))
print(paste0("Shielding hosp mfraction age 60-69:   ", mfraction_60_s1 ))
print(paste0("Shielding hosp mfraction age 70+:     ", mfraction_70_s1 ))

cat("\n")
cat("ShieldA deaths in hospital - fatality proportion by age \n")
mfraction_00_sA1 = rd3(nP_DH_00_sA1/nP_Hosp_00_sA1) #frt2(nP_DH_00_sA1,nP_Hosp_00_sA1)
mfraction_05_sA1 = rd3(nP_DH_05_sA1/nP_Hosp_05_sA1)
mfraction_12_sA1 = rd3(nP_DH_12_sA1/nP_Hosp_12_sA1)
mfraction_18_sA1 = rd3(nP_DH_18_sA1/nP_Hosp_18_sA1)
mfraction_30_sA1 = rd3(nP_DH_30_sA1/nP_Hosp_30_sA1)
mfraction_40_sA1 = rd3(nP_DH_40_sA1/nP_Hosp_40_sA1)
mfraction_50_sA1 = rd3(nP_DH_50_sA1/nP_Hosp_50_sA1)
mfraction_60_sA1 = rd3(nP_DH_60_sA1/nP_Hosp_60_sA1)
mfraction_70_sA1 = rd3(nP_DH_70_sA1/nP_Hosp_70_sA1)
print(paste0("ShieldA hosp mfraction age 0-4:     ", mfraction_00_sA1 ))
print(paste0("ShieldA hosp mfraction age 5-11:    ", mfraction_05_sA1 ))
print(paste0("ShieldA hosp mfraction age 12-17:   ", mfraction_12_sA1 ))
print(paste0("ShieldA hosp mfraction age 18-29:   ", mfraction_18_sA1 ))
print(paste0("ShieldA hosp mfraction age 30-39:   ", mfraction_30_sA1 ))
print(paste0("ShieldA hosp mfraction age 40-49:   ", mfraction_40_sA1 ))
print(paste0("ShieldA hosp mfraction age 50-59:   ", mfraction_50_sA1 ))
print(paste0("ShieldA hosp mfraction age 60-69:   ", mfraction_60_sA1 ))
print(paste0("ShieldA hosp mfraction age 70+:     ", mfraction_70_sA1 ))

sink()


####NOT RELEVANT
####-Patients that neither died nor were hospitalised
####-covid_hosp_cat ~ "COVID-19 hospitalisations per person (n)" - is in all_covid_hosp






### Second filtering - remove carehomes & multiple hospitalisations ############
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
  #mutate(shield1  = ifelse(shielding_v1_binary==TRUE,1,0)) %>% # number: 0:1 - hHigh Risk before 2020-04-21
  #mutate(shield1  = ifelse(is.na(shield1), 0, shield1))    %>% #
  #mutate(shield1_date = shielding_v1_startdate)            %>% # date, or NA if shield1=0
  #mutate(shield1 = as.numeric( min(c(admission_date, ons_death_date), na.rm = T) > shielding_v1_startdate) ) %>% #Limit shielding by date
  ###Restrict to deaths or hospitalisations, not neither
  filter(!is.na(ons_death_date) | all_covid_hosp>0)          %>%   # Tried "| !is.na(admission_date))": too few
  ###Refer admissions to first admission (NB: removed pivot_longer, hence one row per patient)
  mutate(admission_date  = covid_hosp_admitted_1)             %>%
  ###Refer discharge to first discharge 
  mutate(discharge_date  = covid_hosp_discharge_1)            %>%
  ###(Discarded) Refer discharge to last discharge 
  ###Each patient has one row - other vars replaced by numeric flags
  select(-c(care_home_nursing, care_home, #shielding, #patient_id, #all_covid_hosp, 
            dplyr::contains("hosp_admitted"),
            dplyr::contains("hosp_discharge"), 
            dplyr::contains("hirisk"), 
            shielding_v1_binary, shielding_v1_startdate))    %>%   # no longer need 
  ###For now, drop these
  #select(-c(shield1,shield1_date))                           %>%
  ungroup()


# Stats: filter 3 (cohort)
##- exc patients with missing age 
##- exc patients who died of non-covid causes  Jan-2020 - Sep-2021
##      NOTE: could include some covid hospitalisations that did not lead to covid death
##- exc patients with covid H, D events during Dec-2020 - Sep-2021
##- exc CH
nP_tot_f3       = dim(DAT)[1]
nP_tot_f3_id    = sum(!is.na(DAT$patient_id), na.rm = T)      #=nP_tot_f2 (id filter alreday applied)
#
nP_tot_f3_s1    = sum(!is.na(DAT$patient_id) & DAT$shielding=="High Risk", na.rm = T)
nP_tot_f3_s0    = nP_tot_f3 - nP_tot_f3_s1
nP_tot_f3_sA1   = sum(!is.na(DAT$patient_id) & DAT$shieldA=="1", na.rm = T)
nP_tot_f3_sA0   = nP_tot_f3 - nP_tot_f3_sA1
#
print(paste0("Filter 3 - exclude patients with non-covid death - Cohort2"))
print(paste0("Filter 3: data rows:                                   ", nP_tot_f3))
print(paste0("Filter 3: has patient id:                              ", nP_tot_f3_id))
print(paste0("Filter 3: patient shielding (High Risk by 2021-09-01): ", nP_tot_f3_s1))
print(paste0("Filter 3: patient not shielding:                       ", nP_tot_f3_s0))
print(paste0("Filter 3: patient shieldA (High Risk by 2020-12-01):   ", nP_tot_f3_sA1))
print(paste0("Filter 3: patient not shieldA:                         ", nP_tot_f3_sA0))


names2 = names(DAT)
print(paste0("names2: ", names2))
cat("\n")

#NOT RELEVANT: covid_hosp_admitted_i  (want admission_date=covid_hosp_admitted_1), 
#              covid_hosp_discharge_i (want discharge_date=covid_hospt_discharge_1)
#RELEVANT: all_covid_hosp (to filter in: 0, 1, and out: 2-6), 



######## QUESTIONS wo CH & wo multiple H #######################################

sink(file = paste0(output_dir, "/", jobno, filename1b, ".txt"),append=F,split=F)

cat("Patients & hospitalisations - without CH \n")
cat("\n")

print(paste0("Date range: ", Date1," to ", Date2))

cat("\n")
print(paste0("Dataset rows ", dim(DAT)[1], " and columns ", dim(DAT)[2] ))
cat("\n")
cat("Patients \n")
nP = sum(!is.na(DAT$patient_id), na.rm = T)
print(paste0("Patient entries:     ", nP ))
print(paste0("Unique patients:     ", length(unique(DAT$patient_id)) )) #=> row <> one patient
print(paste0("Missing patient id:  ", sum(is.na(DAT$patient_id)) ))
nP_HorD   = sum(!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0)
nP_noHorD = sum( is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) )
print(paste0("Patients with covid hosp or death events:  ", nP_HorD,   ", %all patients: ", pct1(nP_HorD,  nP) ))
print(paste0("Patients wout covid hosp or death events:  ", nP_noHorD, ", %all patients: ", pct1(nP_noHorD,nP) ))

cat("\n")
cat("Patients by age \n")
nP_00 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="0-4", na.rm = T)
nP_05 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="5-11", na.rm = T)
nP_12 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="12-17", na.rm = T)
nP_18 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="18-29", na.rm = T)
nP_30 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="30-39", na.rm = T)
nP_40 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="40-49", na.rm = T)
nP_50 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="50-59", na.rm = T)
nP_60 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="60-69", na.rm = T)
nP_70 = sum(!is.na(DAT$patient_id) & DAT$age_cat=="70+", na.rm = T)
nP_sum = (nP_00+nP_05+nP_12+nP_18+nP_30+nP_40+nP_50+nP_60+nP_70)
print(paste0("Patient % age 0-4:   ", pct1(nP_00,nP) )); 
print(paste0("Patient % age 5-11:  ", pct1(nP_05,nP) ));
print(paste0("Patient % age 12-17: ", pct1(nP_12,nP) ));
print(paste0("Patient % age 18-29: ", pct1(nP_18,nP) ));
print(paste0("Patient % age 30-39: ", pct1(nP_30,nP) ));
print(paste0("Patient % age 40-49: ", pct1(nP_40,nP) ));
print(paste0("Patient % age 50-59: ", pct1(nP_50,nP) ));
print(paste0("Patient % age 60-69: ", pct1(nP_60,nP) ));
print(paste0("Patient % age 70+:   ", pct1(nP_70,nP) ));
print(paste0("Patient % all:       ", pct1(nP_sum,nP) ));

cat("\n")
cat("Hospitalised patients \n")
nP_Hosp    = length( which(DAT$all_covid_hosp>0))
print(paste0("Ever hospitalised (>=once):  ", nP_Hosp))

cat("\n")
cat("Hospitalisations (inc re-admissions) \n")
nH       = sum(DAT$all_covid_hosp, na.rm = T)
nH_70    = sum(DAT$all_covid_hosp[which(DAT$age_cat=="70+")],na.rm=T)
print(paste0("Hospitalisations:            ", nH ))
print(paste0("Hospitalisations age +70:    ", nH_70 ))

cat("\n")
cat("Hospitalised patients (any no. admissions) by age \n") #NB not relevant to restrict to 1st hospitalisation
nP_Hosp_00  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") ) #NB: 'admitted' matters here, no.admissions doens't
nP_Hosp_05  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70  = length( which(DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
nP_Hosp_sum = nP_Hosp_00+nP_Hosp_05+nP_Hosp_12+nP_Hosp_18+nP_Hosp_30+nP_Hosp_40+nP_Hosp_50+nP_Hosp_60+nP_Hosp_70
print(paste0("Patients hospitalised age 0-4:   ", tr(nP_Hosp_00), ", %patients hosp: ", pct1(nP_Hosp_00,nP_Hosp) ))
print(paste0("Patients hospitalised age 5-11:  ", tr(nP_Hosp_05), ", %patients hosp: ", pct1(nP_Hosp_05,nP_Hosp) ))
print(paste0("Patients hospitalised age 12-17: ", tr(nP_Hosp_12), ", %patients hosp: ", pct1(nP_Hosp_12,nP_Hosp) ))
print(paste0("Patients hospitalised age 18-29: ", tr(nP_Hosp_18), ", %patients hosp: ", pct1(nP_Hosp_18,nP_Hosp) ))
print(paste0("Patients hospitalised age 30-39: ", tr(nP_Hosp_30), ", %patients hosp: ", pct1(nP_Hosp_30,nP_Hosp) ))
print(paste0("Patients hospitalised age 40-49: ", tr(nP_Hosp_40), ", %patients hosp: ", pct1(nP_Hosp_40,nP_Hosp) ))
print(paste0("Patients hospitalised age 50-59: ", tr(nP_Hosp_50), ", %patients hosp: ", pct1(nP_Hosp_50,nP_Hosp) ))
print(paste0("Patients hospitalised age 60-69: ", tr(nP_Hosp_60), ", %patients hosp: ", pct1(nP_Hosp_60,nP_Hosp) ))
print(paste0("Patients hospitalised age 70+:   ", tr(nP_Hosp_70), ", %patients hosp: ", pct1(nP_Hosp_70,nP_Hosp) ))
print(paste0("Patients hospitalised total %:   ", pct1(nP_Hosp_sum,nP_Hosp) ))

cat("\n")
cat("Hospitalised patients (any no. admissions) by age - prior 2020-06-30 \n")
nP_Hosp_00a  = length( which(DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05a  = length( which(DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12a  = length( which(DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18a  = length( which(DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30a  = length( which(DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40a  = length( which(DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50a  = length( which(DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60a  = length( which(DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70a  = length( which(DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
nP_Hosp_suma = nP_Hosp_00a+nP_Hosp_05a+nP_Hosp_12a+nP_Hosp_18a+nP_Hosp_30a+nP_Hosp_40a+nP_Hosp_50a+nP_Hosp_60a+nP_Hosp_70a

cat("\n")
cat("Hospitalised patients (any no. admissions) by age - after 2020-06-30 \n")
nP_Hosp_00b  = length( which(DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05b  = length( which(DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12b  = length( which(DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18b  = length( which(DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30b  = length( which(DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40b  = length( which(DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50b  = length( which(DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60b  = length( which(DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70b  = length( which(DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
nP_Hosp_sumb = nP_Hosp_00b+nP_Hosp_05b+nP_Hosp_12b+nP_Hosp_18b+nP_Hosp_30b+nP_Hosp_40b+nP_Hosp_50b+nP_Hosp_60b+nP_Hosp_70b


cat("\n")
cat("Hospitalised patients by number of admissions \n")
nP_Hosp1  = length( which(DAT$all_covid_hosp==1))
nP_Hospgt1= length( which(DAT$all_covid_hosp>1))
nP_Hosp2  = length( which(DAT$all_covid_hosp==2))
nP_Hosp3  = length( which(DAT$all_covid_hosp==3))
nP_Hospgt3= length( which(DAT$all_covid_hosp>3))
print(paste0("Hospitalised with 1 admission:  ", nP_Hosp1,   ", %patients hosp: ", pct1(nP_Hosp1,  nP_Hosp) ))
print(paste0("Hospitalised more than 1x:      ", nP_Hospgt1, ", %patients hosp: ", pct1(nP_Hospgt1,nP_Hosp) ))
print(paste0("Hospitalised once or more:      ", nP_Hosp1+nP_Hospgt1, ", %patients hosp: ", pct1((nP_Hosp1+nP_Hospgt1),nP_Hosp) ))
print(paste0("Hospitalised with 2 admissions: ", nP_Hosp2,   ", %patients hosp: ", pct1(nP_Hosp2,  nP_Hosp) ))
print(paste0("Hospitalised with 3 admissions: ", nP_Hosp3,   ", %patients hosp: ", pct1(nP_Hosp3,  nP_Hosp) ))
print(paste0("Hospitalised more than 3x:      ", nP_Hospgt3, ", %patients hosp: ", pct1(nP_Hospgt3,nP_Hosp) ))

cat("\n")
cat("Hospital re-admissions \n")
nH_all1    = sum(DAT$all_covid_hosp[which(DAT$all_covid_hosp==1)], na.rm = T)
nH_allgt1  = sum(DAT$all_covid_hosp[which(DAT$all_covid_hosp>1)], na.rm = T)    #all admissions of readmitted patients
nH_1st     = length(which(DAT$all_covid_hosp>1)) #first admissions in readmitted patients
nH_post1st = nH_allgt1 - nH_1st
print(paste0("Hospitalisations without re-admission:  ", nH_all1,    ", %all hosp: ", pct1(nH_all1,   nH) ))
print(paste0("Hospitalisations, 1st pre re-admission: ", nH_1st,     ", %all hosp: ", pct1(nH_1st,    nH) ))
print(paste0("All re-admissions (exc 1st admission):  ", nH_post1st, ", %all hosp: ", pct1(nH_post1st,nH) ))
print(paste0("Total of these three:                   ", nH_all1+nH_1st+nH_post1st, ", %all hosp: ", pct1((nH_all1+nH_1st+nH_post1st),nH) ))
print(paste0("Assumed hosp here - discard 'All re-admissions (exc 1st admission)' ",
             sum(!is.na(DAT$admission_date)) ))
nP_Hospgt2 = nP_Hospgt1 - nP_Hosp2
print(paste0("All 2nd re-admissions:  ", nP_Hospgt1, " = patients hosp >1x, %all hosp: ", pct1(nP_Hospgt1,nH) ))
print(paste0("All 3rd re-admissions:  ", nP_Hospgt2, " = patients hosp >2x, %all hosp: ", pct1(nP_Hospgt2,nH) ))
print(paste0("All post 3rd re-admis:  ", nP_Hospgt3, " = patients hosp >3x, %all hosp: ", pct1(nP_Hospgt3,nH) ))

sink()



sink(file = paste0(output_dir, "/", jobno, filename2b, ".txt"),append=F,split=F)

cat("Deaths - without CH \n")
cat("\n")

cat("Deaths \n")
nP_D = sum(!is.na(DAT$ons_death_date))
print(paste0("Patients that died: ", nP_D))

cat("\n")
cat("Deaths in hospital \n")
nP_DH  = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0, na.rm = T)
nP_RH  = nP_Hosp - nP_DH
nP_DH1 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==1, na.rm = T)
nP_DH2 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==2, na.rm = T)
nP_DH3 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp==3, na.rm = T)
nP_DH4to6 = sum(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>=4, na.rm = T)
print(paste0("Patients died in hospital: ", nP_DH,     ", mfraction     ", frt2(nP_DH,    nP_Hosp), ", %all deaths: ", pct1(nP_DH,nP_D) ))
print(paste0("Patients recovered:        ", nP_RH,     ", 1-mfraction   ", frt2(nP_RH,    nP_Hosp) ))
print(paste0("Deaths upon 1st admission: ", nP_DH1,    ", %hosp deaths: ", pct1(nP_DH1,   nP_DH) ))
print(paste0("Deaths upon 2nd admission: ", nP_DH2,    ", %hosp deaths: ", pct1(nP_DH2,   nP_DH) ))
print(paste0("Deaths upon 3rd admission: ", nP_DH3,    ", %hosp deaths: ", pct1(nP_DH3,   nP_DH) ))
print(paste0("Deaths upon >3 admissions: ", nP_DH4to6, ", %hosp deaths: ", pct1(nP_DH4to6,nP_DH) ))
print(paste0("Deaths total 1+ admission: ", nP_DH1+nP_DH2+nP_DH3+nP_DH4to6, ", %hosp deaths: ", pct1((nP_DH1+nP_DH2+nP_DH3+nP_DH4to6),nP_DH) ))

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
nP_DH_sum = (nP_DH_00+nP_DH_05+nP_DH_12+nP_DH_18+nP_DH_30+nP_DH_40+nP_DH_50+nP_DH_60+nP_DH_70)
print(paste0("Patients died in hopital age 0-4:   ", tr(nP_DH_00), ", %hosp deaths: ", pct1(nP_DH_00,nP_DH) ))
print(paste0("Patients died in hopital age 5-11:  ", tr(nP_DH_05), ", %hosp deaths: ", pct1(nP_DH_05,nP_DH) ))
print(paste0("Patients died in hopital age 12-17: ", tr(nP_DH_12), ", %hosp deaths: ", pct1(nP_DH_12,nP_DH) ))
print(paste0("Patients died in hopital age 18-29: ", tr(nP_DH_18), ", %hosp deaths: ", pct1(nP_DH_18,nP_DH) ))
print(paste0("Patients died in hopital age 30-39: ", tr(nP_DH_30), ", %hosp deaths: ", pct1(nP_DH_30,nP_DH) ))
print(paste0("Patients died in hopital age 40-49: ", tr(nP_DH_40), ", %hosp deaths: ", pct1(nP_DH_40,nP_DH) ))
print(paste0("Patients died in hopital age 50-59: ", tr(nP_DH_50), ", %hosp deaths: ", pct1(nP_DH_50,nP_DH) ))
print(paste0("Patients died in hopital age 60-69: ", tr(nP_DH_60), ", %hosp deaths: ", pct1(nP_DH_60,nP_DH) ))
print(paste0("Patients died in hopital age 70+:   ", tr(nP_DH_70), ", %hosp deaths: ", pct1(nP_DH_70,nP_DH) ))
print(paste0("Patients died in hopital total:     ", nP_DH_sum,    ", %hosp deaths: ", pct1(nP_DH_sum,nP_DH) ))

###Mortality fraction 
mfraction_00 = rd3(nP_DH_00/nP_Hosp_00) # frt2(nP_DH_00,nP_Hosp_00)
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
cat("Deaths in hospital by age - prior 2020-06-30 \n")
nP_DH_00a = sum(DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4", na.rm = T)
nP_DH_05a = sum(DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11", na.rm = T)
nP_DH_12a = sum(DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17", na.rm = T)
nP_DH_18a = sum(DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29", na.rm = T)
nP_DH_30a = sum(DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39", na.rm = T)
nP_DH_40a = sum(DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49", na.rm = T)
nP_DH_50a = sum(DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59", na.rm = T)
nP_DH_60a = sum(DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69", na.rm = T)
nP_DH_70a = sum(DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+", na.rm = T)
nP_DH_suma = (nP_DH_00a+nP_DH_05a+nP_DH_12a+nP_DH_18a+nP_DH_30a+nP_DH_40a+nP_DH_50a+nP_DH_60a+nP_DH_70a)
cat("\n")
cat("Deaths in hospital by age - after 2020-06-30 \n")
nP_DH_00b = sum(DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4", na.rm = T)
nP_DH_05b = sum(DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11", na.rm = T)
nP_DH_12b = sum(DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17", na.rm = T)
nP_DH_18b = sum(DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29", na.rm = T)
nP_DH_30b = sum(DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39", na.rm = T)
nP_DH_40b = sum(DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49", na.rm = T)
nP_DH_50b = sum(DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59", na.rm = T)
nP_DH_60b = sum(DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69", na.rm = T)
nP_DH_70b = sum(DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+", na.rm = T)
nP_DH_sumb = (nP_DH_00b+nP_DH_05b+nP_DH_12b+nP_DH_18b+nP_DH_30b+nP_DH_40b+nP_DH_50b+nP_DH_60b+nP_DH_70b)

###Mortality fraction - prior 2020-06-30
mfraction_00a = rd3(nP_DH_00a/nP_Hosp_00a)
mfraction_05a = rd3(nP_DH_05a/nP_Hosp_05a)
mfraction_12a = rd3(nP_DH_12a/nP_Hosp_12a)
mfraction_18a = rd3(nP_DH_18a/nP_Hosp_18a)
mfraction_30a = rd3(nP_DH_30a/nP_Hosp_30a)
mfraction_40a = rd3(nP_DH_40a/nP_Hosp_40a)
mfraction_50a = rd3(nP_DH_50a/nP_Hosp_50a)
mfraction_60a = rd3(nP_DH_60a/nP_Hosp_60a)
mfraction_70a = rd3(nP_DH_70a/nP_Hosp_70a)
print(paste0("Hopital prior 2020-06-30 mfraction age 0-4:     ", mfraction_00a ))
print(paste0("Hopital prior 2020-06-30 mfraction age 5-11:    ", mfraction_05a ))
print(paste0("Hopital prior 2020-06-30 mfraction age 12-17:   ", mfraction_12a ))
print(paste0("Hopital prior 2020-06-30 mfraction age 18-29:   ", mfraction_18a ))
print(paste0("Hopital prior 2020-06-30 mfraction age 30-39:   ", mfraction_30a ))
print(paste0("Hopital prior 2020-06-30 mfraction age 40-49:   ", mfraction_40a ))
print(paste0("Hopital prior 2020-06-30 mfraction age 50-59:   ", mfraction_50a ))
print(paste0("Hopital prior 2020-06-30 mfraction age 60-69:   ", mfraction_60a ))
print(paste0("Hopital prior 2020-06-30 mfraction age 70+:     ", mfraction_70a ))
###Mortality fraction - after 2020-06-30
mfraction_00b = rd3(nP_DH_00b/nP_Hosp_00b)
mfraction_05b = rd3(nP_DH_05b/nP_Hosp_05b)
mfraction_12b = rd3(nP_DH_12b/nP_Hosp_12b)
mfraction_18b = rd3(nP_DH_18b/nP_Hosp_18b)
mfraction_30b = rd3(nP_DH_30b/nP_Hosp_30b)
mfraction_40b = rd3(nP_DH_40b/nP_Hosp_40b)
mfraction_50b = rd3(nP_DH_50b/nP_Hosp_50b)
mfraction_60b = rd3(nP_DH_60b/nP_Hosp_60b)
mfraction_70b = rd3(nP_DH_70b/nP_Hosp_70b)
print(paste0("Hopital after 2020-06-30 mfraction age 0-4:     ", mfraction_00b ))
print(paste0("Hopital after 2020-06-30 mfraction age 5-11:    ", mfraction_05b ))
print(paste0("Hopital after 2020-06-30 mfraction age 12-17:   ", mfraction_12b ))
print(paste0("Hopital after 2020-06-30 mfraction age 18-29:   ", mfraction_18b ))
print(paste0("Hopital after 2020-06-30 mfraction age 30-39:   ", mfraction_30b ))
print(paste0("Hopital after 2020-06-30 mfraction age 40-49:   ", mfraction_40b ))
print(paste0("Hopital after 2020-06-30 mfraction age 50-59:   ", mfraction_50b ))
print(paste0("Hopital after 2020-06-30 mfraction age 60-69:   ", mfraction_60b ))
print(paste0("Hopital after 2020-06-30 mfraction age 70+:     ", mfraction_70b ))



cat("\n")
cat("Deaths outside hospital \n")
nP_DO      = sum(!is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)), na.rm = T)
print(paste0("Patients died outside hosp:   ", nP_DO, ", %all deaths:     ", pct1(nP_DO,nP_D) ))


cat("\n")
cat("Deaths in hospital - average time to death since 1st admission (the admission_date) \n")
cat("                   - assumption: merging with duration any subsequent admissions \n")
cat("                   - assumption: shield/Not have same biological and logitical time contraints \n")

###Across all ages - as the mortality fraction is age specific
i_hopitalised = which(DAT$all_covid_hosp>0)
men_time_to_death = rd3(as.numeric(   mean(
  DAT$ons_death_date[i_hopitalised] 
- DAT$admission_date[i_hopitalised], na.rm =T)) )   #removes patients with ons_death_date=NA up 01-12-2020
med_time_to_death = rd3(as.numeric( median(
  DAT$ons_death_date[i_hopitalised] 
- DAT$admission_date[i_hopitalised], na.rm =T)) )
print(paste0("Mean (median) time to death since 1st admission: ", men_time_to_death, " (", med_time_to_death, ")" ))

###Period 1 - 5 month to 2020-06-30
i_hospitalised_died_by_jun = which(DAT$all_covid_hosp>0 & DAT$ons_death_date<="2020-06-30")
men_time_to_death_1 = rd3(as.numeric(   mean(      #indices identify patient, use same for admission & death
  DAT$ons_death_date[i_hospitalised_died_by_jun] 
- DAT$admission_date[i_hospitalised_died_by_jun], na.rm =T)) )
med_time_to_death_1 = rd3(as.numeric( median(
  DAT$ons_death_date[i_hospitalised_died_by_jun] 
- DAT$admission_date[i_hospitalised_died_by_jun], na.rm =T)) )

###Period 2 - 5 month after 2020-06-30
i_hospitalised_died_af_jun = which(DAT$all_covid_hosp>0 & DAT$ons_death_date>"2020-06-30")
men_time_to_death_2 = rd3(as.numeric(   mean(
  DAT$ons_death_date[i_hospitalised_died_af_jun] 
- DAT$admission_date[i_hospitalised_died_af_jun], na.rm =T)) )
med_time_to_death_2 = rd3(as.numeric( median(
  DAT$ons_death_date[i_hospitalised_died_af_jun] 
- DAT$admission_date[i_hospitalised_died_af_jun], na.rm =T)) )

print(paste0("Mean (median) time to death since 1st admission pre  2020-06-30: ", men_time_to_death_1, " (", med_time_to_death_1, ")" ))
print(paste0("Mean (median) time to death since 1st admission post 2020-06-30: ", men_time_to_death_2, " (", med_time_to_death_2, ")" ))


cat("\n")
cat("Recovery in hospital - average time to 1st discharge \n") 
cat("                     - assumption: discarding time in subsequent admissions \n")

###Across all ages - as the fraction of mortality is age specific
men_time_to_recover  = rd3(as.numeric(   mean(DAT$discharge_date  - DAT$admission_date, na.rm =T)) ) #if one date exists so does the other
med_time_to_recover  = rd3(as.numeric( median(DAT$discharge_date  - DAT$admission_date, na.rm =T)) )
print(paste0("Mean (median) time from 1st admission to 1st discharge:  ", men_time_to_recover, " (", med_time_to_recover, ")" ))

###Period 1 - 5 month to 2020-06-30
i_discharged_by_jun = which(DAT$discharge_date<="2020-06-30")
men_time_to_recover_1  = rd3(as.numeric(   mean(
  DAT$discharge_date[i_discharged_by_jun]
- DAT$admission_date[i_discharged_by_jun], na.rm =T)) )
med_time_to_recover_1  = rd3(as.numeric( median(
  DAT$discharge_date[i_discharged_by_jun]
- DAT$admission_date[i_discharged_by_jun], na.rm =T)) )

###Period 2 - 5 month after 2020-06-30
i_discharged_af_jun = which(DAT$discharge_date>"2020-06-30")
men_time_to_recover_2  = rd3(as.numeric(   mean(
  DAT$discharge_date[i_discharged_af_jun]
- DAT$admission_date[i_discharged_af_jun], na.rm =T)) )
med_time_to_recover_2  = rd3(as.numeric( median(
  DAT$discharge_date[i_discharged_af_jun]
- DAT$admission_date[i_discharged_af_jun], na.rm =T)) )

print(paste0("Mean (median) time from 1st admission to 1st discharge pre  2020-06-30:  ", men_time_to_recover_1, " (", med_time_to_recover_1, ")" ))
print(paste0("Mean (median) time from 1st admission to 1st discharge post 2020-06-30:  ", men_time_to_recover_2, " (", med_time_to_recover_2, ")" ))


sink()



sink(file = paste0(output_dir, "/", jobno, filename3b, ".txt"),append=F,split=F)

cat("Shielding - without CH \n")
cat("\n")

cat("Shielding/Not \n")
#
nP_s1        = sum(DAT$shielding=="High Risk", na.rm=T)
nP_s0        = sum(DAT$shielding!="High Risk", na.rm=T)
nP_HorD_s1   = sum(DAT$shielding=="High Risk" & (!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0))
nP_noHorD_s1 = sum(DAT$shielding=="High Risk" &  (is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) ))
nP_HorD_s0   = sum(DAT$shielding!="High Risk" & (!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0))
nP_noHorD_s0 = sum(DAT$shielding!="High Risk" &  (is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) ))
#
nP_sA1        = sum(DAT$shieldA=="1", na.rm=T)
nP_sA0        = sum(DAT$shieldA!="1", na.rm=T)
nP_HorD_sA1   = sum(DAT$shieldA=="1" & (!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0))
nP_noHorD_sA1 = sum(DAT$shieldA=="1" &  (is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) ))
nP_HorD_sA0   = sum(DAT$shieldA!="1" & (!is.na(DAT$ons_death_date) |  DAT$all_covid_hosp>0))
nP_noHorD_sA0 = sum(DAT$shieldA!="1" &  (is.na(DAT$ons_death_date) & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) ))
#
print(paste0("Patients shield: ", nP_s1,           ", %patients: ", rd2(100*nP_s1/nP) ))
print(paste0("Patients not sh: ", nP_s0,           ", %patients: ", rd2(100*nP_s0/nP) ))
print(paste0("Patient0 shield: ", nP_tot_s1,       ", %cohort0:  ", rd2(100*nP_tot_s1/nP_tot) ))
print(paste0("Patient0 not sh: ", nP_tot_s0,       ", %cohort0:  ", rd2(100*nP_tot_s0/nP_tot) ))
print(paste0("Patient1 shield: ", nP_all_dates_s1, ", %cohort1:  ", rd2(100*nP_all_dates_s1/nP_all_dates) ))
print(paste0("Patient1 not sh: ", nP_all_dates_s0, ", %cohort1:  ", rd2(100*nP_all_dates_s0/nP_all_dates) ))
cat("\n")
print(paste0("Patients shieldA: ", nP_sA1,           ", %patients: ", rd2(100*nP_sA1/nP) ))
print(paste0("Patients not shA: ", nP_sA0,           ", %patients: ", rd2(100*nP_sA0/nP) ))
print(paste0("Patient0 shieldA: ", nP_tot_sA1,       ", %cohort0:  ", rd2(100*nP_tot_sA1/nP_tot) ))
print(paste0("Patient0 not shA: ", nP_tot_sA0,       ", %cohort0:  ", rd2(100*nP_tot_sA0/nP_tot) ))
print(paste0("Patient1 shieldA: ", nP_all_dates_sA1, ", %cohort1:  ", rd2(100*nP_all_dates_sA1/nP_all_dates) ))
print(paste0("Patient1 not shA: ", nP_all_dates_sA0, ", %cohort1:  ", rd2(100*nP_all_dates_sA0/nP_all_dates) ))
print(paste0("Patient0 inc CH, covid deaths Dec-2020-Aug-2021, non-covid deaths"))
print(paste0("Patient1 inc CH, covid deaths Dec-2020-Aug-2021"))
cat("\n")
print(paste0("Patients shield with hosp/death events: ", nP_HorD_s1,   ", %sh patients:   ", rd2(100*nP_HorD_s1/   nP_s1) ))
print(paste0("Patients shield wout hosp/death events: ", nP_noHorD_s1, ", %sh patients:   ", rd2(100*nP_noHorD_s1/ nP_s1) ))
print(paste0("Patients not sh with hosp/death events: ", nP_HorD_s0,   ", %nsh patients:  ", rd2(100*nP_HorD_s0/  nP_s0) ))
print(paste0("Patients not sh w/o hosp/death events:  ", nP_noHorD_s0, ", %nsh patients:  ", rd2(100*nP_noHorD_s0/nP_s0) ))
print(paste0("not sh w/o... inc cohort0 other deaths: ", nP_noHorD_s0 + nP_tot_s0-nP_s0,  ", %nsh cohort0:  ", rd2(100*(nP_noHorD_s0 + nP_tot_s0-nP_s0)/ nP_tot_s0) ))
cat("\n")
print(paste0("Patients shieldA with hosp/death events: ", nP_HorD_sA1,   ", %shA patients:  ", rd2(100*nP_HorD_sA1/   nP_sA1) ))
print(paste0("Patients shieldA wout hosp/death events: ", nP_noHorD_sA1, ", %shA patients:  ", rd2(100*nP_noHorD_sA1/ nP_sA1) ))
print(paste0("Patients not shA with hosp/death events: ", nP_HorD_sA0,   ", %nshA patients: ", rd2(100*nP_HorD_sA0/  nP_sA0) ))
print(paste0("Patients not shA w/o hosp/death events:  ", nP_noHorD_sA0, ", %nshA patients: ", rd2(100*nP_noHorD_sA0/nP_sA0) ))
print(paste0("not shA w/o... inc cohort0 other deaths: ", nP_noHorD_sA0 + nP_tot_sA0-nP_sA0, ", %nsh cohort0:  ", rd2(100*(nP_noHorD_sA0 + nP_tot_sA0-nP_sA0)/ nP_tot_sA0) ))

cat("\n")
cat("Shielding by age \n")
nP_00_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="0-4", na.rm = T)
nP_05_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="5-11", na.rm = T)
nP_12_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="12-17", na.rm = T)
nP_18_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="18-29", na.rm = T)
nP_30_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="30-39", na.rm = T)
nP_40_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="40-49", na.rm = T)
nP_50_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="50-59", na.rm = T)
nP_60_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="60-69", na.rm = T)
nP_70_s1 = sum(DAT$shielding=="High Risk" & DAT$age_cat=="70+", na.rm = T)
nP_sum_s1 = nP_00_s1 + nP_05_s1 + nP_12_s1 + nP_18_s1 + nP_30_s1 + nP_40_s1 + nP_50_s1 + nP_60_s1 + nP_70_s1
print(paste0("Patient age 0-4   % shielding: ", pct1(nP_00_s1,nP_00) ))
print(paste0("Patient age 5-11  % shielding: ", pct1(nP_05_s1,nP_05) ))
print(paste0("Patient age 12-17 % shielding: ", pct1(nP_12_s1,nP_12) ))
print(paste0("Patient age 18-29 % shielding: ", pct1(nP_18_s1,nP_18) ))
print(paste0("Patient age 30-39 % shielding: ", pct1(nP_30_s1,nP_30) ))
print(paste0("Patient age 40-49 % shielding: ", pct1(nP_40_s1,nP_40) ))
print(paste0("Patient age 50-59 % shielding: ", pct1(nP_50_s1,nP_50) ))
print(paste0("Patient age 60-69 % shielding: ", pct1(nP_60_s1,nP_60) ))
print(paste0("Patient age 70+   % shielding: ", pct1(nP_70_s1,nP_70) ))
cat("\n")
print(paste0("Patient shielding % age 0-4:   ", pct1(nP_00_s1,nP_s1) ))
print(paste0("Patient shielding % age 5-11:  ", pct1(nP_05_s1,nP_s1) ))
print(paste0("Patient shielding % age 12-17: ", pct1(nP_12_s1,nP_s1) ))
print(paste0("Patient shielding % age 18-29: ", pct1(nP_18_s1,nP_s1) ))
print(paste0("Patient shielding % age 30-39: ", pct1(nP_30_s1,nP_s1) ))
print(paste0("Patient shielding % age 40-49: ", pct1(nP_40_s1,nP_s1) ))
print(paste0("Patient shielding % age 50-59: ", pct1(nP_50_s1,nP_s1) ))
print(paste0("Patient shielding % age 60-69: ", pct1(nP_60_s1,nP_s1) ))
print(paste0("Patient shielding % age 70+:   ", pct1(nP_70_s1,nP_s1) ))
print(paste0("Patient shielding % all ages:  ", pct1(nP_sum_s1,nP_s1) ))

cat("\n")
cat("ShieldA by age \n")
nP_00_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="0-4", na.rm = T)
nP_05_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="5-11", na.rm = T)
nP_12_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="12-17", na.rm = T)
nP_18_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="18-29", na.rm = T)
nP_30_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="30-39", na.rm = T)
nP_40_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="40-49", na.rm = T)
nP_50_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="50-59", na.rm = T)
nP_60_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="60-69", na.rm = T)
nP_70_sA1 = sum(DAT$shieldA=="1" & DAT$age_cat=="70+", na.rm = T)
nP_sum_sA1 = nP_00_sA1 + nP_05_sA1 + nP_12_sA1 + nP_18_sA1 + nP_30_sA1 + nP_40_sA1 + nP_50_sA1 + nP_60_sA1 + nP_70_sA1
print(paste0("Patient age 0-4   % shieldA: ", pct1(nP_00_sA1,nP_00) ))
print(paste0("Patient age 5-11  % shieldA: ", pct1(nP_05_sA1,nP_05) ))
print(paste0("Patient age 12-17 % shieldA: ", pct1(nP_12_sA1,nP_12) ))
print(paste0("Patient age 18-29 % shieldA: ", pct1(nP_18_sA1,nP_18) ))
print(paste0("Patient age 30-39 % shieldA: ", pct1(nP_30_sA1,nP_30) ))
print(paste0("Patient age 40-49 % shieldA: ", pct1(nP_40_sA1,nP_40) ))
print(paste0("Patient age 50-59 % shieldA: ", pct1(nP_50_sA1,nP_50) ))
print(paste0("Patient age 60-69 % shieldA: ", pct1(nP_60_sA1,nP_60) ))
print(paste0("Patient age 70+   % shieldA: ", pct1(nP_70_sA1,nP_70) ))
cat("\n")
print(paste0("Patient shieldA % age 0-4:   ", pct1(nP_00_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 5-11:  ", pct1(nP_05_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 12-17: ", pct1(nP_12_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 18-29: ", pct1(nP_18_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 30-39: ", pct1(nP_30_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 40-49: ", pct1(nP_40_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 50-59: ", pct1(nP_50_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 60-69: ", pct1(nP_60_sA1,nP_sA1) ))
print(paste0("Patient shieldA % age 70+:   ", pct1(nP_70_sA1,nP_sA1) ))
print(paste0("Patient shieldA % all ages:  ", pct1(nP_sum_sA1,nP_sA1) ))

#cat("\n")
#cat("Shielding by age, in cohort1 (all dates) \n")
#print(paste0("Cohort1 age 0-4   % shielding: ", pct1(nP_00_all_dates_s1,nP_00_all_dates) ))
#print(paste0("Cohort1 age 5-11  % shielding: ", pct1(nP_05_all_dates_s1,nP_05_all_dates) ))
#print(paste0("Cohort1 age 12-17 % shielding: ", pct1(nP_12_all_dates_s1,nP_12_all_dates) ))
#print(paste0("Cohort1 age 18-29 % shielding: ", pct1(nP_18_all_dates_s1,nP_18_all_dates) ))
#print(paste0("Cohort1 age 30-39 % shielding: ", pct1(nP_30_all_dates_s1,nP_30_all_dates) ))
#print(paste0("Cohort1 age 40-49 % shielding: ", pct1(nP_40_all_dates_s1,nP_40_all_dates) ))
#print(paste0("Cohort1 age 50-59 % shielding: ", pct1(nP_50_all_dates_s1,nP_50_all_dates) ))
#print(paste0("Cohort1 age 60-69 % shielding: ", pct1(nP_60_all_dates_s1,nP_60_all_dates) ))
#print(paste0("Cohort1 age 70+   % shielding: ", pct1(nP_70_all_dates_s1,nP_70_all_dates) ))
#cat("\n")
#cat("ShieldA by age, in cohort1 (all dates) \n")
#print(paste0("Cohort1 age 0-4   % shieldA: ", pct1(nP_00_all_dates_sA1,nP_00_all_dates) ))
#print(paste0("Cohort1 age 5-11  % shieldA: ", pct1(nP_05_all_dates_sA1,nP_05_all_dates) ))
#print(paste0("Cohort1 age 12-17 % shieldA: ", pct1(nP_12_all_dates_sA1,nP_12_all_dates) ))
#print(paste0("Cohort1 age 18-29 % shieldA: ", pct1(nP_18_all_dates_sA1,nP_18_all_dates) ))
#print(paste0("Cohort1 age 30-39 % shieldA: ", pct1(nP_30_all_dates_sA1,nP_30_all_dates) ))
#print(paste0("Cohort1 age 40-49 % shieldA: ", pct1(nP_40_all_dates_sA1,nP_40_all_dates) ))
#print(paste0("Cohort1 age 50-59 % shieldA: ", pct1(nP_50_all_dates_sA1,nP_50_all_dates) ))
#print(paste0("Cohort1 age 60-69 % shieldA: ", pct1(nP_60_all_dates_sA1,nP_60_all_dates) ))
#print(paste0("Cohort1 age 70+   % shieldA: ", pct1(nP_70_all_dates_sA1,nP_70_all_dates) ))
#cat(" \n")
##Cohort0
cat("Shielding by age, in cohort0 (all deaths, all dates) \n")
print(paste0("Patient age 0-4   % shielding: ", pct1(nP_00_tot_s1,nP_00_tot) ))
print(paste0("Patient age 5-11  % shielding: ", pct1(nP_05_tot_s1,nP_05_tot) ))
print(paste0("Patient age 12-17 % shielding: ", pct1(nP_12_tot_s1,nP_12_tot) ))
print(paste0("Patient age 18-29 % shielding: ", pct1(nP_18_tot_s1,nP_18_tot) ))
print(paste0("Patient age 30-39 % shielding: ", pct1(nP_30_tot_s1,nP_30_tot) ))
print(paste0("Patient age 40-49 % shielding: ", pct1(nP_40_tot_s1,nP_40_tot) ))
print(paste0("Patient age 50-59 % shielding: ", pct1(nP_50_tot_s1,nP_50_tot) ))
print(paste0("Patient age 60-69 % shielding: ", pct1(nP_60_tot_s1,nP_60_tot) ))
print(paste0("Patient age 70+   % shielding: ", pct1(nP_70_tot_s1,nP_70_tot) ))
cat(" \n")
cat("ShieldA by age, in cohort0 (all deaths, all dates) \n")
print(paste0("Patient age 0-4   % shieldA: ", pct1(nP_00_tot_sA1,nP_00_tot) ))
print(paste0("Patient age 5-11  % shieldA: ", pct1(nP_05_tot_sA1,nP_05_tot) ))
print(paste0("Patient age 12-17 % shieldA: ", pct1(nP_12_tot_sA1,nP_12_tot) ))
print(paste0("Patient age 18-29 % shieldA: ", pct1(nP_18_tot_sA1,nP_18_tot) ))
print(paste0("Patient age 30-39 % shieldA: ", pct1(nP_30_tot_sA1,nP_30_tot) ))
print(paste0("Patient age 40-49 % shieldA: ", pct1(nP_40_tot_sA1,nP_40_tot) ))
print(paste0("Patient age 50-59 % shieldA: ", pct1(nP_50_tot_sA1,nP_50_tot) ))
print(paste0("Patient age 60-69 % shieldA: ", pct1(nP_60_tot_sA1,nP_60_tot) ))
print(paste0("Patient age 70+   % shieldA: ", pct1(nP_70_tot_sA1,nP_70_tot) ))
##Cohort0


cat("\n")
cat("Shielding/Not patients in hospital \n")
nP_Hosp_s1 = length( which(DAT$all_covid_hosp>0 & DAT$shielding=="High Risk"))
nP_Hosp_s0 = nP_Hosp - nP_Hosp_s1
nP_RH_s1   = length( which( is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shielding=="High Risk"))
nP_RH_s0   = nP_RH - nP_RH_s1
nP_DH_s1   = length( which(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shielding=="High Risk"))
nP_DH_s0   = nP_DH - nP_DH_s1
print(paste0("Shielding patients in hospital:             ", nP_Hosp_s1, ", %patients hosp:   ", pct1(nP_Hosp_s1,nP_Hosp) ))
print(paste0("Shielding patients in hosp recovered:       ", nP_RH_s1,   ", %sh patnts hosp:  ", pct1(nP_RH_s1,  nP_Hosp_s1) ))
print(paste0("Shielding patients in hospital died:        ", nP_DH_s1,   ", %sh patnts hosp:  ", pct1(nP_DH_s1,  nP_Hosp_s1) ))
print(paste0("Odds shielder dies in hosp (assuming same exposure): ", rd2((nP_DH_s1/nP_Hosp_s1) / (nP_DH_s0/nP_Hosp_s0)) ))
print(paste0("Non-shield patients in hospital:            ", nP_Hosp_s0, ", %patients hosp:   ", pct1(nP_Hosp_s0,nP_Hosp) ))
print(paste0("Non-shield patients in hosp recovered:      ", nP_RH_s0,   ", %nsh patnts hosp: ", pct1(nP_RH_s0,  nP_Hosp_s0) ))
print(paste0("Non-shield patients in hospital died:       ", nP_DH_s0,   ", %nsh patnts hosp: ", pct1(nP_DH_s0,  nP_Hosp_s0) ))


cat("\n")
cat("ShieldA/Not patients in hospital \n")
nP_Hosp_sA1 = length( which(DAT$all_covid_hosp>0 & DAT$shieldA=="1"))
nP_Hosp_sA0 = nP_Hosp - nP_Hosp_sA1
nP_RH_sA1   = length( which( is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shieldA=="1"))
nP_RH_sA0   = nP_RH - nP_RH_s1
nP_DH_sA1   = length( which(!is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$shieldA=="1"))
nP_DH_sA0   = nP_DH - nP_DH_s1
print(paste0("shieldA patients in hospital:             ", nP_Hosp_sA1, ", %patients hosp:    ", pct1(nP_Hosp_sA1,nP_Hosp) ))
print(paste0("shieldA patients in hosp recovered:       ", nP_RH_sA1,   ", %shA patnts hosp:  ", pct1(nP_RH_sA1,  nP_Hosp_sA1) ))
print(paste0("shieldA patients in hospital died:        ", nP_DH_sA1,   ", %shA patnts hosp:  ", pct1(nP_DH_sA1,  nP_Hosp_sA1) ))
print(paste0("Odds shieldAer dies in hosp (assuming same exposure): ", rd2((nP_DH_sA1/nP_Hosp_sA1) / (nP_DH_sA0/nP_Hosp_sA0)) ))
print(paste0("Non-shieldA patients in hospital:         ", nP_Hosp_sA0, ", %patients hosp:    ", pct1(nP_Hosp_sA0,nP_Hosp) ))
print(paste0("Non-shieldA patients in hosp recovered:   ", nP_RH_sA0,   ", %nshA patnts hosp: ", pct1(nP_RH_sA0,  nP_Hosp_sA0) ))
print(paste0("Non-shieldA patients in hospital died:    ", nP_DH_sA0,   ", %nshA patnts hosp: ", pct1(nP_DH_sA0,  nP_Hosp_sA0) ))


cat("\n")
cat("Shielding/Not patients in hospital by age\n")
nP_Hosp_00_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70_s1 = length( which(DAT$shielding=="High Risk" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
nP_Hosp_sum_s1 = nP_Hosp_00_s1+nP_Hosp_05_s1+nP_Hosp_12_s1+nP_Hosp_18_s1+nP_Hosp_30_s1+nP_Hosp_40_s1+nP_Hosp_50_s1+nP_Hosp_60_s1+nP_Hosp_70_s1
nP_Hosp_00_s0 = nP_Hosp_00 - nP_Hosp_00_s1
nP_Hosp_05_s0 = nP_Hosp_05 - nP_Hosp_05_s1
nP_Hosp_12_s0 = nP_Hosp_12 - nP_Hosp_12_s1
nP_Hosp_18_s0 = nP_Hosp_18 - nP_Hosp_18_s1
nP_Hosp_30_s0 = nP_Hosp_30 - nP_Hosp_30_s1
nP_Hosp_40_s0 = nP_Hosp_40 - nP_Hosp_40_s1
nP_Hosp_50_s0 = nP_Hosp_50 - nP_Hosp_50_s1
nP_Hosp_60_s0 = nP_Hosp_60 - nP_Hosp_60_s1
nP_Hosp_70_s0 = nP_Hosp_70 - nP_Hosp_70_s1
nP_Hosp_sum_s0 = nP_Hosp_00_s0+nP_Hosp_05_s0+nP_Hosp_12_s0+nP_Hosp_18_s0+nP_Hosp_30_s0+nP_Hosp_40_s0+nP_Hosp_50_s0+nP_Hosp_60_s0+nP_Hosp_70_s0
print(paste0("Shielding patients in hosp age 0-4:    ", tr(nP_Hosp_00_s1), ", %pats hosp 0-4:   ", pct1(nP_Hosp_00_s1,nP_Hosp_00) ))
print(paste0("Shielding patients in hosp age 5-11:   ", tr(nP_Hosp_05_s1), ", %pats hosp 5-11:  ", pct1(nP_Hosp_05_s1,nP_Hosp_05) ))
print(paste0("Shielding patients in hosp age 12-17:  ", tr(nP_Hosp_12_s1), ", %pats hosp 12-17: ", pct1(nP_Hosp_12_s1,nP_Hosp_12) ))
print(paste0("Shielding patients in hosp age 18-29:  ", tr(nP_Hosp_18_s1), ", %pats hosp 18-29: ", pct1(nP_Hosp_18_s1,nP_Hosp_18) ))
print(paste0("Shielding patients in hosp age 30-39:  ", tr(nP_Hosp_30_s1), ", %pats hosp 30-39: ", pct1(nP_Hosp_30_s1,nP_Hosp_30) ))
print(paste0("Shielding patients in hosp age 40-49:  ", tr(nP_Hosp_40_s1), ", %pats hosp 40-49: ", pct1(nP_Hosp_40_s1,nP_Hosp_40) ))
print(paste0("Shielding patients in hosp age 50-59:  ", tr(nP_Hosp_50_s1), ", %pats hosp 50-59: ", pct1(nP_Hosp_50_s1,nP_Hosp_50) ))
print(paste0("Shielding patients in hosp age 60-69:  ", tr(nP_Hosp_60_s1), ", %pats hosp 60-69: ", pct1(nP_Hosp_60_s1,nP_Hosp_60) ))
print(paste0("Shielding patients in hosp age 70+:    ", tr(nP_Hosp_70_s1), ", %pats hosp 70+:   ", pct1(nP_Hosp_70_s1,nP_Hosp_70) ))
print(paste0("Shielding patients in hosp total:      ", nP_Hosp_sum_s1,    ", %pats hosp:       ", pct1(nP_Hosp_sum_s1,nP_Hosp) ))
print(paste0("Non-shield patients in hosp age 0-4:   ", tr(nP_Hosp_00_s0), ", %pats hosp 0-4:   ", pct1(nP_Hosp_00_s0,nP_Hosp_00) ))
print(paste0("Non-shield patients in hosp age 5-11:  ", tr(nP_Hosp_05_s0), ", %pats hosp 5-11:  ", pct1(nP_Hosp_05_s0,nP_Hosp_05) ))
print(paste0("Non-shield patients in hosp age 12-17: ", tr(nP_Hosp_12_s0), ", %pats hosp 12-17: ", pct1(nP_Hosp_12_s0,nP_Hosp_12) ))
print(paste0("Non-shield patients in hosp age 18-29: ", tr(nP_Hosp_18_s0), ", %pats hosp 18-29: ", pct1(nP_Hosp_18_s0,nP_Hosp_18) ))
print(paste0("Non-shield patients in hosp age 30-39: ", tr(nP_Hosp_30_s0), ", %pats hosp 30-39: ", pct1(nP_Hosp_30_s0,nP_Hosp_30) ))
print(paste0("Non-shield patients in hosp age 40-49: ", tr(nP_Hosp_40_s0), ", %pats hosp 40-49: ", pct1(nP_Hosp_40_s0,nP_Hosp_40) ))
print(paste0("Non-shield patients in hosp age 50-59: ", tr(nP_Hosp_50_s0), ", %pats hosp 50-59: ", pct1(nP_Hosp_50_s0,nP_Hosp_50) ))
print(paste0("Non-shield patients in hosp age 60-69: ", tr(nP_Hosp_60_s0), ", %pats hosp 60-69: ", pct1(nP_Hosp_60_s0,nP_Hosp_60) ))
print(paste0("Non-shield patients in hosp age 70+:   ", tr(nP_Hosp_70_s0), ", %pats hosp 70+:   ", pct1(nP_Hosp_70_s0,nP_Hosp_70) ))
print(paste0("Non-shield patients in hosp total:     ", nP_Hosp_sum_s0,    ", %pats hosp:       ", pct1(nP_Hosp_sum_s0,nP_Hosp) ))


cat("\n")
cat("ShieldA/Not patients in hospital by age\n")
nP_Hosp_00_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70_sA1 = length( which(DAT$shieldA=="1" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
nP_Hosp_sum_sA1 = nP_Hosp_00_sA1+nP_Hosp_05_sA1+nP_Hosp_12_sA1+nP_Hosp_18_sA1+nP_Hosp_30_sA1+nP_Hosp_40_sA1+nP_Hosp_50_sA1+nP_Hosp_60_sA1+nP_Hosp_70_sA1
nP_Hosp_00_sA0 = nP_Hosp_00 - nP_Hosp_00_sA1
nP_Hosp_05_sA0 = nP_Hosp_05 - nP_Hosp_05_sA1
nP_Hosp_12_sA0 = nP_Hosp_12 - nP_Hosp_12_sA1
nP_Hosp_18_sA0 = nP_Hosp_18 - nP_Hosp_18_sA1
nP_Hosp_30_sA0 = nP_Hosp_30 - nP_Hosp_30_sA1
nP_Hosp_40_sA0 = nP_Hosp_40 - nP_Hosp_40_sA1
nP_Hosp_50_sA0 = nP_Hosp_50 - nP_Hosp_50_sA1
nP_Hosp_60_sA0 = nP_Hosp_60 - nP_Hosp_60_sA1
nP_Hosp_70_sA0 = nP_Hosp_70 - nP_Hosp_70_sA1
nP_Hosp_sum_sA0 = nP_Hosp_00_sA0+nP_Hosp_05_sA0+nP_Hosp_12_sA0+nP_Hosp_18_sA0+nP_Hosp_30_sA0+nP_Hosp_40_sA0+nP_Hosp_50_sA0+nP_Hosp_60_sA0+nP_Hosp_70_sA0
print(paste0("ShieldA patients in hosp age 0-4:       ", tr(nP_Hosp_00_sA1), ", %pats hosp 0-4:   ", pct1(nP_Hosp_00_sA1,nP_Hosp_00) ))
print(paste0("ShieldA patients in hosp age 5-11:      ", tr(nP_Hosp_05_sA1), ", %pats hosp 5-11:  ", pct1(nP_Hosp_05_sA1,nP_Hosp_05) ))
print(paste0("ShieldA patients in hosp age 12-17:     ", tr(nP_Hosp_12_sA1), ", %pats hosp 12-17: ", pct1(nP_Hosp_12_sA1,nP_Hosp_12) ))
print(paste0("ShieldA patients in hosp age 18-29:     ", tr(nP_Hosp_18_sA1), ", %pats hosp 18-29: ", pct1(nP_Hosp_18_sA1,nP_Hosp_18) ))
print(paste0("ShieldA patients in hosp age 30-39:     ", tr(nP_Hosp_30_sA1), ", %pats hosp 30-39: ", pct1(nP_Hosp_30_sA1,nP_Hosp_30) ))
print(paste0("ShieldA patients in hosp age 40-49:     ", tr(nP_Hosp_40_sA1), ", %pats hosp 40-49: ", pct1(nP_Hosp_40_sA1,nP_Hosp_40) ))
print(paste0("ShieldA patients in hosp age 50-59:     ", tr(nP_Hosp_50_sA1), ", %pats hosp 50-59: ", pct1(nP_Hosp_50_sA1,nP_Hosp_50) ))
print(paste0("ShieldA patients in hosp age 60-69:     ", tr(nP_Hosp_60_sA1), ", %pats hosp 60-69: ", pct1(nP_Hosp_60_sA1,nP_Hosp_60) ))
print(paste0("ShieldA patients in hosp age 70+:       ", tr(nP_Hosp_70_sA1), ", %pats hosp 70+:   ", pct1(nP_Hosp_70_sA1,nP_Hosp_70) ))
print(paste0("ShieldA patients in hosp total:         ", nP_Hosp_sum_sA1,    ", %pats hosp:       ", pct1(nP_Hosp_sum_sA1,nP_Hosp) ))
print(paste0("Non-shieldA patients in hosp age 0-4:   ", tr(nP_Hosp_00_sA0), ", %pats hosp 0-4:   ", pct1(nP_Hosp_00_sA0,nP_Hosp_00) ))
print(paste0("Non-shieldA patients in hosp age 5-11:  ", tr(nP_Hosp_05_sA0), ", %pats hosp 5-11:  ", pct1(nP_Hosp_05_sA0,nP_Hosp_05) ))
print(paste0("Non-shieldA patients in hosp age 12-17: ", tr(nP_Hosp_12_sA0), ", %pats hosp 12-17: ", pct1(nP_Hosp_12_sA0,nP_Hosp_12) ))
print(paste0("Non-shieldA patients in hosp age 18-29: ", tr(nP_Hosp_18_sA0), ", %pats hosp 18-29: ", pct1(nP_Hosp_18_sA0,nP_Hosp_18) ))
print(paste0("Non-shieldA patients in hosp age 30-39: ", tr(nP_Hosp_30_sA0), ", %pats hosp 30-39: ", pct1(nP_Hosp_30_sA0,nP_Hosp_30) ))
print(paste0("Non-shieldA patients in hosp age 40-49: ", tr(nP_Hosp_40_sA0), ", %pats hosp 40-49: ", pct1(nP_Hosp_40_sA0,nP_Hosp_40) ))
print(paste0("Non-shieldA patients in hosp age 50-59: ", tr(nP_Hosp_50_sA0), ", %pats hosp 50-59: ", pct1(nP_Hosp_50_sA0,nP_Hosp_50) ))
print(paste0("Non-shieldA patients in hosp age 60-69: ", tr(nP_Hosp_60_sA0), ", %pats hosp 60-69: ", pct1(nP_Hosp_60_sA0,nP_Hosp_60) ))
print(paste0("Non-shieldA patients in hosp age 70+:   ", tr(nP_Hosp_70_sA0), ", %pats hosp 70+:   ", pct1(nP_Hosp_70_sA0,nP_Hosp_70) ))
print(paste0("Non-shieldA patients in hosp total:     ", nP_Hosp_sum_sA0,    ", %pats hosp:       ", pct1(nP_Hosp_sum_sA0,nP_Hosp) ))

cat("\n")
cat("ShieldA/Not patients in hospital by age - prior 2020-06-30 \n") #allow delay admission to death 10d
nP_Hosp_00_sA1a = length( which(DAT$shieldA=="1" & DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05_sA1a = length( which(DAT$shieldA=="1" & DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12_sA1a = length( which(DAT$shieldA=="1" & DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18_sA1a = length( which(DAT$shieldA=="1" & DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30_sA1a = length( which(DAT$shieldA=="1" & DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40_sA1a = length( which(DAT$shieldA=="1" & DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50_sA1a = length( which(DAT$shieldA=="1" & DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60_sA1a = length( which(DAT$shieldA=="1" & DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70_sA1a = length( which(DAT$shieldA=="1" & DAT$admission_date<="2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
nP_Hosp_sum_sA1a = nP_Hosp_00_sA1a+nP_Hosp_05_sA1a+nP_Hosp_12_sA1a+nP_Hosp_18_sA1a+nP_Hosp_30_sA1a+nP_Hosp_40_sA1a+nP_Hosp_50_sA1a+nP_Hosp_60_sA1a+nP_Hosp_70_sA1a
nP_Hosp_00_sA0a = nP_Hosp_00a - nP_Hosp_00_sA1a
nP_Hosp_05_sA0a = nP_Hosp_05a - nP_Hosp_05_sA1a
nP_Hosp_12_sA0a = nP_Hosp_12a - nP_Hosp_12_sA1a
nP_Hosp_18_sA0a = nP_Hosp_18a - nP_Hosp_18_sA1a
nP_Hosp_30_sA0a = nP_Hosp_30a - nP_Hosp_30_sA1a
nP_Hosp_40_sA0a = nP_Hosp_40a - nP_Hosp_40_sA1a
nP_Hosp_50_sA0a = nP_Hosp_50a - nP_Hosp_50_sA1a
nP_Hosp_60_sA0a = nP_Hosp_60a - nP_Hosp_60_sA1a
nP_Hosp_70_sA0a = nP_Hosp_70a - nP_Hosp_70_sA1a
nP_Hosp_sum_sA0a = nP_Hosp_00_sA0a+nP_Hosp_05_sA0a+nP_Hosp_12_sA0a+nP_Hosp_18_sA0a+nP_Hosp_30_sA0a+nP_Hosp_40_sA0a+nP_Hosp_50_sA0a+nP_Hosp_60_sA0a+nP_Hosp_70_sA0a

cat("\n")
cat("ShieldA/Not patients in hospital by age - after 2020-06-30 \n") #allow delay admission to death 10d
nP_Hosp_00_sA1b = length( which(DAT$shieldA=="1" & DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4") )
nP_Hosp_05_sA1b = length( which(DAT$shieldA=="1" & DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11") )
nP_Hosp_12_sA1b = length( which(DAT$shieldA=="1" & DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17") )
nP_Hosp_18_sA1b = length( which(DAT$shieldA=="1" & DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29") )
nP_Hosp_30_sA1b = length( which(DAT$shieldA=="1" & DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39") )
nP_Hosp_40_sA1b = length( which(DAT$shieldA=="1" & DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49") )
nP_Hosp_50_sA1b = length( which(DAT$shieldA=="1" & DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59") )
nP_Hosp_60_sA1b = length( which(DAT$shieldA=="1" & DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69") )
nP_Hosp_70_sA1b = length( which(DAT$shieldA=="1" & DAT$admission_date> "2020-06-20" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+") )
nP_Hosp_sum_sA1b = nP_Hosp_00_sA1b+nP_Hosp_05_sA1b+nP_Hosp_12_sA1b+nP_Hosp_18_sA1b+nP_Hosp_30_sA1b+nP_Hosp_40_sA1b+nP_Hosp_50_sA1b+nP_Hosp_60_sA1b+nP_Hosp_70_sA1b
nP_Hosp_00_sA0b = nP_Hosp_00b - nP_Hosp_00_sA1b
nP_Hosp_05_sA0b = nP_Hosp_05b - nP_Hosp_05_sA1b
nP_Hosp_12_sA0b = nP_Hosp_12b - nP_Hosp_12_sA1b
nP_Hosp_18_sA0b = nP_Hosp_18b - nP_Hosp_18_sA1b
nP_Hosp_30_sA0b = nP_Hosp_30b - nP_Hosp_30_sA1b
nP_Hosp_40_sA0b = nP_Hosp_40b - nP_Hosp_40_sA1b
nP_Hosp_50_sA0b = nP_Hosp_50b - nP_Hosp_50_sA1b
nP_Hosp_60_sA0b = nP_Hosp_60b - nP_Hosp_60_sA1b
nP_Hosp_70_sA0b = nP_Hosp_70b - nP_Hosp_70_sA1b
nP_Hosp_sum_sA0a = nP_Hosp_00_sA0a+nP_Hosp_05_sA0a+nP_Hosp_12_sA0a+nP_Hosp_18_sA0a+nP_Hosp_30_sA0a+nP_Hosp_40_sA0a+nP_Hosp_50_sA0a+nP_Hosp_60_sA0a+nP_Hosp_70_sA0a



cat("\n")
cat("Shielding/Not deaths outside hospital \n")
nP_DO_s1 = length( which(!is.na(DAT$ons_death_date) 
           & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) & DAT$shielding=="High Risk" ))
nP_DO_s0 = nP_DO - nP_DO_s1
print(paste0("Shielding patients died outside hospital:    ", nP_DO_s1,  ", %deaths out hosp: ", pct1(nP_DO_s1,nP_DO) ))
print(paste0("Non-shield patients died outside hospital:   ", nP_DO_s0,  ", %deaths out hosp: ", pct1(nP_DO_s0,nP_DO) ))
cat("\n")

cat("ShieldA/Not deaths outside hospital \n")
nP_DO_sA1 = length( which(!is.na(DAT$ons_death_date) 
                         & (DAT$all_covid_hosp==0 | is.na(DAT$all_covid_hosp)) & DAT$shieldA=="1" ))
nP_DO_sA0 = nP_DO - nP_DO_sA1
print(paste0("ShieldA patients died outside hospital:     ", nP_DO_sA1,  ", %deaths out hosp: ", pct1(nP_DO_sA1,nP_DO) ))
print(paste0("Non-shieldA patients died outside hospital: ", nP_DO_sA0,  ", %deaths out hosp: ", pct1(nP_DO_sA0,nP_DO) ))


cat("\n")
cat("Shielding/Not deaths in hospital by age \n")
nP_DH_00_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4" ))
nP_DH_05_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11" ))
nP_DH_12_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17" ))
nP_DH_18_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29" ))
nP_DH_30_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39" ))
nP_DH_40_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49" ))
nP_DH_50_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59" ))
nP_DH_60_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69" ))
nP_DH_70_s1   = length( which(DAT$shielding=="High Risk" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="70+" ))
nP_DH_sum_s1  = (nP_DH_00_s1+nP_DH_05_s1+nP_DH_12_s1+nP_DH_18_s1+nP_DH_30_s1+nP_DH_40_s1+nP_DH_50_s1+nP_DH_60_s1+nP_DH_70_s1)
nP_DH_00_s0   = nP_DH_00 - nP_DH_00_s1
nP_DH_05_s0   = nP_DH_05 - nP_DH_05_s1
nP_DH_12_s0   = nP_DH_12 - nP_DH_12_s1
nP_DH_18_s0   = nP_DH_18 - nP_DH_18_s1
nP_DH_30_s0   = nP_DH_30 - nP_DH_30_s1
nP_DH_40_s0   = nP_DH_40 - nP_DH_40_s1
nP_DH_50_s0   = nP_DH_50 - nP_DH_50_s1
nP_DH_60_s0   = nP_DH_60 - nP_DH_60_s1
nP_DH_70_s0   = nP_DH_70 - nP_DH_70_s1
nP_DH_sum_s0  = (nP_DH_00_s0+nP_DH_05_s0+nP_DH_12_s0+nP_DH_18_s0+nP_DH_30_s0+nP_DH_40_s0+nP_DH_50_s0+nP_DH_60_s0+nP_DH_70_s0)
print(paste0("Shielding patients died in hosp age 0-4:    ", tr(nP_DH_00_s1), ", %hosp sh deaths: ", pct1(nP_DH_00_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 5-11:   ", tr(nP_DH_05_s1), ", %hosp sh deaths: ", pct1(nP_DH_05_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 12-17:  ", tr(nP_DH_12_s1), ", %hosp sh deaths: ", pct1(nP_DH_12_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 18-29:  ", tr(nP_DH_18_s1), ", %hosp sh deaths: ", pct1(nP_DH_18_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 30-39:  ", tr(nP_DH_30_s1), ", %hosp sh deaths: ", pct1(nP_DH_30_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 40-49:  ", tr(nP_DH_40_s1), ", %hosp sh deaths: ", pct1(nP_DH_40_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 50-59:  ", tr(nP_DH_50_s1), ", %hosp sh deaths: ", pct1(nP_DH_50_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 60-69:  ", tr(nP_DH_60_s1), ", %hosp sh deaths: ", pct1(nP_DH_60_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp age 70+:    ", tr(nP_DH_70_s1), ", %hosp sh deaths: ", pct1(nP_DH_70_s1,nP_DH_s1) ))
print(paste0("Shielding patients died in hosp total:      ", nP_DH_sum_s1,    ", %hosp sh deaths: ", pct1(nP_DH_sum_s1,nP_DH_s1) ))
cat("\n")
print(paste0("Non-shield patients died in hosp age 0-4:   ", tr(nP_DH_00_s0), ", %hosp nsh deaths: ", pct1(nP_DH_00_s0,nP_DH_s0) ))
print(paste0("Non-shield patients died in hosp age 5-11:  ", tr(nP_DH_05_s0), ", %hosp nsh deaths: ", pct1(nP_DH_05_s0,nP_DH_s0) ))
print(paste0("Non-shield patients died in hosp age 12-17: ", tr(nP_DH_12_s0), ", %hosp nsh deaths: ", pct1(nP_DH_12_s0,nP_DH_s0) ))
print(paste0("Non-shield patients died in hosp age 18-29: ", tr(nP_DH_18_s0), ", %hosp nsh deaths: ", pct1(nP_DH_18_s0,nP_DH_s0) ))
print(paste0("Non-shield patients died in hosp age 30-39: ", tr(nP_DH_30_s0), ", %hosp nsh deaths: ", pct1(nP_DH_30_s0,nP_DH_s0) ))
print(paste0("Non-shield patients died in hosp age 40-49: ", tr(nP_DH_40_s0), ", %hosp nsh deaths: ", pct1(nP_DH_40_s0,nP_DH_s0) ))
print(paste0("Non-shield patients died in hosp age 50-59: ", tr(nP_DH_50_s0), ", %hosp nsh deaths: ", pct1(nP_DH_50_s0,nP_DH_s0) ))
print(paste0("Non-shield patients died in hosp age 60-69: ", tr(nP_DH_60_s0), ", %hosp nsh deaths: ", pct1(nP_DH_60_s0,nP_DH_s0) ))
print(paste0("Non-shield patients died in hosp age 70+:   ", tr(nP_DH_70_s0), ", %hosp nsh deaths: ", pct1(nP_DH_70_s0,nP_DH_s0) ))
print(paste0("Non-shield patients died in hosp total:     ", nP_DH_sum_s0,    ", %hosp nsh deaths: ", pct1(nP_DH_sum_s0,nP_DH_s0) ))


cat("\n")
cat("ShieldA/Not deaths in hospital by age \n")
nP_DH_00_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4" ))
nP_DH_05_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11" ))
nP_DH_12_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17" ))
nP_DH_18_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29" ))
nP_DH_30_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39" ))
nP_DH_40_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49" ))
nP_DH_50_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59" ))
nP_DH_60_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69" ))
nP_DH_70_sA1   = length( which(DAT$shieldA=="1" & !is.na(DAT$ons_death_date) & DAT$all_covid_hosp>0 & DAT$age_cat=="70+" ))
nP_DH_sum_sA1  = (nP_DH_00_sA1+nP_DH_05_sA1+nP_DH_12_sA1+nP_DH_18_sA1+nP_DH_30_sA1+nP_DH_40_sA1+nP_DH_50_sA1+nP_DH_60_sA1+nP_DH_70_sA1)
nP_DH_00_sA0   = nP_DH_00 - nP_DH_00_sA1
nP_DH_05_sA0   = nP_DH_05 - nP_DH_05_sA1
nP_DH_12_sA0   = nP_DH_12 - nP_DH_12_sA1
nP_DH_18_sA0   = nP_DH_18 - nP_DH_18_sA1
nP_DH_30_sA0   = nP_DH_30 - nP_DH_30_sA1
nP_DH_40_sA0   = nP_DH_40 - nP_DH_40_sA1
nP_DH_50_sA0   = nP_DH_50 - nP_DH_50_sA1
nP_DH_60_sA0   = nP_DH_60 - nP_DH_60_sA1
nP_DH_70_sA0   = nP_DH_70 - nP_DH_70_sA1
nP_DH_sum_sA0  = (nP_DH_00_sA0+nP_DH_05_sA0+nP_DH_12_sA0+nP_DH_18_sA0+nP_DH_30_sA0+nP_DH_40_sA0+nP_DH_50_sA0+nP_DH_60_sA0+nP_DH_70_sA0)
print(paste0("ShieldA patients died in hosp age 0-4:    ", tr(nP_DH_00_sA1), ", %hosp shA deaths: ", pct1(nP_DH_00_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 5-11:   ", tr(nP_DH_05_sA1), ", %hosp shA deaths: ", pct1(nP_DH_05_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 12-17:  ", tr(nP_DH_12_sA1), ", %hosp shA deaths: ", pct1(nP_DH_12_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 18-29:  ", tr(nP_DH_18_sA1), ", %hosp shA deaths: ", pct1(nP_DH_18_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 30-39:  ", tr(nP_DH_30_sA1), ", %hosp shA deaths: ", pct1(nP_DH_30_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 40-49:  ", tr(nP_DH_40_sA1), ", %hosp shA deaths: ", pct1(nP_DH_40_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 50-59:  ", tr(nP_DH_50_sA1), ", %hosp shA deaths: ", pct1(nP_DH_50_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 60-69:  ", tr(nP_DH_60_sA1), ", %hosp shA deaths: ", pct1(nP_DH_60_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp age 70+:    ", tr(nP_DH_70_sA1), ", %hosp shA deaths: ", pct1(nP_DH_70_sA1,nP_DH_sA1) ))
print(paste0("ShieldA patients died in hosp total:      ", nP_DH_sum_sA1,    ", %hosp shA deaths: ", pct1(nP_DH_sum_sA1,nP_DH_sA1) ))
cat("\n")
print(paste0("Non-shieldA patients died in hosp age 0-4:   ", tr(nP_DH_00_sA0), ", %hosp nshA deaths: ", pct1(nP_DH_00_sA0,nP_DH_sA0) ))
print(paste0("Non-shieldA patients died in hosp age 5-11:  ", tr(nP_DH_05_sA0), ", %hosp nshA deaths: ", pct1(nP_DH_05_sA0,nP_DH_sA0) ))
print(paste0("Non-shieldA patients died in hosp age 12-17: ", tr(nP_DH_12_sA0), ", %hosp nshA deaths: ", pct1(nP_DH_12_sA0,nP_DH_sA0) ))
print(paste0("Non-shieldA patients died in hosp age 18-29: ", tr(nP_DH_18_sA0), ", %hosp nshA deaths: ", pct1(nP_DH_18_sA0,nP_DH_sA0) ))
print(paste0("Non-shieldA patients died in hosp age 30-39: ", tr(nP_DH_30_sA0), ", %hosp nshA deaths: ", pct1(nP_DH_30_sA0,nP_DH_sA0) ))
print(paste0("Non-shieldA patients died in hosp age 40-49: ", tr(nP_DH_40_sA0), ", %hosp nshA deaths: ", pct1(nP_DH_40_sA0,nP_DH_sA0) ))
print(paste0("Non-shieldA patients died in hosp age 50-59: ", tr(nP_DH_50_sA0), ", %hosp nshA deaths: ", pct1(nP_DH_50_sA0,nP_DH_sA0) ))
print(paste0("Non-shieldA patients died in hosp age 60-69: ", tr(nP_DH_60_sA0), ", %hosp nshA deaths: ", pct1(nP_DH_60_sA0,nP_DH_sA0) ))
print(paste0("Non-shieldA patients died in hosp age 70+:   ", tr(nP_DH_70_sA0), ", %hosp nshA deaths: ", pct1(nP_DH_70_sA0,nP_DH_sA0) ))
print(paste0("Non-shieldA patients died in hosp total:     ", nP_DH_sum_sA0,    ", %hosp nshA deaths: ", pct1(nP_DH_sum_sA0,nP_DH_sA0) ))

cat("\n")
cat("ShieldA/Not deaths in hospital by age - prior 2020-06-30 \n")
nP_DH_00_sA1a   = length( which(DAT$shieldA=="1" & DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4" ))
nP_DH_05_sA1a   = length( which(DAT$shieldA=="1" & DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11" ))
nP_DH_12_sA1a   = length( which(DAT$shieldA=="1" & DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17" ))
nP_DH_18_sA1a   = length( which(DAT$shieldA=="1" & DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29" ))
nP_DH_30_sA1a   = length( which(DAT$shieldA=="1" & DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39" ))
nP_DH_40_sA1a   = length( which(DAT$shieldA=="1" & DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49" ))
nP_DH_50_sA1a   = length( which(DAT$shieldA=="1" & DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59" ))
nP_DH_60_sA1a   = length( which(DAT$shieldA=="1" & DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69" ))
nP_DH_70_sA1a   = length( which(DAT$shieldA=="1" & DAT$ons_death_date<="2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+" ))
nP_DH_sum_sA1a  = (nP_DH_00_sA1a+nP_DH_05_sA1a+nP_DH_12_sA1a+nP_DH_18_sA1a+nP_DH_30_sA1a+nP_DH_40_sA1a+nP_DH_50_sA1a+nP_DH_60_sA1a+nP_DH_70_sA1a)
nP_DH_00_sA0a   = nP_DH_00a - nP_DH_00_sA1a
nP_DH_05_sA0a   = nP_DH_05a - nP_DH_05_sA1a
nP_DH_12_sA0a   = nP_DH_12a - nP_DH_12_sA1a
nP_DH_18_sA0a   = nP_DH_18a - nP_DH_18_sA1a
nP_DH_30_sA0a   = nP_DH_30a - nP_DH_30_sA1a
nP_DH_40_sA0a   = nP_DH_40a - nP_DH_40_sA1a
nP_DH_50_sA0a   = nP_DH_50a - nP_DH_50_sA1a
nP_DH_60_sA0a   = nP_DH_60a - nP_DH_60_sA1a
nP_DH_70_sA0a   = nP_DH_70a - nP_DH_70_sA1a
nP_DH_sum_sA0a  = (nP_DH_00_sA0a+nP_DH_05_sA0a+nP_DH_12_sA0a+nP_DH_18_sA0a+nP_DH_30_sA0a+nP_DH_40_sA0a+nP_DH_50_sA0a+nP_DH_60_sA0a+nP_DH_70_sA0a)

cat("\n")
cat("ShieldA/Not deaths in hospital by age - after 30-06-2020 \n")
nP_DH_00_sA1b   = length( which(DAT$shieldA=="1" & DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="0-4" ))
nP_DH_05_sA1b   = length( which(DAT$shieldA=="1" & DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="5-11" ))
nP_DH_12_sA1b   = length( which(DAT$shieldA=="1" & DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="12-17" ))
nP_DH_18_sA1b   = length( which(DAT$shieldA=="1" & DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="18-29" ))
nP_DH_30_sA1b   = length( which(DAT$shieldA=="1" & DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="30-39" ))
nP_DH_40_sA1b   = length( which(DAT$shieldA=="1" & DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="40-49" ))
nP_DH_50_sA1b   = length( which(DAT$shieldA=="1" & DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="50-59" ))
nP_DH_60_sA1b   = length( which(DAT$shieldA=="1" & DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="60-69" ))
nP_DH_70_sA1b   = length( which(DAT$shieldA=="1" & DAT$ons_death_date>"2020-06-30" & DAT$all_covid_hosp>0 & DAT$age_cat=="70+" ))
nP_DH_sum_sA1b  = (nP_DH_00_sA1b+nP_DH_05_sA1b+nP_DH_12_sA1b+nP_DH_18_sA1b+nP_DH_30_sA1b+nP_DH_40_sA1b+nP_DH_50_sA1b+nP_DH_60_sA1b+nP_DH_70_sA1b)
nP_DH_00_sA0b   = nP_DH_00b - nP_DH_00_sA1b
nP_DH_05_sA0b   = nP_DH_05b - nP_DH_05_sA1b
nP_DH_12_sA0b   = nP_DH_12b - nP_DH_12_sA1b
nP_DH_18_sA0b   = nP_DH_18b - nP_DH_18_sA1b
nP_DH_30_sA0b   = nP_DH_30b - nP_DH_30_sA1b
nP_DH_40_sA0b   = nP_DH_40b - nP_DH_40_sA1b
nP_DH_50_sA0b   = nP_DH_50b - nP_DH_50_sA1b
nP_DH_60_sA0b   = nP_DH_60b - nP_DH_60_sA1b
nP_DH_70_sA0b   = nP_DH_70b - nP_DH_70_sA1b
nP_DH_sum_sA0b  = (nP_DH_00_sA0b+nP_DH_05_sA0b+nP_DH_12_sA0b+nP_DH_18_sA0b+nP_DH_30_sA0b+nP_DH_40_sA0b+nP_DH_50_sA0b+nP_DH_60_sA0b+nP_DH_70_sA0b)


cat("\n")
cat("Shielding/Not deaths in hospital - fatality proportion by age \n")
##NOTE: NOT NEC FOR SHIELD: men_time_to_death_1, men_time_to_recover_1 (time to death since last admission 1x) - assume are shielding independent
mfraction_00_s1 = rd3(nP_DH_00_s1/nP_Hosp_00_s1) #frt2(nP_DH_00_s1,nP_Hosp_00_s1)
mfraction_05_s1 = rd3(nP_DH_05_s1/nP_Hosp_05_s1)
mfraction_12_s1 = rd3(nP_DH_12_s1/nP_Hosp_12_s1)
mfraction_18_s1 = rd3(nP_DH_18_s1/nP_Hosp_18_s1)
mfraction_30_s1 = rd3(nP_DH_30_s1/nP_Hosp_30_s1)
mfraction_40_s1 = rd3(nP_DH_40_s1/nP_Hosp_40_s1)
mfraction_50_s1 = rd3(nP_DH_50_s1/nP_Hosp_50_s1)
mfraction_60_s1 = rd3(nP_DH_60_s1/nP_Hosp_60_s1)
mfraction_70_s1 = rd3(nP_DH_70_s1/nP_Hosp_70_s1)
#
mfraction_00_s0 = rd3(nP_DH_00_s0/nP_Hosp_00_s0) #frt2(nP_DH_00_s0,nP_Hosp_00_s0)
mfraction_05_s0 = rd3(nP_DH_05_s0/nP_Hosp_05_s0)
mfraction_12_s0 = rd3(nP_DH_12_s0/nP_Hosp_12_s0)
mfraction_18_s0 = rd3(nP_DH_18_s0/nP_Hosp_18_s0)
mfraction_30_s0 = rd3(nP_DH_30_s0/nP_Hosp_30_s0)
mfraction_40_s0 = rd3(nP_DH_40_s0/nP_Hosp_40_s0)
mfraction_50_s0 = rd3(nP_DH_50_s0/nP_Hosp_50_s0)
mfraction_60_s0 = rd3(nP_DH_60_s0/nP_Hosp_60_s0)
mfraction_70_s0 = rd3(nP_DH_70_s0/nP_Hosp_70_s0)
#
print(paste0("Shielding hosp mfraction age 0-4:    ", mfraction_00_s1 ))
print(paste0("Shielding hosp mfraction age 5-11:   ", mfraction_05_s1 ))
print(paste0("Shielding hosp mfraction age 12-17:  ", mfraction_12_s1 ))
print(paste0("Shielding hosp mfraction age 18-29:  ", mfraction_18_s1 ))
print(paste0("Shielding hosp mfraction age 30-39:  ", mfraction_30_s1 ))
print(paste0("Shielding hosp mfraction age 40-49:  ", mfraction_40_s1 ))
print(paste0("Shielding hosp mfraction age 50-59:  ", mfraction_50_s1 ))
print(paste0("Shielding hosp mfraction age 60-69:  ", mfraction_60_s1 ))
print(paste0("Shielding hosp mfraction age 70+:    ", mfraction_70_s1 ))
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
cat("ShieldA/Not deaths in hospital - fatality proportion by age \n")
##NOTE: NOT NEC FOR SHIELD: men_time_to_death_1, men_time_to_recover_1 (time to death since last admission 1x) - assume are shielding independent
mfraction_00_sA1 = rd3(nP_DH_00_sA1/nP_Hosp_00_sA1) #frt2(nP_DH_00_sA1,nP_Hosp_00_sA1)
mfraction_05_sA1 = rd3(nP_DH_05_sA1/nP_Hosp_05_sA1)
mfraction_12_sA1 = rd3(nP_DH_12_sA1/nP_Hosp_12_sA1)
mfraction_18_sA1 = rd3(nP_DH_18_sA1/nP_Hosp_18_sA1)
mfraction_30_sA1 = rd3(nP_DH_30_sA1/nP_Hosp_30_sA1)
mfraction_40_sA1 = rd3(nP_DH_40_sA1/nP_Hosp_40_sA1)
mfraction_50_sA1 = rd3(nP_DH_50_sA1/nP_Hosp_50_sA1)
mfraction_60_sA1 = rd3(nP_DH_60_sA1/nP_Hosp_60_sA1)
mfraction_70_sA1 = rd3(nP_DH_70_sA1/nP_Hosp_70_sA1)
#
mfraction_00_sA0 = rd3(nP_DH_00_sA0/nP_Hosp_00_sA0) #frt2(nP_DH_00_sA0,nP_Hosp_00_sA0)
mfraction_05_sA0 = rd3(nP_DH_05_sA0/nP_Hosp_05_sA0)
mfraction_12_sA0 = rd3(nP_DH_12_sA0/nP_Hosp_12_sA0)
mfraction_18_sA0 = rd3(nP_DH_18_sA0/nP_Hosp_18_sA0)
mfraction_30_sA0 = rd3(nP_DH_30_sA0/nP_Hosp_30_sA0)
mfraction_40_sA0 = rd3(nP_DH_40_sA0/nP_Hosp_40_sA0)
mfraction_50_sA0 = rd3(nP_DH_50_sA0/nP_Hosp_50_sA0)
mfraction_60_sA0 = rd3(nP_DH_60_sA0/nP_Hosp_60_sA0)
mfraction_70_sA0 = rd3(nP_DH_70_sA0/nP_Hosp_70_sA0)
#
print(paste0("ShieldA hosp mfraction age 0-4:    ", mfraction_00_sA1 ))
print(paste0("ShieldA hosp mfraction age 5-11:   ", mfraction_05_sA1 ))
print(paste0("ShieldA hosp mfraction age 12-17:  ", mfraction_12_sA1 ))
print(paste0("ShieldA hosp mfraction age 18-29:  ", mfraction_18_sA1 ))
print(paste0("ShieldA hosp mfraction age 30-39:  ", mfraction_30_sA1 ))
print(paste0("ShieldA hosp mfraction age 40-49:  ", mfraction_40_sA1 ))
print(paste0("ShieldA hosp mfraction age 50-59:  ", mfraction_50_sA1 ))
print(paste0("ShieldA hosp mfraction age 60-69:  ", mfraction_60_sA1 ))
print(paste0("ShieldA hosp mfraction age 70+:    ", mfraction_70_sA1 ))
cat("\n")
print(paste0("Non-shieldA hosp mfraction age 0-4:   ", mfraction_00_sA0 ))
print(paste0("Non-shieldA hosp mfraction age 5-11:  ", mfraction_05_sA0 ))
print(paste0("Non-shieldA hosp mfraction age 12-17: ", mfraction_12_sA0 ))
print(paste0("Non-shieldA hosp mfraction age 18-29: ", mfraction_18_sA0 ))
print(paste0("Non-shieldA hosp mfraction age 30-39: ", mfraction_30_sA0 ))
print(paste0("Non-shieldA hosp mfraction age 40-49: ", mfraction_40_sA0 ))
print(paste0("Non-shieldA hosp mfraction age 50-59: ", mfraction_50_sA0 ))
print(paste0("Non-shieldA hosp mfraction age 60-69: ", mfraction_60_sA0 ))
print(paste0("Non-shieldA hosp mfraction age 70+:   ", mfraction_70_sA0 ))

cat("\n")
cat("ShieldA/Not deaths in hospital - fatality proportion by age - prior 2020-06-30 \n")
##NOTE: NOT NEC FOR SHIELD: men_time_to_death_1, men_time_to_recover_1 (time to death since last admission 1x) - assume are shielding independent
mfraction_00_sA1a = rd3(nP_DH_00_sA1a/nP_Hosp_00_sA1a) #frt2(nP_DH_00_sA1,nP_Hosp_00_sA1)
mfraction_05_sA1a = rd3(nP_DH_05_sA1a/nP_Hosp_05_sA1a)
mfraction_12_sA1a = rd3(nP_DH_12_sA1a/nP_Hosp_12_sA1a)
mfraction_18_sA1a = rd3(nP_DH_18_sA1a/nP_Hosp_18_sA1a)
mfraction_30_sA1a = rd3(nP_DH_30_sA1a/nP_Hosp_30_sA1a)
mfraction_40_sA1a = rd3(nP_DH_40_sA1a/nP_Hosp_40_sA1a)
mfraction_50_sA1a = rd3(nP_DH_50_sA1a/nP_Hosp_50_sA1a)
mfraction_60_sA1a = rd3(nP_DH_60_sA1a/nP_Hosp_60_sA1a)
mfraction_70_sA1a = rd3(nP_DH_70_sA1a/nP_Hosp_70_sA1a)
#
mfraction_00_sA0a = rd3(nP_DH_00_sA0a/nP_Hosp_00_sA0a) #frt2(nP_DH_00_sA0,nP_Hosp_00_sA0)
mfraction_05_sA0a = rd3(nP_DH_05_sA0a/nP_Hosp_05_sA0a)
mfraction_12_sA0a = rd3(nP_DH_12_sA0a/nP_Hosp_12_sA0a)
mfraction_18_sA0a = rd3(nP_DH_18_sA0a/nP_Hosp_18_sA0a)
mfraction_30_sA0a = rd3(nP_DH_30_sA0a/nP_Hosp_30_sA0a)
mfraction_40_sA0a = rd3(nP_DH_40_sA0a/nP_Hosp_40_sA0a)
mfraction_50_sA0a = rd3(nP_DH_50_sA0a/nP_Hosp_50_sA0a)
mfraction_60_sA0a = rd3(nP_DH_60_sA0a/nP_Hosp_60_sA0a)
mfraction_70_sA0a = rd3(nP_DH_70_sA0a/nP_Hosp_70_sA0a)
#
print(paste0("ShieldA prior 2020-06-30 hosp mfraction age 0-4:    ", mfraction_00_sA1a ))
print(paste0("ShieldA prior 2020-06-30 hosp mfraction age 5-11:   ", mfraction_05_sA1a ))
print(paste0("ShieldA prior 2020-06-30 hosp mfraction age 12-17:  ", mfraction_12_sA1a ))
print(paste0("ShieldA prior 2020-06-30 hosp mfraction age 18-29:  ", mfraction_18_sA1a ))
print(paste0("ShieldA prior 2020-06-30 hosp mfraction age 30-39:  ", mfraction_30_sA1a ))
print(paste0("ShieldA prior 2020-06-30 hosp mfraction age 40-49:  ", mfraction_40_sA1a ))
print(paste0("ShieldA prior 2020-06-30 hosp mfraction age 50-59:  ", mfraction_50_sA1a ))
print(paste0("ShieldA prior 2020-06-30 hosp mfraction age 60-69:  ", mfraction_60_sA1a ))
print(paste0("ShieldA prior 2020-06-30 hosp mfraction age 70+:    ", mfraction_70_sA1a ))
cat("\n")
print(paste0("Non-shieldA prior 2020-06-30 hosp mfraction age 0-4:   ", mfraction_00_sA0a ))
print(paste0("Non-shieldA prior 2020-06-30 hosp mfraction age 5-11:  ", mfraction_05_sA0a ))
print(paste0("Non-shieldA prior 2020-06-30 hosp mfraction age 12-17: ", mfraction_12_sA0a ))
print(paste0("Non-shieldA prior 2020-06-30 hosp mfraction age 18-29: ", mfraction_18_sA0a ))
print(paste0("Non-shieldA prior 2020-06-30 hosp mfraction age 30-39: ", mfraction_30_sA0a ))
print(paste0("Non-shieldA prior 2020-06-30 hosp mfraction age 40-49: ", mfraction_40_sA0a ))
print(paste0("Non-shieldA prior 2020-06-30 hosp mfraction age 50-59: ", mfraction_50_sA0a ))
print(paste0("Non-shieldA prior 2020-06-30 hosp mfraction age 60-69: ", mfraction_60_sA0a ))
print(paste0("Non-shieldA prior 2020-06-30 hosp mfraction age 70+:   ", mfraction_70_sA0a ))

cat("\n")
cat("ShieldA/Not deaths in hospital - fatality proportion by age - after 2020-06-30 \n")
##NOTE: NOT NEC FOR SHIELD: men_time_to_death_1, men_time_to_recover_1 (time to death since last admission 1x) - assume are shielding independent
mfraction_00_sA1b = rd3(nP_DH_00_sA1b/nP_Hosp_00_sA1b) #frt2(nP_DH_00_sA1,nP_Hosp_00_sA1)
mfraction_05_sA1b = rd3(nP_DH_05_sA1b/nP_Hosp_05_sA1b)
mfraction_12_sA1b = rd3(nP_DH_12_sA1b/nP_Hosp_12_sA1b)
mfraction_18_sA1b = rd3(nP_DH_18_sA1b/nP_Hosp_18_sA1b)
mfraction_30_sA1b = rd3(nP_DH_30_sA1b/nP_Hosp_30_sA1b)
mfraction_40_sA1b = rd3(nP_DH_40_sA1b/nP_Hosp_40_sA1b)
mfraction_50_sA1b = rd3(nP_DH_50_sA1b/nP_Hosp_50_sA1b)
mfraction_60_sA1b = rd3(nP_DH_60_sA1b/nP_Hosp_60_sA1b)
mfraction_70_sA1b = rd3(nP_DH_70_sA1b/nP_Hosp_70_sA1b)
#
mfraction_00_sA0b = rd3(nP_DH_00_sA0b/nP_Hosp_00_sA0b) #frt2(nP_DH_00_sA0,nP_Hosp_00_sA0)
mfraction_05_sA0b = rd3(nP_DH_05_sA0b/nP_Hosp_05_sA0b)
mfraction_12_sA0b = rd3(nP_DH_12_sA0b/nP_Hosp_12_sA0b)
mfraction_18_sA0b = rd3(nP_DH_18_sA0b/nP_Hosp_18_sA0b)
mfraction_30_sA0b = rd3(nP_DH_30_sA0b/nP_Hosp_30_sA0b)
mfraction_40_sA0b = rd3(nP_DH_40_sA0b/nP_Hosp_40_sA0b)
mfraction_50_sA0b = rd3(nP_DH_50_sA0b/nP_Hosp_50_sA0b)
mfraction_60_sA0b = rd3(nP_DH_60_sA0b/nP_Hosp_60_sA0b)
mfraction_70_sA0b = rd3(nP_DH_70_sA0b/nP_Hosp_70_sA0b)
#
print(paste0("Shielding after 2020-30-06 hosp mfraction age 0-4:    ", mfraction_00_sA1b ))
print(paste0("Shielding after 2020-30-06 hosp mfraction age 5-11:   ", mfraction_05_sA1b ))
print(paste0("Shielding after 2020-30-06 hosp mfraction age 12-17:  ", mfraction_12_sA1b ))
print(paste0("Shielding after 2020-30-06 hosp mfraction age 18-29:  ", mfraction_18_sA1b ))
print(paste0("Shielding after 2020-30-06 hosp mfraction age 30-39:  ", mfraction_30_sA1b ))
print(paste0("Shielding after 2020-30-06 hosp mfraction age 40-49:  ", mfraction_40_sA1b ))
print(paste0("Shielding after 2020-30-06 hosp mfraction age 50-59:  ", mfraction_50_sA1b ))
print(paste0("Shielding after 2020-30-06 hosp mfraction age 60-69:  ", mfraction_60_sA1b ))
print(paste0("Shielding after 2020-30-06 hosp mfraction age 70+:    ", mfraction_70_sA1b ))
cat("\n")
print(paste0("Non-shield after 2020-30-06 hosp mfraction age 0-4:   ", mfraction_00_sA0b ))
print(paste0("Non-shield after 2020-30-06 hosp mfraction age 5-11:  ", mfraction_05_sA0b ))
print(paste0("Non-shield after 2020-30-06 hosp mfraction age 12-17: ", mfraction_12_sA0b ))
print(paste0("Non-shield after 2020-30-06 hosp mfraction age 18-29: ", mfraction_18_sA0b ))
print(paste0("Non-shield after 2020-30-06 hosp mfraction age 30-39: ", mfraction_30_sA0b ))
print(paste0("Non-shield after 2020-30-06 hosp mfraction age 40-49: ", mfraction_40_sA0b ))
print(paste0("Non-shield after 2020-30-06 hosp mfraction age 50-59: ", mfraction_50_sA0b ))
print(paste0("Non-shield after 2020-30-06 hosp mfraction age 60-69: ", mfraction_60_sA0b ))
print(paste0("Non-shield after 2020-30-06 hosp mfraction age 70+:   ", mfraction_70_sA0b ))


sink()



