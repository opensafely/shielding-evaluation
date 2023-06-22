library(tidyverse)
library(magrittr)
library(here)
library(lubridate)
library(arrow)

source(here::here("analysis/functions/redaction.R"))

data <- readr::read_csv(here("output/dataset_all.csv.gz"),
                        col_types = cols(
                          covid_testdate_1 = col_date(),
                          covid_testdate_2 = col_date(),
                          covid_testdate_3 = col_date(),
                          covid_testdate_4 = col_date(),
                          covid_testdate_5 = col_date(),
                          covid_hosp_admitted_1 = col_date(),
                          covid_hosp_admitted_2 = col_date(),
                          covid_hosp_admitted_3 = col_date(),
                          covid_hosp_admitted_4 = col_date(),
                          covid_hosp_admitted_5 = col_date(),
                          covid_hosp_admitted_6 = col_date(),
                          covid_hosp_discharge_1 = col_date(),
                          covid_hosp_discharge_2 = col_date(),
                          covid_hosp_discharge_3 = col_date(),
                          covid_hosp_discharge_4 = col_date(),
                          covid_hosp_discharge_5 = col_date(),
                          covid_hosp_discharge_6 = col_date(),
                          hirisk_codedate_1 = col_date(),
                          hirisk_codedate_2 = col_date(),
                          hirisk_codedate_3 = col_date(),
                          lorisk_codedate_1 = col_date(),
                          lorisk_codedate_2 = col_date(),
                          lorisk_codedate_3 = col_date(),
                          hi_risk_only = col_date(),
                          one_hirisk_start = col_date(),
                          one_hirisk_end = col_date()
                        )) %>% 
  janitor::clean_names()
spec(data) %>% print()

cleaned_data <- data %>%
  mutate(
    # create time variable for follow up (years)
    t =  pt_start_date %--% pt_end_date / dyears(1),
    # convert IMD to quintiles
    imd_q5 = cut(imd,
                 breaks = c(32844 * seq(0, 1, 0.2)),
                 labels = c("1 (most deprived)",
                            "2",
                            "3",
                            "4",
                            "5 (least deprived)")
    ),
    # create an age category variable for easy stratification
    age_cat = cut(
      age, 
      breaks = c(0, 4, 11, 17, seq(29, 69, 10), Inf),
      labels = c(
        "0-4",
        "5-11",
        "12-17",
        "18-29",
        "30-39",
        "40-49",
        "50-59",
        "60-69",
        "70+"
      )),
    # age centred (for modelling purposes)
    age_centred = age - mean(age, na.rm = TRUE),
  ethnicity = factor(
    ethnicity,
    levels = 1:6, 
    labels = c(
      "White",
      "Mixed", 
      "South Asian", 
      "Black",
      "Other",
      "Not stated"
    ))) %>% 
  # only keep people with recorded sex
  filter(
    sex %in% c("male", "female")
  ) %>% 
  mutate(sex = factor(sex, levels = c("male", "female"))) %>% 
  # treat region as a factor
  mutate(practice_nuts = factor(practice_nuts)) %>% 
  # convert number of comorbidities to factor (0,1,2+)
  mutate(
    comorbidities_factor = cut(
      comorbid_count,
      breaks = c(-Inf, 0:1, Inf),
      labels = c(as.character(0:1), "2+")
    )
  ) %>% 
  # create number of hospitalisations as factor (0-3+)
  mutate(covid_hosp_cat = cut(
    all_covid_hosp, 
    breaks = c(-Inf, 0:3, Inf),
    labels = c(as.character(0:2), "3+", "3+"))
  ) %>% 
  # create number of covid records as factor (0-5+)
  mutate(covid_primary_cat = cut(
    total_primarycare_covid, 
    breaks = c(-Inf, 0:5, Inf),
    labels = c(as.character(0:4), "5+", "5+"))
  ) %>% 
  # create number of covid positive Tests as factor (0-5+)
  mutate(test_positive_cat = cut(
    all_test_positive, 
    breaks = c(-Inf, 0:5, Inf),
    labels = c(as.character(0:4), "5+", "5+"))
  ) %>% 
  # create number of covid Tests as factor (0-5+)
  mutate(test_total_cat = cut(
    all_tests, 
    breaks = c(-Inf, 0:5, Inf),
    labels = c(as.character(0:4), "5+", "5+"))
  ) %>% 
  mutate(fracture = !is.na(first_fracture_hosp)) 

# create the different versions of shielding ------------------------------
cleaned_data <- cleaned_data %>% 
  # v0: simple crude version. IF people have a high-risk flag then they 
  # are "High risk" throughout the pandemic period. ELSE they are "Low/Moderate risk" 
  # if they have a lowrisk flag. ELSE they are not shielding
  mutate(highrisk_shield_bin = !is.na(highrisk_shield)) %>% 
  mutate(lowrisk_shield_bin = !is.na(lowrisk_shield)) %>% 
  mutate(shielding = factor(
    case_when(
      highrisk_shield_bin ~ 2,
      lowrisk_shield_bin ~ 1,
      !highrisk_shield_bin & !lowrisk_shield_bin ~ 0),
    levels = c(0, 1, 2),
    labels = c("No shielding", "Low/Moderate risk", "High Risk")
  )) %>% 
  # v1: high risk only. This creates two variables in a group 
  # of people who have a high risk flag before 2020-04-21 and 
  # never got a low risk flag. This group we can be confident were 
  # shielding from the start.
  # So start "shielding" from date of hirisk flag:
  # 0-----------hi----------->t 
  mutate(shielding_v1_binary = !is.na(hi_risk_only)) %>% 
  rename(shielding_v1_startdate = hi_risk_only) %>% 
  # v2: one high risk flag. This creates 3 variables in a 
  # group of people that have one high-risk flag, but stop 
  # the shielding if they receive a low/moderate risk flag.
  # One binary indicator for people that fit this description
  # 2 date variables:
  # So start and end date variables define the shielding period
  # 0------------------hi------------------lo------------>t
  # 0------------------shieldingshieldingshie------------>t
  mutate(shielding_v2_binary = !is.na(one_hirisk_start)) %>% 
  rename(shielding_v2_startdate = one_hirisk_start,
         shielding_v2_enddate = one_hirisk_end)

# NOTE: v1 and v2 will exclude people how have multiple
# hi-risk and lo-risk flags. We are assuming that these are 
# in the minority and too complex to accurately capture 
# their "shielding" behaviour

# output the data ---------------------------------------------------------
arrow::write_parquet(cleaned_data,
                     sink = here::here("output/data_edited.gz.parquet"),
                     compression = "gzip", compression_level = 5)

# summarise data ----------------------------------------------------------
options(width=200) # set output width for capture.output

output_dir <- "output/data_properties"
filenamebase <- "clean_dataset"

dir.create(here(output_dir), showWarnings = FALSE, recursive=TRUE)

## high-level variable overview ----
capture.output(
  skimr::skim_without_charts(cleaned_data),
  file = here(output_dir, paste0(filenamebase, "_skim", ".txt")),
  split = FALSE
)

## tabulated data ----

# delete file if it exists
if(file.exists(here(output_dir, paste0(filenamebase, "_tabulate", ".txt")))){
  file.remove(here(output_dir, paste0(filenamebase, "_tabulate", ".txt")))
}


### categorical and logical ----
sumtabs_cat <-
  cleaned_data %>%
  select(-ends_with("_id")) %>%
  select(where(is.character), where(is.logical), where(is.factor)) %>%
  map(redacted_summary_cat) %>%
  enframe()

capture.output(
  walk2(sumtabs_cat$value, sumtabs_cat$name, print_cat),
  file = here(output_dir, paste0(filenamebase, "_tabulate", ".txt")),
  append=FALSE
)

### numeric ----
sumtabs_num <-
  cleaned_data %>%
  select(-ends_with("_id")) %>%
  select(where(~ {!is.logical(.x) & is.numeric(.x) & !is.Date(.x)})) %>%
  map(redacted_summary_num) %>%
  enframe()

capture.output(
  walk2(sumtabs_num$value, sumtabs_num$name, print_num),
  file = here(output_dir, paste0(filenamebase, "_tabulate", ".txt")),
  append=TRUE
)

### dates ----

sumtabs_date <-
  cleaned_data %>%
  select(-ends_with("_id")) %>%
  select(where(is.Date)) %>%
  map(redacted_summary_date) %>%
  enframe()

capture.output(
  walk2(sumtabs_date$value, sumtabs_date$name, print_num),
  file = here(output_dir, paste0(filenamebase, "_tabulate", ".txt")),
  append=TRUE
)

