library(tidyverse)
library(magrittr)
library(here)
library(lubridate)
library(arrow)
library(gtsummary)
library(glue)

source(here::here("analysis/functions/redaction.R"))

shielding_cohort <- arrow::read_parquet(file = here::here("output/data_edited.gz.parquet"),
                                         compression = "gzip", compression_level = 5)
output_dir_tab <- here("output/tables")
fs::dir_create(output_dir_tab)

# redaction functions for data --------------------------------------------
roundmid_any <- function(x, to=1){
  # like ceiling_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}

# will update this when releasing outputs
threshold <- 10

shielding_data <- shielding_cohort %>%
  # select only baseline variables 
  dplyr::select(pt_start_date, 
                pt_end_date,
                t,
                sex,
                age,
                age_cat,
                practice_nuts, 
                imd_q5,
                ethnicity,
                has_died, 
                all_test_positive, 
                all_tests,
                all_covid_hosp,
                total_primarycare_covid,
                comorbidities_factor,
                care_home, 
                care_home_nursing,
                shielding,
                shielding_v1_binary,
                shielding_v2_binary,
                hirisk_shield_count,
                lorisk_shield_count,
                covid_hosp_cat, 
                covid_primary_cat, 
                test_positive_cat,
                test_total_cat,
                appts_1yr_before, 
                allhosp_1yr_before) %>% 
  # calculate year of study enrollment
  dplyr::mutate(pt_start_year = factor(year(pt_start_date))) %>% 
  dplyr::select(-pt_start_date,
                -pt_end_date) %>% 
  dplyr::select(pt_start_year, everything())

var_labels <- list(
  N  ~ "Total N",
  pt_start_year ~ "Follow-up start (year)",
  sex ~ "Sex",
  t ~ "Follow-up time (years)",
  age ~ "Age",
  age_cat ~ "Age (categorised)",
  ethnicity ~ "Ethnicity",
  practice_nuts ~ "NHS region",
  imd_q5 ~ "Index of multiple deprivation (quintile)",
  comorbidities_factor ~ "Comorbidities",
  care_home ~ "Resident in care home",
  care_home_nursing ~ "Resident in care home (with nursing)",
  shielding ~ "Shielding category",
  shielding_v1_binary ~ "Shielding v1: shielding from the start",
  shielding_v2_binary ~ "Shielding v2: shielding until low/moderate flag",
  hirisk_shield_count ~ "Codes for high-risk shielding (n)",
  lorisk_shield_count ~ "Codes for low/moderate-risk shielding (n)",
  fracture ~ "Hospitalised for fracture",
  all_covid_hosp ~ "Total COVID-19 hospitalisations (n)",
  covid_hosp_cat ~ "COVID-19 hospitalisations per person (n)",
  covid_primary_cat ~ "COVID-19 primary care record (n)", 
  test_positive_cat ~ "COVID-19 positive tests (n)",
  test_total_cat ~ "COVID-19 tests (n)",
  vaccine_schedule_detail ~ "Vaccination schedule received",
  appts_1yr_before ~ "Healthcare use: appointments 1 year before study entry (n)",
  allhosp_1yr_before ~ "Healthcare use: hospital admissions 1 year before study entry (n)"
)

var_labels <- var_labels %>%
  set_names(., map_chr(., all.vars))

# make table 1 ------------------------------------------------------------
table1 <- shielding_cohort %>% 
  dplyr::select(any_of(names(var_labels))) %>% 
  tbl_summary(
    label = unname(var_labels[names(.)]),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  )

table1 %>%
  as_gt() %>%
  gt::gtsave(
    filename = "shielding_table1.html",
    path = fs::path(output_dir_tab)
  )

raw_stats <- table1$meta_data %>%
  select(var_label, df_stats) %>%
  unnest(df_stats)

raw_stats_redacted <- raw_stats %>%
  mutate(
    n=roundmid_any(n, threshold),
    N=roundmid_any(N, threshold),
    p=round(100*n/N,1),
    N_miss = roundmid_any(N_miss, threshold),
    N_obs = roundmid_any(N_obs, threshold),
    p_miss = round(100*N_miss/N_obs,1),
    N_nonmiss = roundmid_any(N_nonmiss, threshold),
    p_nonmiss = round(100*N_nonmiss/N_obs,1),
    var_label = factor(var_label, levels=map_chr(var_labels[-c(1)], ~last(as.character(.)))),
    variable_levels = replace_na(as.character(variable_levels), "")
  )
write_csv(raw_stats_redacted, here::here("output/table1_data.csv"))

table1_data <- raw_stats_redacted %>%
  rowwise() %>%
  transmute(
    var_label,
    # gt creates a column called `label` when run locally, `variable_labels` 
    # when run in opensafely (probs different versions)
    # label,
    variable_levels,
    value = glue(stat_display)
  ) 

table1_review <- table1_data %>%
  knitr::kable(format = "html") %>%
  kableExtra::kable_paper() %>%
  kableExtra::kable_styling(
    full_width = FALSE
  )

# table to help reviewing
kableExtra::save_kable(table1_review, file = fs::path(output_dir_tab, glue("shielding_table1_redacted.html")))


# table 2 - by shielding status -------------------------------------------
table2 <- shielding_cohort %>% 
  dplyr::select(any_of(names(var_labels))) %>% 
  tbl_summary(
    by = shielding, 
    label = unname(var_labels[names(.)]),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  )

table2 %>%
  as_gt() %>%
  gt::gtsave(
    filename = "shielding_table2.html",
    path = fs::path(output_dir_tab)
  )


# table 2 - by shielding status v1 and v2-----------------------------------
table2_v1 <- shielding_cohort %>% 
  dplyr::select(any_of(names(var_labels))) %>% 
  tbl_summary(
    by = shielding_v1_binary, 
    label = unname(var_labels[names(.)]),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  )

table2_v1 %>%
  as_gt() %>%
  gt::gtsave(
    filename = "shielding_v1_table2.html",
    path = fs::path(output_dir_tab)
  )

table2_v2 <- shielding_cohort %>% 
  dplyr::select(any_of(names(var_labels))) %>% 
  tbl_summary(
    by = shielding_v2_binary, 
    label = unname(var_labels[names(.)]),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  )

table2_v2 %>%
  as_gt() %>%
  gt::gtsave(
    filename = "shielding_v2_table2.html",
    path = fs::path(output_dir_tab)
  )


