from databuilder.ehrql import Dataset, days, case, when

# from datasets import add_common_variables, study_end_date, study_start_date

# this is where we import the schema to run the study with
from databuilder.tables.beta.tpp import (
  practice_registrations,
  patients,
  clinical_events,
  sgss_covid_all_tests,
  hospital_admissions
)

from variable_lib import (
  age_as_of,
  has_died,
  address_as_of,
  # has_prior_event_comorbiditydate,
  create_sequential_variables,
  hospitalisation_diagnosis_matches
)

import codelists

import datetime

dataset = Dataset()

study_start_date = datetime.date(2020, 3, 1)
study_end_date = datetime.date(2021, 3, 1)
minimum_registration = 90

# get eligible registrations
registrations = practice_registrations \
    .except_where(practice_registrations.end_date <= study_start_date)

# get the number of registrations in this period to exclude anyone with >1 in the `set_population` later
registrations_number = registrations.count_for_patient()

# need to get the start and end date of last registration only
registration = registrations \
    .sort_by(practice_registrations.start_date).last_for_patient()

dataset.pt_start_date = case(
    when(registration.start_date + days(minimum_registration) > study_start_date).then(registration.start_date + days(minimum_registration)),
    default=study_start_date,
)

dataset.pt_end_date = case(
    when(registration.end_date.is_null()).then(study_end_date),
    when(registration.end_date > study_end_date).then(study_end_date),
    default=registration.end_date,
)

# get NHS region one by one
dataset.practice_nuts = registration.practice_nuts1_region_name

# run the common codes
# Demographic variables
dataset.sex = patients.sex
dataset.age = age_as_of(study_start_date)
dataset.has_died = has_died(study_start_date)
dataset.msoa = address_as_of(study_start_date).msoa_code
dataset.imd = address_as_of(study_start_date).imd_rounded
dataset.death_date = patients.date_of_death

# Ethnicity in 6 categories ------------------------------------------------------------
dataset.ethnicity = clinical_events.where(clinical_events.ctv3_code.is_in(codelists.ethnicity)) \
    .sort_by(clinical_events.date) \
    .last_for_patient() \
    .ctv3_code.to_category(codelists.ethnicity)

# covid tests
all_test_positive = sgss_covid_all_tests \
    .where(sgss_covid_all_tests.is_positive) \
    .except_where(sgss_covid_all_tests.specimen_taken_date <= dataset.pt_start_date) \
    .except_where(sgss_covid_all_tests.specimen_taken_date >= dataset.pt_end_date)

dataset.all_test_positive = all_test_positive.count_for_patient()

dataset.all_tests = sgss_covid_all_tests \
    .except_where(sgss_covid_all_tests.specimen_taken_date <= dataset.pt_start_date) \
    .except_where(sgss_covid_all_tests.specimen_taken_date >= dataset.pt_end_date) \
    .count_for_patient()

# get the date of each of up to 5 test positives
create_sequential_variables(
    dataset,
    "covid_testdate_{n}",
    num_variables=5,
    events=all_test_positive,
    column="specimen_taken_date"
)

# covid hospitalisation
covid_hospitalisations = hospitalisation_diagnosis_matches(hospital_admissions, codelists.hosp_covid)

all_covid_hosp = covid_hospitalisations \
    .where(covid_hospitalisations.admission_date >= dataset.pt_start_date) \
    .except_where(covid_hospitalisations.admission_date >= dataset.pt_end_date)

# get the date of each of up to 3 COVID hospitalisations
create_sequential_variables(
    dataset,
    "covid_hosp_admitted_{n}",
    num_variables=3,
    events=all_covid_hosp,
    column="admission_date"
)

# get the discharge date of each of up to 3 COVID hospitalisations
create_sequential_variables(
    dataset,
    "covid_hosp_discharge_{n}",
    num_variables=3,
    events=all_covid_hosp,
    column="discharge_date"
)

dataset.all_covid_hosp = all_covid_hosp \
    .count_for_patient()

# Any covid identification
primarycare_covid = clinical_events \
    .where(clinical_events.ctv3_code.is_in(codelists.any_primary_care_code)) \
    .where(clinical_events.date >= dataset.pt_start_date) \
    .except_where(clinical_events.date >= dataset.pt_end_date)

dataset.latest_primarycare_covid = primarycare_covid \
    .sort_by(primarycare_covid.date) \
    .last_for_patient().date

dataset.total_primarycare_covid = primarycare_covid \
    .count_for_patient()

# comorbidities - unique per comorbidity

# We define baseline variables on the day _before_ the study date (start date = day of
# first possible booster vaccination)
baseline_date = dataset.pt_start_date - days(1)

events = clinical_events
prior_events = events.where(events.date.is_on_or_before(baseline_date))


def has_prior_event(codelist, where=True):
    return (
        prior_events.where(where)
        .where(prior_events.ctv3_code.is_in(codelist))
        .sort_by(prior_events.date)
        .last_for_patient().date
    )


def has_prior_event_numeric(codelist, where=True):
    prior_events_exists = prior_events.where(where) \
        .where(prior_events.ctv3_code.is_in(codelist)) \
        .exists_for_patient()
    return (
        case(
            when(prior_events_exists).then(1),
            when(~prior_events_exists).then(0)
            )
    )


dataset.diabetes = has_prior_event(codelists.diabetes_codes)
dataset.haem_cancer = has_prior_event(codelists.haem_cancer_codes)
dataset.lung_cancer = has_prior_event(codelists.lung_cancer_codes)
dataset.other_cancer = has_prior_event(codelists.other_cancer_codes)
dataset.asthma = has_prior_event(codelists.asthma_codes)
dataset.chronic_cardiac_disease = has_prior_event(codelists.chronic_cardiac_disease_codes)
dataset.chronic_liver_disease = has_prior_event(codelists.chronic_liver_disease_codes)
dataset.chronic_respiratory_disease = has_prior_event(codelists.chronic_respiratory_disease_codes)
dataset.other_neuro = has_prior_event(codelists.other_neuro_codes)
dataset.stroke_gp = has_prior_event(codelists.stroke_gp_codes)
dataset.dementia = has_prior_event(codelists.dementia_codes)
dataset.ra_sle_psoriasis = has_prior_event(codelists.ra_sle_psoriasis_codes)
dataset.psychosis_schizophrenia_bipolar = has_prior_event(codelists.psychosis_schizophrenia_bipolar_codes)
dataset.permanent_immune = has_prior_event(codelists.permanent_immune_codes)
dataset.temp_immune = has_prior_event(codelists.temp_immune_codes)

binary_diabetes = has_prior_event_numeric(codelists.diabetes_codes)
binary_haem_cancer = has_prior_event_numeric(codelists.haem_cancer_codes)
binary_lung_cancer = has_prior_event_numeric(codelists.lung_cancer_codes)
binary_other_cancer = has_prior_event_numeric(codelists.other_cancer_codes)
binary_asthma = has_prior_event_numeric(codelists.asthma_codes)
binary_chronic_cardiac_disease = has_prior_event_numeric(codelists.chronic_cardiac_disease_codes)
binary_chronic_liver_disease = has_prior_event_numeric(codelists.chronic_liver_disease_codes)
binary_chronic_respiratory_disease = has_prior_event_numeric(codelists.chronic_respiratory_disease_codes)
binary_other_neuro = has_prior_event_numeric(codelists.other_neuro_codes)
binary_stroke_gp = has_prior_event_numeric(codelists.stroke_gp_codes)
binary_dementia = has_prior_event_numeric(codelists.dementia_codes)
binary_ra_sle_psoriasis = has_prior_event_numeric(codelists.ra_sle_psoriasis_codes)
binary_psychosis_schizophrenia_bipolar = has_prior_event_numeric(codelists.psychosis_schizophrenia_bipolar_codes)
binary_permanent_immune = has_prior_event_numeric(codelists.permanent_immune_codes)
binary_temp_immune = has_prior_event_numeric(codelists.temp_immune_codes)

dataset.comorbid_count = binary_diabetes + \
    binary_haem_cancer + \
    binary_lung_cancer + \
    binary_other_cancer + \
    binary_asthma + \
    binary_chronic_cardiac_disease + \
    binary_chronic_liver_disease + \
    binary_chronic_respiratory_disease + \
    binary_other_neuro + \
    binary_stroke_gp + \
    binary_dementia + \
    binary_ra_sle_psoriasis + \
    binary_psychosis_schizophrenia_bipolar + \
    binary_permanent_immune + \
    binary_temp_immune

# negative control - hospital fractures
fracture_hospitalisations = hospitalisation_diagnosis_matches(hospital_admissions, codelists.hosp_fractures)

dataset.first_fracture_hosp = fracture_hospitalisations \
    .where(fracture_hospitalisations.admission_date.is_between(dataset.pt_start_date, dataset.pt_end_date)) \
    .sort_by(fracture_hospitalisations.admission_date) \
    .first_for_patient().admission_date

# shielding codes
dataset.highrisk_shield = clinical_events \
    .where(clinical_events.snomedct_code.is_in(codelists.high_risk_shield)) \
    .sort_by(clinical_events.date) \
    .first_for_patient().date

dataset.lowrisk_shield = clinical_events \
    .where(clinical_events.snomedct_code.is_in(codelists.low_risk_shield)) \
    .sort_by(clinical_events.date) \
    .first_for_patient().date

# care home flag
dataset.care_home = address_as_of(dataset.pt_start_date) \
    .care_home_is_potential_match.if_null_then(False)

dataset.care_home_nursing = address_as_of(dataset.pt_start_date) \
    .care_home_requires_nursing.if_null_then(False)

# final age restriction
pop_restrict = (registrations_number == 1) & (dataset.age <= 100) & (dataset.age >= 18) & (dataset.sex.contains("male"))
dataset.define_population(pop_restrict)
