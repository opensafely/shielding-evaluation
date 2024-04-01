from ehrql import Dataset, days, case, when, years

# from datasets import add_common_variables, study_end_date, study_start_date

# this is where we import the schema to run the study with
from ehrql.tables.beta.tpp import (
  practice_registrations,
  appointments,
  vaccinations,
  patients,
  ons_deaths,
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


# ------- Variable lib
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


def covid_on_deathcert(cause_of_death):
    covid_mentioned = cause_of_death.is_in(codelists.hosp_covid)
    return (
        case(
            when(covid_mentioned).then(1),
            when(~covid_mentioned).then(0)
            )
    )


# Function codes for extracting monthly GP visit from `appointments` table
#def add_visits(from_date, to_date):
#    # Number of GP visits between `from_date` and `to_date`
#    return appointments \
#        .where(appointments.start_date.is_between_but_not_on(from_date, to_date)) \
#        .count_for_patient()


def add_hospitalisations(from_date, to_date):
    # Hospitalisation within `num_months` of `from_date`
    return hospital_admissions \
        .where(hospital_admissions.admission_date.is_between_but_not_on(from_date, to_date)) \
        .count_for_patient()


# -------
dataset = Dataset()

# start in 
study_start_date = datetime.date(2020, 1, 1)
study_end_date = datetime.date(2021, 9, 1)
minimum_registration = 90

# get eligible registrations
registrations = practice_registrations \
    .except_where(practice_registrations.end_date <= study_start_date) \
    .except_where(practice_registrations.start_date + days(minimum_registration) > study_end_date)

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

# Date and underlying cause of earliest registered death for patients
# where registered death date >= pt_start_date and < pt_end_date
dataset.ons_death_date = case(
    when(
        (ons_deaths.date >= dataset.pt_start_date)
        & (ons_deaths.date < dataset.pt_end_date)
    ).then(ons_deaths.date),
    default=None,
)

dataset.ons_underlying_cause = case(
    when(
        (ons_deaths.date >= dataset.pt_start_date)
        & (ons_deaths.date < dataset.pt_end_date)
    ).then(ons_deaths.underlying_cause_of_death),
    default=None,
)

# Ethnicity in 6 categories ------------------------------------------------------------
dataset.ethnicity = clinical_events.where(clinical_events.ctv3_code.is_in(codelists.ethnicity)) \
    .sort_by(clinical_events.date) \
    .last_for_patient() \
    .ctv3_code.to_category(codelists.ethnicity)

# covid tests
all_test_positive = sgss_covid_all_tests \
    .where(sgss_covid_all_tests.is_positive) \
    .where(sgss_covid_all_tests.specimen_taken_date.is_between_but_not_on(dataset.pt_start_date, dataset.pt_end_date))

dataset.all_test_positive = all_test_positive.count_for_patient()

dataset.all_tests = sgss_covid_all_tests \
    .where(sgss_covid_all_tests.specimen_taken_date.is_between_but_not_on(dataset.pt_start_date, dataset.pt_end_date)) \
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
# Aiming to mathc previous OS methods on COVID hosp identification https://github.com/opensafely/post-covid-outcomes-research/blob/f3b58c7167659873c74fbc4694bbb542682711e2/analysis/study_definition_covid.py#L34-L41
covid_hospitalisations = hospitalisation_diagnosis_matches(hospital_admissions, codelists.hosp_covid)

all_covid_hosp = covid_hospitalisations \
    .where(covid_hospitalisations.admission_date >= dataset.pt_start_date) \
    .except_where(covid_hospitalisations.admission_date >= dataset.pt_end_date)

# get the date of each of up to 6 COVID hospitalisations - note only counting upto 12 covid hospitalisations in a \
#  year for data memory reasons
create_sequential_variables(
    dataset,
    "covid_hosp_admitted_{n}",
    num_variables=6,
    events=all_covid_hosp,
    column="admission_date"
)

# get the discharge date of each of up to 6 COVID hospitalisations
create_sequential_variables(
    dataset,
    "covid_hosp_discharge_{n}",
    num_variables=6,
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

# We define baseline variables on the day _before_ the study date (start date = day of study entry for each patient)
baseline_date = dataset.pt_start_date - days(1)

events = clinical_events
prior_events = events.where(events.date.is_on_or_before(baseline_date))

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
    .where(fracture_hospitalisations.admission_date.is_between_but_not_on(dataset.pt_start_date, dataset.pt_end_date)) \
    .sort_by(fracture_hospitalisations.admission_date) \
    .first_for_patient().admission_date

# care home flag
dataset.care_home = address_as_of(dataset.pt_start_date) \
    .care_home_is_potential_match.if_null_then(False)

dataset.care_home_nursing = address_as_of(dataset.pt_start_date) \
    .care_home_requires_nursing.if_null_then(False)

# Measure of healtchare utilisation - appointments
#dataset.appts_in_study = add_visits(dataset.pt_start_date, dataset.pt_end_date)
#dataset.appts_1yr_before = add_visits(dataset.pt_start_date - years(1), dataset.pt_start_date)
dataset.allhosp_in_study = add_hospitalisations(dataset.pt_start_date, dataset.pt_end_date)
dataset.allhosp_1yr_before = add_hospitalisations(dataset.pt_start_date - years(1), dataset.pt_start_date)

# vaccination codes
all_vacc = vaccinations \
    .where(vaccinations.date.is_between_but_not_on(study_start_date, study_end_date)) \
    .where(vaccinations.target_disease == "SARS-2 CORONAVIRUS")

# this will be replaced with distinct_count_for_patient() once it is developed
dataset.total_vacc = all_vacc \
    .count_for_patient()

# FIRST VACCINE DOSE ------------------------------------------------------------
# first vaccine dose was 8th December 2020
vaccine_dose_1 = all_vacc \
    .where(all_vacc.date.is_after(datetime.date(2020, 12, 7))) \
    .sort_by(all_vacc.date) \
    .first_for_patient()
dataset.vaccine_dose_1_date = vaccine_dose_1.date

# SECOND VACCINE DOSE ------------------------------------------------------------
# first recorded 2nd dose was 29th December 2020
# need a 19 day gap from first dose
vaccine_dose_2 = all_vacc \
    .where(all_vacc.date.is_after(datetime.date(2020, 12, 28))) \
    .where(all_vacc.date.is_after(dataset.vaccine_dose_1_date + days(19))) \
    .sort_by(all_vacc.date) \
    .first_for_patient()
dataset.vaccine_dose_2_date = vaccine_dose_2.date

# shielding codes ---------------------------------------------------------------
hirisk_shield_codes = clinical_events \
    .where(clinical_events.date.is_between_but_not_on(dataset.pt_start_date, dataset.pt_end_date)) \
    .where(clinical_events.snomedct_code.is_in(codelists.high_risk_shield))

lorisk_shield_codes = clinical_events \
    .where(clinical_events.date.is_between_but_not_on(dataset.pt_start_date, dataset.pt_end_date)) \
    .where(clinical_events.snomedct_code.is_in(codelists.low_risk_shield))

dataset.highrisk_shield = hirisk_shield_codes \
    .sort_by(hirisk_shield_codes.date) \
    .first_for_patient().date
dataset.hirisk_shield_count = hirisk_shield_codes.count_for_patient()

dataset.lowrisk_shield = lorisk_shield_codes \
    .sort_by(lorisk_shield_codes.date) \
    .first_for_patient().date
dataset.lorisk_shield_count = lorisk_shield_codes.count_for_patient()

create_sequential_variables(
    dataset,
    "hirisk_codedate_{n}",
    num_variables=3,
    events=hirisk_shield_codes,
    column="date"
)
create_sequential_variables(
    dataset,
    "lorisk_codedate_{n}",
    num_variables=3,
    events=lorisk_shield_codes,
    column="date"
)

# different "shielding" definitions
# 1 - people who were shielding from the start and never had a lowrisk flag
# using 2020-04-21 because it is the median value of all high risk flags
dataset.hi_risk_only = (
    hirisk_shield_codes
    .where(hirisk_shield_codes.date.is_before(datetime.date(2020, 4, 21)))
    .except_where(lorisk_shield_codes.exists_for_patient())
    .sort_by(hirisk_shield_codes.date)
    .first_for_patient()
    .date
    )

# 2 - people who had ONE high-risk and zero or one lowrisk flag afterwards
dataset.one_hirisk_start = (
    hirisk_shield_codes
    .where(dataset.hirisk_shield_count == 1)
    .date
    .minimum_for_patient()
)

dataset.one_hirisk_end = (
    lorisk_shield_codes
    .where(dataset.lorisk_codedate_1 > dataset.hirisk_codedate_1)
    .date
    .minimum_for_patient()
)

# final age restriction
# 1- only one registration
# 2- age between 0 and 100
# 3- sex is either male or female (to avoid small cell counts)
# 4- check they haven't died already. Shouldn't be able to die and then start a registration but strange things can happen

pop_restrict = (registrations_number == 1) \
    & (dataset.age <= 100) & (dataset.age >= 0) \
    & (dataset.sex.contains("male")) 

dataset.define_population(pop_restrict)
