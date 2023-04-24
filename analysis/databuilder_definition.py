from databuilder.ehrql import Dataset, days, case, when

from datasets import add_common_variables, study_end_date, study_start_date

from databuilder.tables.beta.tpp import practice_registrations

dataset = Dataset()

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
population_in = (registrations_number == 1)
dataset.define_population(population_in)

add_common_variables(dataset, study_start_date, study_end_date)

# any additonal editing e.g., making the region code
