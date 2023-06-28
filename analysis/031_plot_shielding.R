library(tidyverse)
library(data.table)
library(here)
library(lubridate)
library(arrow)
library(glue)

source(here::here("analysis/functions/redaction.R"))

shielding_cohort <- arrow::read_parquet(file = here::here("output/data_edited.gz.parquet"),
                                        compression = "gzip", compression_level = 5)

output_dir_plot <- here("output/figures")
fs::dir_create(output_dir_plot)

# summarise shielding additions/deductions over time  --------------------
glimpse(shielding_cohort)
shielding_dynamics <- shielding_cohort %>% 
  dplyr::select(patient_id,
                starts_with("pt_"),
                shielding_v1_startdate, 
                shielding_v2_startdate, 
                shielding_v2_enddate)

shielding_dynamics %>% glimpse()

# Convert the data frame to a data.table
setDT(shielding_dynamics)

# Create a sequence of dates from the minimum eligible start date to the maximum eligible end date
dates <- seq(min(shielding_dynamics$pt_start_date), max(shielding_dynamics$pt_end_date), by = "1 week")

# Create a data table with the first day of each month as a column
dt_weekly <- data.table(week_start_date = dates)

# Define a modified version of the count_eligible_patients function that takes only one argument
count_date_events <- function(week, date_var) {
  shielding_dynamics[get(date_var) >= week & get(date_var) < week %m+% weeks(1), .N]
}

dt_weekly[, n_shielding_v1_startdate := cumsum(mapply(count_date_events, week = week_start_date, date_var = "shielding_v1_startdate"))]
dt_weekly[, n_shielding_v2_startdate := cumsum(mapply(count_date_events, week = week_start_date, date_var = "shielding_v2_startdate"))]
dt_weekly[, n_shielding_v2_enddate := cumsum(mapply(count_date_events, week = week_start_date, date_var = "shielding_v2_enddate"))*-1]

# make the plot over time  ------------------------------------------------
col_add <- rgb(0, 0.5, 0.5)
col_deduct <- rgb(1, 0.5, 0.5)
fills <- c("GP addition" = col_add, "GP deduction" = col_deduct)

p2 <- ggplot(dt_weekly, aes(x = week_start_date)) +
  geom_col(aes(y = n_shielding_v2_startdate, fill = "GP addition"), lwd = 0.05, col = 1, alpha = 0.75) +
  geom_col(aes(y = n_shielding_v2_enddate, fill = "GP deduction"), lwd = 0.05, col = 1, alpha = 0.75) +
  labs(y = "# patients", x = "Date", main = "Version2", fill = "") +
  scale_fill_manual(values = fills) +
  theme_classic()

p1 <- ggplot(dt_weekly, aes(x = week_start_date)) +
  geom_col(aes(y = n_shielding_v1_startdate, fill = "GP addition"), lwd = 0.05, col = 1, alpha = 0.75) +
  labs(y = "# patients", x = "Date", main = "Version1", fill = "") +
  scale_fill_manual(values = fills) +
  theme_classic()


pdf(here::here("output/figures/shielding_over_time.pdf"), width = 8, height = 6)
  cowplot::plot_grid(p1, p2, ncol = 1, labels = "auto")
dev.off()
