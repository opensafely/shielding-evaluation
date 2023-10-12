## 040 hospital admissions

source(here::here("analysis/functions/redaction.R"))

output_plot <- here("output/figures")
fs::dir_create(output_plot)

shielding_cohort <- arrow::read_parquet(file = here::here("output/data_edited.gz.parquet"),
                                        compression = "gzip", compression_level = 5)

# week and year for first hospitalisation
shielding_hosp <- shielding_cohort                        %>% 
  dplyr::select(patient_id,
                dplyr::starts_with("pt_"),
                shielding, 
                age_cat,
                practice_nuts,
                dplyr::contains("hosp_admitted"))         %>% 
  pivot_longer(cols = dplyr::contains("hosp_admitted"), 
                names_pattern = "covid_hosp_admitted_(.)",
                names_to = "covid_admission",
                values_to = "admission_date")              %>% 
  drop_na()
# week, year, date-by-week variables
StartDate <- min(shielding_hosp$admission_date, na.rm = TRUE)

shielding_hosp <- shielding_hosp %>%
  mutate(hosp_week = lubridate::week(admission_date), #non-unique id across years
         hosp_year = lubridate::year(admission_date),
         plot_date = StartDate + weeks(hosp_week - week(StartDate)) + years(hosp_year - year(StartDate))) %>%
  arrange(hosp_week)              

#Week density and cumulative
By_week <- function(data, vars){
  data %>% 
    group_by(across({{vars}}))      %>%
    mutate(weekly_admissions = n()) %>%
    ungroup(hosp_year, hosp_week)   %>%
    arrange(plot_date)              %>%
    mutate(total_admissions = cumsum(weekly_admissions)) #%>% #CUMULATIVE                                  
  #ungroup()
}

#overall
shielding_hosp_summ   <- shielding_hosp %>% By_week(c(hosp_year, hosp_week))
# by age
shielding_hosp_summ_a <- shielding_hosp %>% By_week(c(hosp_year, hosp_week, age_cat))                         

#Date range of Data
#plot_date (in weeks) is the date identifier because counts are aggregated in weeks
Week1OfData = min(shielding_hosp_summ$plot_date)
Week2OfData = max(shielding_hosp_summ$plot_date)
print(paste0("Date by week in data, range  ", Week1OfData, ", ", Week2OfData))

#Figures - overlapping groups
fig0 <- function(data, x , y, col, xname='Date', yname='Weekly admissions') {
  ggplot(data, aes(x = {{x}}, y = {{y}}, col = {{col}})) +
    geom_line() + 
    geom_point(size = 1.2, pch = 1) +
    labs(x = xname, y = yname) +
    ylim(c(0, NA)) +
    theme_bw() }
#Figures - panels for groups
fig <- function(data, x , y, col, facets, xname='Date', yname='Weekly admissions') {
  ggplot(data, aes(x = {{x}}, y = {{y}}, col = {{col}})) +
    geom_line() + 
    geom_point(size = 1.2, pch = 1) +
    facet_wrap(facets, ncol = 1, scales = 'free_y') +
    labs(x = xname, y = yname) +
    ylim(c(0, NA)) +
    theme_bw() }

##PLOTS WEEKLY
pdf(here::here("output/figures/covid_hosp_over_time2again.pdf"), width = 8, height = 6)
  ##All
  fig(shielding_hosp_summ,   plot_date, weekly_admissions,                      facets = 'NULL',          xname = 'Date',  yname = 'Weekly admissions')
  #by age_cat
  #fig0(shielding_hosp_summ_a, plot_date, weekly_admissions, col = age_cat,      xname = 'Date',  yname = 'Weekly admissions')
  #by age_cat - panels
  #fig(shielding_hosp_summ_a, plot_date, weekly_admissions, col = age_cat,       facets = "age_cat",       xname = 'Date',  yname = 'Weekly admissions')
dev.off()

datD <- tibble(Weeks = shielding_hosp_summ$hosp_week, 
              Dataz  = shielding_hosp_summ$weekly_admissions,
              Dates  = shielding_hosp_summ$plot_date,
              Week1OfData = min(shielding_hosp_summ$plot_date),
              Week2OfData = max(shielding_hosp_summ$plot_date))

p1 <- ggplot(datD, aes(x = Weeks)) + #Dates))
      geom_point(aes(y = Dataz)) +
      labs(x = 'Weeks', y = 'Weekly admissions') 

if (pset$iplatform==1) { gridExtra::grid.arrange(p1, nrow = 1) }

pdf(file = paste0(output_dir,"/",pset$File_data_plots))
  gridExtra::grid.arrange(p1, nrow = 1)
dev.off()
