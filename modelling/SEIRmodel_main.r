## SEIR and SEIUHRD compartment models: age-structured with CoMix-contacts 
## 2-OS data, 1-model simulation, 3-MCMC fitting, 4-plots


#1#Model########################################################################
library(Rcpp)
library(BayesianTools)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(here)
library(lubridate)
#library(deSolve)
#library(bench)


#wd=repo #run without subfolders
input_dir  <- paste0(getwd(),"/modelling/input")  # getwd() #
output_dir <- paste0(getwd(),"/output/modelling") # getwd() #
fs::dir_create(output_dir)


# 0-SET UP
TIME  = format(Sys.time(),'%H.%M.%S_%d-%m-%Y')
source(file = paste0(input_dir,"/SEIRmodel_setup.r"))
## Model choice
if (pset$iplatform==0) { print(paste0("Model chosen: ",pset$MODEL[pset$imodel]))
                         print(paste0("Platform: ",pset$PLATFORM[pset$iplatform+1])) }


# 1-OS DATA
## return: datD(Weeks, Dataz, Dates, Week1OfData, Week2OfData)
## pdf: Infected_data_by_week 
## pdf: output/figures/covid_hosp_over_time2again
## plot (1): Weeks:Dataz
if (pset$iplatform>0) {
  if(pset$imodel==1) source(file = paste0(input_dir,"/SEIR_data.r"))
  if(pset$imodel==2) source(file = paste0(input_dir,"/SEIUHRD_data.r"))
}

# 2-MODEL (for fitting or simulation)
## Contact matrix
### return: cm, Week1OfModel
### csv: Contact_matrix_year-from-24Feb20_vector-lenght-9x9x52_norm=maxEV1
### txt: Contact_matrix_year-from-24Feb20_vector-lenght-9x9x52_norm=maxEV1_stats
source(file = paste0(input_dir,"/SEIR_contacts.r"))

## Parameters and initial condition
### return pars
source(file = paste0(input_dir,"/",pset$File_parameters))
pars$cm  <- as.vector(cm)

## output settings and parameters
### txt:
sink(file = paste0(output_dir,"/",pset$File_run), append=TRUE, split=FALSE)
  print(format(Sys.time(),'%H.%M.%S_%d-%m-%Y'))
  rev(pset); cat("\n")
  print(paste0("Date in contact-data, range  ", Week1OfModel, ", ", Week2OfModel)); cat("\n")
  if(pset$iplatform>0) {
  if (pset$imodel==1)  {
  print(paste0("Date by week in data, range  ", Week1OfData,  ", ", Week2OfData )); cat("\n")}
  if (pset$imodel==2)  {
  print(paste0("Date by week in H data, range  ", Week1OfDataH,", ", Week2OfDataH )); cat("\n")
  print(paste0("Date by week in D data, range  ", Week1OfDataD,", ", Week2OfDataD )); cat("\n")}     }
  rev(pars[-1]); #exclude cm
  cat("\n \n")
sink()

## R0 by week
### csv: R0_by_week
source(file = paste0(input_dir,"/SEIR_R0.r"))


# 3-Simulation
## return: datM(Weeks, Dataz, PIw, ..., Week1OfModel, Week2OfModel)
## pdf: Infected_and_R0_by_week 
## csv: Infected_by_week_26sep23.csv
## txt: adds to SEIRmodel_run.txt
## plot (0): Pinf, PIw
if (pset$iplatform==0) {
  source(file = paste0(input_dir,"/SEIR_simulation.r")) }


# 4-Fitting - weekly infected (zd)
## pdf
## pdf
## txt
if (pset$DOfit==1) {
  if (pset$iplatform<2) {
    source(file=paste0(input_dir,"/SEIR_fit.r")) } else {
    if (dim_sc[1]>1000) {
    source(file=paste0(input_dir,"/SEIR_fit.r")) } else {
    source(file=paste0(input_dir,"/SEIR_fit_(skip-fit-iplatform=2).r")) }      }
}

  
