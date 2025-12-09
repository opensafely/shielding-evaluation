## Fitting the transmission model 

library(arrow)
library(BayesianTools)
library(coda)
library(ggplot2)
library(glue)
library(gridExtra)
library(here)
library(lubridate)
library(tidyverse)
library(magrittr)
library(Rcpp)
library(fs)
#library(bench)
#library(svglite)


# Folders
input_data  <- paste0(getwd(),"/data")  
input_code  <- paste0(getwd(),"/code")  
output_dir  <- paste0(getwd(),"/output")  
fs::dir_create(input_data)
fs::dir_create(input_code)
fs::dir_create(output_dir)


# 1-SETUP ######################################################################
cat("Setup...\n")
TIME  <- format(Sys.time(),'%H.%M.%S_%d-%m-%Y')
TODAY <- format(Sys.Date(), "%d-%m-%Y")
source(file = paste0(input_code,"/setup.r"))
cat(paste0("... Model:    ",pset$MODEL[pset$imodel]), "\n")
cat(paste0("... Platform: ",pset$PLATFORM[pset$iplatform+1]), "\n")


# 2-DATA #######################################################################
if (pset$iplatform==2) {
cat("Data...\n")
source(file = paste0(input_code,"/HDdata.R")) }


# 3-MODEL (for fitting or simulation) ##########################################
cat("Model...\n")
## Parameters and initial condition
source(file = paste0(input_code,"/","parameters.r"))

## Contact matrix & Sensitivity analyses
cat("Contacts...\n")
ic=0 #baseline
if (ic==0) filecontacts="Contact_matrix_year-from-27Jan20_vector-lenght-9x9x52_spsynv.csv"
if (ic==1) filecontacts="Contact_matrix_year-from-27Jan20_vector-lenght-9x9x52_spsynv_reduced10.csv"
if (ic==2) filecontacts="Contact_matrix_year-from-27Jan20_vector-lenght-9x9x52_spsynv_reduced20.csv"
if (ic==3) filecontacts="Contact_matrix_year-from-27Jan20_vector-lenght-9x9x52_spsynv_reduced30.csv"
if (ic==4) filecontacts="Contact_matrix_year-from-27Jan20_vector-lenght-9x9x52_spsynv_alt.csv"
if (ic==5) filecontacts="Contact_matrix_year-from-27Jan20_vector-lenght-9x9x52_spsynv_nb.csv"
source(file = paste0(input_code,"/contacts.r"))
### returns: cm, cm_0, cm_1
### insert into 'parameters' list
pars$cm    <- as.vector(cm)
pars$cm_0  <- as.vector(cm_0)
pars$cm_1  <- as.vector(cm_1)

## output settings and parameters
sink(file = paste0(output_dir,"/",pset$File_run,"_",TODAY,".txt"), append=FALSE, split=FALSE)
  cat(" \n")
  print(format(Sys.time(),'%H.%M.%S_%d-%m-%Y'))
  cat(" \n")
  print("pset: \n ")
  print(rev(pset)); cat("\n")
  print(paste0("Date range of model:   ", Week1_Model, ", ", Week2_Model)); cat("\n")
  if(pset$iplatform>0) {
    Week1_Data = lubridate::week("2020-01-01")
    Week2_Data = lubridate::week("2020-12-01")
    print(paste0("Date range in H, D data:     ", "2020-01-01", " - ", "2020-12-01")); cat("\n")
    print(paste0("Week range in H, D data:     ", Week1_Data,   " - ", Week2_Data  )); cat("\n") }
  cat(" \n")
  print("pars: \n ")
  print(rev(pars[-c(1:3)])); #exclude cm
  cat("\n \n")
sink()


# 4-Simulation #################################################################
if (pset$iplatform==0) {
  cat("Simulation...\n")
  library(svglite)
  source(file = paste0(input_code,"/simulation.r")) }


# 5-Fitting ####################################################################
if (pset$DOfit==1) {
  cat("Fit...\n")
  niterations = 400000 #number of mcmc iterations
  source(file=paste0(input_code,"/fit.r")) }

