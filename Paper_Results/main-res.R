## Analyse output of fitted transmission model: sample the posterior + make summaries & figures

library(arrow)
library(BayesianTools)
library(coda)
library(ggplot2)
library(glue)
library(gridExtra)
library(here)
library(Hmisc)
library(lubridate)
library(magrittr)
library(tidyverse)
library(Rcpp)
library(fs)
#library(bench)
#library(svglite)


# Folders
input_data  <- paste0(getwd(),"/data") #getwd() #
input_code  <- paste0(getwd(),"/code") #getwd() #
output_dir  <- paste0(getwd(),"/output") #getwd() #
fs::dir_create(input_data)
fs::dir_create(input_code)
fs::dir_create(output_dir)


# 1-SETUP #####################################################################
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


# 3-MODEL ######################################################################
cat("Model...\n")
## Parameters and initial condition
source(file = paste0(input_code,"/","parameters.r"))
## week indices
imodelH          = 1:45 #(idataH - Week_shift_model) #4:48-(4-1)
Week1_Model      = lubridate::week("2020-01-27")     #4
Week_shift_model = (Week1_Model-1)

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
sink(file = paste0(output_dir,"/",pset$File_run_res,"_",TODAY,".txt"), append=FALSE, split=FALSE)
  cat(" \n")
  print(format(Sys.time(),'%H.%M.%S_%d-%m-%Y'))
  cat(" \n")
  print(paste0("Date range of model,  ", Week1_Model, ", ", Week2_Model)); cat("\n")
  print("pars: \n ")
  print(rev(pars[-1])); #exclude cm
  cat("\n \n")
sink()

## R0 by week
cat("R0...\n")
source(file = paste0(input_code,"/R0_0.r"))
beta_0    = pars$beta
r0        = R0_0(pars,GetBeta=1,GetOutput=0) #R0_week not outputted 
pars$beta = r0$pars$beta
cat(paste0("... beta (based on input parameters): ", round(pars$beta,3)), "\n")


# 4-Analysis ###################################################################
cat("Analysis of model fit...\n")
source(file = paste0(input_code,"/results.r"))
