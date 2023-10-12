## SEIR and SEIUHRD compartment models: age-structured with CoMix-contacts 
## 1-model simulation, 2-OS data, 3-MCMC fitting, 4-plots


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


#0-SET UP
TIME  = format(Sys.time(),'%H.%M.%S_%d-%m-%Y')
source(file = paste0(input_dir,"/SEIRmodel_setup.r"))
## Model choice
if (pset$iplatform==0) { print(paste0("Model chosen: ",pset$MODEL[pset$imodel]))
                         print(paste0("Platform: ",pset$PLATFORM[pset$iplatform])) }


# 1-OS DATA
## return: datD(Weeks, Dataz, Dates, Week1OfData, Week2OfData)
## 1 pdf: Infected_data_and_R0_by_week 
## 2 pdf: output/figures/covid_hosp_over_time2again
if (pset$iplatform>0) {
  source(file = paste0(input_dir,"/SEIR_data.r"))
}
## plot (1): Weeks:Dataz


#1-MODEL (for fitting or simulation)
## Contact matrix
### return: cm, Week1OfModel
### csv: Contact_matrix_year-from-24Feb20_vector-lenght-9x9x52_norm=maxEV1
### 3 txt: Contact_matrix_year-from-24Feb20_vector-lenght-9x9x52_norm=maxEV1_stats
source(file = paste0(input_dir,"/SEIR_contacts.r"))

## Parameters and initial condition
### return pars
source(file = paste0(input_dir,"/",pset$File_parameters))
pars$cm  <- as.vector(cm)

## output settings and parameters
### 4 txt:
sink(file = paste0(output_dir,"/",pset$File_run), append=TRUE, split=FALSE)
  print(format(Sys.time(),'%H.%M.%S_%d-%m-%Y'))
  rev(pset); cat("\n")
  print(paste0("Date in contact-data, range  ", Week1OfModel, ", ", Week2OfModel)); cat("\n")
  print(paste0("Date by week in data, range  ", Week1OfData,  ", ", Week2OfData )); cat("\n")
  rev(pars[-1]); #exclude cm
  cat("\n \n")
sink()

## R0 by week
### 5 csv: R0_by_week
source(file = paste0(input_dir,"/SEIR_R0.r"))


# 3-Simulation
## return: datM(Weeks, Dataz, PIw, ..., Week1OfModel, Week2OfModel)
## 1 pdf: Infected_and_R0_by_week 
## 2 csv: Infected_by_week_26sep23.csv
## txt: adds to SEIRmodel_run.txt
if (pset$iplatform==0) {
  source(file = paste0(input_dir,"/SEIR_simulation.r"))
}



# 4-Fitting
if (pset$DOfit==1) {
### Fitting: Infected (yd) or weekly infected (zd)

#Range of dates (weeks) for fitting
if (pset$iplatform==0) {
   iweeksmodel = seq_along(datM$Weeks)
   iweeksdata  = iweeksmodel} else {
   if (pset$iplatform==1) Week2ofStudy = Week2OfModel #Dummy data - max model range
   if (pset$iplatform==2) Week2ofStudy = "2020-12-01" #Real data: start of vacc & alpha
   Week1OfFit = max(c(Week1OfData, Week1OfModel))
   Week2OfFit = min(c(Week2OfData, Week2OfModel, Week2ofStudy))
   #Dates within the range
   iweeksdata = which(Week1OfFit <= datD$Dates & datD$Dates <= Week2OfFit)
   Dates = datD$Dates[iweeksdata]
   #convert data-weeks to model-weeks (indices 1:52) counted from start week of model
   iweeksmodel = ceiling(as.numeric(difftime(Dates, Week1OfModel, units = "weeks")))  
}
#Data within the range
zd = datD$Dataz[iweeksdata]


######## 1 BayesianTools #####################
### reproducibility of MCMC sample
set.seed(7777)
### number of estimated parameters
if(pset$iplatform==0) {npar = length(thetaTrue)} else {npar = 6}
### Multiple model options
if (pset$iplatform>0) {
   ## Compile & run model (weekly points)
   sourceCpp(file = paste0(input_dir,"/",pset$File_model_choice))}
if (pset$imodel==1) model <- SEIR
if (pset$imodel==2) model <- SEIUHRD

### Likelihood in R and model in Rcpp
if (pset$iplatform==0) {sdUpper=round(0.4*mean(ym),2)} else {    
                        sdUpper=round(0.4*mean(zd),2)}; 

LogLikelihood = function(theta){
  #Proposed
  pars$rEI = theta[1] #thetaTrue[1]
  pars$rIR = theta[2] #thetaTrue[2]
  pars$R0  = theta[3] #thetaTrue[3]
  pars$pE0 = theta[4] #thetaTrue[4]
  pars$pdm = theta[5] #thetaTrue[5]
  sd       = theta[6] #thetaTrue[6] 
  #Dependent
  pars$Ea0 = pars$Na0*pars$pE0
  pars$Sa0 = pars$Na0-pars$Ra0-pars$Ea0-pars$Ia0
  pars$beta= pars$R0*(pars$rIR+pars$d)/pars$u_mean
  #Model in Rcpp with proposed parameters
  m <- model(pars)
  #likelihood of (weekly) data
  if (pset$iplatform==0) {Mean=m$Iw} else {Mean=m$Iw[iweeksmodel]}
  return(sum(dnorm(zd, mean = pars$pdm*Mean, sd = sd, log = T))) 
  #return(sum(dnorm(yd, mean = modelout$It, sd = sd, log = T))) 
}

## Likelihood definition, parameter ranges
niter    = 200000 #50000 #40000
setup    = createBayesianSetup(likelihood=LogLikelihood,
           lower = c(0,0,0,0,0,0), upper =c(1,1,30,1,1,sdUpper)) #rEI, rIR, R0, pE0, pdm, sd
settings = list (iterations = niter, burnin = round(niter*npar/15), message=F)
## Bayesian sample
tout1 <- system.time(out <- runMCMC(bayesianSetup=setup, settings=settings) )
## summary
if (pset$iplatform==0) { 
  tout1
  if(pset$platform==0) round(thetaTrue,3)
  print(paste0("Mean by chain and parameter:"))
  out$X }
## Estimates
parsE     <- pars
MAPE      <- MAP(out)
parsE$rEI <- MAPE$parametersMAP[1] #thetaTrue[1]
parsE$rIR <- MAPE$parametersMAP[2] #thetaTrue[2]
parsE$R0  <- MAPE$parametersMAP[3] #thetaTrue[3]
parsE$pE0 <- MAPE$parametersMAP[4] #thetaTrue[4]
parsE$pdm <- MAPE$parametersMAP[5] #thetaTrue[5]
mE        <- model(parsE)
mT        <- model(pars)

if(pset$iplatform==0) {
dat <- tibble(Weeks= mT$time/7, 
              PIt  = parsE$pdm*mT$It/pars$Npop, DataIt=yd/pars$Npop, PIte = mE$It/pars$Npop,
              PIw  = parsE$pdm*mT$Iw/pars$Npop, DataIw=zd/pars$Npop, PIwe = mE$Iw/pars$Npop) } else {
                
dat <- tibble(Weeks= mE$time[iweeksmodel+1]/7, DataIw=zd/pars$Npop, PIwe = mE$Iw[iweeksmodel]/pars$Npop) }

## Plots
pdf(file = paste0(output_dir,"/",pset$File_fit_output))
    par(mfrow = c(1,2))
    marginalPlot(out)
    par(mar = c(1, 1, 1, 1))
    plot(out)
    correlationPlot(out)

    if (pset$iplatform==0) {
    ggplot(dat, aes(x= Weeks)) +
      geom_point(aes(y=DataIw), col=1) +
      geom_line(aes(y=PIw),  col=3) +
      geom_line(aes(y=PIwe), col=2) +
      labs(x = 'Weeks', y = 'Infections per week') 
    ggplot(dat, aes(x= Weeks)) +
      geom_point(aes(y=DataIt), col=1) +
      geom_line(aes(y=PIt),  col=3) +
      geom_line(aes(y=PIte), col=2) +
      labs(x = 'Weeks', y = 'Infected')} else {
        
    ggplot(dat, aes(x= Weeks)) +
          geom_point(aes(y=DataIw), col=1) +
          geom_line (aes(y=PIwe), col=2) +
          labs(x = 'Weeks', y = 'hospitalisations per week') }
dev.off()

## Summary - output
sink(file = paste0(output_dir,"/",pset$File_fit_summary),append=TRUE,split=FALSE)
  print(summary(out)); print(""); print("")
  if(pset$iplatform==0) print(paste0("True: ", c("rEI = ","rIR = ", "R0 = ", "pE0 = ", "pdm = ", "sd = "), round(thetaTrue,3)))
  print(paste0("Mean by chain and parameter:"))
  out$X
  print(paste0("Time used:"))
  tout1
  names(out[[1]])
  names(out[[2]])
sink()

} #DOfit


######## 2 Basic Metropolis Hastings MCMC ####

