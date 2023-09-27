## SEIR and SEIUHRD compartment models: age-structured with CoMix-contacts 
## 1-model run, 2-data generation, 3-MCMC fitting, 4-plots


#1#Model########################################################################
library(Rcpp)
library(BayesianTools)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(here)
#library(deSolve)
#library(bench)

#wd=repo
input_dir  <- here::here(paste0(getwd(),"/modelling/input")) #getwd())#  
output_dir <- here::here(paste0(getwd(),"/output/modelling")) #getwd())#  
fs::dir_create(output_dir)

#1-SET UP
TIME  = format(Sys.time(),'%H.%M.%S_%d-%m-%Y')
source(file = paste0(input_dir,"/SEIRmodel_setup.r"))

## Model choice
if (pset$iplatform==1) print(paste0("Model chosen: ",pset$MODEL[pset$imodel]))
if (pset$iplatform==1) print(paste0("Platform: ",pset$PLATFORM[pset$iplatform]))

## Contact matrix
### Length of model run
nd = pset$nd
### Summary variables
cm_weekmean  <- vector() #average contact (group-to-group) each week
cm_colsumw1  <- vector() #total contact (group-all-groups) each week
cm_weekmaxev <- vector() #max eigenvalue each week
#### Read contact matrix
if (pset$DOcmREAD==1 & pset$iplatform==1) {
   #### file names with relevant weeks
   files <- list.files(path = "./m_9x9matrices", pattern="Eng")
   cdate <- sort(substring(files,8,17))
   cdate <- cdate[(1+3):(nd+3)] #### Select 52 weeks from 1st week showing change (4th week, 2020-02-24, to 2021-02-15") 
   cl = list()
   for (i in seq_along(cdate)) {
      cl[[i]]=read.csv(paste0("./m_9x9matrices/England",cdate[i],"all.csv"),header=T) }
   #### Build matrix
   cm <- array(0,dim=c(9,9,nd))            #9 age groups, 52 weeks
   for (i in seq_along(cdate)) {
      cm[,,i] = as.matrix(cl[[i]]) }       #By week
   maxEV1 = max(eigen(cm[,,1])[[1]])       #maximum EV in week 1
   cm = cm/maxEV1                          #Normalise by max EV of CoMix in week 1 (strongest contacts)
   #### write matrix
   write.csv(c(cm), row.names = F, file = paste0(output_dir,"/",pset$File_contact_data))

} else {
  
  ####get normalised contact vector
  cmomax <- read.csv(file = paste0(input_dir,"/",pset$File_contact_data))
  cm <- array(cmomax[[1]],dim=c(9,9,nd))
  if (pset$DOcmONE==1) cm=cm*0+1/9
}

#### statistics of contacts
  for (i in 1:nd) {
      #### average contact over all cells
      cm_weekmean[i]  = sum(cm[,,i])/(dim(cm)[1]*dim(cm)[2])
      #### Max eigenvalue of cm in week 1
      cm_weekmaxev[i] = max(eigen(cm[,,i])[[1]])    }
  for (i in 1:9) { 
      #### total contacts of each age group
      cm_colsumw1[i]   = sum(cm[,i,1]) }
#### output
sink(file = paste0(output_dir,"/",pset$File_contact_summary), append=FALSE, split=FALSE)
   print("total contacts per day of each person in each age group, week 1")
   cm_colsumw1
   print("max eigenvalue of cm, weeks 1 to 52")
   cm_weekmaxev
   print("mean of all pair contacts, week 1 yo 52")
   cm_weekmean
sink()


#2-MODEL
## Read parameters and initial condition
source(file = paste0(input_dir,"/",pset$File_parameters))
pars$cm  <- as.vector(cm)
log10pinf0 <- log10((pars$Ea0[2]+pars$Ia0[2]+pars$Ua0[2])/(pars$Na0[2])) #proportion Inf age-group 2

## output settings
sink(file = paste0(output_dir,"/",pset$File_run), append=TRUE, split=FALSE)
  rev(pset); print("")
  rev(pars[-1]); print("") #exclude cm
sink()

## R0 by week
R0_week <- vector()
ngm <- cm
for (i in 1:pset$nd) { 
   for (j in 1:9){ for (k in 1:9) { 
     if (pset$imodel==1){
         ngm[j,k,i] = pars$beta * pars$u[j]*cm[j,k,i]*( 1/pars$rIR ) }
     if (pset$imodel==2){
         ngm[j,k,i] = pars$beta * pars$u[j]*cm[j,k,i]*( pars$y[k]/(pars$rIR*(1-pars$h[k]) + pars$rHR*(pars$h[k])) + pars$fu*mean(1-pars$y[k])/pars$rUR) }
    }}
    R0_week[i] =max(eigen(ngm[,,i])[[1]]) } #max eigenvalue of NGM
write.csv(R0_week, row.names = F, file = paste0(output_dir,"/",pset$File_R0_week))

## Compile & run model
sourceCpp(file = paste0(input_dir,"/",pset$File_model_choice))
if (pset$imodel==1){ t1<- system.time( out.df <- SEIR(pars) )}
if (pset$imodel==2){ t1<- system.time( out.df <- SEIUHRD(pars) )}

## Test Rcpp code:
  it_week = 1 + (0:51)*(7/pars$dt) #vector elements at start of each week
  if (pset$iplatform==1) {
    t1
    print("should be zero...")
    sum(cm_weekmean- out.df$cmdtmean[it_week]) }

## Dataset based on model output
#### time each week
tr = floor(range(out.df$time)/7)
xd = seq(tr[1],tr[2],diff(tr)/(nd-1)) #seq(0,51,1) for nd=52
id = which(is.element((out.df$time/7),xd)) 
#### True model each week
if (pset$imodel==1){ ym = out.df$It[id]}
if (pset$imodel==2){ ym = out.df$It[id]+out.df$Ut[id]}
#### True parameters
thetaTrue = c(pars$rIR, pars$R0, round(0.05*mean(ym),3)); #sd of noise
#### Simulated Data each week
yd = ym + rnorm(n=nd,mean=0,sd=thetaTrue[3])
for (i in seq_along(yd)) {yd[i] = max(0,yd[i])} #truncate to zero at low end

#### Plot model and R0
dat <- tibble(Weeks=xd, PInf=ym/pars$Npop, Data=yd/pars$Npop, 
              PSus=out.df$St[id]/pars$Npop, PRec=out.df$Rt[id]/pars$Npop, R0=R0_week) 
xx = quantile(xd,0.6)[[1]]; yy = max(dat$PInf)*0.9; y2 = max(dat$PInf)*0.6;
p1  <- ggplot(dat, aes(x = Weeks)) +
    geom_line(aes(y = PInf), col = 'red') +
    geom_line(aes(y = PRec/100), col = 'green') +
    geom_line(aes(y = PSus/100), col = 'blue') +
    geom_point(aes(y = Data), size = 1.2, pch = 1) +
    annotate("text", size=3, x=xx, y=yy, label= paste0("beta=",round(pars$beta,3),", R0unif=",round(pars$R0,2),", R0=",round(max(R0_week),2))) +
    annotate("text", size=3, x=xx, y=y2, label= paste0("pinf2(log10)=",round(log10pinf0,2),", IP=",round(1/pars$rIR,2))) +
    labs(x = 'Weeks', y = 'PInfected') + #scale_y_continuous(trans = "log10") +
    if (pset$imodel==2){geom_line(aes(y =out.df$Ht[id]/Npop),col="blue")}
p2 <-  ggplot(dat, aes(x=Weeks)) +
    geom_point(aes(y=R0)) +
    geom_line(aes(y=rep(1,nd)),col='red') +
    labs(x = 'Weeks', y = 'R0') 

if (pset$iplatform==1) {
   gridExtra::grid.arrange(p1, p2, nrow = 2) }
pdf(file = paste0(output_dir,"/",pset$File_model_plots))
   gridExtra::grid.arrange(p1, p2, nrow = 2)
dev.off()

#### Output model simulation
if (pset$DOfit==1) {
  datout <- data.frame(xd, yd, names=c("Weeks","DataInfected"))
  write.csv(datout, row.names = T, file = paste0(output_dir,"/",pset$File_model_simulation))
}



#2#Fitting######################################################################
if (pset$DOfit==1) {

### Fitting Infected
### Given the times xd


######## 1 BayesianTools #####################
set.seed(7777)     ### reproducibility of MCMC sample
niter  = 20000#0; ### number of chain iterations
burnin = niter*0.2
### Multiple model options
if (pset$imodel==1) model <- SEIR
if (pset$imodel==2) model <- SEIUHRD

### Likelihood in R and model in Rcpp
LogLikelihood = function(theta){
  #Proposed
  rIR = theta[1] #thetaTrue[1]
  R0  = theta[2] #thetaTrue[2]
  sd  = theta[3] #thetaTrue[3] 
  #Estimated
  pars$rIR = rIR
  pars$R0  = R0
  #Dependent on estimated and fixed
  pars$beta= R0*(rIR+pars$d)/pars$u_mean        #transmission rate
  #Model in Rcpp with proposed parameters
  out.df <- model(pars)
  loglikelihood_times = dnorm(yd, mean = out.df$It[id], sd = sd, log = T)
  return(sum(loglikelihood_times)) 
}

## Likelihood definition, parameter ranges
setup = createBayesianSetup(likelihood=LogLikelihood,lower = c(0,0,0), upper =c(1,30,round(0.4*mean(ym),3)))
settings = list (iterations = niter, burnin = burnin, message=F)
## Bayesian sample
tout1 <- system.time(out <- runMCMC(bayesianSetup=setup, settings=settings) )
## summary
if (pset$iplatform==1) { 
  tout1
  round(thetaTrue,3)
  print(paste0("Mean by chain and parameter:"))
  out$X }

## Plots
pdf(file = paste0(output_dir,"/",pset$File_fit_output))
    par(mfrow = c(1,2))
    marginalPlot(out)
    par(mar = c(1, 1, 1, 1))
    plot(out)
    correlationPlot(out)
dev.off()

## Summary - output
sink(file = paste0(output_dir,"/",pset$File_fit_summary),append=TRUE,split=FALSE)
  print(summary(out)); print(""); print("")
  print(paste0("True: ", c("rIR = ", "R0 = ", "sd = "), round(thetaTrue,3)))
  print(paste0("Mean by chain and parameter:"))
  out$X
  print(paste0("Time used:"))
  tout1
  names(out[[1]])
  names(out[[2]])
sink()

######## 2 Basic Metropolis Hastings MCMC ####
} #DOfit

