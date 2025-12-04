# Results of fitted model

#library(BayesianTools)
#library(Rcpp)
#library(ggplot2)
#library(gridExtra)
#library(tidyverse)
#library(lubridate)
#library(bench)
#library(coda)
#library("posterior")

# Imputs: 
# 1) MCMC parameter posterior samples
# 2) incidence data (aggregated EHR, rm6)
# 3) ONS CSI data
# Outputs
# 1) posterior samples of parameters and model variables
# 2) summaries
# 3) figures



### DATA #######################################################################

######## Week range & dataset subsets for fitting ##############################
  ## Each age group has:
  ## - weeks 1-48 in 2020
  ## - freq=0 (inputted) when events unreported
  ## Date range for fitting
  ## Convert dates to WEEKS 
  Week1_Model  = lubridate::week("2020-01-27")    #  4 #start of "contact" data
  Week2_Model  = Week1_Model + (pars$nw-1)        # 55 #model set to run 52=pars$nw weeks 
  Week2_Study  = lubridate::week("2020-12-01")    # 48 #start of vacc & alpha
  Week1_Fit_H  = max( c(min( datHas_l$Week), Week1_Model), na.rm=T)              #  4 #max(c(min(1:48),4))
  Week1_Fit_DH = max( c(min(datDHas_l$Week), Week1_Model), na.rm=T)              #  4 #max(c(min(1:48),4))
  Week1_Fit_DO = max( c(min(datDOas_l$Week), Week1_Model), na.rm=T)              #  4 #max(c(min(1:48),4))
  Week2_Fit_H  = min( c(max( datHas_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(max(1:48),55,48))
  Week2_Fit_DH = min( c(max(datDHas_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(max(1:48),55,48))
  Week2_Fit_DO = min( c(max(datDOas_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(max(1:48),55,48))
  
  ## Indices
  ## - long data
  ## - every week
  idataH    = which(!is.na(datH_l$Week)   & Week1_Fit_H  <=   datH_l$Week &   datH_l$Week <= Week2_Fit_H) #dates relative to "2020-01-01
  idataDH   = which(!is.na(datDH_l$Week)  & Week1_Fit_DH <=  datDH_l$Week &  datDH_l$Week <= Week2_Fit_DH)
  idataDO   = which(!is.na(datDO_l$Week)  & Week1_Fit_DO <=  datDO_l$Week &  datDO_l$Week <= Week2_Fit_DO)
  idataH_0  = which(!is.na(datHs_l$Week)  & Week1_Fit_H  <=  datHs_l$Week &  datHs_l$Week <= Week2_Fit_H  & datHs_l$Shield==0) #dates relative to "2020-01-01
  idataDH_0 = which(!is.na(datDHs_l$Week) & Week1_Fit_DH <= datDHs_l$Week & datDHs_l$Week <= Week2_Fit_DH & datDHs_l$Shield==0)
  idataDO_0 = which(!is.na(datDOs_l$Week) & Week1_Fit_DO <= datDOs_l$Week & datDOs_l$Week <= Week2_Fit_DO & datDOs_l$Shield==0)
  idataH_1  = which(!is.na(datHs_l$Week)  & Week1_Fit_H  <=  datHs_l$Week &  datHs_l$Week <= Week2_Fit_H  & datHs_l$Shield==1) #dates relative to "2020-01-01
  idataDH_1 = which(!is.na(datDHs_l$Week) & Week1_Fit_DH <= datDHs_l$Week & datDHs_l$Week <= Week2_Fit_DH & datDHs_l$Shield==1)
  idataDO_1 = which(!is.na(datDOs_l$Week) & Week1_Fit_DO <= datDOs_l$Week & datDOs_l$Week <= Week2_Fit_DO & datDOs_l$Shield==1)
  
  ## Temporal-Incidence data - used for plots at the end
  zd    =   datH_l$Freq[idataH]
  wd    =  datDH_l$Freq[idataDH] 
  vd    =  datDO_l$Freq[idataDO]
  zd_0  =  datHs_l$Freq[idataH_0]
  wd_0  = datDHs_l$Freq[idataDH_0] 
  vd_0  = datDOs_l$Freq[idataDO_0]
  zd_1  =  datHs_l$Freq[idataH_1]
  wd_1  = datDHs_l$Freq[idataDH_1] 
  vd_1  = datDOs_l$Freq[idataDO_1]
  
  ## Incidence data by age and shielding, weekly hospital admissions and deaths
  for (i in 1:9){values = which(  !is.na(datHas_l$Week) &  datHas_l$Ageg==i   &  datHas_l$Shield==0 #idata= 8:48
                                  & Week1_Fit_H  <=   datHas_l$Week  &  datHas_l$Week <= Week2_Fit_H) #dates relative to "2020-01-01
  assign(paste0("zd",eval(i),"_0"),  datHas_l$Freq[values])  }           #zd1_0 ... zd9_0
  
  for (i in 1:9){values = which( !is.na(datDHas_l$Week) & datDHas_l$Ageg==i   &  datDHas_l$Shield==0  
                                 & Week1_Fit_DH <=  datDHas_l$Week  & datDHas_l$Week <= Week2_Fit_DH) # 
  assign(paste0("wd",eval(i),"_0"), datDHas_l$Freq[values])  }           #wd1_0 ... wd9_0 
  
  for (i in 1:9){values = which( !is.na(datDOas_l$Week) & datDOas_l$Ageg==i   &  datDOas_l$Shield==0               
                                 & Week1_Fit_DO <=  datDOas_l$Week  & datDOas_l$Week <= Week2_Fit_DO)  
  assign(paste0("vd",eval(i),"_0"), datDOas_l$Freq[values])  }           #vd1_0 ... vd9_0
  
  for (i in 1:9){values = which(  !is.na(datHas_l$Week) &  datHas_l$Ageg==i   &  datHas_l$Shield==1  
                                  & Week1_Fit_H  <=   datHas_l$Week  &  datHas_l$Week <= Week2_Fit_H)  
  assign(paste0("zd",eval(i),"_1"),  datHas_l$Freq[values])  }           #zd1_1 ... zd9_1
  
  for (i in 1:9){values = which( !is.na(datDHas_l$Week) & datDHas_l$Ageg==i   &  datDHas_l$Shield==1  
                                 & Week1_Fit_DH <=  datDHas_l$Week  & datDHas_l$Week <= Week2_Fit_DH)  
  assign(paste0("wd",eval(i),"_1"), datDHas_l$Freq[values])  }           #wd1_1 ... wd9_1 
  
  for (i in 1:9){values = which( !is.na(datDOas_l$Week) & datDOas_l$Ageg==i   &  datDOas_l$Shield==1              
                                 & Week1_Fit_DO <=  datDOas_l$Week  & datDOas_l$Week <= Week2_Fit_DO)  
  assign(paste0("vd",eval(i),"_1"), datDOas_l$Freq[values])  }           #vd1_1 ... vd9_1
   
  ## Convert data week-range (index 4:48, since Week1_Model=2020-01-27 to Week2_Study=2020-12-01) 
  ## to model-weeks          (index 1:52, since Week1_Model=2020-01-27)
  ## => use model in likelihood
  Week_shift_model = (Week1_Model-1)
  imodelH  = idataH  - Week_shift_model #1:45 = 4:48-(4-1)
  imodelDH = idataDH - Week_shift_model #1:45 = 4:48-(4-1)
  imodelDO = idataDO - Week_shift_model #1:45 = 4:48-(4-1)

  Week1_Avert  = lubridate::week("2020-03-24") # 12 #start of policy - 8w later than 27-jan-2020
  iaverted     = imodelH[-c(1:(Week1_Avert-Week1_Model))] #9:45 (weeks 12:48)
  
  cat(paste0("#H  data pts fitted, #: ", length(idataH),   ", weeks 2020: ", range(idataH)[1],  "...",range(idataH)[2]),   "\n")  #45, 4...48
  cat(paste0("#DH data pts fitted, #: ", length(idataDH),  ", weeks 2020: ", range(idataDH)[1], "...",range(idataDH)[2]),  "\n")  #45, 4...48
  cat(paste0("#DO data pts fitted, #: ", length(idataDO),  ", weeks 2020: ", range(idataDO)[1], "...",range(idataDO)[2]),  "\n")  #45, 4...48
  cat(paste0("#H  model pts used,  #: ", length(imodelH),  ", weeks 2020: ", range(imodelH)[1], "...",range(imodelH)[2]),  "\n")  #45, 1...45
  cat(paste0("#DH model pts used,  #: ", length(imodelDH), ", weeks 2020: ", range(imodelDH)[1],"...",range(imodelDH)[2]), "\n")  #45, 1...45
  cat(paste0("#DO model pts used,  #: ", length(imodelDO), ", weeks 2020: ", range(imodelDO)[1],"...",range(imodelDO)[2]), "\n")  #45, 1...45

#### WEIGHTS to scale data (each age group) to modelled demography #############
  # England, ONS
  ageons = pars$ageons         #1:9
  # Cohort, TPP
  agecoh = pars$agecoh         #1:
  #weights
  weight = ageons*pars$Npop/(agecoh*pars$Npopcoh)


######## Prevalence data #######################################################
cis<-read.csv(paste0(input_data,"/CIS_ONS_England_03May-12Dec-2020_Positivity.csv"))
posi_av_perc = 100*cis$Pos.average.prop #100 x proportion
##Upper and Lower CI - bits above and below data
posi_upp_perc = 100*(cis$Pos.U95CI.prop-cis$Pos.average.prop)
posi_dow_perc = 100*(cis$Pos.average.prop-cis$Pos.L95CI.prop)
##Absolute weeks CIS Positivity: 18-50
##Absolute weeks Model:           4-48 = 4-17 + 18-48
##=> fit index imodel = 15-45
imodel_posi=15:45
idata_posi = 1:31 #range(cis$Date[1:31]) [1] "2020-05-03" "2020-11-29" #
shift_posi = unique(imodel_posi-idata_posi)
##data to be plotted (percentage)
posi_data_perc = posi_av_perc [idata_posi]
posi_datu_perc = posi_upp_perc[idata_posi]
posi_datd_perc = posi_dow_perc[idata_posi]



### Fitting ##############################################################################
### Parameter transformation functions
fI <- function(theta){  return(par = theta)          } #identity
fi <- function(theta){  return(par = 1/theta)        } #inverse
fp <- function(theta){  return(par = 1/(theta*theta))} #pareto
fs <- function(theta){  return(par = theta*theta)    } #square
fe <- function(theta){  return(par = exp(theta))     } #exponential
fl <- function(theta){  return(par = log(theta))     } #log
#
gI <- function(par)  {  return(theta = par)          }
gi <- function(par)  {  return(theta = 1/par)        }
gp <- function(par)  {  return(theta = 1/sqrt(par))  }
gs <- function(par)  {  return(theta = sqrt(par))    }
ge <- function(par)  {  return(theta = log(par))     }
gl <- function(par)  {  return(theta = exp(par))     }

#rIR
f1  <- function(theta) { fi(theta)}; 
g1  <- function(par)   { gi(par)}
#rUR #rIH
f2  <- function(theta) { fi(theta)};
g2  <- function(par)   { gi(par)}
#rOD
f3  <- function(theta) { fi(theta)};
g3  <- function(par)   { gi(par)}
#pI0 #pE0
f4  <- function(theta) { fe(theta)};
g4  <- function(par)   { ge(par)}   
#hM_0
f5 <- function(theta) { fI(theta)};
g5 <- function(par)   { gI(par)};
#hM_1
f6 <- function(theta) { fI(theta)};
g6 <- function(par)   { gI(par)};
#hR_0
f7 <- function(theta) { fI(theta)};
g7 <- function(par)   { gI(par)};
#hR_1
f8 <- function(theta) { fI(theta)};
g8 <- function(par)   { gI(par)};
#dM_0  
f9 <- function(theta) { fI(theta)};
g9 <- function(par)   { gI(par)};
#dM_1
f10 <- function(theta) { fI(theta)};
g10 <- function(par)   { gI(par)};
#dR_0
f11 <- function(theta) { fI(theta)};
g11 <- function(par)   { gI(par)}
#dR_1
f12 <- function(theta) { fI(theta)};
g12 <- function(par)   { gI(par)}
#yM1_0
f13 <- function(theta) { fI(theta)}; 
g13 <- function(par)   { gI(par)}
#yM1_1
f14 <- function(theta) { fI(theta)};
g14 <- function(par)   { gI(par)}
#yR1_0
f15 <- function(theta) { fI(theta)};
g15 <- function(par)   { gI(par)}
#yR1_1
f16 <- function(theta) { fI(theta)};
g16 <- function(par)   { gI(par)}
#kH #pk = theta = 1/sqrt(k) => k = 1/pk^2
f17 <- function(theta) { fp(theta)}; 
g17 <- function(par)   { gp(par)}
#kDH
f18 <- function(theta) { fp(theta)}; 
g18 <- function(par)   { gp(par)}
#R0
f19 <- function(theta) { fI(theta)};
g19 <- function(par)   { gI(par)}
#tEI
f20 <- function(theta) { fi(theta)}; 
g20 <- function(par)   { gi(par)}
#fu
f21 <- function(theta) { fI(theta)};
g21 <- function(par)   { gI(par)}
#pars$rID
f22 <- function(theta){ fi(theta)}; 
g22 <- function(par)  { gi(par)}

### Parameter bounds
age  = c(mean(0:4),mean(5:11),mean(12:17),mean(18:29),mean(30:39),mean(40:49),mean(50:59),mean(60:69),mean(70:90))
age9 = age[9]

### MCMC #################################################################################
## MCMC Traces input
cat("Reading traces...", "\n")
#library(coda)
out <- mcmc.list()
for (i in 1:3) {
   c <- coda::as.mcmc(read.csv(paste0(input_data,"/","Fit_MCMC_mcmcChain",eval(i),".csv"),header=T))
   assign(paste0("c", eval(i)), coda::as.mcmc(as.matrix( c[,2:(dim(c)[2])])) ) #remove “x” col #NB: as.matrix() necessary
} 
out$chain <- as.mcmc.list(list(as.mcmc(c1),as.mcmc(c2),as.mcmc(c3)) )
out$setup$numPars = 21
npar = 21

## MCMC input without LP, LL, LPr
out21<-mcmc.list()
for (i in 1:3) {
  d <- coda::as.mcmc(read.csv(paste0(input_data,"/","Fit_MCMC_mcmcChain",eval(i),".csv"),header=T))
  assign(paste0("d", eval(i)), coda::as.mcmc(as.matrix( d[,2:(dim(d)[2]-3)])) ) #remove “x” col #NB: as.matrix() necessary
} 
out21$chain <- as.mcmc.list(list(as.mcmc(d1),as.mcmc(d2),as.mcmc(d3)) )
#class(out$chain[[1]])  #[1] "mcmc"
#class(out$chain[[2]])  #[1] "mcmc"
#class(out$chain[[3]])  #[1] "mcmc"
#class(out)             #[1] "list"
### MCMC #################################################################################



### Full sample of parameters ############################################################
### (not using sample() defaults)
nPars = npar
Chains= 3
lout  = dim(out$chain[[1]])[1]*Chains
samples = getSample(out$chain,start=1,end=lout,thin=1) #dim = (3*xxxx, nPars+3) - each col merges chain rows
### Full sample of parameters ############################################################



### ESS Estimate #########################################################################
#Geyer approach
library("posterior")
essg = vector(); 
for (i in (1:21)) {essg[i] = posterior::ess_basic(samples[,i])}
ESS_mean_0 = round(mean(essg),1)	 
#Accounting for convergence, by inputting the 3 chains separately, not as a single vector
essgc = vector(); 
for (i in (1:21)) {sp = array(samples[,i],dim=c(9524,3)); essgc[i] = posterior::ess_basic(sp)}
ESS_mean_c=round(mean(essgc))
### ESS Estimate #########################################################################


### Parameter names ######################################################################
Pname=c("rIR  ",  "rUR  ",  "rOD  ", "pI0  ",
        "hA_0 ",  "hA_1 ",  "hr_0 ", "hr_1 ",  
        "dA_0 ",  "dA_1 ",  "dr_0 ", "dr_1 ",
		"yA_0",   "yA_1",   "yr_0 ", "yr_1 ",
		"1/kH^2 ",    "1/kDH^2 ",   "R0 ",   "rEI ", "f ")
Pnamei=c("tIR", "tUR",  "tOD",  "log(pI0)", Pname[5:(npar-2)], "tEI", Pname[npar], "tID")
varnames(out$chain)[1:npar]   = Pnamei[1:npar]
varnames(out21$chain)[1:npar] = Pnamei[1:npar]
Pnamei[4] = "I0"
### parameters - add rID - to be derived from other parameter's estimates
pars <- within(pars, rID <- 1/(1/rIH + 1/rOD))
### Parameter names ###################################################################### 
		


### Tools for estimation #################################################################
### BETA - to derive beta from the sampled parameters
source(file = paste0(input_code,"/BETA_0.r")) #BETA <- function(pars)
### Model compilation - to calculate quantiles fot model variables
Rcpp::sourceCpp(file = paste0(input_code,"/","model.cpp"))
model <- SEIUHRD
###
oN = 1/pars$Npop
### Tools for estimation #################################################################



### MAP Estimates ########################################################################
cat("MAP...", "\n")
parsE <- pars
ibest <- which.max(samples[, nPars + 1]) #LogPosterior col
MAPE  <- list(parametersMAP = samples[ibest, 1:nPars], valuesMAP = samples[ibest, (nPars + 1):(nPars + 3)])
## tID
MAPE[[1]][npar+1] = as.numeric(MAPE[[1]][1]) + as.numeric(MAPE[[1]][3]) #tIR + tOD
setNames(MAPE[[1]][npar+1],"tID")
##
parsE$rIR <- f1(as.vector(MAPE$parametersMAP[1]))
parsE$rUR <- f2(as.vector(MAPE$parametersMAP[2]))
parsE$rOD <- f3(as.vector(MAPE$parametersMAP[3]))
parsE$pI0 <- f4(as.vector(MAPE$parametersMAP[4]))
parsE$rID <- as.vector(MAPE$parametersMAP[npar+1]) #tID
hME_0     <- f5(as.vector(MAPE$parametersMAP[5]))
hME_1     <- f6(as.vector(MAPE$parametersMAP[6]))
hRE_0     <- f7(as.vector(MAPE$parametersMAP[7]))
hRE_1     <- f8(as.vector(MAPE$parametersMAP[8]))
dME_0     <- f9(as.vector(MAPE$parametersMAP[9]))
dME_1     <- f10(as.vector(MAPE$parametersMAP[10]))
dRE_0     <- f11(as.vector(MAPE$parametersMAP[11]))
dRE_1     <- f12(as.vector(MAPE$parametersMAP[12]))
yM1E_0    <- f13(as.vector(MAPE$parametersMAP[13]))
yM1E_1    <- f14(as.vector(MAPE$parametersMAP[14]))
yR1E_0    <- f15(as.vector(MAPE$parametersMAP[15]))
yR1E_1    <- f16(as.vector(MAPE$parametersMAP[16]))
parsE$kH  <- f17(as.vector(MAPE$parametersMAP[17]))
parsE$kDH <- f18(as.vector(MAPE$parametersMAP[18]))
parsE$R0  <- f19(as.vector(MAPE$parametersMAP[19]))
parsE$rEI <- f20(as.vector(MAPE$parametersMAP[20]))
parsE$fu  <- f21(as.vector(MAPE$parametersMAP[21]))
#Dependent parameters
parsE$kDO = parsE$kDH
parsE$rIH = parsE$rIR
parsE$rIO = parsE$rIR
parsE$h_0 = hME_0*exp((age-age9)*hRE_0)
parsE$h_1 = hME_1*exp((age-age9)*hRE_1)
parsE$d_0 = dME_0*exp((age-age9)*dRE_0)
parsE$d_1 = dME_1*exp((age-age9)*dRE_1)
parsE$y_0[3:9] = yM1E_0*exp((age[3:9]-age9)*yR1E_0)
parsE$y_1[3:9] = yM1E_1*exp((age[3:9]-age9)*yR1E_1)
parsE$yh_0 = parsE$y_0*parsE$h_0
parsE$yh_1 = parsE$y_1*parsE$h_1
parsE$Ia0   = (parsE$agehosp*parsE$Npop)*parsE$pI0
parsE$Sa0   = parsE$Na0 - parsE$Ea0 - parsE$Ia0 - parsE$Ua0 - parsE$Ha0 - parsE$Oa0 - parsE$Ra0 - parsE$Da0   
parsE$beta  = BETA(parsE)
#Predictions
mE        <- model(parsE)
### MAP Estimates ########################################################################



## Credible Intervals for parameters #####################################################
## Quantiles
q1=0.025
q2=0.975
Q=c(q1,0.5,q2)
Pq <- t(getCredibleIntervals(samples[,1:npar],Q)) #Pq[ip,1:3] <= getCredibleIntervals[1:3,ip]
#tID
Pq <- rbind(Pq, as.numeric(quantile(samples[,1]+samples[,3],Q,na.rm=T))) #tIR + tOD (tIH=tIR)
## Credible Intervals for parameters #####################################################



## Parameter formatting ##################################################################
## Quantile transformations
#    - 1:3, npar-1 - time - keep as is
#    - 4:npar-2, npar - transform, the Identity ones have no effect
for (ip in c(4:(npar-2),npar)) {
  Pq[ip,] = eval(parse(text = paste0("f",eval(ip),"(Pq[",eval(ip),",])")))
 }
Pq[4,]  = Pq[4,]*pars$Npop #pI0
Pr      = round(Pq,3); 
Pr[4,]  = round(Pr[4,],0) #pI0
## MAP
PMAP=rep(0,npar+1)
for (ip in 1:(npar+1)) {
  PMAP[ip] = eval(parse(text = paste0("f",eval(ip),"(MAPE[[1]][[",eval(ip),"]])")))
  ## untransform 1/rate as MAPE are already transformed (in previous line)
  if(ip<=3 || ip==(npar-1)|| ip==(npar+1))  PMAP[ip] = 1/PMAP[ip]
}
PMAP[4]  = PMAP[4]*pars$Npop #I0
PMAPr    = round(PMAP,3)
PMAPr[4] = round(PMAP[4],0) #I0
## Parameter formatting ##################################################################



### SAMPLING #############################################################################
tout0 <- Sys.time()
cat("Sampling...", "\n")

### reproducibility of sampling
set.seed(7777)

### Full sample of parameters without logposterior, etc ##################################
#nPars  = npar
Thin    = 1
Chains  = 3
nsample = lout/Thin #lout = dim(out$chain[[1]])[1]*Chains
psample = getSample(samples[,1:nPars], parametersOnly = T, start=1, end= lout, thin=Thin) # fit output was already thinned 1/7

### Aggregated groups
nh=5; nd=3; agg1=nh/10;
groupsH=(nh + (pars$na-nh)*pars$ktot/45)*agg1
groupsD=(nd + (pars$na-nd)*pars$ktot/45)*agg1
kall=c(groupsH*pars$parsKH, groupsD*pars$parsKDH, groupsD*pars$parsKDO)
###
nh=3; nd=1; agg2=nh/2;
groupsH=(nh + ((pars$na-2)-nh)*pars$ktot/45)*agg2
groupsD=(nd + ((pars$na-2)-nd)*pars$ktot/45)*agg2
k00=c((groupsH*pars$parsKH)^(agg2/groupsH), (groupsD*pars$parsKD)^(agg2/groupsD), (groupsD*pars$parsKD)^(agg2/groupsD))
###
nh=2; agg3=nh/2;
groups=(nh + (2-nh)*pars$ktot/45)*agg3
k60=c((groups*pars$parsKH)^(agg3/groups), (groups*pars$parsKD)^(agg3/groups),(groups*pars$parsKD)^(agg3/groups))

### Run model for each parameter set in psample ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ntimes = length(imodelH)
zsample_0 = matrix(0,ntimes,nsample)
wsample_0 = matrix(0,ntimes,nsample)
vsample_0 = matrix(0,ntimes,nsample)
zsample_1 = matrix(0,ntimes,nsample)
wsample_1 = matrix(0,ntimes,nsample)
vsample_1 = matrix(0,ntimes,nsample)
Psample   = matrix(0,ntimes,nsample)
csample   = matrix(0,ntimes,nsample)
# data simul
zdsample_0 = matrix(0,ntimes,nsample)
wdsample_0 = matrix(0,ntimes,nsample)
vdsample_0 = matrix(0,ntimes,nsample)
zdsample_1 = matrix(0,ntimes,nsample)
wdsample_1 = matrix(0,ntimes,nsample)
vdsample_1 = matrix(0,ntimes,nsample)
cdsample   = matrix(0,ntimes,nsample)
# age partition
zdsampleA_0 = matrix(0,ntimes,nsample)
wdsampleA_0 = matrix(0,ntimes,nsample)
vdsampleA_0 = matrix(0,ntimes,nsample)
zdsampleA_1 = matrix(0,ntimes,nsample)
wdsampleA_1 = matrix(0,ntimes,nsample)
vdsampleA_1 = matrix(0,ntimes,nsample)
zdsampleB_0 = matrix(0,ntimes,nsample)
wdsampleB_0 = matrix(0,ntimes,nsample)
vdsampleB_0 = matrix(0,ntimes,nsample)
zdsampleB_1 = matrix(0,ntimes,nsample)
wdsampleB_1 = matrix(0,ntimes,nsample)
vdsampleB_1 = matrix(0,ntimes,nsample)
### probs
na = pars$na
ysample_0 = matrix(0,na,nsample)
ysample_1 = matrix(0,na,nsample)
hsample_0 = matrix(0,na,nsample)
hsample_1 = matrix(0,na,nsample)
yhsample_0= matrix(0,na,nsample)
yhsample_1= matrix(0,na,nsample)
dsample_0 = matrix(0,na,nsample)
dsample_1 = matrix(0,na,nsample)
### beta - function of other parameters
betasample= rep(0,times=nsample)
### cumulative outcomes predicted
### (not being used, using zsamples instead)
CHosp_0   = rep(0,times=nsample)
CHosp_1   = rep(0,times=nsample)
CHosp     = rep(0,times=nsample)
CMorH_0   = rep(0,times=nsample)
CMorH_1   = rep(0,times=nsample)
CMorH     = rep(0,times=nsample)
CMorO_0   = rep(0,times=nsample)
CMorO_1   = rep(0,times=nsample)
CMorO     = rep(0,times=nsample)
### scenario b - Averted amounts
dHosp_0   = rep(0,times=nsample)
dHosp_1   = rep(0,times=nsample)
dHosp     = rep(0,times=nsample)
dMort_0   = rep(0,times=nsample)
dMort_1   = rep(0,times=nsample)
dMort     = rep(0,times=nsample)
###
pHosp_0   = rep(0,times=nsample)
pHosp_1   = rep(0,times=nsample)
pHosp     = rep(0,times=nsample)
pMort_0   = rep(0,times=nsample)
pMort_1   = rep(0,times=nsample)
pMort     = rep(0,times=nsample)

### reproducibility of sampling predictive posterior
set.seed(7)
###
parsES    = pars
###
for(i in 1:nsample){
  parsES$rIR <- f1(as.vector(psample[i,1])) 
  parsES$rUR <- f2(as.vector(psample[i,2]))
  parsES$rOD <- f3(as.vector(psample[i,3]))
  parsES$pI0 <- f4(as.vector(psample[i,4]))
  parsES$rID <- parsES$rIR + parsES$rOD;    #tID
  hMES_0     <- f5(as.vector(psample[i,5]))
  hMES_1     <- f6(as.vector(psample[i,6]))
  hRES_0     <- f7(as.vector(psample[i,7]))
  hRES_1     <- f8(as.vector(psample[i,8]))
  dMES_0     <- f9(as.vector(psample[i,9]))
  dMES_1     <- f10(as.vector(psample[i,10]))
  dRES_0     <- f11(as.vector(psample[i,11]))
  dRES_1     <- f12(as.vector(psample[i,12]))
  yM1ES_0    <- f13(as.vector(psample[i,13]))
  yM1ES_1    <- f14(as.vector(psample[i,14]))
  yR1ES_0    <- f15(as.vector(psample[i,15]))
  yR1ES_1    <- f16(as.vector(psample[i,16]))
  #parsES$kH  <- f17(as.vector(psample[17])) #not used in model
  #parsES$kDH <- f18(as.vector(psample[18]))
  parsES$R0  <- f19(as.vector(psample[i,19]))
  parsES$rEI <- f20(as.vector(psample[i,20]))
  parsES$fu  <- f21(as.vector(psample[i,21]))
  #Dependent parameters
  #parsES$kDO = parsES$kDH                   #not used in model
  parsES$rIH  = parsES$rIR
  parsES$rIO  = parsES$rIR  
  parsES$h_0  = hMES_0*exp((age-age9)*hRES_0)
  parsES$h_1  = hMES_1*exp((age-age9)*hRES_1)
  parsES$d_0  = dMES_0*exp((age-age9)*dRES_0) 
  parsES$d_1  = dMES_1*exp((age-age9)*dRES_1) 
  parsES$y_0[3:9] = yM1ES_0*exp((age[3:9]-age9)*yR1ES_0)
  parsES$y_1[3:9] = yM1ES_1*exp((age[3:9]-age9)*yR1ES_1)
  parsES$yh_0 = parsES$y_0*parsES$h_0
  parsES$yh_1 = parsES$y_1*parsES$h_1
  parsES$Ia0  = (parsES$agehosp*parsES$Npop)*parsES$pI0
  parsES$Sa0  = parsES$Na0 - parsES$Ea0 - parsES$Ia0 - parsES$Ua0 - parsES$Ha0 - parsES$Oa0 - parsES$Ra0 - parsES$Da0 
  parsES$beta = BETA(parsES)
  betasample[i] = parsES$beta
  outs        = model(as.vector(parsES))
  zsam_0 = outs$byw_0$Hw[imodelH]
  wsam_0 = outs$byw_0$DHw[imodelH]
  vsam_0 = outs$byw_0$DOw[imodelH]
  zsam_1 = outs$byw_1$Hw[imodelH]
  wsam_1 = outs$byw_1$DHw[imodelH]
  vsam_1 = outs$byw_1$DOw[imodelH]
  zsample_0[,i] = zsam_0
  wsample_0[,i] = wsam_0
  vsample_0[,i] = vsam_0
  zsample_1[,i] = zsam_1
  wsample_1[,i] = wsam_1
  vsample_1[,i] = vsam_1
  ### age partition
  zdsampleA_0_ = outs$byw_ageH_0$H1w[imodelH]   + outs$byw_ageH_0$H2w[imodelH]   + outs$byw_ageH_0$H3w[imodelH] + 
                 outs$byw_ageH_0$H4w[imodelH]   + outs$byw_ageH_0$H5w[imodelH]   + outs$byw_ageH_0$H6w[imodelH] + 
                 outs$byw_ageH_0$H7w[imodelH]
  zdsampleA_1_ = outs$byw_ageH_1$H1w[imodelH]   + outs$byw_ageH_1$H2w[imodelH]   + outs$byw_ageH_1$H3w[imodelH] + 
                 outs$byw_ageH_1$H4w[imodelH]   + outs$byw_ageH_1$H5w[imodelH]   + outs$byw_ageH_1$H6w[imodelH] + 
                 outs$byw_ageH_1$H7w[imodelH]
  wdsampleA_0_ = outs$byw_ageD_0$DH1w[imodelDH] + outs$byw_ageD_0$DH2w[imodelDH] + outs$byw_ageD_0$DH3w[imodelDH] + 
                 outs$byw_ageD_0$DH4w[imodelDH] + outs$byw_ageD_0$DH5w[imodelDH] + outs$byw_ageD_0$DH6w[imodelDH] + 
                 outs$byw_ageD_0$DH7w[imodelDH]
  wdsampleA_1_ = outs$byw_ageD_1$DH1w[imodelDH] + outs$byw_ageD_1$DH2w[imodelDH] + outs$byw_ageD_1$DH3w[imodelDH] + 
                 outs$byw_ageD_1$DH4w[imodelDH] + outs$byw_ageD_1$DH5w[imodelDH] + outs$byw_ageD_1$DH6w[imodelDH] + 
                 outs$byw_ageD_1$DH7w[imodelDH]
  vdsampleA_0_ = outs$byw_ageD_0$DO1w[imodelDO] + outs$byw_ageD_0$DO2w[imodelDO] + outs$byw_ageD_0$DO3w[imodelDO] + 
                 outs$byw_ageD_0$DO4w[imodelDO] + outs$byw_ageD_0$DO5w[imodelDO] + outs$byw_ageD_0$DO6w[imodelDO] + 
                 outs$byw_ageD_0$DO7w[imodelDO]
  vdsampleA_1_ = outs$byw_ageD_1$DO1w[imodelDO] + outs$byw_ageD_1$DO2w[imodelDH] + outs$byw_ageD_1$DO3w[imodelDO] + 
                 outs$byw_ageD_1$DO4w[imodelDO] + outs$byw_ageD_1$DO5w[imodelDH] + outs$byw_ageD_1$DO6w[imodelDO] + 
                 outs$byw_ageD_1$DO7w[imodelDO]
  zdsampleB_0_ = outs$byw_ageH_0$H8w[imodelH]   + outs$byw_ageH_0$H9w[imodelH]
  zdsampleB_1_ = outs$byw_ageH_1$H8w[imodelH]   + outs$byw_ageH_1$H9w[imodelH]
  wdsampleB_0_ = outs$byw_ageD_0$DH8w[imodelDH] + outs$byw_ageD_0$DH9w[imodelDH]
  wdsampleB_1_ = outs$byw_ageD_1$DH8w[imodelDH] + outs$byw_ageD_1$DH9w[imodelDH]
  vdsampleB_0_ = outs$byw_ageD_0$DO8w[imodelDO] + outs$byw_ageD_0$DO9w[imodelDO]
  vdsampleB_1_ = outs$byw_ageD_1$DO8w[imodelDO] + outs$byw_ageD_1$DO9w[imodelDO]
  ### data simul 0-59
  kHs=k00[1]
  kDHs=k00[2]
  kDOs=k00[3]
  zdsampleA_0[,i] = rnbinom(ntimes, size=kHs,  mu=zdsampleA_0_)
  zdsampleA_1[,i] = rnbinom(ntimes, size=kHs,  mu=zdsampleA_1_)
  wdsampleA_0[,i] = rnbinom(ntimes, size=kDHs, mu=wdsampleA_0_)
  wdsampleA_1[,i] = rnbinom(ntimes, size=kDHs, mu=wdsampleA_1_)
  vdsampleA_0[,i] = rnbinom(ntimes, size=kDOs, mu=vdsampleA_0_)
  vdsampleA_1[,i] = rnbinom(ntimes, size=kDOs, mu=vdsampleA_1_)
  ### data simul 60+
  kHs=k60[1]
  kDHs=k60[2]
  kDOs=k60[3]
  zdsampleB_0[,i] = rnbinom(ntimes, size=kHs,  mu=zdsampleB_0_)
  zdsampleB_1[,i] = rnbinom(ntimes, size=kHs,  mu=zdsampleB_1_)
  wdsampleB_0[,i] = rnbinom(ntimes, size=kDHs, mu=wdsampleB_0_)
  wdsampleB_1[,i] = rnbinom(ntimes, size=kDHs, mu=wdsampleB_1_)
  vdsampleB_0[,i] = rnbinom(ntimes, size=kDOs, mu=vdsampleB_0_)
  vdsampleB_1[,i] = rnbinom(ntimes, size=kDOs, mu=vdsampleB_1_)  
  ### positivity
  Psample[,i]   = (outs$byw_0$It[imodelH] + outs$byw_1$It[imodelH] + outs$byw_0$Ut[imodelH] + outs$byw_1$Ut[imodelH])*oN
  csam          = (outs$byw_0$Ct[imodelH] + outs$byw_1$Ct[imodelH])*oN
  csample[,i]   = csam
  ### data simul all
  kHs=kall[1]
  kDHs=kall[2]
  kDOs=kall[3]
  zdsample_0[,i] = rnbinom(ntimes, size=kHs,  mu=zsam_0)
  wdsample_0[,i] = rnbinom(ntimes, size=kDHs, mu=wsam_0)
  vdsample_0[,i] = rnbinom(ntimes, size=kDOs, mu=vsam_0)
  zdsample_1[,i] = rnbinom(ntimes, size=kHs,  mu=zsam_1)
  wdsample_1[,i] = rnbinom(ntimes, size=kDHs, mu=wsam_1)
  vdsample_1[,i] = rnbinom(ntimes, size=kDOs, mu=vsam_1)
  ###
  nsamp = 7500; # favoured by match Binom vs Data
  cdsample[,i] = rbinom(n=length(csam),size=nsamp,prob=csam)/nsamp
  ### probs
  ysample_0[,i] = parsES$y_0
  ysample_1[,i] = parsES$y_1
  hsample_0[,i] = parsES$h_0
  hsample_1[,i] = parsES$h_1
  yhsample_0[,i]= parsES$yh_0
  yhsample_1[,i]= parsES$yh_1
  dsample_0[,i] = parsES$d_0
  dsample_1[,i] = parsES$d_1
  ### cumulative outcomes
  ### (not being used, using zsamples instead)
  CHosp_0[i]   = sum(outs$byw_0$Hw[imodelH])
  CHosp_1[i]   = sum(outs$byw_1$Hw[imodelH])
  CHosp[i]     = CHosp_0[i] + CHosp_1[i]
  CMorH_0[i]   = sum(outs$byw_0$DHw[imodelH])
  CMorH_1[i]   = sum(outs$byw_1$DHw[imodelH])
  CMorH[i]     = CMorH_0[i] + CMorH_1[i]
  CMorO_0[i]   = sum(outs$byw_0$DOw[imodelH])
  CMorO_1[i]   = sum(outs$byw_0$DOw[imodelH])
  CMorO[i]     = CMorO_0[i] + CMorO_1[i]
  ### scenario b
  parsES_b      = parsES
  parsES_b$cm_1 = parsES_b$cm_0
  outs_b        = model(as.vector(parsES_b))
  
    ### scenario difference - averted
	### from start of policy - 8w later than 27-jan-2020
    dHosp_0[i] = sum(outs_b$byw_0$Hw[iaverted] - outs$byw_0$Hw[iaverted]) 
    dHosp_1[i] = sum(outs_b$byw_1$Hw[iaverted] - outs$byw_1$Hw[iaverted])
    dMort_0[i] = sum(outs_b$byw_0$DHw[iaverted]- outs$byw_0$DHw[iaverted] + outs_b$byw_0$DOw[iaverted]- outs$byw_0$DOw[iaverted])
    dMort_1[i] = sum(outs_b$byw_1$DHw[iaverted]- outs$byw_1$DHw[iaverted] + outs_b$byw_1$DOw[iaverted]- outs$byw_1$DOw[iaverted])
    dHosp[i]   = dHosp_0[i] + dHosp_1[i]
    dMort[i]   = dMort_0[i] + dMort_1[i]
    ###
    pHosp_0[i] = 100*dHosp_0[i]/sum(outs_b$byw_0$Hw[iaverted]) 
    pHosp_1[i] = 100*dHosp_1[i]/sum(outs_b$byw_1$Hw[iaverted]) 
    pHosp[i]   = 100*dHosp[i]  /sum(outs_b$byw_1$Hw[iaverted]  + outs_b$byw_0$Hw[iaverted])
    pMort_0[i] = 100*dMort_0[i]/sum(outs_b$byw_0$DHw[iaverted] + outs_b$byw_0$DOw[iaverted])
    pMort_1[i] = 100*dMort_1[i]/sum(outs_b$byw_1$DHw[iaverted] + outs_b$byw_1$DOw[iaverted])
    pMort[i]   = 100*dMort[i]  /sum(outs_b$byw_1$DHw[iaverted] + outs_b$byw_1$DOw[iaverted] + outs_b$byw_0$DHw[iaverted] + outs_b$byw_0$DOw[iaverted])
  }
### Run model for each parameter set in psample ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ###
  Weekssample = 1 + outs$byw_0$time[imodelH]/7 + Week_shift_model
  Datessample = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1) #"2020-01-06" Mon
  ### 95% CrI
  zsample95_0 = matrix(0,ntimes,3)
  zsample95_1 = matrix(0,ntimes,3)
  wsample95_0 = matrix(0,ntimes,3)
  wsample95_1 = matrix(0,ntimes,3)
  vsample95_0 = matrix(0,ntimes,3)
  vsample95_1 = matrix(0,ntimes,3)
  ### positivity
  Psample95   = matrix(0,ntimes,3)
  csample95   = matrix(0,ntimes,3)
  ### data simul (PIs and mean)
  zdsample95_0 = matrix(0,ntimes,3)
  zdsample95_1 = matrix(0,ntimes,3)
  wdsample95_0 = matrix(0,ntimes,3)
  wdsample95_1 = matrix(0,ntimes,3)
  vdsample95_0 = matrix(0,ntimes,3)
  vdsample95_1 = matrix(0,ntimes,3)
  zdsampleMn_0= rep(0,ntimes)
  wdsampleMn_0= rep(0,ntimes)
  vdsampleMn_0= rep(0,ntimes)
  zdsampleMn_1= rep(0,ntimes)
  wdsampleMn_1= rep(0,ntimes)
  vdsampleMn_1= rep(0,ntimes)
  cdsample95   = matrix(0,ntimes,3)
  ### age partition
  zdsampleA95_0 = matrix(0,ntimes,3)
  wdsampleA95_0 = matrix(0,ntimes,3)
  vdsampleA95_0 = matrix(0,ntimes,3)
  zdsampleA95_1 = matrix(0,ntimes,3)
  wdsampleA95_1 = matrix(0,ntimes,3)
  vdsampleA95_1 = matrix(0,ntimes,3)
  zdsampleB95_0 = matrix(0,ntimes,3)
  wdsampleB95_0 = matrix(0,ntimes,3)
  vdsampleB95_0 = matrix(0,ntimes,3)
  zdsampleB95_1 = matrix(0,ntimes,3)
  wdsampleB95_1 = matrix(0,ntimes,3)
  vdsampleB95_1 = matrix(0,ntimes,3)
  ### probs
  ysample95_0 = matrix(0,na,3)
  ysample95_1 = matrix(0,na,3)
  hsample95_0 = matrix(0,na,3)
  hsample95_1 = matrix(0,na,3)
  yhsample95_0= matrix(0,na,3)
  yhsample95_1= matrix(0,na,3)
  dsample95_0 = matrix(0,na,3)
  dsample95_1 = matrix(0,na,3)
  ###
  betasample95= rep(0,times=3)
  ###
  CHosp95_0   = rep(0,times=3)
  CHosp95_1   = rep(0,times=3)
  CHosp95     = rep(0,times=3)
  CMorH95_0   = rep(0,times=3)
  CMorH95_1   = rep(0,times=3)
  CMorH95     = rep(0,times=3)
  CMorO95_0   = rep(0,times=3)
  CMorO95_1   = rep(0,times=3)
  CMorO95     = rep(0,times=3)
  ###
  dHosp95_0   = rep(0,times=3)
  dHosp95_1   = rep(0,times=3)
  dHosp95     = rep(0,times=3)
  dMort95_0   = rep(0,times=3)
  dMort95_1   = rep(0,times=3)
  dMort95     = rep(0,times=3)
  ###
  pHosp95_0   = rep(0,times=3)
  pHosp95_1   = rep(0,times=3)
  pHosp95     = rep(0,times=3)
  pMort95_0   = rep(0,times=3)
  pMort95_1   = rep(0,times=3)
  pMort95     = rep(0,times=3)
  ### beta
  betasample95[1:3] = as.numeric(quantile(betasample,Q,na.rm=T))
  ### cumulative outcomes q1, med, q2; mean
  ### (not being used, using zsamples instead)
  CHosp95_0[1:3] = as.numeric(quantile(CHosp_0,Q,na.rm=T))
  CHosp95_1[1:3] = as.numeric(quantile(CHosp_1,Q,na.rm=T))
  CMorH95_0[1:3] = as.numeric(quantile(CMorH_0,Q,na.rm=T))
  CMorH95_1[1:3] = as.numeric(quantile(CMorH_1,Q,na.rm=T))
  CMorO95_0[1:3] = as.numeric(quantile(CMorO_0,Q,na.rm=T))
  CMorO95_1[1:3] = as.numeric(quantile(CMorO_1,Q,na.rm=T))
  CHosp95[1:3]   = as.numeric(quantile(CHosp,Q,na.rm=T))
  CMorH95[1:3]   = as.numeric(quantile(CMorH,Q,na.rm=T))
  CMorO95[1:3]   = as.numeric(quantile(CMorO,Q,na.rm=T))
  CHospMn_0      = as.numeric(mean(CHosp_0,na.rm=T))
  CHospMn_1      = as.numeric(mean(CHosp_1,na.rm=T))
  CMorHMn_0      = as.numeric(mean(CMorH_0,na.rm=T))
  CMorHMn_1      = as.numeric(mean(CMorH_1,na.rm=T))
  CMorOMn_0      = as.numeric(mean(CMorO_0,na.rm=T))
  CMorOMn_1      = as.numeric(mean(CMorO_1,na.rm=T))
  CHospMn        = as.numeric(mean(CHosp,na.rm=T))
  CMorHMn        = as.numeric(mean(CMorH,na.rm=T))
  CMorOMn        = as.numeric(mean(CMorO,na.rm=T))

  ### averted_0, averted_1 q1, med, q2
  dHosp95_0[1:3] = as.numeric(quantile(dHosp_0,Q,na.rm=T))
  dHosp95_1[1:3] = as.numeric(quantile(dHosp_1,Q,na.rm=T))
  dMort95_0[1:3] = as.numeric(quantile(dMort_0,Q,na.rm=T))
  dMort95_1[1:3] = as.numeric(quantile(dMort_1,Q,na.rm=T))
  pHosp95_0[1:3] = as.numeric(quantile(pHosp_0,Q,na.rm=T))
  pHosp95_1[1:3] = as.numeric(quantile(pHosp_1,Q,na.rm=T))
  pMort95_0[1:3] = as.numeric(quantile(pMort_0,Q,na.rm=T))
  pMort95_1[1:3] = as.numeric(quantile(pMort_1,Q,na.rm=T))
  ### averted q1, med, q2
  dHosp95[1:3] = as.numeric(quantile(dHosp,Q,na.rm=T))
  dMort95[1:3] = as.numeric(quantile(dMort,Q,na.rm=T))
  pHosp95[1:3] = as.numeric(quantile(pHosp,Q,na.rm=T))
  pMort95[1:3] = as.numeric(quantile(pMort,Q,na.rm=T))

  ### probs
  for(ia in 1:na){   
    #y
    samp_ia <- ysample_0[ia,]
    ysample95_0[ia,1:3] = as.numeric(quantile(samp_ia,Q,na.rm=T))
    samp_ia <- ysample_1[ia,]
    ysample95_1[ia,1:3] = as.numeric(quantile(samp_ia,Q,na.rm=T))
    #h
    samp_ia <- hsample_0[ia,]
    hsample95_0[ia,1:3] = as.numeric(quantile(samp_ia,Q,na.rm=T))
    samp_ia <- hsample_1[ia,]
    hsample95_1[ia,1:3] = as.numeric(quantile(samp_ia,Q,na.rm=T))
    #yh
    samp_ia <- yhsample_0[ia,]
    yhsample95_0[ia,1:3] = as.numeric(quantile(samp_ia,Q,na.rm=T))
    samp_ia <- yhsample_1[ia,]
    yhsample95_1[ia,1:3] = as.numeric(quantile(samp_ia,Q,na.rm=T))
    #d
    samp_ia <- dsample_0[ia,]
    dsample95_0[ia,1:3] = as.numeric(quantile(samp_ia,Q,na.rm=T))
    samp_ia <- dsample_1[ia,]
    dsample95_1[ia,1:3] = as.numeric(quantile(samp_ia,Q,na.rm=T))  
  }
  
  #timeseries, inc. incidence 95%CrI
  for(it in 1:ntimes){
    #z
    samp_it <- zsample_0[it,]
    zsample95_0[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    samp_it <- zsample_1[it,]
    zsample95_1[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    #w
    samp_it <- wsample_0[it,]
    wsample95_0[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    samp_it <- wsample_1[it,]
    wsample95_1[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    #v
    samp_it <- vsample_0[it,]
    vsample95_0[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    samp_it <- vsample_1[it,]
    vsample95_1[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    ### age partition
    ### 0-59
    samp_it <- zdsampleA_0[it,]
    zdsampleA95_0[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    samp_it <- zdsampleA_1[it,]
    zdsampleA95_1[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    samp_it <- wdsampleA_0[it,]
    wdsampleA95_0[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    samp_it <- wdsampleA_1[it,]
    wdsampleA95_1[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    samp_it <- vdsampleA_0[it,]
    vdsampleA95_0[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    samp_it <- vdsampleA_1[it,]
    vdsampleA95_1[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    ### 60+
    samp_it <- zdsampleB_0[it,]
    zdsampleB95_0[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    samp_it <- zdsampleB_1[it,]
    zdsampleB95_1[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    samp_it <- wdsampleB_0[it,]
    wdsampleB95_0[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    samp_it <- wdsampleB_1[it,]
    wdsampleB95_1[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    samp_it <- vdsampleB_0[it,]
    vdsampleB95_0[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    samp_it <- vdsampleB_1[it,]
    vdsampleB95_1[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    ### positivity
    #P
    samp_it <- Psample[it,]
    Psample95[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    #C
    samp_it <- csample[it,]
    csample95[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    ### data simul
    #zd
    samp_it <- zdsample_0[it,]
    zdsample95_0[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    zdsampleMn_0[it]    = as.numeric(mean(samp_it, na.rm=T))
    samp_it <- zdsample_1[it,]
    zdsample95_1[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    zdsampleMn_1[it]    = as.numeric(mean(samp_it, na.rm=T))
    #wd
    samp_it <- wdsample_0[it,]
    wdsample95_0[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    wdsampleMn_0[it]    = as.numeric(mean(samp_it, na.rm=T))
    samp_it <- wdsample_1[it,]
    wdsample95_1[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    wdsampleMn_1[it]    = as.numeric(mean(samp_it, na.rm=T))
    #vd
    samp_it <- vdsample_0[it,]
    vdsample95_0[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    vdsampleMn_0[it]    = as.numeric(mean(samp_it, na.rm=T))
    samp_it <- vdsample_1[it,]
    vdsample95_1[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
    vdsampleMn_1[it]    = as.numeric(mean(samp_it, na.rm=T))
    #cd
    samp_it <- cdsample[it,]
    cdsample95[it,1:3] = as.numeric(quantile(samp_it,Q,na.rm=T))
  } ## timeseries
  
  ## R0 over time - R0_week
  source(file = paste0(input_code,"/R0_0.r"))
  ntimes = length(imodelH)
  nsampleR0 = min(4000,nsample) #shorter as calling R0_0()
  R0weeksample = matrix(0,ntimes,nsampleR0)
  R0sample     = rep(0,times=nsampleR0) #in case R0 is estimated
  for(i in 1:nsampleR0){
    parsES$rIR <- f1(as.vector(psample[i,1])) 
    parsES$rUR <- f2(as.vector(psample[i,2]))
    parsES$rOD <- f3(as.vector(psample[i,3]))
    parsES$pI0 <- f4(as.vector(psample[i,4]))
    parsES$rID <- parsES$rIR+parsES$rOD;     #tID
    hMES_0     <- f5(as.vector(psample[i,5]))
    hMES_1     <- f6(as.vector(psample[i,6]))
    hRES_0     <- f7(as.vector(psample[i,7]))
    hRES_1     <- f8(as.vector(psample[i,8]))
    dMES_0     <- f9(as.vector(psample[i,9]))
    dMES_1     <- f10(as.vector(psample[i,10]))
    dRES_0     <- f11(as.vector(psample[i,11]))
    dRES_1     <- f12(as.vector(psample[i,12]))
    yM1ES_0    <- f13(as.vector(psample[i,13]))
    yM1ES_1    <- f14(as.vector(psample[i,14]))
    yR1ES_0    <- f15(as.vector(psample[i,15]))
    yR1ES_1    <- f16(as.vector(psample[i,16]))
    #parsES$kH  <- f17(as.vector(psample[17])) #not used in model
    #parsES$kDH <- f18(as.vector(psample[18]))
    parsES$R0  <- f19(as.vector(psample[i,19]))
    parsES$rEI <- f20(as.vector(psample[i,20]))
    parsES$fu  <- f21(as.vector(psample[i,21]))
	#Dependent parameters
	#parsES$kDO = parsES$kDH                   #not used in model
    parsES$rIH  = parsES$rIR
    parsES$rIO  = parsES$rIR  
    parsES$h_0  = hMES_0*exp((age-age9)*hRES_0)
    parsES$h_1  = hMES_1*exp((age-age9)*hRES_1)
    parsES$d_0  = dMES_0*exp((age-age9)*dRES_0) 
    parsES$d_1  = dMES_1*exp((age-age9)*dRES_1) 
    parsES$y_0[3:9] = yM1ES_0*exp((age[3:9]-age9)*yR1ES_0)
    parsES$y_1[3:9] = yM1ES_1*exp((age[3:9]-age9)*yR1ES_1)
    parsES$yh_0 = parsES$y_0*parsES$h_0
    parsES$yh_1 = parsES$y_1*parsES$h_1
	parsES$Ia0  = (parsES$agehosp*parsES$Npop)*parsES$pI0
    parsES$Sa0  = parsES$Na0 - parsES$Ea0 - parsES$Ia0 - parsES$Ua0 - parsES$Ha0 - parsES$Oa0 - parsES$Ra0 - parsES$Da0 
    parsES$beta = BETA(parsES)
    r0 = R0_0(parsES, GetBeta=0, GetOutput=1, Sampling=1, nt=ntimes)
    trend <- r0$WeekDateR0$R0_week
    R0weeksample[,i] = parsES$R0*trend/trend[1]
  }
  R0weeksample95 = matrix(0,length(imodelH),3)
  for(it in 1:ntimes) {
    samp_it <- R0weeksample[it,]
    R0weeksample95[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
    R0weeksample95[it,2] = quantile(samp_it,0.5,na.rm=T)[[1]]
    R0weeksample95[it,3] = quantile(samp_it,q2,na.rm=T)[[1]]
  }
#print(R0weeksample95[1,]) #week 1

tout1 <- Sys.time() - tout0
### SAMPLING #############################################################################



### SUMMARY ##############################################################################

### Dataframes for Summary and Figures ###################################################
### y axis log transformation - default: linear
off  <- 1
YLOG <- function(y,LOG=0){ if (LOG==1) {z=log10(y+off)} else {z=y}; return(z) }
YEXP <- function(y,LOG=0){ if (LOG==1) {z=10^(y)-off}   else {z=y}; return(z) }
LOG  <- 1; #0 #1: apply log scale to Age profile plots; 0: don't
#(repeated below)

### Variables (Model and data)
### initialise overall (summed over age groups)
Hd_0    = 0
DHd_0   = 0
DOd_0   = 0
Hdnw_0  = 0
DHdnw_0 = 0
DOdnw_0 = 0
Hd_1    = 0
DHd_1   = 0
DOd_1   = 0
Hdnw_1  = 0
DHdnw_1 = 0
DOdnw_1 = 0

for (i in 1:9){
  #MAP
  valuesH_0  = eval(parse(text = paste0("mE$byw_ageH_0$H", eval(i),"w[imodelH]")))
  valuesDH_0 = eval(parse(text = paste0("mE$byw_ageD_0$DH",eval(i),"w[imodelDH]")))
  valuesDO_0 = eval(parse(text = paste0("mE$byw_ageD_0$DO",eval(i),"w[imodelDO]")))
  valuesH_1  = eval(parse(text = paste0("mE$byw_ageH_1$H", eval(i),"w[imodelH]")))
  valuesDH_1 = eval(parse(text = paste0("mE$byw_ageD_1$DH",eval(i),"w[imodelDH]")))
  valuesDO_1 = eval(parse(text = paste0("mE$byw_ageD_1$DO",eval(i),"w[imodelDO]")))
  assign(paste0("H", eval(i),"w_0"), YLOG(valuesH_0, LOG))                        #H1w_0-H9w_0
  assign(paste0("DH",eval(i),"w_0"), YLOG(valuesDH_0,LOG))                        #DH1w_0-DH9w_0
  assign(paste0("DO",eval(i),"w_0"), YLOG(valuesDO_0,LOG))                        #DO1w_0-DO9w_0
  assign(paste0("H", eval(i),"w_1"), YLOG(valuesH_1, LOG))                        #H1w_1-H9w_1
  assign(paste0("DH",eval(i),"w_1"), YLOG(valuesDH_1,LOG))                        #DH1w_1-DH9w_1
  assign(paste0("DO",eval(i),"w_1"), YLOG(valuesDO_1,LOG))                        #DO1w_1-DO9w_1
  
  #Data
  ivaluesH = which( !is.na(datHas_l$Week) &  datHas_l$Ageg==i &  datHas_l$Shield==0 &  datHas_l$Week >= Week1_Fit_H  &  datHas_l$Week <= Week2_Fit_H)  #idataH1-idataH9 
  ivaluesDH= which(!is.na(datDHas_l$Week) & datDHas_l$Ageg==i & datDHas_l$Shield==0 & datDHas_l$Week >= Week1_Fit_DH & datDHas_l$Week <= Week2_Fit_DH) #idataDH1-idataDH9   
  ivaluesDO= which(!is.na(datDOas_l$Week) & datDOas_l$Ageg==i & datDOas_l$Shield==0 & datDOas_l$Week >= Week1_Fit_DO & datDOas_l$Week <= Week2_Fit_DO) #idataDO1-idataDO9 
  valuesH_0  =  datHas_l$Freq[ivaluesH]*weight[i] 
  valuesDH_0 = datDHas_l$Freq[ivaluesDH]*weight[i]
  valuesDO_0 = datDOas_l$Freq[ivaluesDO]*weight[i]
  ivaluesH = which( !is.na(datHas_l$Week) &  datHas_l$Ageg==i &  datHas_l$Shield==1 &  datHas_l$Week >= Week1_Fit_H  &  datHas_l$Week <= Week2_Fit_H)  #idataH1-idataH9 
  ivaluesDH= which(!is.na(datDHas_l$Week) & datDHas_l$Ageg==i & datDHas_l$Shield==1 & datDHas_l$Week >= Week1_Fit_DH & datDHas_l$Week <= Week2_Fit_DH) #idataDH1-idataDH9   
  ivaluesDO= which(!is.na(datDOas_l$Week) & datDOas_l$Ageg==i & datDOas_l$Shield==1 & datDOas_l$Week >= Week1_Fit_DO & datDOas_l$Week <= Week2_Fit_DO) #idataDO1-idataDO9 
  valuesH_1  =  datHas_l$Freq[ivaluesH]*weight[i] 
  valuesDH_1 = datDHas_l$Freq[ivaluesDH]*weight[i]
  valuesDO_1 = datDOas_l$Freq[ivaluesDO]*weight[i]  
  
  assign(paste0("H", eval(i),"d_0"), YLOG(valuesH_0, LOG))                        #H1d_0-H9d_0
  assign(paste0("DH",eval(i),"d_0"), YLOG(valuesDH_0,LOG))                        #DH1d_0-DH9d_0
  assign(paste0("DO",eval(i),"d_0"), YLOG(valuesDO_0,LOG))                        #DO1d_0-DO9d_0
  assign(paste0("H", eval(i),"d_1"), YLOG(valuesH_1, LOG))                        #H1d_1-H9d_1
  assign(paste0("DH",eval(i),"d_1"), YLOG(valuesDH_1,LOG))                        #DH1d_1-DH9d_1
  assign(paste0("DO",eval(i),"d_1"), YLOG(valuesDO_1,LOG))                        #DO1d_1-DO9d_1
  
  #Data overall - weighted and unweighted (nw) - no logs
  Hd_0    = Hd_0    + valuesH_0
  DHd_0   = DHd_0   + valuesDH_0
  DOd_0   = DOd_0   + valuesDO_0
  Hd_1    = Hd_1    + valuesH_1
  DHd_1   = DHd_1   + valuesDH_1
  DOd_1   = DOd_1   + valuesDO_1
  #
  Hdnw_0  = Hdnw_0  + valuesH_0/weight[i]
  DHdnw_0 = DHdnw_0 + valuesDH_0/weight[i]
  DOdnw_0 = DOdnw_0 + valuesDO_0/weight[i]
  Hdnw_1  = Hdnw_1  + valuesH_1/weight[i]
  DHdnw_1 = DHdnw_1 + valuesDH_1/weight[i]
  DOdnw_1 = DOdnw_1 + valuesDO_1/weight[i]   
}
### Dataframes for Summary and Figures ###################################################
cat("Summary...", "\n")

  ##### Summary - txt output #############################################################
  sink(file = paste0(output_dir,"/","Results_summary_1_",TODAY,".txt"),append=FALSE,split=FALSE)
  ##Averted events
  cat("\n");
  cat("Averted events from 24 Mar \n")
  print(paste0("Averted Hosp_1: ",  round(dHosp95_1[2],0), " [",round(dHosp95_1[1],0),",",round(dHosp95_1[3],0),"]"))
  print(paste0("Averted Hosp:   ",  round(dHosp95[2],0),   " [",round(dHosp95[1],  0),",",round(dHosp95[3],  0),"]"))
  print(paste0("Averted Mort_1: ",  round(dMort95_1[2],0), " [",round(dMort95_1[1],0),",",round(dMort95_1[3],0),"]"))
  print(paste0("Averted Mort:   ",  round(dMort95[2],0),   " [",round(dMort95[1],  0),",",round(dMort95[3],  0),"]"))
  print(paste0("Averted Hosp_1 : ", round(pHosp95_1[2],0), "% [",round(pHosp95_1[1],0),",",round(pHosp95_1[3],0),"]"))
  print(paste0("Averted Hosp   : ", round(pHosp95[2],0),   "% [",round(pHosp95[1],  0),",",round(pHosp95[3],  0),"]"))
  print(paste0("Averted Mort_1 : ", round(pMort95_1[2],0), "% [",round(pMort95_1[1],0),",",round(pMort95_1[3],0),"]"))
  print(paste0("Averted Mort   : ", round(pMort95[2],0),   "% [",round(pMort95[1],  0),",",round(pMort95[3],  0),"]"))
  print(paste0("Averted Hosp_0: ",  round(dHosp95_0[2],0), " [",round(dHosp95_0[1],0),",",round(dHosp95_0[3],0),"]"))
  print(paste0("Averted Mort_0: ",  round(dMort95_0[2],0), " [",round(dMort95_0[1],0),",",round(dMort95_0[3],0),"]"))
  print(paste0("Averted Hosp_0 : ", round(pHosp95_0[2],0), "% [",round(pHosp95_0[1],0),",",round(pHosp95_0[3],0),"]"))
  print(paste0("Averted Mort_0 : ", round(pMort95_0[2],0), "% [",round(pMort95_0[1],0),",",round(pMort95_0[3],0),"]"))
  cat("\n");
  ## Fitting info
  print(paste0("Posterior based on a NB likelihood"))
  ## Run time
  print(paste0("Sampling time (sec): ", round(as.numeric(tout1),5)))
  cat("\n");
  ## Assumptions
  cat("Assumptions \n")
  print(paste0("kDO = kDH "))
  print(paste0("tIH = tIR "))
  cat("\n")
  ## MAP & quantile estimates
  cat("Parameter estimates (MAP, quantiles) \n")
  for(i in 1:(npar+1)){
  print(paste0(Pnamei[i],", MAP: ", PMAPr[[i]],", Med: ", Pr[i,2],", CI: [",Pr[i,1],",",Pr[i,3],"]")) }
  print(paste0("beta dependent: ",", MAP: ", round(parsE$beta,3),", Med: ", round(betasample95[2],3),", CI: [",round(betasample95[1],3),",",round(betasample95[3],3),"]"))
  print(paste0("I0 dependent:   ",", MAP: ", round(sum(parsE$Ia0), 0)))
  print(paste0("E0      fixed:  ", round(sum(parsE$Ea0), 0)))
  print(paste0("Ha0_sum input:  ", sum(parsE$Ha0)))
  print(paste0("Da0_sum input:  ", sum(parsE$Da0)))
  cat("\n");
  ## Chain statistics  
  cat("Parameters estimated (chain summary statistics) \n")
  print(summary(out$chain)); 
  cat("\n");
  ##
  cat("Cumulative events \n")
  print(paste0("Cumulat Hosp_0 (mean, quant, data): ", round(sum(zdsampleMn_0)),                ", ", round(sum(zdsample95_0[,2]),0), " [",round(sum(zdsample95_0[,1]),0),",",round(sum(zdsample95_0[,3]),0),"], ", round(sum(Hd_0)) )) #datH$zdw_0
  print(paste0("Cumulat Hosp_1 (mean, quant, data): ", round(sum(zdsampleMn_1)),                ", ", round(sum(zdsample95_1[,2]),0), " [",round(sum(zdsample95_1[,1]),0),",",round(sum(zdsample95_1[,3]),0),"], ", round(sum(Hd_1)) )) #datH$zdw_1
  print(paste0("Cumulat Hosp   (mean, quant, data): ", round(sum(zdsampleMn_0+zdsampleMn_1)),   ", ", round(sum(zdsample95_0[,2]+zdsample95_1[,2]),0), " [",round(sum(zdsample95_0[,1]+zdsample95_1[,1]),0),",",round(sum(zdsample95_0[,3]+zdsample95_1[,3])),"], ", round(sum(Hd_0+Hd_1)) ))
  print(paste0("Cumulat MorH_0 (mean, quant, data): ", round(sum(wdsampleMn_0)),                ", ", round(sum(wdsample95_0[,2]),0), " [",round(sum(wdsample95_0[,1]),0),",",round(sum(wdsample95_0[,3]),0),"], ", round(sum(DHd_0)) ))
  print(paste0("Cumulat MorH_1 (mean, quant, data): ", round(sum(wdsampleMn_1)),                ", ", round(sum(wdsample95_1[,2]),0), " [",round(sum(wdsample95_1[,1]),0),",",round(sum(wdsample95_1[,3]),0),"], ", round(sum(DHd_1)) ))
  print(paste0("Cumulat MorH   (mean, quant, data): ", round(sum(wdsampleMn_0+wdsampleMn_1)),   ", ", round(sum(wdsample95_0[,2]+wdsample95_1[,2]),0), " [",round(sum(wdsample95_0[,1]+wdsample95_1[,1]),0),",",round(sum(wdsample95_0[,3]+wdsample95_1[,3])),"], ", round(sum(DHd_0+DHd_1)) ))
  print(paste0("Cumulat MorO_0 (mean, quant, data): ", round(sum(vdsampleMn_0)),                ", ", round(sum(vdsample95_0[,2]),0), " [",round(sum(vdsample95_0[,1]),0),",",round(sum(vdsample95_0[,3]),0),"], ", round(sum(DOd_0)) ))
  print(paste0("Cumulat MorO_1 (mean, quant, data): ", round(sum(vdsampleMn_1)),                ", ", round(sum(vdsample95_1[,2]),0), " [",round(sum(vdsample95_1[,1]),0),",",round(sum(vdsample95_1[,3]),0),"], ", round(sum(DOd_1)) ))
  print(paste0("Cumulat MorO   (mean, quant, data): ", round(sum(vdsampleMn_0+vdsampleMn_1)),   ", ", round(sum(vdsample95_0[,2]+vdsample95_1[,2]),0), " [",round(sum(vdsample95_0[,1]+vdsample95_1[,1]),0),",",round(sum(vdsample95_0[,3]+vdsample95_1[,3])),"] ", round(sum(DOd_0+DOd_1)) ))
  cat("\n");
  ##
  cat("Effective sample size \n")
  cat("Geyer approach in 'posterior' package")
  print(paste0("ESS mean: ", round(mean(essg))))
  print(paste0("and accounting for convergence, by inputting the 3 chains separately, not as a single vector:"))
  print(paste0("ESS mean: ", round(mean(essgc))))
  cat("\n");
  cat("\n")
  sink()
  ##### Summary - txt output #############################################################
### SUMMARY ##############################################################################



### FIGURES ##############################################################################

  #### Plots - Overall dataframes ########################################################
  N  = pars$Npop
  Nc = pars$Npopcoh
  weight0 = N/Nc
  
  datp <- tibble(Weeks  = 1 + mE$byw_0$time[imodelH]/7 + Week_shift_model,
                 Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1), #"2020-01-06" Mon
                 Prev   = (mE$byw_0$It[imodelH] + mE$byw_0$Ut[imodelH] + mE$byw_1$It[imodelH] + mE$byw_1$Ut[imodelH])*oN )
  
  datH <- tibble(Weeks  = 1 + mE$byw_0$time[imodelH]/7 + Week_shift_model,  #model time 0 <> week1 + (Week1_Model-1) = 1 + (4-1) = week4
                 Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1), #"2020-01-06" Mon
                 H_est_0= mE$byw_0$Hw[imodelH],
                 H_est_1= mE$byw_1$Hw[imodelH],
                 H_est  = H_est_0 + H_est_1,
                 zdw    = zd*weight0,
                 zdw_0  = zd_0*weight0, 
                 zdw_1  = zd_1*weight0) 
  
  datD <- tibble(Weeks  = 1 + mE$byw_0$time[imodelDH]/7 + Week_shift_model,
                 Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1), #"2020-01-06" Mon
                 DH_est_0 = mE$byw_0$DHw[imodelDH],
                 DH_est_1 = mE$byw_1$DHw[imodelDH],  
                 DH_est   = DH_est_0 + DH_est_1,
                 DO_est_0 = mE$byw_0$DOw[imodelDO],
                 DO_est_1 = mE$byw_1$DOw[imodelDO],  
                 DO_est   = DO_est_0 + DO_est_1,               
                 wdw    = wd*weight0, 
                 wdw_0  = wd_0*weight0,
                 wdw_1  = wd_1*weight0,
                 vdw    = vd*weight0,  
                 vdw_0  = vd_0*weight0,
                 vdw_1  = vd_1*weight0)
  
    datH <- tibble(datH,
                   Weeksz = datH_l$Week[idataH],
                   Datesz = Dates)

    datD <- tibble(datD,
                   Weeksw = datDH_l$Week[idataDH],
                   Weeksv = datDO_l$Week[idataDO],
                   Datesw = Dates,
                   Datesv = Dates)
  #### Plots - Overall dataframes ########################################################

  
  ###### Plots - Age-profile dataframes ##################################################

  #y axis log transformation - default: linear
  #defined near '### SUMMARY'
  
    #Indices of data vectors (pset$iplatform==2)
    #=> defined in "## Indices" at the top
    
    #Time
    datT    <- tibble(Weeks = 1 + mE$byw_0$time[imodelH]/7 + Week_shift_model,  #Considering that imodelH = imodelDH = imodelDO
                      Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1)) #"2020-01-06" Mon
    #H
    datHa_0 <- tibble(datT,
                      H1w=H1w_0, H2w=H2w_0, H3w=H3w_0, H4w=H4w_0, H5w=H5w_0, #estimated
                      H6w=H6w_0, H7w=H7w_0, H8w=H8w_0, H9w=H9w_0,
                      H1d=H1d_0, H2d=H2d_0, H3d=H3d_0, H4d=H4d_0, H5d=H5d_0, #data
                      H6d=H6d_0, H7d=H7d_0, H8d=H8d_0, H9d=H9d_0,
                      H1to7w= YLOG(YEXP(H1w_0,LOG)+YEXP(H2w_0,LOG)+YEXP(H3w_0,LOG)+YEXP(H4w_0,LOG)+YEXP(H5w_0,LOG)+YEXP(H6w_0,LOG)+YEXP(H7w_0,LOG),LOG),
                      H1to7d= YLOG(YEXP(H1d_0,LOG)+YEXP(H2d_0,LOG)+YEXP(H3d_0,LOG)+YEXP(H4d_0,LOG)+YEXP(H5d_0,LOG)+YEXP(H6d_0,LOG)+YEXP(H7d_0,LOG),LOG),
                      H8to9w= YLOG(YEXP(H8w_0,LOG)+YEXP(H9w_0,LOG),LOG),
                      H8to9d= YLOG(YEXP(H8d_0,LOG)+YEXP(H9d_0,LOG),LOG))
    datHa_1 <- tibble(datT,
                      H1w=H1w_1, H2w=H2w_1, H3w=H3w_1, H4w=H4w_1, H5w=H5w_1, #estimated
                      H6w=H6w_1, H7w=H7w_1, H8w=H8w_1, H9w=H9w_1,
                      H1d=H1d_1, H2d=H2d_1, H3d=H3d_1, H4d=H4d_1, H5d=H5d_1, #data
                      H6d=H6d_1, H7d=H7d_1, H8d=H8d_1, H9d=H9d_1, 
                      H1to7w= YLOG(YEXP(H1w_1,LOG)+YEXP(H2w_1,LOG)+YEXP(H3w_1,LOG)+YEXP(H4w_1,LOG)+YEXP(H5w_1,LOG)+YEXP(H6w_1,LOG)+YEXP(H7w_1),LOG),
                      H1to7d= YLOG(YEXP(H1d_1,LOG)+YEXP(H2d_1,LOG)+YEXP(H3d_1,LOG)+YEXP(H4d_1,LOG)+YEXP(H5d_1,LOG)+YEXP(H6d_1,LOG)+YEXP(H7d_1),LOG),
                      H8to9w= YLOG(YEXP(H8w_1,LOG)+YEXP(H9w_1,LOG),LOG),
                      H8to9d= YLOG(YEXP(H8d_1,LOG)+YEXP(H9d_1,LOG),LOG))
#DH
    datDHa_0<- tibble(datT,
                      DH1w=DH1w_0, DH2w=DH2w_0, DH3w=DH3w_0, DH4w=DH4w_0, DH5w=DH5w_0,    #estimated
                      DH6w=DH6w_0, DH7w=DH7w_0, DH8w=DH8w_0, DH9w=DH9w_0,
                      DH1d=DH1d_0, DH2d=DH2d_0, DH3d=DH3d_0, DH4d=DH4d_0, DH5d=DH5d_0,    #data
                      DH6d=DH6d_0, DH7d=DH7d_0, DH8d=DH8d_0, DH9d=DH9d_0,
                      DH1to7w= YLOG(YEXP(DH1w_0,LOG)+YEXP(DH2w_0,LOG)+YEXP(DH3w_0,LOG)+YEXP(DH4w_0,LOG)+YEXP(DH5w_0,LOG)+YEXP(DH6w_0,LOG)+YEXP(DH7w_0,LOG),LOG),
                      DH1to7d= YLOG(YEXP(DH1d_0,LOG)+YEXP(DH2d_0,LOG)+YEXP(DH3d_0,LOG)+YEXP(DH4d_0,LOG)+YEXP(DH5d_0,LOG)+YEXP(DH6d_0,LOG)+YEXP(DH7d_0,LOG),LOG),
                      DH8to9w= YLOG(YEXP(DH8w_0,LOG)+YEXP(DH9w_0,LOG),LOG),
                      DH8to9d= YLOG(YEXP(DH8d_0,LOG)+YEXP(DH9d_0,LOG),LOG))
    datDHa_1<- tibble(datT,
                      DH1w=DH1w_1, DH2w=DH2w_1, DH3w=DH3w_1, DH4w=DH4w_1, DH5w=DH5w_1,    #estimated
                      DH6w=DH6w_1, DH7w=DH7w_1, DH8w=DH8w_1, DH9w=DH9w_1,
                      DH1d=DH1d_1, DH2d=DH2d_1, DH3d=DH3d_1, DH4d=DH4d_1, DH5d=DH5d_1,    #data
                      DH6d=DH6d_1, DH7d=DH7d_1, DH8d=DH8d_1, DH9d=DH9d_1,
                      DH1to7w= YLOG(YEXP(DH1w_1,LOG)+YEXP(DH2w_1,LOG)+YEXP(DH3w_1,LOG)+YEXP(DH4w_1,LOG)+YEXP(DH5w_1,LOG)+YEXP(DH6w_1,LOG)+YEXP(DH7w_1,LOG),LOG),
                      DH1to7d= YLOG(YEXP(DH1d_1,LOG)+YEXP(DH2d_1,LOG)+YEXP(DH3d_1,LOG)+YEXP(DH4d_1,LOG)+YEXP(DH5d_1,LOG)+YEXP(DH6d_1,LOG)+YEXP(DH7d_1,LOG),LOG),
                      DH8to9w= YLOG(YEXP(DH8w_1,LOG)+YEXP(DH9w_1,LOG),LOG),
                      DH8to9d= YLOG(YEXP(DH8d_1,LOG)+YEXP(DH9d_1,LOG),LOG))
#DO
    datDOa_0<- tibble(datT,
                      DO1w=DO1w_0, DO2w=DO2w_0, DO3w=DO3w_0, DO4w=DO4w_0, DO5w=DO5w_0,    #estimated
                      DO6w=DO6w_0, DO7w=DO7w_0, DO8w=DO8w_0, DO9w=DO9w_0,
                      DO1d=DO1d_0, DO2d=DO2d_0, DO3d=DO3d_0, DO4d=DO4d_0, DO5d=DO5d_0,    #data
                      DO6d=DO6d_0, DO7d=DO7d_0, DO8d=DO8d_0, DO9d=DO9d_0,
                      DO1to7w= YLOG(YEXP(DO1w_0,LOG)+YEXP(DO2w_0,LOG)+YEXP(DO3w_0,LOG)+YEXP(DO4w_0,LOG)+YEXP(DO5w_0,LOG)+YEXP(DO6w_0,LOG)+YEXP(DO7w_0,LOG),LOG),
                      DO1to7d= YLOG(YEXP(DO1d_0,LOG)+YEXP(DO2d_0,LOG)+YEXP(DO3d_0,LOG)+YEXP(DO4d_0,LOG)+YEXP(DO5d_0,LOG)+YEXP(DO6d_0,LOG)+YEXP(DO7d_0,LOG),LOG),
                      DO8to9w= YLOG(YEXP(DO8w_0,LOG)+YEXP(DO9w_0,LOG),LOG),
                      DO8to9d= YLOG(YEXP(DO8d_0,LOG)+YEXP(DO9d_0,LOG),LOG))
    datDOa_1<- tibble(datT,
                      DO1w=DO1w_1, DO2w=DO2w_1, DO3w=DO3w_1, DO4w=DO4w_1, DO5w=DO5w_1,    #estimated
                      DO6w=DO6w_1, DO7w=DO7w_1, DO8w=DO8w_1, DO9w=DO9w_1,
                      DO1d=DO1d_1, DO2d=DO2d_1, DO3d=DO3d_1, DO4d=DO4d_1, DO5d=DO5d_1,    #data
                      DO6d=DO6d_1, DO7d=DO7d_1, DO8d=DO8d_1, DO9d=DO9d_1,
                      DO1to7w= YLOG(YEXP(DO1w_1,LOG)+YEXP(DO2w_1,LOG)+YEXP(DO3w_1,LOG)+YEXP(DO4w_1,LOG)+YEXP(DO5w_1,LOG)+YEXP(DO6w_1,LOG)+YEXP(DO7w_1,LOG),LOG),
                      DO1to7d= YLOG(YEXP(DO1d_1,LOG)+YEXP(DO2d_1,LOG)+YEXP(DO3d_1,LOG)+YEXP(DO4d_1,LOG)+YEXP(DO5d_1,LOG)+YEXP(DO6d_1,LOG)+YEXP(DO7d_1,LOG),LOG),
                      DO8to9w= YLOG(YEXP(DO8w_1,LOG)+YEXP(DO9w_1,LOG),LOG),
                      DO8to9d= YLOG(YEXP(DO8d_1,LOG)+YEXP(DO9d_1,LOG),LOG))
  ###### Plots - Age-profile dataframes ##################################################
  cat("Figures...", "\n")
 
      
  #### Diagnostics #######################################################################
  pdf(paste0(output_dir,"/","Results_diagnostics_",TODAY,".pdf"))

  #FigS7 marginal posterior densities
  par(mfrow = c(5, 4))
  par(mar = c(2, 4, 2, 1)) #c(bottom, left, top, right))
  for (i in c(1:17,19:21)) {
  coda::densplot( out$chain[,i], main = Pnamei[i], ylab="density", col="darkblue") } 

  #FigS8 Traces
  par(mfrow = c(6, 4))
  par(mar = c(1, 4, 2, 1)) #c(bottom, left, top, right))  
  for (i in c(1:16)) {
  coda::traceplot( out21$chain[,i], main = Pnamei[i], xlab = "iterations") }
  par(mar = c(1.2, 4, 2, 1)) 
  for (i in c(17,19:21)) { #skipping par 18 (kDH) so total 20 panels - simpler plot
  coda::traceplot( out21$chain[,i], main = Pnamei[i], xlab = "iterations") }
  for (i in 1:4){
  plot(0,type='n',axes=FALSE,ann=FALSE); mtext("Iterations", cex=0.75)} #makes xlab along lower row of matrix 

  #3 correlations
  par(mar = c(0,0,0,0))
  print(correlationPlot(out21$chain))
  
  dev.off()
  #### Diagnostics #######################################################################

  
  
  ### Results ############################################################################
  pdf(paste0(output_dir,"/","Results_figures_",TODAY,".pdf"))
  
  Title_0 = c("Non-shielding")
  Title_1 = c("Shielding")
  tags    = c("a","b","c","d","e","f","g","h","i","j")
  tagpos  = c(0.05,0.99)
  tagsize = 18  
  
  
    ##Fig2 posterior samples - overall
    ##
    colors <- c("Data" = 1,  "Median" = 2, "95% CrI" = "grey70", "95% PI" = "grey70")
    ynamsize = 11;
    ptsize   = 1.2; #default 1.5
	xlabsize = 8;   #default 11
	
	breaksx=seq(as.Date("2020-02-01"), as.Date("2020-12-01"), by ="1 months")
    date_labels = "%b" #month "%b-%d" # "%d/%m/%y"
	
    #H
    dzsample_0<- tibble(Date=Datessample, Datez=datH$Datesz, zsample05=zdsample95_0[,1],
                        zsample95=zdsample95_0[,3], zMAP=zdsample95_0[,2], zdw=datH$zdw_0)  
    dzsample_1<- tibble(Date=Datessample, Datez=datH$Datesz, zsample05=zdsample95_1[,1], 
                        zsample95=zdsample95_1[,3], zMAP=zdsample95_1[,2], zdw=datH$zdw_1)  
    Ymax=ceiling(max(zdsample95_0[,3])/1000)*1000;
   
    pz_0b<-ggplot(dzsample_0, aes(x=Date)) + 
	  ggtitle(Title_0) + 
      geom_ribbon(aes(ymin = zsample05, ymax = zsample95, color="95% PI"), fill = "grey70") + 
      geom_line (aes(x=Date,  y = zMAP,      color="Median")) +
      geom_point(aes(x=Datez, y = zdw,       color="Data"), size=ptsize) + 
      labs(x = "", y = 'Hospitalisations',   color = "") + 
      scale_color_manual(values = colors) +
	  scale_x_date(breaks= breaksx, date_labels= date_labels) + 
	  theme_bw() + 
	  theme(legend.position="none") +	  
      theme(axis.title = element_text(size = ynamsize)) +
	  theme(axis.text.x= element_text(size = xlabsize)) +	
	  labs(tag=tags[1]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos)
	  
    pz_1b<-ggplot(dzsample_1, aes(x=Date)) + 
	  ggtitle(Title_1) +
      geom_ribbon(aes(ymin = zsample05, ymax = zsample95, color="95% PI"), fill = "grey70") +
      geom_line (aes(x=Date,  y = zMAP,      color="Median")) +
      geom_point(aes(x=Datez, y = zdw,       color="Data"), size=ptsize) + 
      labs(x = "", y = "",   color = "") + 
      scale_color_manual(values = colors) +
	  scale_x_date(breaks= breaksx, date_labels= date_labels) +
	  theme_bw() + 
      theme(legend.title=element_blank(), 
	        legend.position = "inside", 
			legend.position.inside = c(0.55, 0.65))  + #x, y
      #theme(axis.title = element_text(size = ynamsize)) +  
	  theme(axis.text.x= element_text(size = xlabsize)) +	
      ylim(0,Ymax) +
	  labs(tag=tags[2]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos)

    #DH
    dwsample_0<- tibble(Date=Datessample, Datew=datD$Datesw, wsample05=wdsample95_0[,1], 
                        wsample95=wdsample95_0[,3], wMAP=wdsample95_0[,2], wdw=datD$wdw_0)
    dwsample_1<- tibble(Date=Datessample, Datew=datD$Datesw, wsample05=wdsample95_1[,1], 
                        wsample95=wdsample95_1[,3], wMAP=wdsample95_1[,2], wdw=datD$wdw_1)
    Ymax=ceiling(max(wdsample95_0[,3])/1000)*1000;
    Ymax=ceiling(max(wdsample95_0[,3])/500)*500
    tagpos2=tagpos; tagpos2[2]=1.05

    pw_0b <-ggplot(dwsample_0, aes(x=Date)) + 
      geom_ribbon(aes(ymin = wsample05, ymax = wsample95, color="95% PI"), fill = "grey70") +
      geom_point(aes(x=Datew, y = wdw,       color="Data"), size=ptsize) + 
      geom_line (aes(x=Date,  y = wMAP,      color="Median")) +
      labs(x = "", y = 'Deaths in hospital', color = "") + 
      scale_color_manual(values = colors) +
	  scale_x_date(breaks= breaksx, date_labels= date_labels) +
	  theme_bw() + 
	  theme(legend.position="none") +	  
      theme(axis.title = element_text(size = ynamsize)) +
	  theme(axis.text.x= element_text(size = xlabsize)) +	
      ylim(0,Ymax) +
	  labs(tag=tags[3]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2)
    
    pw_1b <-ggplot(dwsample_1, aes(x=Date)) + 
      geom_ribbon(aes(ymin = wsample05, ymax = wsample95, color="95% PI"), fill = "grey70") +
      geom_line (aes(x=Date,  y = wMAP,      color="Median")) +
      geom_point(aes(x=Datew, y = wdw,       color="Data"), size=ptsize) + 
      labs(x = "", y = "", color = "") + 
      scale_color_manual(values = colors) +
	  scale_x_date(breaks= breaksx, date_labels= date_labels) +
	  theme_bw() + 
	  theme(legend.position="none") +	  
      #theme(axis.title = element_text(size = ynamsize))
	  theme(axis.text.x= element_text(size = xlabsize)) +	
      ylim(0,Ymax) +
	  labs(tag=tags[4]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2)
	  
    #DO
    dvsample_0<- tibble(Date=Datessample, Datev=datD$Datesv, vsample05=vdsample95_0[,1], 
                        vsample95=vdsample95_0[,3], vMAP=vdsample95_0[,2], vdw=datD$vdw_0)
    dvsample_1<- tibble(Date=Datessample, Datev=datD$Datesv, vsample05=vdsample95_1[,1], 
                        vsample95=vdsample95_1[,3], vMAP=vdsample95_1[,2], vdw=datD$vdw_1)
    Ymax=ceiling(max(vdsample95_0[,3])/1000)*1000;
	tagpos2=tagpos; tagpos2[2]=1.05

    pv_0b <-ggplot(dvsample_0, aes(x=Date)) + 
      geom_ribbon(aes(ymin = vsample05, ymax = vsample95, color="95% PI"), fill = "grey70") +
      geom_line (aes(x=Date,  y = vMAP,      color="Median")) +
      geom_point(aes(x=Datev, y = vdw,       color="Data"), size=ptsize) + 
      labs(x = '', y = 'Deaths outside hospital', color = "") + 
      scale_color_manual(values = colors) +
	  scale_x_date(breaks= breaksx, date_labels= date_labels) +
	  theme_bw() + 
	  theme(legend.position="none") +	  
      theme(axis.title = element_text(size = ynamsize)) +
	  theme(axis.text.x= element_text(size = xlabsize)) +	
      ylim(0,Ymax) +
	  labs(tag=tags[5]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2)
    
    pv_1b <-ggplot(dvsample_1, aes(x=Date)) + 
      geom_ribbon(aes(ymin = vsample05, ymax = vsample95, color="95% PI"), fill = "grey70") +
      geom_line (aes(x=Date,  y = vMAP,      color="Median")) +
      geom_point(aes(x=Datev, y = vdw,       color="Data"), size=ptsize) + 
      labs(x = '', y = '', color = "") + 
      scale_color_manual(values = colors) +
	  scale_x_date(breaks= breaksx, date_labels= date_labels) +
	  theme_bw() + 
	  theme(legend.position="none") +	  
      #theme(axis.title = element_text(size = ynamsize)) +
	  theme(axis.text.x= element_text(size = xlabsize)) +	
      ylim(0,Ymax) +
	  labs(tag=tags[6]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2)
     
    gridExtra::grid.arrange(pz_0b, pz_1b, pw_0b, pw_1b, pv_0b, pv_1b, ncol = 2)


    
  ##FigS9 age profiles - log10
    colors <- c("0-4" = 1, "05-11" = 2,  "12-17" = 3, "18-29" = 4, "30-39" = 5, 
                "40-49" = 6, "50-59" = 7,  "60-69" = 8, "70+" = 9)
    Yname = c('Hospitalisations', 'Deaths in hospital', 'Deaths outside hospital')
    ynamsize= 10;
	xlabsize= 8;  #default 11

    #H
    Ymax=round(max(datHa_0$H9d)*1.05,1); 
    pH_0 <- ggplot() +
	  ggtitle(Title_0) +
      labs(x = "", y = Yname[1], color = "") + 
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
	  theme(axis.title = element_text(size = ynamsize)) +  
	  theme(axis.text.x= element_text(size = xlabsize)) +
	  theme(legend.position="none") +	  
      geom_line (data=datHa_0, aes(x=Dates,y=H1w, color = "0-4")) +
      geom_line (data=datHa_0, aes(x=Dates,y=H2w, color = "05-11")) +
      geom_line (data=datHa_0, aes(x=Dates,y=H3w, color = "12-17")) +
      geom_line (data=datHa_0, aes(x=Dates,y=H4w, color = "18-29")) +
      geom_line (data=datHa_0, aes(x=Dates,y=H5w, color = "30-39")) +
      geom_line (data=datHa_0, aes(x=Dates,y=H6w, color = "40-49")) +
      geom_line (data=datHa_0, aes(x=Dates,y=H7w, color = "50-59")) +
      geom_line (data=datHa_0, aes(x=Dates,y=H8w, color = "60-69")) +
      geom_line (data=datHa_0, aes(x=Dates,y=H9w, color = "70+")) +
      geom_point(data=datHa_0, aes(x=Dates,y=H1d, color = "0-4"),   size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=H2d, color = "05-11"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=H3d, color = "12-17"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=H4d, color = "18-29"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=H5d, color = "30-39"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=H6d, color = "40-49"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=H7d, color = "50-59"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=H8d, color = "60-69"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=H9d, color = "70+"),   size=ptsize) +
	  labs(tag=tags[1]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos) +
      scale_y_continuous(breaks=c(0, 1, 2, 3, 4), labels=c('1', '10', '100', '1000', '10000'), limits=c(0,Ymax))
	  
    pH_1 <- ggplot() +
	  ggtitle(Title_1) +
      labs(x = "", y = "", color = "") + 
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      #theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +
	  theme(legend.position="none") +	  
      geom_line (data=datHa_1, aes(x=Dates,y=H1w, color = "0-4")) +
      geom_line (data=datHa_1, aes(x=Dates,y=H2w, color = "05-11")) +
      geom_line (data=datHa_1, aes(x=Dates,y=H3w, color = "12-17")) +
      geom_line (data=datHa_1, aes(x=Dates,y=H4w, color = "18-29")) +
      geom_line (data=datHa_1, aes(x=Dates,y=H5w, color = "30-39")) +
      geom_line (data=datHa_1, aes(x=Dates,y=H6w, color = "40-49")) +
      geom_line (data=datHa_1, aes(x=Dates,y=H7w, color = "50-59")) +
      geom_line (data=datHa_1, aes(x=Dates,y=H8w, color = "60-69")) +
      geom_line (data=datHa_1, aes(x=Dates,y=H9w, color = "70+")) +
      geom_point(data=datHa_1, aes(x=Dates,y=H1d, color = "0-4"),   size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=H2d, color = "05-11"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=H3d, color = "12-17"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=H4d, color = "18-29"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=H5d, color = "30-39"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=H6d, color = "40-49"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=H7d, color = "50-59"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=H8d, color = "60-69"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=H9d, color = "70+"),   size=ptsize) + 
	  labs(tag=tags[2]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos) +
      scale_y_continuous(breaks=c(0, 1, 2, 3, 4), labels=c('1', '10', '100', '1000', '10000'), limits=c(0,Ymax))
	  
    #DH
    Ymax=round(max(datDHa_0$DH9d)*1.055,2);
    pDH_0 <- ggplot() +
      labs(x = "", y = Yname[2], color = "Age group") + 
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +
	  theme(legend.position="none") +	  
      geom_line (data=datDHa_0, aes(x=Dates,y=DH1w, color = "0-4")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=DH2w, color = "05-11")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=DH3w, color = "12-17")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=DH4w, color = "18-29")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=DH5w, color = "30-39")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=DH6w, color = "40-49")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=DH7w, color = "50-59")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=DH8w, color = "60-69")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=DH9w, color = "70+")) +
      geom_point(data=datDHa_0, aes(x=Dates,y=DH1d, color = "0-4"),   size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=DH2d, color = "05-11"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=DH3d, color = "12-17"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=DH4d, color = "18-29"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=DH5d, color = "30-39"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=DH6d, color = "40-49"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=DH7d, color = "50-59"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=DH8d, color = "60-69"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=DH9d, color = "70+"),   size=ptsize) +
	  labs(tag=tags[3]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos) +
      scale_y_continuous(breaks=c(0, 1, 2, 3, 4), labels=c('1', '10', '100', '1000', '10000'), limits=c(0,Ymax))
	  
    pDH_1 <- ggplot() +
      labs(x = "", y = "", color = "Age group") + 
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      #theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +
	  theme(legend.position="none") +	  
      geom_line (data=datDHa_1, aes(x=Dates,y=DH1w, color = "0-4")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=DH2w, color = "05-11")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=DH3w, color = "12-17")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=DH4w, color = "18-29")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=DH5w, color = "30-39")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=DH6w, color = "40-49")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=DH7w, color = "50-59")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=DH8w, color = "60-69")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=DH9w, color = "70+")) +
      geom_point(data=datDHa_1, aes(x=Dates,y=DH1d, color = "0-4"),   size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=DH2d, color = "05-11"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=DH3d, color = "12-17"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=DH4d, color = "18-29"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=DH5d, color = "30-39"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=DH6d, color = "40-49"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=DH7d, color = "50-59"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=DH8d, color = "60-69"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=DH9d, color = "70+"),   size=ptsize) +
	  labs(tag=tags[4]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos) +
      scale_y_continuous(breaks=c(0, 1, 2, 3, 4), labels=c('1', '10', '100', '1000', '10000'), limits=c(0,Ymax))
	  
    #DO
    Ymax=round(max(datDOa_0$DO9d)*1.055,2)
	tagpos2=tagpos; tagpos2[2]=1.05
    pDO_0 <- ggplot() +
      labs(x = "", y = Yname[3], color = "") + 
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +
	  theme(legend.position="none") +	  
      geom_line (data=datDOa_0, aes(x=Dates,y=DO1w, color = "0-4")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=DO2w, color = "05-11")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=DO3w, color = "12-17")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=DO4w, color = "18-29")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=DO5w, color = "30-39")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=DO6w, color = "40-49")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=DO7w, color = "50-59")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=DO8w, color = "60-69")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=DO9w, color = "70+")) +
      geom_point(data=datDOa_0, aes(x=Dates,y=DO1d, color = "0-4"),   size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=DO2d, color = "05-11"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=DO3d, color = "12-17"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=DO4d, color = "18-29"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=DO5d, color = "30-39"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=DO6d, color = "40-49"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=DO7d, color = "50-59"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=DO8d, color = "60-69"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=DO9d, color = "70+"),   size=ptsize) +
	  labs(tag=tags[5]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2) +  
      scale_y_continuous(breaks=c(0, 1, 2, 3, 4), labels=c('1', '10', '100', '1000', '10000'), limits=c(0,Ymax))
  
    pDO_1 <- ggplot() +
      labs(x = "", y = "", color = "") +
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      #theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +
	  #theme(legend.position="none") +	 
	  theme(legend.position = "inside", legend.position.inside = c(0.9, 0.55)) + #x, y
      theme(legend.title       = element_blank(),
            legend.text        = element_text(size = 8),   
            legend.key.size = unit(0.7, 'lines'))       +  
      geom_line (data=datDOa_1, aes(x=Dates,y=DO1w, color = "0-4")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=DO2w, color = "05-11")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=DO3w, color = "12-17")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=DO4w, color = "18-29")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=DO5w, color = "30-39")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=DO6w, color = "40-49")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=DO7w, color = "50-59")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=DO8w, color = "60-69")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=DO9w, color = "70+")) +
      geom_point(data=datDOa_1, aes(x=Dates,y=DO1d, color = "0-4"),   size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=DO2d, color = "05-11"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=DO3d, color = "12-17"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=DO4d, color = "18-29"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=DO5d, color = "30-39"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=DO6d, color = "40-49"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=DO7d, color = "50-59"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=DO8d, color = "60-69"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=DO9d, color = "70+"),   size=ptsize) +
	  labs(tag=tags[6]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2) + 
      scale_y_continuous(breaks=c(0, 1, 2, 3, 4), labels=c('1', '10', '100', '1000', '10000'), limits=c(0,Ymax))  
	  
	gridExtra::grid.arrange(pH_0, pH_1, pDH_0, pDH_1, pDO_0, pDO_1, nrow = 3, ncol = 2)
    

    ##Fig3 age profiles - age partition 0-59 & 60+ - log
    ##    
    colors <- c("Data 0-59" = 7,  "Data 60+" = 1, "Model 0-59" = "grey40", "Model 60+" = "grey70")
    Yname = c('Hospitalisations', 'Deaths in hospital', 'Deaths outside hospital')
    ynamsize= 10;  
	xlabsize= 8;  #default 11

    #H
    datHa2_0 <- tibble(datHa_0,a05=YLOG(zdsampleA95_0[,1],LOG),a95=YLOG(zdsampleA95_0[,3],LOG),b05=YLOG(zdsampleB95_0[,1],LOG),b95=YLOG(zdsampleB95_0[,3],LOG))
    datHa2_1 <- tibble(datHa_1,a05=YLOG(zdsampleA95_1[,1],LOG),a95=YLOG(zdsampleA95_1[,3],LOG),b05=YLOG(zdsampleB95_1[,1],LOG),b95=YLOG(zdsampleB95_1[,3],LOG))
    Ymax=round((max(YLOG(zdsampleB95_0[,3],LOG))*1.05),1);
	
    pH_0 <- ggplot() +
	  ggtitle(Title_0) +
      labs(x = "", y = Yname[1], color = "") +  
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +
      theme(legend.position="none") +	
      geom_ribbon(data=datHa2_0, aes(x=Dates, ymin = a05, ymax = a95, color = "Model 0-59"), fill = "grey40", alpha=0.7) +
      geom_ribbon(data=datHa2_0, aes(x=Dates, ymin = b05, ymax = b95, color = "Model 60+"),  fill = "grey70", alpha=0.7) +
      geom_line  (data=datHa2_0, aes(x=Dates,y=H1to7w, color = "Data 0-59")) +  
      geom_line  (data=datHa2_0, aes(x=Dates,y=H8to9w, color = "Data 60+"))  +
      geom_point (data=datHa2_0, aes(x=Dates,y=H1to7d, color = "Data 0-59"), size=ptsize) +
      geom_point (data=datHa2_0, aes(x=Dates,y=H8to9d, color = "Data 60+"),  size=ptsize) +
	  labs(tag=tags[1]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos) +
      scale_y_continuous(breaks=c(0, 1, 2, 3, 4), labels=c('1', '10', '100', '1000', '10000'), limits=c(0,Ymax))

    pH_1 <- ggplot() +
	  ggtitle(Title_1) +
      labs(x = "", y = "", color = "") +  
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      #theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +
      theme(legend.position="none") +	
      geom_ribbon(data=datHa2_1, aes(x=Dates, ymin = a05, ymax = a95, color = "Model 0-59"), fill = "grey40", alpha=0.7) +
      geom_ribbon(data=datHa2_1, aes(x=Dates, ymin = b05, ymax = b95, color = "Model 60+"),  fill = "grey70", alpha=0.7) +
      geom_line  (data=datHa2_1, aes(x=Dates,y=H1to7w, color = "Data 0-59")) +  
      geom_line  (data=datHa2_1, aes(x=Dates,y=H8to9w, color = "Data 60+"))  +
      geom_point (data=datHa2_1, aes(x=Dates,y=H1to7d, color = "Data 0-59"), size=ptsize) + 
      geom_point (data=datHa2_1, aes(x=Dates,y=H8to9d, color = "Data 60+"),  size=ptsize) +
	  labs(tag=tags[2]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos) +
      scale_y_continuous(breaks=c(0, 1, 2, 3, 4), labels=c('1', '10', '100', '1000', '10000'), limits=c(0,Ymax))

    #DH
    datDHa2_0 <- tibble(datDHa_0,a05=YLOG(wdsampleA95_0[,1],LOG),a95=YLOG(wdsampleA95_0[,3],LOG),b05=YLOG(wdsampleB95_0[,1],LOG),b95=YLOG(wdsampleB95_0[,3],LOG))
    datDHa2_1 <- tibble(datDHa_1,a05=YLOG(wdsampleA95_1[,1],LOG),a95=YLOG(wdsampleA95_1[,3],LOG),b05=YLOG(wdsampleB95_1[,1],LOG),b95=YLOG(wdsampleB95_1[,3],LOG))
    Ymax=round((max(YLOG(wdsampleB95_0[,3],LOG))*1.05),1);
	tagpos2=tagpos; tagpos2[2]=1.05

    pDH_0 <- ggplot() +
      labs(x = "", y = Yname[2], color = "Age group") +  
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +
      theme(legend.position="none") +	
      geom_ribbon(data=datDHa2_0, aes(x=Dates, ymin = a05, ymax = a95, color = "Model 0-59"), fill = "grey40", alpha=0.7) +
      geom_ribbon(data=datDHa2_0, aes(x=Dates, ymin = b05, ymax = b95, color = "Model 60+"),  fill = "grey70", alpha=0.7) +
      geom_line  (data=datDHa2_0, aes(x=Dates,y=DH1to7w, color = "Data 0-59")) +  
      geom_line  (data=datDHa2_0, aes(x=Dates,y=DH8to9w, color = "Data 60+"))  +
      geom_point (data=datDHa2_0, aes(x=Dates,y=DH1to7d, color = "Data 0-59"), size=ptsize) +
      geom_point (data=datDHa2_0, aes(x=Dates,y=DH8to9d, color = "Data 60+"),  size=ptsize) +
	  labs(tag=tags[3]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2) +  
      scale_y_continuous(breaks=c(0, 1, 2, 3, 4), labels=c('1', '10', '100', '1000', '10000'), limits=c(0,Ymax))

    pDH_1 <- ggplot() +
      labs(x = "", y = "", color = "Age group") +  
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      #theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +
      theme(legend.position="none") +	
      geom_ribbon(data=datDHa2_1, aes(x=Dates, ymin = a05, ymax = a95, color = "Model 0-59"), fill = "grey40", alpha=0.7) +
      geom_ribbon(data=datDHa2_1, aes(x=Dates, ymin = b05, ymax = b95, color = "Model 60+"),  fill = "grey70", alpha=0.7) +
      geom_line  (data=datDHa2_1, aes(x=Dates,y=DH1to7w, color = "Data 0-59")) +  
      geom_line  (data=datDHa2_1, aes(x=Dates,y=DH8to9w, color = "Data 60+"))  +
      geom_point (data=datDHa2_1, aes(x=Dates,y=DH1to7d, color = "Data 0-59"), size=ptsize) +
      geom_point (data=datDHa2_1, aes(x=Dates,y=DH8to9d, color = "Data 60+"),  size=ptsize) +
	  labs(tag=tags[4]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2) + 
      scale_y_continuous(breaks=c(0, 1, 2, 3, 4), labels=c('1', '10', '100', '1000', '10000'), limits=c(0,Ymax))
    
    #DO
    datDOa2_0 <- tibble(datDOa_0,a05=YLOG(vdsampleA95_0[,1],LOG),a95=YLOG(vdsampleA95_0[,3],LOG),b05=YLOG(vdsampleB95_0[,1],LOG),b95=YLOG(vdsampleB95_0[,3],LOG))
    datDOa2_1 <- tibble(datDOa_1,a05=YLOG(vdsampleA95_1[,1],LOG),a95=YLOG(vdsampleA95_1[,3],LOG),b05=YLOG(vdsampleB95_1[,1],LOG),b95=YLOG(vdsampleB95_1[,3],LOG))
    Ymax=round((max(YLOG(vdsampleB95_0[,3],LOG))*1.05),1);

    pDO_0 <- ggplot() +
      labs(x = "", y = Yname[3], color = "") +  
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +
      theme(legend.position="none") +
      geom_ribbon(data=datDOa2_0, aes(x=Dates, ymin = a05, ymax = a95, color = "Model 0-59"), fill = "grey40", alpha=0.7) +
      geom_ribbon(data=datDOa2_0, aes(x=Dates, ymin = b05, ymax = b95, color = "Model 60+"),  fill = "grey70", alpha=0.7) +
      geom_line  (data=datDOa2_0, aes(x=Dates,y=DO1to7w, color = "Data 0-59")) + 
      geom_line  (data=datDOa2_0, aes(x=Dates,y=DO8to9w, color = "Data 60+"))  +
      geom_point (data=datDOa2_0, aes(x=Dates,y=DO1to7d, color = "Data 0-59"), size=ptsize) +
      geom_point (data=datDOa2_0, aes(x=Dates,y=DO8to9d, color = "Data 60+"),  size=ptsize) +
	  labs(tag=tags[5]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2) + 
      scale_y_continuous(breaks=c(0, 1, 2, 3, 4), labels=c('1', '10', '100', '1000', '10000'), limits=c(0,Ymax))

    pDO_1 <- ggplot() +
      labs(x = "", y = "", color = "") + 
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      #theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +
   	  #theme(legend.position="none") +	 
	  theme(legend.position = "inside", legend.position.inside = c(0.62, 0.78)) +  #x, y
      theme(legend.title       = element_blank(),          
            legend.text        = element_text(size = 8),   
            legend.key.size = unit(0.7, 'lines'))      +   
      geom_ribbon(data=datDOa2_1, aes(x=Dates, ymin = a05, ymax = a95, color = "Model 0-59"), fill = "grey40", alpha=0.7) +
      geom_ribbon(data=datDOa2_1, aes(x=Dates, ymin = b05, ymax = b95, color = "Model 60+"),  fill = "grey70", alpha=0.7) +
      geom_line  (data=datDOa2_1, aes(x=Dates,y=DO1to7w, color = "Model 0-59")) + 
      geom_line  (data=datDOa2_1, aes(x=Dates,y=DO8to9w, color = "Model 60+"))  +
      geom_line  (data=datDOa2_1, aes(x=Dates,y=DO1to7w, color = "Data 0-59"))  + 
      geom_line  (data=datDOa2_1, aes(x=Dates,y=DO8to9w, color = "Data 60+"))   +
      geom_point (data=datDOa2_1, aes(x=Dates,y=DO1to7d, color = "Data 0-59"), size=ptsize) +
      geom_point (data=datDOa2_1, aes(x=Dates,y=DO8to9d, color = "Data 60+"),  size=ptsize) +
	  labs(tag=tags[6]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2) + 
      scale_y_continuous(breaks=c(0, 1, 2, 3, 4), labels=c('1', '10', '100', '1000', '10000'), limits=c(0,Ymax))
    
	gridExtra::grid.arrange(pH_0, pH_1, pDH_0, pDH_1, pDO_0, pDO_1, nrow = 3, ncol = 2)
       

 
    ##Fig5 Positivity
	##FigS11 R0
	##Prevalence
    ##
    colors <- c("Contacts" = 1, "ONS CIS" = 1,  "Model" = 2, "Median" = 2, "95% CrI" = "grey70")
    #breaksx=seq(as.Date("2020-02-01"), as.Date("2020-12-01"), by ="1 months")
    #date_labels = "%b" #month "%b-%d" # "%d/%m/%y"

    ### Plot positivity
    posi_data_plot <- vector() #same length as model
    posi_data_plot = (posi_data_perc/100) 
    posi_datu_plot = (posi_datu_perc/100)
    posi_datd_plot = (posi_datd_perc/100)
    Datessample_posi = Datessample[idata_posi + shift_posi]
 
    dcsample <- tibble(Date=Datessample,      cMAP = 100*csample95[,2],    c05= 100*cdsample95[,1],   c95= 100*cdsample95[,3]) 
    dcsample2<- tibble(Date=Datessample_posi, cdata= 100*posi_data_plot, cdatu= 100*posi_datu_plot, cdatd= 100*posi_datd_plot)
    pc <-ggplot() + 
      geom_ribbon(data=dcsample,  aes(x=Date, ymin = c05, ymax = c95), fill = "grey70")  +  
      geom_line (data=dcsample,  aes(x=Date, y = cMAP,    color="Model"), linewidth=1.1) +
      geom_point(data=dcsample2,    aes(x=Date, y = cdata,   color="ONS CIS"), size=0.9) +
      geom_errorbar(data=dcsample2, aes(x=Date, ymin=cdata-cdatd, ymax=cdata+cdatu), color=1) +
      labs(x = '', y = 'Positivity estimate (%)', color = "") +
      scale_color_manual(values = colors) +
      scale_x_date(breaks = breaksx, date_labels = date_labels) + 
      theme_bw() +
      theme(axis.title = element_text(size = 12)) +
      theme(legend.title    = element_blank(), 
	        legend.position = "inside", 
			legend.position.inside = c(0.9, 0.85),  #x, y  
            legend.text     = element_text(size = 10),
            legend.key.size = unit(1.0, 'lines'))
			
    #R0
    drsample <- tibble(Date=Datessample, R0sample05=R0weeksample95[,1], R0sample95=R0weeksample95[,3],
                       R0MAP=R0weeksample95[,2] )
    pR0 <-ggplot(drsample, aes(x=Date)) + 
      geom_ribbon(aes(ymin = R0sample05, ymax = R0sample95, color="95% CrI"), fill = "grey70") +
      geom_line (aes(x=Date, y = R0MAP,  color="Median"), linewidth=1.2) +
      labs(x = '', y = 'R0 estimate', color = "") +
      scale_color_manual(values = colors) +
      scale_x_date(breaks = breaksx, date_labels = date_labels) + 
      theme_bw() +
      theme(axis.title = element_text(size = 12)) + 
      theme(axis.text.x= element_text(size = 8)) +
      theme(legend.title    = element_blank(), 
	        legend.position = "inside", 
			legend.position.inside = c(0.5, 0.9),  #x, y  
            legend.text     = element_text(size = 10),
            legend.key.size = unit(1.0, 'lines'))  +
	  ylim(c(0,4))
    
    ### prevalence
    dcsample <- tibble(Date=Datessample, p05=Psample95[,1], p95=Psample95[,3], pMAP = Psample95[,2])
    pp  <-ggplot(dcsample, aes(x=Date)) + 
      geom_ribbon(aes(ymin = p05, ymax = p95, color="95% CrI"), fill = "grey70") +
      geom_line (aes(x=Date, y = pMAP,    color="Median")) +
      labs(x = '', y = 'Prevalence estimate', color = "")  +
      scale_color_manual(values = colors) +
      scale_x_date(breaks = breaksx, date_labels = date_labels) + 
      theme_bw() +
      theme(axis.title = element_text(size = 12))
	

    gridExtra::grid.arrange(pc, nrow = 2, ncol = 1)
    gridExtra::grid.arrange(pR0, nrow = 2, ncol = 2)
    


    ##FigS10 probs
    ##
	## dataframes
	##
    Age <- c("0-4", "05-11",  "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70+")
    #y - clinical probability  
    DFy_0 <- tibble(Age=Age, s05=ysample95_0[,1],  s95=ysample95_0[,3],  MAP=ysample95_0[,2])   
    DFy_1 <- tibble(Age=Age, s05=ysample95_1[,1],  s95=ysample95_1[,3],  MAP=ysample95_1[,2])
    #h - hospitalisation probability
    DFh_0 <- tibble(Age=Age, s05=hsample95_0[,1],  s95=hsample95_0[,3],  MAP=hsample95_0[,2])  
    DFh_1 <- tibble(Age=Age, s05=hsample95_1[,1],  s95=hsample95_1[,3],  MAP=hsample95_1[,2]) 
    #yh - infection hospitalisation ratio IHR - more identifiable than y or h alone
    DFyh_0 <-tibble(Age=Age, s05=yhsample95_0[,1], s95=yhsample95_0[,3], MAP=yhsample95_0[,2]) 
    DFyh_1 <-tibble(Age=Age, s05=yhsample95_1[,1], s95=yhsample95_1[,3], MAP=yhsample95_1[,2])
    #d - death outside hospital probability
    DFd_0 <- tibble(Age=Age, s05=dsample95_0[,1],  s95=dsample95_0[,3],  MAP=dsample95_0[,2])
    DFd_1 <- tibble(Age=Age, s05=dsample95_1[,1],  s95=dsample95_1[,3],  MAP=dsample95_1[,2]) 
    #m - death in hospital probability
    DFm_0 <- tibble(Age=Age, DATa=pars$ma_0, DATb=pars$mb_0, DAT=pars$m_0)
    DFm_1 <- tibble(Age=Age, DATa=pars$ma_1, DATb=pars$mb_1, DAT=pars$m_1)

    ## settings
    colors <- c("Lit" = 1,  "Data" = 1,  "Median" = 2, "95% CrI" = "grey70", "Period 1" = 1, "Period 2" = 2)
    ynamsize=9  
    xlabsize=7  
    ylabsize=9  
    Yname[1:5] = c('Pr. clinical', 'Pr. hosp | clinical', 'Pr. hosp', 'Pr. death out', 'Pr. death in')

    #y - clinical probability  
    py_0<-ggplot(data=DFy_0, aes(x=Age, group=1)) +   #"group=1" because x var is character/factor
      geom_ribbon(aes(ymin = s05, ymax = s95, color="95% CrI"), fill = "grey70") +
      geom_line (aes( y = MAP,      color="Median")) +
	  ggtitle(Title_0) +
      labs(x = "", y = Yname[1],   color = "") +
      scale_color_manual(values = colors) + ylim(0, 1) +
	  theme(legend.position="none") +
      theme(axis.title = element_text(size = ynamsize)) + 
      theme(axis.text.x= element_text(size = xlabsize))   +
      theme(axis.text.y= element_text(size = ylabsize))   +
	  labs(tag=tags[1]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos)
	  
    py_1<-ggplot(data=DFy_1, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95, color="95% CrI"), fill = "grey70") +
      geom_line (aes( y = MAP,      color="Median")) +
	  ggtitle(Title_1) +
      labs(x = "", y = "",   color = "") +
      scale_color_manual(values = colors) + ylim(0, 1) + 
      #theme(legend.position="none") +
	  theme(legend.direction   ="horizontal",
			legend.position    = "inside", legend.position.inside = c(0.45, 0.85)) + #x, y
      theme(legend.title       = element_text(size = 9),   
            legend.text        = element_text(size = 9),   
            legend.key.size    = unit(0.8, 'lines'))    +  
      theme(axis.title = element_text(size = ynamsize)) +
      theme(axis.text.x= element_text(size = xlabsize))   +
      theme(axis.text.y= element_text(size = ylabsize))   +
	  labs(tag=tags[2]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos)
    
    #h - hospitalisation probability
    tagpos2=tagpos; tagpos2[2]=1.1  

    ph_0 <-ggplot(data=DFh_0, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95, color="95% CrI"), fill = "grey70") +
      geom_line (aes( y = MAP,      color="Median")) +
      labs(x = "", y = Yname[2],   color = "") + 
      scale_color_manual(values = colors) + ylim(0, 1) +
	  theme(legend.position="none") +
      theme(axis.title = element_text(size = ynamsize)) +  
      theme(axis.text.x= element_text(size = xlabsize))   +
      theme(axis.text.y= element_text(size = ylabsize))   +
	  labs(tag=tags[3]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2)
	
    ph_1<-ggplot(data=DFh_1, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95, color="95% CrI"), fill = "grey70") +
      geom_line (aes( y = MAP,      color="Median")) +
      labs(x = "", y = "",   color = "") +
      scale_color_manual(values = colors) + ylim(0, 1) + 
      theme(legend.position="none") +
      theme(axis.title = element_text(size = ynamsize)) + 
      theme(axis.text.x= element_text(size = xlabsize))   +
      theme(axis.text.y= element_text(size = ylabsize))   +
	  labs(tag=tags[4]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2) 

    #yh - infection hospitalisation ratio IHR - more identifiable than y or h alone
    pyh_0<-ggplot(data=DFyh_0, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95, color="95% CrI"), fill = "grey70") +
      geom_line (aes( y = MAP,      color="Median")) +
      labs(x = "", y = Yname[3],   color = "") +
      scale_color_manual(values = colors) + ylim(0, 0.5) + 
	  theme(legend.position="none") +
      theme(axis.title = element_text(size = ynamsize)) +
      theme(axis.text.x= element_text(size = xlabsize))   +
      theme(axis.text.y= element_text(size = ylabsize))   +
	  labs(tag=tags[5]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2)
	  
    pyh_1<-ggplot(data=DFyh_1, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95, color="95% CrI"), fill = "grey70") +
      geom_line (aes( y = MAP,      color="Median")) +
      labs(x = "", y = "",   color = "") + 
      scale_color_manual(values = colors) + ylim(0, 0.5) + 
      theme(legend.position="none") +
      theme(axis.title = element_text(size = ynamsize)) + 
      theme(axis.text.x= element_text(size = xlabsize))   +
      theme(axis.text.y= element_text(size = ylabsize))   +
	  labs(tag=tags[6]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2) 
    
    #d - death outside hospital probability
    pd_0<-ggplot(data=DFd_0, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95, color="95% CrI"), fill = "grey70") +
      geom_line (aes( y = MAP,      color="Median")) +
      labs(x = "", y = Yname[4],   color = "") +
      scale_color_manual(values = colors) + ylim(0, 0.5) +
	  theme(legend.position="none") +
      theme(axis.title = element_text(size = ynamsize)) +
      theme(axis.text.x= element_text(size=xlabsize))   +
      theme(axis.text.y= element_text(size=ylabsize))   +
	  labs(tag=tags[7]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2)
	  
    pd_1<-ggplot(data=DFd_1, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95), fill = "grey70") +
      geom_line (aes( y = MAP,      color="Median")) +
      labs(x = "", y = "",   color = "") +
      scale_color_manual(values = colors) + ylim(0, 0.5) + 
      theme(legend.position="none") +
      theme(axis.title = element_text(size = ynamsize)) +
      theme(axis.text.x= element_text(size = xlabsize))   +
      theme(axis.text.y= element_text(size = ylabsize))   +
	  labs(tag=tags[8]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2)

    #m - death in hospital probability
    pm_0<-ggplot(data=DFm_0, aes(x=Age, group=1)) + 
      geom_point(aes(x=Age,  y = DATa,     color="Period 1")) +
      geom_point(aes(x=Age,  y = DATb,     color="Period 2")) +
      geom_line (aes(x=Age,  y = DATa,     color="Period 1")) +
      geom_line (aes(x=Age,  y = DATb,     color="Period 2")) +
      labs(x = "", y = Yname[5],   color = "") + 
      scale_color_manual(values = colors) + ylim(0, 0.52) +
	  theme(legend.position="none") +
      theme(axis.title = element_text(size = ynamsize)) + 
      theme(axis.text.x= element_text(size = xlabsize))   +
      theme(axis.text.y= element_text(size = ylabsize))   +
	  labs(tag=tags[9]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2) 
	  
    pm_1<-ggplot(data=DFm_1, aes(x=Age, group=1)) + 
      geom_point(aes(x=Age,  y = DATa,     color="Period 1")) +
      geom_point(aes(x=Age,  y = DATb,     color="Period 2")) +
      geom_line (aes(x=Age,  y = DATa,     color="Period 1")) +
      geom_line (aes(x=Age,  y = DATb,     color="Period 2")) +
      labs(x = "", y = "",   color = "") + 
      scale_color_manual(values = colors) + ylim(0, 0.52) +
      #theme(legend.position="none") +
	  theme(legend.direction   ="horizontal",
			legend.position    = "inside", legend.position.inside = c(0.45, 0.85)) +  #x, y
      theme(legend.title       = element_text(size = 9),   
            legend.text        = element_text(size = 9),   
            legend.key.size    = unit(0.8, 'lines'))    +  
      theme(axis.title = element_text(size = ynamsize)) + 
      theme(axis.text.x= element_text(size = xlabsize))   +
      theme(axis.text.y= element_text(size = ylabsize))   +
	  labs(tag=tags[10]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos2)

	gridExtra::grid.arrange(py_0, py_1, ph_0, ph_1, pyh_0, pyh_1, pd_0, pd_1, pm_0, pm_1, ncol = 2)


	##Figure data output (can be read by FigS10_readdata.r) 
	WRITE=0#1
	if (WRITE==1){ #write S10
	options(scipen=999) #turn off sci notation
	DF_S10 <- list()
	#prevent csv changing strings to dates
	Age2 <- c("'0-4", "'05-11",  "'12-17", "'18-29", "'30-39", "'40-49", "'50-59", "'60-69", "'70+")
	DFy_0 ["Age"]<- Age2
	DFy_1 ["Age"]<- Age2
	DFh_0 ["Age"]<- Age2
	DFh_1 ["Age"]<- Age2
	DFyh_0["Age"]<- Age2
	DFyh_1["Age"]<- Age2
	DFd_0 ["Age"]<- Age2
	DFd_1 ["Age"]<- Age2
	DFm_0 ["Age"]<- Age2
	DFm_1 ["Age"]<- Age2
	DF_S10[[1]]  <- DFy_0
	DF_S10[[2]]  <- DFy_1
	DF_S10[[3]]  <- DFh_0
	DF_S10[[4]]  <- DFh_1
	DF_S10[[5]]  <- DFyh_0
	DF_S10[[6]]  <- DFyh_1
	DF_S10[[7]]  <- DFd_0
	DF_S10[[8]]  <- DFd_1
	DF_S10[[9]]  <- DFm_0
	DF_S10[[10]] <- DFm_1
	names(DF_S10) <- c("DFy_0", "DFy_1", "DFh_0", "DFh_1", "DFyh_0", "DFyh_1", "DFd_0", "DFd_1", "DFm_0", "DFm_1")
	write.csv(DF_S10,paste0(output_dir,"/","FigS10_data_",TODAY,".csv"),row.names=FALSE)
	options(scipen=0) #back to sci notation
	pdf(paste0(output_dir,"/","FigS10_",TODAY,".pdf")) 
	gridExtra::grid.arrange(py_0, py_1, ph_0, ph_1, pyh_0, pyh_1, pd_0, pd_1, pm_0, pm_1, ncol = 2)
    dev.off()
	} #write S10


  ##FigS9b age profiles - linear
  FIGS9b=0
  if(FIGS9b==1){
    colors <- c("0-4" = 1, "05-11" = 2,  "12-17" = 3, "18-29" = 4, "30-39" = 5, 
                "40-49" = 6, "50-59" = 7,  "60-69" = 8, "70+" = 9)
    Yname = c('Hospitalisations', 'Deaths in hospital', 'Deaths outside hospital')
    if (LOG==1) {Yname = c('Hospitalisations', 'Deaths in hospital', 'Deaths outside hospital')}
    ynamsize = 10;
	xlabsize =  8; #default 11
    ptsize   = 1.2 #default 1.5
	
    #H
    Ymax=round(max(YEXP((datHa_0$H9d)*1.05,LOG)),1); 
    pH_0 <- ggplot() +
	  ggtitle(Title_0) +
      labs(x = "", y = Yname[1], color = "") + 
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +	
	  theme(legend.position="none") +	  
      geom_line (data=datHa_0, aes(x=Dates,y=YEXP(H1w,LOG), color = "0-4")) +
      geom_line (data=datHa_0, aes(x=Dates,y=YEXP(H2w,LOG), color = "05-11")) +
      geom_line (data=datHa_0, aes(x=Dates,y=YEXP(H3w,LOG), color = "12-17")) +
      geom_line (data=datHa_0, aes(x=Dates,y=YEXP(H4w,LOG), color = "18-29")) +
      geom_line (data=datHa_0, aes(x=Dates,y=YEXP(H5w,LOG), color = "30-39")) +
      geom_line (data=datHa_0, aes(x=Dates,y=YEXP(H6w,LOG), color = "40-49")) +
      geom_line (data=datHa_0, aes(x=Dates,y=YEXP(H7w,LOG), color = "50-59")) +
      geom_line (data=datHa_0, aes(x=Dates,y=YEXP(H8w,LOG), color = "60-69")) +
      geom_line (data=datHa_0, aes(x=Dates,y=YEXP(H9w,LOG), color = "70+")) +
      geom_point(data=datHa_0, aes(x=Dates,y=YEXP(H1d,LOG), color = "0-4"),   size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=YEXP(H2d,LOG), color = "05-11"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=YEXP(H3d,LOG), color = "12-17"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=YEXP(H4d,LOG), color = "18-29"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=YEXP(H5d,LOG), color = "30-39"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=YEXP(H6d,LOG), color = "40-49"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=YEXP(H7d,LOG), color = "50-59"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=YEXP(H8d,LOG), color = "60-69"), size=ptsize) +
      geom_point(data=datHa_0, aes(x=Dates,y=YEXP(H9d,LOG), color = "70+"),   size=ptsize) + 
	  labs(tag=tags[1]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos)   
	  
    pH_1 <- ggplot() +
 	  ggtitle(Title_1) +
      labs(x = "", y = "", color = "") + 
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      theme(axis.title = element_text(size = ynamsize)) +   
	  theme(axis.text.x= element_text(size = xlabsize)) +	
	  theme(legend.position="none") +	  
      geom_line (data=datHa_1, aes(x=Dates,y=YEXP(H1w,LOG), color = "0-4")) +
      geom_line (data=datHa_1, aes(x=Dates,y=YEXP(H2w,LOG), color = "05-11")) +
      geom_line (data=datHa_1, aes(x=Dates,y=YEXP(H3w,LOG), color = "12-17")) +
      geom_line (data=datHa_1, aes(x=Dates,y=YEXP(H4w,LOG), color = "18-29")) +
      geom_line (data=datHa_1, aes(x=Dates,y=YEXP(H5w,LOG), color = "30-39")) +
      geom_line (data=datHa_1, aes(x=Dates,y=YEXP(H6w,LOG), color = "40-49")) +
      geom_line (data=datHa_1, aes(x=Dates,y=YEXP(H7w,LOG), color = "50-59")) +
      geom_line (data=datHa_1, aes(x=Dates,y=YEXP(H8w,LOG), color = "60-69")) +
      geom_line (data=datHa_1, aes(x=Dates,y=YEXP(H9w,LOG), color = "70+")) +
      geom_point(data=datHa_1, aes(x=Dates,y=YEXP(H1d,LOG), color = "0-4"),   size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=YEXP(H2d,LOG), color = "05-11"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=YEXP(H3d,LOG), color = "12-17"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=YEXP(H4d,LOG), color = "18-29"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=YEXP(H5d,LOG), color = "30-39"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=YEXP(H6d,LOG), color = "40-49"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=YEXP(H7d,LOG), color = "50-59"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=YEXP(H8d,LOG), color = "60-69"), size=ptsize) +
      geom_point(data=datHa_1, aes(x=Dates,y=YEXP(H9d,LOG), color = "70+"),   size=ptsize) +
	  labs(tag=tags[2]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos)   
	  
    #DH
    Ymax=round(max(YEXP((datDHa_0$DH9d)*1.055,LOG)),2);
    pDH_0 <- ggplot() +
      labs(x = "", y = Yname[2], color = "Age group") +  
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +	
	  theme(legend.position="none") +	  
      geom_line (data=datDHa_0, aes(x=Dates,y=YEXP(DH1w,LOG), color = "0-4")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=YEXP(DH2w,LOG), color = "05-11")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=YEXP(DH3w,LOG), color = "12-17")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=YEXP(DH4w,LOG), color = "18-29")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=YEXP(DH5w,LOG), color = "30-39")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=YEXP(DH6w,LOG), color = "40-49")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=YEXP(DH7w,LOG), color = "50-59")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=YEXP(DH8w,LOG), color = "60-69")) +
      geom_line (data=datDHa_0, aes(x=Dates,y=YEXP(DH9w,LOG), color = "70+")) +
      geom_point(data=datDHa_0, aes(x=Dates,y=YEXP(DH1d,LOG), color = "0-4"),   size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=YEXP(DH2d,LOG), color = "05-11"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=YEXP(DH3d,LOG), color = "12-17"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=YEXP(DH4d,LOG), color = "18-29"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=YEXP(DH5d,LOG), color = "30-39"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=YEXP(DH6d,LOG), color = "40-49"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=YEXP(DH7d,LOG), color = "50-59"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=YEXP(DH8d,LOG), color = "60-69"), size=ptsize) +
      geom_point(data=datDHa_0, aes(x=Dates,y=YEXP(DH9d,LOG), color = "70+"),   size=ptsize) +
 	  labs(tag=tags[3]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos) 
	  
    pDH_1 <- ggplot() +
      labs(x = "", y = "", color = "Age group") +  
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +	
	  theme(legend.position="none") +	  
      geom_line (data=datDHa_1, aes(x=Dates,y=YEXP(DH1w,LOG), color = "0-4")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=YEXP(DH2w,LOG), color = "05-11")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=YEXP(DH3w,LOG), color = "12-17")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=YEXP(DH4w,LOG), color = "18-29")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=YEXP(DH5w,LOG), color = "30-39")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=YEXP(DH6w,LOG), color = "40-49")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=YEXP(DH7w,LOG), color = "50-59")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=YEXP(DH8w,LOG), color = "60-69")) +
      geom_line (data=datDHa_1, aes(x=Dates,y=YEXP(DH9w,LOG), color = "70+")) +
      geom_point(data=datDHa_1, aes(x=Dates,y=YEXP(DH1d,LOG), color = "0-4"),   size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=YEXP(DH2d,LOG), color = "05-11"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=YEXP(DH3d,LOG), color = "12-17"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=YEXP(DH4d,LOG), color = "18-29"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=YEXP(DH5d,LOG), color = "30-39"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=YEXP(DH6d,LOG), color = "40-49"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=YEXP(DH7d,LOG), color = "50-59"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=YEXP(DH8d,LOG), color = "60-69"), size=ptsize) +
      geom_point(data=datDHa_1, aes(x=Dates,y=YEXP(DH9d,LOG), color = "70+"),   size=ptsize) +
	  labs(tag=tags[4]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos) 
	  
    #DO
    Ymax=round(max(YEXP((datDOa_0$DO9d)*1.055,LOG)),2)
    pDO_0 <- ggplot() +
      labs(x = "", y = Yname[3], color = "") + 
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +	
	  theme(legend.position="none") +	  
      geom_line (data=datDOa_0, aes(x=Dates,y=YEXP(DO1w,LOG), color = "0-4")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=YEXP(DO2w,LOG), color = "05-11")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=YEXP(DO3w,LOG), color = "12-17")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=YEXP(DO4w,LOG), color = "18-29")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=YEXP(DO5w,LOG), color = "30-39")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=YEXP(DO6w,LOG), color = "40-49")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=YEXP(DO7w,LOG), color = "50-59")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=YEXP(DO8w,LOG), color = "60-69")) +
      geom_line (data=datDOa_0, aes(x=Dates,y=YEXP(DO9w,LOG), color = "70+")) +
      geom_point(data=datDOa_0, aes(x=Dates,y=YEXP(DO1d,LOG), color = "0-4"),   size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=YEXP(DO2d,LOG), color = "05-11"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=YEXP(DO3d,LOG), color = "12-17"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=YEXP(DO4d,LOG), color = "18-29"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=YEXP(DO5d,LOG), color = "30-39"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=YEXP(DO6d,LOG), color = "40-49"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=YEXP(DO7d,LOG), color = "50-59"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=YEXP(DO8d,LOG), color = "60-69"), size=ptsize) +
      geom_point(data=datDOa_0, aes(x=Dates,y=YEXP(DO9d,LOG), color = "70+"),   size=ptsize) +
	  labs(tag=tags[5]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos)
	  
    pDO_1 <- ggplot() +
      labs(x = "", y = "", color = "") +    
      scale_color_manual(values = colors) +
      scale_x_date(breaks= breaksx, date_labels= date_labels) +
      theme_bw() +
      theme(axis.title = element_text(size = ynamsize)) + 
	  theme(axis.text.x= element_text(size = xlabsize)) +	
	  #theme(legend.position="none") +	 
	  theme(legend.position = "inside", legend.position.inside = c(0.9, 0.5)) +  #x, y
      theme(legend.title       = element_blank(),         
            legend.text        = element_text(size = 8),  
            legend.key.size = unit(0.7, 'lines'))      +  
      geom_line (data=datDOa_1, aes(x=Dates,y=YEXP(DO1w,LOG), color = "0-4")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=YEXP(DO2w,LOG), color = "05-11")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=YEXP(DO3w,LOG), color = "12-17")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=YEXP(DO4w,LOG), color = "18-29")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=YEXP(DO5w,LOG), color = "30-39")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=YEXP(DO6w,LOG), color = "40-49")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=YEXP(DO7w,LOG), color = "50-59")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=YEXP(DO8w,LOG), color = "60-69")) +
      geom_line (data=datDOa_1, aes(x=Dates,y=YEXP(DO9w,LOG), color = "70+")) +
      geom_point(data=datDOa_1, aes(x=Dates,y=YEXP(DO1d,LOG), color = "0-4"),   size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=YEXP(DO2d,LOG), color = "05-11"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=YEXP(DO3d,LOG), color = "12-17"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=YEXP(DO4d,LOG), color = "18-29"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=YEXP(DO5d,LOG), color = "30-39"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=YEXP(DO6d,LOG), color = "40-49"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=YEXP(DO7d,LOG), color = "50-59"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=YEXP(DO8d,LOG), color = "60-69"), size=ptsize) +
      geom_point(data=datDOa_1, aes(x=Dates,y=YEXP(DO9d,LOG), color = "70+"),   size=ptsize) +
	  labs(tag=tags[6]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos)   

    gridExtra::grid.arrange(pH_0, pH_1, pDH_0, pDH_1, pDO_0, pDO_1, nrow = 3, ncol = 2)

} #FigS9b

	SOURCE=1
	source(file = paste0(input_code,"/Fig4.r")) #Fig4

    dev.off() #Results
	
  ### Results ############################################################################
	
### FIGURES ##############################################################################

