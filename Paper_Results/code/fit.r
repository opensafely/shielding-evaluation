### Fit model to incidence timeseries

#library(BayesianTools)
#library(Rcpp)
#library(coda)

# Imputs: 
# 1) dataframes from model (simulation) or with incidence data (aggregated EHRs, rm6)
# 2) contacts
# Outputs
# 1) parameter marginal posterior MAP and quantiles
# 2) summaries
# 3) diagnostics

### Dataframes from model:
###   datM$  Weeks, Dataz, Dataw, Datav, etc

### Dataframes with incidence data - from HDdata sourced in main-fit:
###   datH_l,   datDH_l,   datDO_l
###   datHs_l,  datDHs_l,  datDOs_l
###   datHas_l, datDHas_l, datDOas_l
###   names: Week, Date, Freq, Ageg, Shield



######## Week range & dataset timeseries for fitting ###########################
if (pset$iplatform==0) {

## Indices
## - every week
## - from simulation.r
  imodel   = seq_along(datM$Weeks)
  idata    = imodel 
  imodelH  = imodel;
  imodelDH = imodel;
  imodelDO = imodel;
  idataH   = imodel;
  idataDH  = imodel;
  idataDO  = imodel;

## Temporal-Incidence data - used for plots at the end
  zd   = datM$Dataz[idata]
  wd   = datM$Dataw[idata] 
  vd   = datM$Datav[idata]
  zd_0 = datM$Dataz_0[idata]
  wd_0 = datM$Dataw_0[idata] 
  vd_0 = datM$Datav_0[idata]
  zd_1 = datM$Dataz_1[idata]
  wd_1 = datM$Dataw_1[idata] 
  vd_1 = datM$Datav_1[idata]
  
## Incidence data by age and shielding, weekly hospital admissions and deaths
  for (i in 1:9){ assign(paste0("zd",eval(i),"_0"), datM[paste0("Dataz",i,"_0")][[1]] ) }#zd1-zd9 - Hospitalisations
  for (i in 1:9){ assign(paste0("wd",eval(i),"_0"), datM[paste0("Dataw",i,"_0")][[1]] ) }#wd1-wd9 - Deaths in hospital
  for (i in 1:9){ assign(paste0("vd",eval(i),"_0"), datM[paste0("Datav",i,"_0")][[1]] ) }#vd1-vd9 - Deaths outside hospital
  for (i in 1:9){ assign(paste0("zd",eval(i),"_1"), datM[paste0("Dataz",i,"_1")][[1]] ) }#zd1-zd9 - Hospitalisations
  for (i in 1:9){ assign(paste0("wd",eval(i),"_1"), datM[paste0("Dataw",i,"_1")][[1]] ) }#wd1-wd9 - Deaths in hospital
  for (i in 1:9){ assign(paste0("vd",eval(i),"_1"), datM[paste0("Datav",i,"_1")][[1]] ) }#vd1-vd9 - Deaths outside hospital
  
  cat(paste0("#H  data pts fitted, #: ", length(idataH),   ", weeks 2020: ", range(idataH)[1],  "...",range(idataH)[2]),   "\n")  #45, 4...48
  cat(paste0("#DH data pts fitted, #: ", length(idataDH),  ", weeks 2020: ", range(idataDH)[1], "...",range(idataDH)[2]),  "\n")  #45, 4...48
  cat(paste0("#DO data pts fitted, #: ", length(idataDO),  ", weeks 2020: ", range(idataDO)[1], "...",range(idataDO)[2]),  "\n")  #45, 4...48
  cat(paste0("#H  model pts used,  #: ", length(imodelH),  ", weeks 2020: ", range(imodelH)[1], "...",range(imodelH)[2]),  "\n")  #45, 1...45
  cat(paste0("#DH model pts used,  #: ", length(imodelDH), ", weeks 2020: ", range(imodelDH)[1],"...",range(imodelDH)[2]), "\n")  #45, 1...45
  cat(paste0("#DO model pts used,  #: ", length(imodelDO), ", weeks 2020: ", range(imodelDO)[1],"...",range(imodelDO)[2]), "\n")  #45, 1...45

#Date range of model  
  Week1_Model  = lubridate::week("2020-01-27")    #  4  
  Week2_Model  = Week1_Model + (pars$nw-1)        # 55 #model set to run 52=pars$nw weeks
  Week_shift_model = (Week1_Model-1)

  
} else { #if iplatform>0

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
  
  cat(paste0("#H  data pts fitted, #: ", length(idataH),   ", weeks 2020: ", range(idataH)[1],  "...",range(idataH)[2]),   "\n")  #45, 4...48
  cat(paste0("#DH data pts fitted, #: ", length(idataDH),  ", weeks 2020: ", range(idataDH)[1], "...",range(idataDH)[2]),  "\n")  #45, 4...48
  cat(paste0("#DO data pts fitted, #: ", length(idataDO),  ", weeks 2020: ", range(idataDO)[1], "...",range(idataDO)[2]),  "\n")  #45, 4...48
  cat(paste0("#H  model pts used,  #: ", length(imodelH),  ", weeks 2020: ", range(imodelH)[1], "...",range(imodelH)[2]),  "\n")  #45, 1...45
  cat(paste0("#DH model pts used,  #: ", length(imodelDH), ", weeks 2020: ", range(imodelDH)[1],"...",range(imodelDH)[2]), "\n")  #45, 1...45
  cat(paste0("#DO model pts used,  #: ", length(imodelDO), ", weeks 2020: ", range(imodelDO)[1],"...",range(imodelDO)[2]), "\n")  #45, 1...45
  
} #iplatform
######## Week range & dataset timeseries for fitting ###########################


#### WEIGHTS to scale data (each age group) to modelled demography #############
if(pset$iplatform==2){ 
  ageons = pars$ageons         #1:9
  # Cohort, TPP
  agecoh = pars$agecoh         #1:9
  #weights
  weight = ageons*pars$Npop/(agecoh*pars$Npopcoh)

} else{
  weight = rep(1,times=length(pars$ageons))
}


## WEIGHTED INCIDENCE DATA #####################################################
## RAW & TIME-TOTALS ###########################################################

### raw & total groups
for (i in 1:9){
  ## -Counts for NB need be integer => round()
  assign(paste0("zd",  eval(i),"w_0"), round( eval(parse(text = paste0("zd",eval(i),"_0")))*weight[i], 0)) #zd1w_0-zd9w_0
  assign(paste0("wd",  eval(i),"w_0"), round( eval(parse(text = paste0("wd",eval(i),"_0")))*weight[i], 0)) #wd1w_0-wd9w_0
  assign(paste0("vd",  eval(i),"w_0"), round( eval(parse(text = paste0("vd",eval(i),"_0")))*weight[i], 0)) #vd1w_0-vd9w_0
  assign(paste0("zd",  eval(i),"w_1"), round( eval(parse(text = paste0("zd",eval(i),"_1")))*weight[i], 0)) #zd1w_1-zd9w_1
  assign(paste0("wd",  eval(i),"w_1"), round( eval(parse(text = paste0("wd",eval(i),"_1")))*weight[i], 0)) #wd1w_1-wd9w_1
  assign(paste0("vd",  eval(i),"w_1"), round( eval(parse(text = paste0("vd",eval(i),"_1")))*weight[i], 0)) #vd1w_1-vd9w_1

  ## WEIGHTED TIME-TOTALS
  assign(paste0("Tzd",eval(i),"w_0"), sum( eval(parse(text = paste0("zd",eval(i),"w_0")))) )              #Tzd1w_0-Tzd9w_0
  assign(paste0("Twd",eval(i),"w_0"), sum( eval(parse(text = paste0("wd",eval(i),"w_0")))) )              #Twd1w_0-Twd9w_0
  assign(paste0("Tvd",eval(i),"w_0"), sum( eval(parse(text = paste0("vd",eval(i),"w_0")))) )              #Tvd1w_0-Tvd9w_0
  assign(paste0("Tzd",eval(i),"w_1"), sum( eval(parse(text = paste0("zd",eval(i),"w_1")))) )              #Tzd1w_1-Tzd9w_1
  assign(paste0("Twd",eval(i),"w_1"), sum( eval(parse(text = paste0("wd",eval(i),"w_1")))) )              #Twd1w_1-Twd9w_1
  assign(paste0("Tvd",eval(i),"w_1"), sum( eval(parse(text = paste0("vd",eval(i),"w_1")))) )              #Tvd1w_01Tvd9w_1
}


######## Prevalence data #######################################################
cis<-read.csv(paste0(input_data,"/CIS_ONS_England_03May-12Dec-2020_Positivity.csv"))
posi_av_perc = 100*cis$Pos.average.prop #100 x proportion
#
posi_sd_perc = 100*(cis$Pos.U95CI.prop-cis$Pos.L95CI.prop)/1.96
##Absolute weeks CIS Positivity: 18-50
##Absolute weeks Model:           4-48 = 4-17 + 18-48
##=> fit index imodel = 15-45
imodel_posi=15:45
idata_posi = 1:31 #range(cis$Date[1:31]) [1] "2020-05-03" "2020-11-29" #
##data to be fittted
posi_data_perc = posi_av_perc[idata_posi]


######## Fitting #########################################################################
### Likelihood	R
### Model		Rcpp)
### MCMC		BayesianTools

### reproducibility of samples
set.seed(7777)

### Model compilation
#library(Rcpp)
if (pset$iplatform>0) { 
Rcpp::sourceCpp(file = paste0(input_code,"/","model.cpp")) }
model <- SEIUHRD


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
f20  <- function(theta) { fi(theta)};
g20  <- function(par)   { gi(par)}
#fu
f21 <- function(theta) { fI(theta)};
g21 <- function(par)   { gI(par)}


### Parameter prior bounds
age  = c(mean(0:4),mean(5:11),mean(12:17),mean(18:29),mean(30:39),mean(40:49),mean(50:59),mean(60:69),mean(70:90)) #85))
age9 = age[9]
age1 = age[1]
age3 = age[3]

tMax     = 10
tMax2    = 30
tMin     = 1
tMin2    = 1

R0Max    = 5
R0Min    = 0.1

pE0Max   = 0.05
pE0Min   = 10/pars$Npop

pI0Max   = 0.005
pI0Min   = 1/pars$Npop

fuMax    = 1
fuMin    = 0.1

hMMax  = 1
hMMin  = 0.01
hRMax  = 1.5*max(1/abs(age[1:8]-age9))  #[1] 0.09677419
hRMin  = 0.01*max(1/abs(age[1:8]-age9)) #[1] 0.0006451613

yM1Max = 1 
yM1Min = 0.01 
yR1Max = hRMax 
yR1Min = hRMin 

kMin   = 0.1
kMax   = 10

oN = 1/pars$Npop



### LIKELIHOOD FUNCTION

source(file = paste0(input_code,"/BETA_0.r")) #BETA <- function(pars)

LogLikelihood <- function(theta){
  ### Proposed parameters
  pars$rIR = f1(theta[1])
  pars$rUR = f2(theta[2])
  pars$rOD = f3(theta[3])
  pars$pI0 = f4(theta[4])
  hM_0=      f5(theta[5])
  hM_1=      f6(theta[6])
  hR_0=      f7(theta[7])
  hR_1=      f8(theta[8])
  dM_0=      f9(theta[9])
  dM_1=      f10(theta[10])
  dR_0=      f11(theta[11])
  dR_1=      f12(theta[12])
  yM1_0=     f13(theta[13])
  yM1_1=     f14(theta[14])
  yR1_0=     f15(theta[15])
  yR1_1=     f16(theta[16])
  pars$kH=   f17(theta[17])
  pars$kDH=  f18(theta[18])
  pars$R0=   f19(theta[19])
  pars$rEI=  f20(theta[20])
  pars$fu=   f21(theta[21])
  
  #Dependent parameters
  pars$rIH = pars$rIR
  pars$rIO = pars$rIR
  pars$h_0 = hM_0*exp((age-age9)*hR_0)
  pars$h_1 = hM_1*exp((age-age9)*hR_1)
  pars$d_0 = dM_0*exp((age-age9)*dR_0)
  pars$d_1 = dM_1*exp((age-age9)*dR_0)
  pars$y_0[3:9] = yM1_0*exp((age[3:9]-age9)*yR1_0)
  pars$y_1[3:9] = yM1_1*exp((age[3:9]-age9)*yR1_1)
  pars$Ia0   = (pars$agehosp*pars$Npop)*pars$pI0
  pars$Sa0   = pars$Na0 - pars$Ea0 - pars$Ia0 - pars$Ua0 - pars$Ha0 - pars$Oa0 - pars$Ra0 - pars$Da0   
  pars$beta  = BETA(pars)
 
  #Likelihood parameters
  kH  =      pars$kH
  kDH =      pars$kDH
  kDO =      pars$kDH
  ktot=      pars$ktot

  ### Model outputs (from Rcpp, given the above proposed parameters)
  m <- model(pars)

  
  ### Model prediction - positivity
  MeanPosi_perc = ( m$byw_0$Ct[imodel_posi] + m$byw_1$Ct[imodel_posi] )*oN*100

  ### Model prediction - incidence
  ### (Note: Has, DHas, DOas and are proportional to ageons)
  #Totals
  nmH_0  = 4
  nmD_0  = 6 
  nmH_1  = 4
  nmD_1  = 6
  for (i in 1:nmH_0) { 
    assign(paste0("TMeanH", eval(i),"_0"), sum(eval(parse(text = paste0("m$byw_ageH_0$H", eval(i),"w[imodelH]")))))  }  # TMeanH1_0-TMeanH4_0
  for (i in 1:nmH_1) { 
    assign(paste0("TMeanH", eval(i),"_1"), sum(eval(parse(text = paste0("m$byw_ageH_1$H", eval(i),"w[imodelH]")))))  }  # TMeanH1_0-TMeanH4_0
  for (i in 1:nmD_0) { 
    assign(paste0("TMeanDH",eval(i),"_0"), sum(eval(parse(text = paste0("m$byw_ageD_0$DH",eval(i),"w[imodelDH]")))))    # TMeanDH1_0-TMeanDH6_0
    assign(paste0("TMeanDO",eval(i),"_0"), sum(eval(parse(text = paste0("m$byw_ageD_0$DO",eval(i),"w[imodelDO]"))))) }  # TMeanDO1_0-TMeanDO6_0
  for (i in 1:nmD_1) { 
    assign(paste0("TMeanDH",eval(i),"_1"), sum(eval(parse(text = paste0("m$byw_ageD_1$DH",eval(i),"w[imodelDH]")))))    # TMeanDH1_1-TMeanDH6_1
    assign(paste0("TMeanDO",eval(i),"_1"), sum(eval(parse(text = paste0("m$byw_ageD_1$DO",eval(i),"w[imodelDO]"))))) }  # TMeanDO1_1-TMeanDO6_1
  #Timeseries: model and data
  for (i in 1:9) { #model
                   valuesz_0 = eval(parse(text = paste0("m$byw_ageH_0$H", eval(i),"w[imodelH]")))
                   valuesw_0 = eval(parse(text = paste0("m$byw_ageD_0$DH",eval(i),"w[imodelDH]")))
                   valuesv_0 = eval(parse(text = paste0("m$byw_ageD_0$DO",eval(i),"w[imodelDO]")))
                   valuesz_1 = eval(parse(text = paste0("m$byw_ageH_1$H", eval(i),"w[imodelH]")))
                   valuesw_1 = eval(parse(text = paste0("m$byw_ageD_1$DH",eval(i),"w[imodelDH]")))
                   valuesv_1 = eval(parse(text = paste0("m$byw_ageD_1$DO",eval(i),"w[imodelDO]")))
                   valuesz_0[1]= max(valuesz_0[1],1);                #avoid 0 mean and NAs in likelihood
                   valuesw_0[1]= max(valuesw_0[1],1); 
                   valuesv_0[1]= max(valuesv_0[1],1); 
                   valuesz_1[1]= max(valuesz_1[1],1);
                   valuesw_1[1]= max(valuesw_1[1],1); 
                   valuesv_1[1]= max(valuesv_1[1],1);
                   #data				   
                   assign(paste0("MeanH", eval(i),"_0"),valuesz_0)   #MeanH1_0  - MeanH9_0
                   assign(paste0("MeanDH",eval(i),"_0"),valuesw_0)   #MeanDH1_0 - MeanDH9_0
                   assign(paste0("MeanDO",eval(i),"_0"),valuesv_0)   #MeanDO1_0 - MeanDO9_0
                   assign(paste0("MeanH", eval(i),"_1"),valuesz_1)   #MeanH1_1  - MeanH9_1
                   assign(paste0("MeanDH",eval(i),"_1"),valuesw_1)   #MeanDH1_1 - MeanDH9_1
                   assign(paste0("MeanDO",eval(i),"_1"),valuesv_1) } #MeanDO1_1 - MeanDO9_1

  ## Likelihood of data
  ## is product over: 1) datasets (H,DH,DO), 2) age & shiedling strata, and 3) weeks

  ## Totals - sum over 45 pt timeseries - but several are inputted (non-reported) zeros
  kHT  = kH*ktot 
  kDHT = kDH*ktot
  kDOT = kDO*ktot

  ll_0 =
  ###H 
    (     dnbinom(x= Tzd1w_0, size = kHT,        mu   = TMeanH1_0,  log = T)) + 
    (     dnbinom(x= Tzd2w_0, size = kHT,        mu   = TMeanH2_0,  log = T)) +
    (     dnbinom(x= Tzd3w_0, size = kHT,        mu   = TMeanH3_0,  log = T)) +
    (     dnbinom(x= Tzd4w_0, size = kHT,        mu   = TMeanH4_0,  log = T)) +
    sum(  dnbinom(x= zd5w_0,  size = kH,         mu   = MeanH5_0,   log = T)) + 
    sum(  dnbinom(x= zd6w_0,  size = kH,         mu   = MeanH6_0,   log = T)) +
    sum(  dnbinom(x= zd7w_0,  size = kH,         mu   = MeanH7_0,   log = T)) +
    sum(  dnbinom(x= zd8w_0,  size = kH,         mu   = MeanH8_0,   log = T)) +
    sum(  dnbinom(x =zd9w_0,  size = kH,         mu   = MeanH9_0,   log = T)) +    
  ###DH
    (     dnbinom(x= Twd1w_0, size = kDHT,       mu   = TMeanDH1_0, log = T)) +  
    (     dnbinom(x= Twd2w_0, size = kDHT,       mu   = TMeanDH2_0, log = T)) +
    (     dnbinom(x= Twd3w_0, size = kDHT,       mu   = TMeanDH3_0, log = T)) +
    (     dnbinom(x= Twd4w_0, size = kDHT,       mu   = TMeanDH4_0, log = T)) +
    (     dnbinom(x= Twd5w_0, size = kDHT,       mu   = TMeanDH5_0, log = T)) +
    (     dnbinom(x= Twd6w_0, size = kDHT,       mu   = TMeanDH6_0, log = T)) +
    sum(  dnbinom(x = wd7w_0, size = kDH,        mu   = MeanDH7_0,  log = T)) +  
    sum(  dnbinom(x = wd8w_0, size = kDH,        mu   = MeanDH8_0,  log = T)) +
    sum(  dnbinom(x = wd9w_0, size = kDH,        mu   = MeanDH9_0,  log = T)) +
  ###DO
    (     dnbinom(x= Tvd1w_0, size = kDOT,       mu   = TMeanDO1_0, log = T)) +  
    (     dnbinom(x= Tvd2w_0, size = kDOT,       mu   = TMeanDO2_0, log = T)) +
    (     dnbinom(x= Tvd3w_0, size = kDOT,       mu   = TMeanDO3_0, log = T)) +
    (     dnbinom(x= Tvd4w_0, size = kDOT,       mu   = TMeanDO4_0, log = T)) +
    (     dnbinom(x= Tvd5w_0, size = kDOT,       mu   = TMeanDO5_0, log = T)) +
    (     dnbinom(x= Tvd6w_0, size = kDOT,       mu   = TMeanDO6_0, log = T)) +
    sum(  dnbinom(x = vd7w_0, size = kDO,        mu   = MeanDO7_0,  log = T)) + 
    sum(  dnbinom(x = vd8w_0, size = kDO,        mu   = MeanDO8_0,  log = T)) +
    sum(  dnbinom(x = vd9w_0, size = kDO,        mu   = MeanDO9_0,  log = T))

  ll_1 =
  ###H 
  (      dnbinom(x= Tzd1w_1, size = kHT,        mu   = TMeanH1_1,  log = T)) + 
  (      dnbinom(x= Tzd2w_1, size = kHT,        mu   = TMeanH2_1,  log = T)) +
  (      dnbinom(x= Tzd3w_1, size = kHT,        mu   = TMeanH3_1,  log = T)) +
  (      dnbinom(x= Tzd4w_1, size = kHT,        mu   = TMeanH4_1,  log = T)) +
   sum(  dnbinom(x= zd5w_1,  size = kH,         mu   = MeanH5_1,   log = T)) + 
   sum(  dnbinom(x= zd6w_1,  size = kH,         mu   = MeanH6_1,   log = T)) +
   sum(  dnbinom(x= zd7w_1,  size = kH,         mu   = MeanH7_1,   log = T)) +
   sum(  dnbinom(x= zd8w_1,  size = kH,         mu   = MeanH8_1,   log = T)) +
   sum(  dnbinom(x =zd9w_1,  size = kH,         mu   = MeanH9_1,   log = T)) +    
  ###DH
  (      dnbinom(x= Twd1w_1, size = kDHT,       mu   = TMeanDH1_1, log = T)) + 
  (      dnbinom(x= Twd2w_1, size = kDHT,       mu   = TMeanDH2_1, log = T)) +
  (      dnbinom(x= Twd3w_1, size = kDHT,       mu   = TMeanDH3_1, log = T)) +
  (      dnbinom(x= Twd4w_1, size = kDHT,       mu   = TMeanDH4_1, log = T)) +
  (      dnbinom(x= Twd5w_1, size = kDHT,       mu   = TMeanDH5_1, log = T)) +
  (      dnbinom(x= Twd6w_1, size = kDHT,       mu   = TMeanDH6_1, log = T)) +
   sum(  dnbinom(x = wd7w_1, size = kDH,        mu   = MeanDH7_1,  log = T)) + 
   sum(  dnbinom(x = wd8w_1, size = kDH,        mu   = MeanDH8_1,  log = T)) +
   sum(  dnbinom(x = wd9w_1, size = kDH,        mu   = MeanDH9_1,  log = T)) +
  ###DO
  (      dnbinom(x= Tvd1w_1, size = kDOT,       mu   = TMeanDO1_1, log = T)) + 
  (      dnbinom(x= Tvd2w_1, size = kDOT,       mu   = TMeanDO2_1, log = T)) +
  (      dnbinom(x= Tvd3w_1, size = kDOT,       mu   = TMeanDO3_1, log = T)) +
  (      dnbinom(x= Tvd4w_1, size = kDOT,       mu   = TMeanDO4_1, log = T)) +
  (      dnbinom(x= Tvd5w_1, size = kDOT,       mu   = TMeanDO5_1, log = T)) +
  (      dnbinom(x= Tvd6w_1, size = kDOT,       mu   = TMeanDO6_1, log = T)) +
   sum(  dnbinom(x = vd7w_1, size = kDO,        mu   = MeanDO7_1,  log = T)) + 
   sum(  dnbinom(x = vd8w_1, size = kDO,        mu   = MeanDO8_1,  log = T)) +
   sum(  dnbinom(x = vd9w_1, size = kDO,        mu   = MeanDO9_1,  log = T))
  
 ll = ll_0 + ll_1 #+

    return(ll)
} #Likelihood


## MCMC definition, parameter ranges #####################################################
cat("MCMC...", "\n")
niter = 30000 
if (pset$iplatform==2){niter= niterations} #400000}

        #1/rIR,     1/rUR,       1/rOD,       pI0,          hM,                         hR,                
LOWER = c(tMin,     tMin,        tMin2,       g4(pI0Min),   g5(hMMin),    g6(hMMin),    g7(hRMin),    g8(hRMin),
        #dM,                     dR,                        yM1,                        yR1,
        g9(hMMin),  g10(hMMin),  g11(hRMin),  g12(hRMin),   g13(yM1Min),  g14(yM1Min),  g15(yR1Min),  g16(yR1Min),
		#kH,        kDH,         R0,          tEI,          fu
        g17(kMax),  g18(kMax),   g19(R0Min),  tMin,         g21(fuMin));

        #1/rIR,     1/rUR,       1/rOD,       pI0,          hM,                         hR,                
UPPER = c(tMax,     tMax,        tMax2,       g4(pI0Max),   g5(hMMax),    g6(hMMax),    g7(hRMax),    g8(hRMax),
        #dM,                     dR,                        yM1,                       yR1,
        g9(hMMax),  g10(hMMax),  g11(hRMax),  g12(hRMax),   g13(yM1Max),  g14(yM1Max),  g15(yR1Max),  g16(yR1Max),
		#kH,        kDH,         R0,          tEI,          fu
        g17(kMin),  g18(kMin),   g19(R0Max),  tMax,         g21(fuMax));

npar = length(LOWER)


#Uniform priors
  #setup    = createBayesianSetup(likelihood=LogLikelihood, lower = LOWER, upper = UPPER)
#Beta priors
  Burnin = round(niter/2)+1  #start of effective sample
  PRIOR <- createBetaPrior(4,4,lower = LOWER, upper = UPPER)
  setup  = createBayesianSetup(likelihood=LogLikelihood, prior = PRIOR) #parallel = T,
  settings = list (iterations = niter, burnin = Burnin, message=T) 

## Bayesian sample #######################################################################
tout1 <- system.time(out <- runMCMC(bayesianSetup=setup, settings=settings) )

######## Fitting #########################################################################



## MCMC chain objects for output #########################################################
cat("Writing traces...", "\n")
#library(coda)
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_mcmcChain")
## thinning (x7)
Y1 <- out$chain[[1]]
i7 <- seq(seq_along(Y1[,1])[1]+6,seq_along(Y1[,1])[length(Y1[,1])],7)
## Thinned mcmc object and csv writing
out_thin=out
for (ic in 1:3) { #default: nchain=3
  out_thin$chain[[ic]] <- coda::as.mcmc(out$chain[[ic]][i7,])
  write.csv(data.frame(out_thin$chain[[ic]]), file=paste0(filenamepath,ic,"_new.csv")) 
}



## Parameter names #######################################################################
Pname=c("rIR  ",  "rUR  ",  "rOD  ", "pI0  ",
        "hM_0 ",  "hM_1 ",  "hR_0 ", "hR_1 ",  
        "dM_0 ",  "dM_1 ",  "dR_0 ", "dR_1 ",
        "yM1_0",  "yM1_1",  "yR1_0", "yR1_1", 
		"kH ", "kDH ", "R0 ", "rEI ", "fu ")
		
Pnamei=c("1/rIR", "1/rUR",  "1/rOD",  Pname[4:(npar-2)], "1/rEI", Pname[npar]) 


## MAP Estimates  ########################################################################
cat("MAP, quantiles...", "\n")
parsE     <- pars     #pars setup initially
MAPE      <- MAP(out) #pars estimated
parsE$rIR <- f1(as.vector(MAPE$parametersMAP[1]))
parsE$rUR <- f2(as.vector(MAPE$parametersMAP[2]))
parsE$rOD <- f3(as.vector(MAPE$parametersMAP[3]))
parsE$pI0 <- f4(as.vector(MAPE$parametersMAP[4]))
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
## MAP Estimates  ########################################################################



## Credible Intervals for parameters #####################################################
## Sample the traces
Thin = 4
Chains = 3
StartSampChainPostBurn = 1
LengtMcmcChainPostBurn = floor((niter-Burnin+1)/Chains)
LengtSampChainPostBurn = LengtMcmcChainPostBurn - StartSampChainPostBurn + 1
nsample = Chains*LengtMcmcChainPostBurn/Thin #length of each sample (all chains) 
psample = getSample(out, parametersOnly = T, start=StartSampChainPostBurn, end= LengtSampChainPostBurn, thin=Thin) #Note: parametersOnly = F => gives extra: Lposterior Llikelihood Lprior
## Quantiles
Pq=matrix(0,npar,3)
q1=0.025
q2=0.975
Pq <- t(getCredibleIntervals(getSample(out),c(q1,0.5,q2))) #Pq[ip,1:3] <= getCredibleIntervals[1:3,ip]
## Credible Intervals for parameters #####################################################



## Parameter formatting ##################################################################
## Quantile transformations
#    - 1:3, npar-1 - time - keep as is
#    - 4:npar-2, npar - transform, the Identity ones have no effect
for (ip in c(4:(npar-2),npar)) {
  Pq[ip,] = eval(parse(text = paste0("f",eval(ip),"(Pq[",eval(ip),",])")))
 }
Pr=round(Pq,3)
## MAP
PMAP=rep(0,npar)
for (ip in 1:npar) {
  PMAP[ip] = eval(parse(text = paste0("f",eval(ip),"(MAPE[[1]][[",eval(ip),"]])")))
  ## untransform 1/rate as MAPE are already transformed (in previous line)
  if(ip<=3 || ip==(npar-1))  PMAP[ip] = 1/PMAP[ip]    # 
}
PMAPr=round(PMAP,3)
## Parameter formatting ##################################################################


## Summaries #############################################################################
 
##### Summary_1 - txt output #############################################################
cat("Summary 1...", "\n")
sink(file = paste0(output_dir,"/",pset$File_fit_summary_1),append=FALSE,split=FALSE)
print(paste0("Likelihood NB"))
## Run time
print(out$settings$runtime)
print(paste0("Time used (sec): ", round(tout1[[3]],3)))
cat("\n")
## Assumptions
print(paste0("kDO = kDH "))
print(paste0("tIH = tIR "))
cat("\n")
## MAP & quantile estimates
for(i in 1:npar){
  print(paste0(Pnamei[i],", MAP: ", PMAPr[[i]],", Med: ", Pr[i,2],", CI: [",Pr[i,1],",",Pr[i,3],"]")) }
print(paste0("beta dependent: ",", MAP: ", round(parsE$beta,3)))
print(paste0("I0   dependent: ",", MAP: ", round(sum(parsE$Ia0), 0)))
print(paste0("E0      fixed:  ", round(sum(parsE$Ea0), 0)))
print(paste0("Ha0_sum input:  ", sum(parsE$Ha0)))
print(paste0("Da0_sum input:  ", sum(parsE$Da0)))
cat("\n");
## BT output
print(summary(out)); 
cat("\n")
print(paste0("Mean by chain and parameter:"))
print(out$X)
sink() ###################################################################################


##### Summary_2 - txt output #############################################################
cat("Summary 2...", "\n")
sink(file = paste0(output_dir,"/",pset$File_fit_summary_2),append=FALSE,split=FALSE) #append=TRUE,split=FALSE)
## Data and model
print(paste0("#H  data pts fitted, #: ", length(idataH),   ", list: ", range(idataH)[1],  "...",range(idataH)[2]))    #45, 4...48
print(paste0("#DH data pts fitted, #: ", length(idataDH),  ", list: ", range(idataDH)[1], "...",range(idataDH)[2]))   #45, 4...48
print(paste0("#DO data pts fitted, #: ", length(idataDO),  ", list: ", range(idataDO)[1], "...",range(idataDO)[2]))   #45, 4...48
print(paste0("#H  model pts used,  #: ", length(imodelH),  ", list: ", range(imodelH)[1], "...",range(imodelH)[2]))   #45, 1...45
print(paste0("#DH model pts used,  #: ", length(imodelDH), ", list: ", range(imodelDH)[1],"...",range(imodelDH)[2]))  #45, 1...45
print(paste0("#DO model pts used,  #: ", length(imodelDO), ", list: ", range(imodelDO)[1],"...",range(imodelDO)[2]))  #45, 1...45
cat("\n")

#print("Priors")
print(out$setup$prior)
cat("\n")
print("Parameters (doesn't apply to those estimated)"); cat("\n")
print(pars[-c(1:3)]) #"cm, cm_0, cm_1"
cat("\n"); 
print("Parameters (with MAP estimated)"); cat("\n")
print(parsE[-(1:3)]) #"cm, cm_0, cm_1
cat("\n"); 
cat("\n")
print("names(out$setup)")
print(names(out[[1]]))
cat("\n")
print("names(out$settings)")
print(names(out[[2]]))
cat("\n")
print("out$settings"); 
cat("\n")
print(out$settings)
sink() ###################################################################################



#### pdf Plots ###########################################################################


#### Diagnostics #########################################################################
cat("Diagnostics...", "\n")
pdf(paste0(output_dir,"/",pset$File_fit_output0,"_diagnostics",".pdf"))

#1 marginal posterior densities
par(mar=c(0.5, 1, 1, 1)) #shows axis scale better #bottom, left, top, right
marginalPlot(out)

#2-traces 
par(mar=c(0.5, 1, 1, 1))  #bottom, left, top, right
# column sets
J  = unique(ceiling((1:npar)/4))
Ja = unique(4*(floor((1:npar-1)/4))+1) 
Jb = unique(4*(ceiling((1:npar)/4)))   
# split in 4-par sets
# narrower-column mcmc objects
out_thin_block = out_thin
par(mar=c(0.5, 1, 1, 1))  #bottom, left, top, right #default: par("mar") #[1] 5.1 4.1 4.1 2.1
for (icolset in seq_along(J)){
  for(ic in 1:3) {
    out_thin_block$chain[[ic]] <- coda::as.mcmc(out_thin$chain[[ic]][,c(Ja[icolset]:Jb[icolset])]) }
  plot(out_thin_block$chain); 
}

#3 correlations
par(mar = c(0,0,0,0))
correlationPlot(out)

invisible(dev.off())
#### Diagnostics #########################################################################