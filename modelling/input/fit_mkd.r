
### Dataframes from model:
###   datM$  Weeks, Dataz, Dataw, Datav, Datazi, Datawi, Datavi

### Sourced from main: HDdata.R
### Dataframes with OS data:
###   dataH_l, dataDH_l, datDO_l
###   names(datHa_m_l)  [1] "ageg2"  "Week" "Date" "Freq"  (shield)
###   names(datDHa_m_l) [1] "ageg2"  "Week" "Date" "Freq"  (shield)
###   names(datDOa_m_l) [1] "ageg2"  "Week" "Date" "Freq"  (shield) 


#TODO: extend to shielding: idataH40-90 or 60-90, and 41-91 or 61-91 <= idataH4-9 or 6-9

#TODO: mcmc - revise parameters used


###### Output settings and parameters
sink(file = paste0(output_dir,"/",pset$File_run), append=TRUE, split=FALSE)
cat(" \n")
print(format(Sys.time(),'%H.%M.%S_%d-%m-%Y'))
cat(" \n")
print("pset: \n ")
print(rev(pset))
cat("\n")
print(paste0("Date range in contact-data:   ", Week1_Model, ", ", Week2_Model)); cat("\n")
if(pset$iplatform>0) {
  Week1_Data = lubridate::week("2020-01-01") #HDdata.r, HDdata.Rmd
  Week2_Data = lubridate::week("2020-12-01")
  print(paste0("Date range in H, D data:      ", "2020-01-01", " - ", "2020-12-01")); cat("\n")
  print(paste0("Week range in H, D data:      ", Week1_Data,   " - ", Week2_Data  )); cat("\n") }
cat(" \n")
print("pars: \n ")
print(rev(pars[-1])); #exclude cm
cat("\n \n")
sink()



######## Week range & dataset subsets for fitting ##############################
if (pset$iplatform==0) {
  imodel = seq_along(datM$Weeks)
  idata  = imodel 
  imodelH = imodel;  imodelDH = imodel;  imodelDO = imodel
  idataH  = imodel;  idataDH  = imodel;  idataDO  = imodel 

  #Temporal data (weekly incidence)
  #defined in simulation.r
  zd = datM$Dataz[idata]
  wd = datM$Dataw[idata] 
  vd = datM$Datav[idata] 
  
  #Replicate in simulated data the merging of ageg in the real data
  # zd4-zd9 - Hospitalisations
  # wd6-wd9 - Deaths in hospital
  # vd6-vd9 - Deaths outside hospital
  ndH  = 6  #data "4" merges 1:4
  ndDH = 4  #data "6" merges 1:6
  ndDO = 4  #data "6" merges 1:6
  zd4 = rep(0,times=length(imodelH))
  wd6 = rep(0,times=length(imodelDH))
  vd6 = rep(0,times=length(imodelDO))
  for (i in 1:(9-ndH +1)){
    zd4 = zd4 + datM[paste0("Dataz",i)][[1]] } #[[1]] numeric part
  for (i in 1:(9-ndDH+1)){
    wd6 = wd6 + datM[paste0("Dataw",i)][[1]] } #[[1]] numeric part
  for (i in 1:(9-ndDO+1)){
    vd6 = vd6 + datM[paste0("Datav",i)][[1]] } #[[1]] numeric part
  for (i in (9-ndH+1+1):9){
    assign(paste0("zd",eval(i)), datM[paste0("Dataz",i)][[1]] ) }#zd5-zd9
  for (i in (9-ndDH+1+1):9){
    assign(paste0("wd",eval(i)), datM[paste0("Dataw",i)][[1]] ) }#wd7-wd9
  for (i in (9-ndDO+1+1):9){
    assign(paste0("vd",eval(i)), datM[paste0("Datav",i)][[1]] ) }#vd7-vd9

  print(paste0("#H  data pts fitted, #: ", length(idataH),   ", list: ", range(idataH)[1],  "...",range(idataH)[2]))    #41, 1...41
  print(paste0("#DH data pts fitted, #: ", length(idataDH),  ", list: ", range(idataDH)[1], "...",range(idataDH)[2]))   #41, 1...41
  print(paste0("#DO data pts fitted, #: ", length(idataDO),  ", list: ", range(idataDO)[1], "...",range(idataDO)[2]))   #41, 1...41
  print(paste0("#H  model pts used,  #: ", length(imodelH),  ", list: ", range(imodelH)[1], "...",range(imodelH)[2]))   #41, 1...41
  print(paste0("#DH model pts used,  #: ", length(imodelDH), ", list: ", range(imodelDH)[1],"...",range(imodelDH)[2]))  #41, 1...41
  print(paste0("#DO model pts used,  #: ", length(imodelDO), ", list: ", range(imodelDO)[1],"...",range(imodelDO)[2]))  #41, 1...41
  
  Week1_Model  = lubridate::week("2020-02-24")    #[1]  8 #from SEIR_contacts
  Week2_Model  = lubridate::week("2021-02-15")+53 #no.weeks in 2020 = 53
  Week_shift_model = 0
  
} else { #if iplatform>0

## Each agegroup ageg2 (merged), has
#-- weeks 1-48 in 2020
#-- freq=0 (date=NA) when unreported
#=> set same date range for every ageg2
#=> start at model start 2020-02-24
#   -only few data ageg have freq=0 (date=NA) after 2020-02-24
#   -(but, given they can do so) use WEEK (not date)
#
# Date range for fitting every ageg
# Convert dates to WEEKS
# week("2020-01-01") #[1] 1
# week("2020-01-07") #[1] 1
  Week1_Model  = lubridate::week("2020-02-24")    #[1]  8 #from SEIR_contacts
  Week2_Study  = lubridate::week("2020-12-01")    #[1] 48 #Data: use start of vacc & alpha
  Week2_Model  = lubridate::week("2021-02-15")+53 #no.weeks in 2020 = 53
  Week1_Fit_H  = max( c(min( datHa_m_l$Week), Week1_Model), na.rm=T)              #  8 #max(c(1,8))
  Week1_Fit_DH = max( c(min(datDHa_m_l$Week), Week1_Model), na.rm=T)              #  8 #max(c(1,8))
  Week1_Fit_DO = max( c(min(datDOa_m_l$Week), Week1_Model), na.rm=T)              #  8 #max(c(1,8))
  Week2_Fit_H  = min( c(max( datHa_m_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(48,60,48))
  Week2_Fit_DH = min( c(max(datDHa_m_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(48,60,48))
  Week2_Fit_DO = min( c(max(datDOa_m_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(48,60,48))

#For each ageg2 - 1) elements of long-pivot data vector - within the date range for fitting
              # - 2) data vector with these elements
  #weekly incidence of hospital admissions 
  ndH = 6  #data "4" merges 1:4 - e.g. unique(datHa_m_l$ageg2) #[1] 4 5 6 7 8 9
  for (i in (9-ndH+1):9){
    values =  which(      !is.na(datHa_m_l$Week) &  datHa_m_l$ageg2==i  #idata= 8:48
               & Week1_Fit_H <=  datHa_m_l$Week  &  datHa_m_l$Week <= Week2_Fit_H) #dates relative to "2020-01-01
    assign(paste0("zd",eval(i)), datHa_m_l$Freq[values]) }              #zd4 ... zd9
  #weekly incidence of deaths in hospital
  ndDH = 4 #data "6" merges 1:6
  for (i in (9-ndDH+1):9){
    values =  which(      !is.na(datDHa_m_l$Week) & datDHa_m_l$ageg2==i  #idata= 8:48
               & Week1_Fit_DH <= datDHa_m_l$Week  & datDHa_m_l$Week <= Week2_Fit_DH) #dates relative to "2020-01-01
    assign(paste0("wd",eval(i)),datDHa_m_l$Freq[values]) }               #wd6 ... wd9 
  #weekly incidence of deaths outside hospital
  ndDO = 4  #data "6" merges 1:6
  for (i in (9-ndDO+1):9){ 
    values =  which(      !is.na(datDOa_m_l$Week) & datDOa_m_l$ageg2==i  #idata= 8:48              
               & Week1_Fit_DO <= datDOa_m_l$Week  & datDOa_m_l$Week <= Week2_Fit_DO) #dates relative to "2020-01-01
    assign(paste0("vd",eval(i)), datDOa_m_l$Freq[values]) }              #vd6 ... vd9

#Temporal data (weekly incidence)
  idataH  = which(!is.na(datH_l$Week)  & Week1_Fit_H  <=  datH_l$Week &  datH_l$Week <= Week2_Fit_H) #dates relative to "2020-01-01
  idataDH = which(!is.na(datDH_l$Week) & Week1_Fit_DH <= datDH_l$Week & datDH_l$Week <= Week2_Fit_DH)
  idataDO = which(!is.na(datDO_l$Week) & Week1_Fit_DO <= datDO_l$Week & datDO_l$Week <= Week2_Fit_DO)
  zd      =  datH_l$Freq[idataH]  
  wd      = datDH_l$Freq[idataDH] 
  vd      = datDO_l$Freq[idataDO]

#Convert data-week range (index 8:48 from Week1_Model=2020-02-24 to Week2_Study=2020-12-01) 
#to model-weeks (index 1:52 from Week1_Model=2020-02-24 - want to stop at Week2_Study=2020-12-01)
#=> used in likelihood
  Week_shift_model = (Week1_Model-1)
  imodelH  = idataH  - Week_shift_model #1:41=8:48-(8-1)
  imodelDH = idataDH - Week_shift_model #1:41=8:48-(8-1)
  imodelDO = idataDO - Week_shift_model #1:41=8:48-(8-1)
  
  print(paste0("#H  data pts fitted, #: ", length(idataH),   ", list: ", range(idataH)[1],  "...",range(idataH)[2]))    #41, 8...48
  print(paste0("#DH data pts fitted, #: ", length(idataDH),  ", list: ", range(idataDH)[1], "...",range(idataDH)[2]))   #41, 8...48
  print(paste0("#DO data pts fitted, #: ", length(idataDO),  ", list: ", range(idataDO)[1], "...",range(idataDO)[2]))   #41, 8...48
  print(paste0("#H  model pts used,  #: ", length(imodelH),  ", list: ", range(imodelH)[1], "...",range(imodelH)[2]))   #41, 8...48
  print(paste0("#DH model pts used,  #: ", length(imodelDH), ", list: ", range(imodelDH)[1],"...",range(imodelDH)[2]))  #41, 8...48
  print(paste0("#DO model pts used,  #: ", length(imodelDO), ", list: ", range(imodelDO)[1],"...",range(imodelDO)[2]))  #41, 8...48
  
} #iplatform



######## Fitting ###############################################################
### Likelihood (in R)
### Model (in Rcpp)
### MCMC (in BayesianTools)

### reproducibility of sampling
set.seed(7777)


### Model compilation
if (pset$iplatform>0) { #library(Rcpp)
sourceCpp(file = paste0(input_dir,"/",pset$File_model_choice)) }
model <- SEIUHRD


#### Weighing the data vs model
if(pset$iplatform==2){
  
  #### (1) -If weighting the data (regardless of which proposal) (rather than the mean (=model))
  ###Demograhy (ageons and agecoh) merged ageg
  # H (ndH=4) - DH or DO (ndDH=ndDO=6)
  # England, ONS
  ageonsH = pars$ageons         #1:9
  ageonsD = pars$ageons         #1:9
  ageonsH[1:(9-(ndH-1) -1)] = 0 #1:3
  ageonsD[1:(9-(ndDH-1)-1)] = 0 #1:5
  ageonsH[(9-(ndH -1))]     = sum(pars$ageons[1:(9-(ndH -1))]) #4 <= 1:4
  ageonsD[(9-(ndDH-1))]     = sum(pars$ageons[1:(9-(ndDH-1))]) #6 <= 1:6
  # Cohort, TPP
  agecohH = pars$agecoh         #1:9
  agecohD = pars$agecoh         #1:9
  agecohH[1:(9-(ndH -1)-1)] = 0 #1:3
  agecohD[1:(9-(ndDH-1)-1)] = 0 #1:5
  agecohH[(9-(ndH -1))]     = sum(pars$agecoh[1:(9-(ndH -1))]) #4 <= 1:4
  agecohD[(9-(ndDH-1))]     = sum(pars$agecoh[1:(9-(ndDH-1))]) #6 <= 1:6
  #ageonsH [1] 0.0000000 0.0000000 0.0000000 0.3569645 0.1351365 0.1271516 0.1365515 0.1069367 0.1372591
  #ageonsD [1] 0.0000000 0.0000000 0.0000000 0.0000000 0.0000000 0.6192527 0.1365515 0.1069367 0.1372591
  #agecohH [1] 0.000 0.000 0.000             0.347     0.143     0.131     0.137     0.107     0.135
  #agecohD [1] 0.000 0.000 0.000             0.000     0.000     0.621     0.137     0.107     0.135
  weightz_m=c(ageonsH*pars$Npop/(agecohH*pars$Npopcoh))
  weightw_m=c(ageonsD*pars$Npop/(agecohD*pars$Npopcoh))
  weightv_m=c(ageonsD*pars$Npop/(agecohD*pars$Npopcoh))
  #non-merged
  weightz  =c(pars$ageons*pars$Npop/(pars$agecoh*pars$Npopcoh))
  weightw  =c(pars$ageons*pars$Npop/(pars$agecoh*pars$Npopcoh))
  weightv  =c(pars$ageons*pars$Npop/(pars$agecoh*pars$Npopcoh))
  #### Alternative:
  #### (2) -if weighting means for each mean/parameter proposal
  #### -But, 1) would scale down the counts to cohort rather than country
  ####       2) slower, need to do for each proposal
  # mweightz = 1/c(ageonsH*pars$Npop/(agecohH*pars$Npopcoh))
  # mweightw = 1/c(ageonsD*pars$Npop/(agecohD*pars$Npopcoh))
  # mweightv = 1/c(ageonsD*pars$Npop/(agecohD*pars$Npopcoh)) 
  
  } else{
  weightz_m=rep(1,times=length(pars$ageons))
  weightw_m=rep(1,times=length(pars$ageons))
  weightv_m=rep(1,times=length(pars$ageons)) 
  weightz  = weightz_m
  weightw  = weightw_m
  weightv  = weightv_m
  }

####### -Counts into NB need be integer
#######  => round()
## zdiw, wdiw, vdiw i=4:9 or 6:9
for (i in (9-ndH+1):9){ #weekly incidence of hospitalisations
  values =  round( eval(parse(text = paste0("zd",eval(i))))*weightz_m[i], 0)
  assign(paste0("zd",eval(i),"w"),values) }
for (i in (9-ndDH+1):9){ #weekly incidence of deaths in hospital
  values =  round( eval(parse(text = paste0("wd",eval(i))))*weightw_m[i], 0)
  assign(paste0("wd",eval(i),"w"),values) }
for (i in (9-ndDO+1):9){ #weekly incidence of deaths in hospital
  values =  round( eval(parse(text = paste0("vd",eval(i))))*weightv_m[i], 0)
  assign(paste0("vd",eval(i),"w"),values) }



### Parameter bounds
tMax     = 10
R0Max    = 10
adMax    = 0.99
adMin    = 0.0099
logadMax = log(adMax)
pE0Max   = 0.10
pE0Min   = 0.0001   #0.005  #pop = 5655
logpE0Max= log(pE0Max)
pkLower  = 0.1       #1/k^2, NB likelihood and data
pkLower2 = 0.1       #Assume: same kD for DH and DO
pkUpper  = 5
pkUpper2 = 5
pH0Max   = 0.05
pH0Min   = 0.000001   #0.005  #pop = 56.55
logpH0Max= log(pH0Max)

### LIKELIHOOD FUNCTION
source(file = paste0(input_dir,"/BETA.r")) #Used within Likelihood2
LogLikelihood2 <- function(theta){
  ### UPDATE: @@
  ###         proposal pars$
  ###         MAP      parsE$, 
  ###         sample   parsES$
  ### Proposed parameters
  pars$rEI = 1/(  theta[1]*tMax)
  pars$rIR = 1/(  theta[2]*tMax)
  pars$R0  = exp( theta[3])             #*logR0Max)#*R0Max 
  pars$pE0 = exp(-theta[4] + logpE0Max) #
  pars$ad  = exp(-theta[5] + logadMax)
  ### NB likelihood
  kH       = 1/(  theta[6]*theta[6])    # pk = theta = 1/sqrt(k) => k = 1/pk^2
  kDH      = 1/(  theta[7]*theta[7])
  kDO      = kDH
  #kDO      = 1/(  theta[8]*theta[8])
  pars$pH0 = exp(-theta[8] + logpH0Max) #
  #Dependent parameters
  pars$Ea0 = pars$Na0*pars$pE0
  pars$Ha0 = pars$Na0*pars$pH0
  pars$Sa0 = pars$Na0 - pars$Ea0 - pars$Ia0 - pars$Ua0 - pars$Ha0 - pars$Ra0 - pars$Da0   
  pars$beta= BETA(pars) 

  ### Model outputs (from Rcpp, given the proposed parameters)
  m <- model(pars)

  #Model merged ageg - #NB: no ageons weighing: Ha (Da) are proportional to ageg (via Na)
  MeanH4  = rep(0,times=length(imodelH))
  MeanDH6 = rep(0,times=length(imodelDH))
  MeanDO6 = rep(0,times=length(imodelDO))
  for (i in 1:(9-(ndH-1))) { #1:4,  ndH=6
  MeanH4  = MeanH4  + eval(parse(text = paste0("m$byw_age$H", eval(i),"w[imodelH]")));
  MeanH4[1]= max(MeanH4[1],1) } #avoid 0 mean and NAs in likelihood
  for (i in 1:(9-(ndDH-1))){ #1:6, ndDH=4
  MeanDH6 = MeanDH6 + eval(parse(text = paste0("m$byw_aHO$DH",eval(i),"w[imodelDH]")));
  MeanDH6[1]= max(MeanDH6[1],1) } #avoid 0 mean and NAs in likelihood
  for (i in 1:(9-(ndDO-1))){ #1:6, ndDO=4
  MeanDO6 = MeanDO6 + eval(parse(text = paste0("m$byw_aHO$DO",eval(i),"w[imodelDO]")));
  MeanDO6[1]= max(MeanDO6[1],1) } #avoid 0 mean and NAs in likelihood

  #MeanH5-9
  for (i in (9-(ndH-1)+1):9) { #5:9, ndH=6
  values = eval(parse(text = paste0("m$byw_age$H",eval(i),"w[imodelH]"))) #Consistent: 1 + mE$byw$time[imodelH]/7 + (Week1_Model-1)
  values[1]= max(values[1],1); #avoid avoid 0 mean and NAs in likelihood
  assign(paste0("MeanH",eval(i)),values) } 
  #MeanDH7-9
  for (i in (9-(ndDH-1)+1):9){ #7:9, ndDH=
  values = eval(parse(text = paste0("m$byw_aHO$DH",eval(i),"w[imodelDH]")))
  values[1]= max(values[1],1); #avoid avoid 0 mean and NAs in likelihood
  assign(paste0("MeanDH",eval(i)),values) }
  #MeanDO7-9
  for (i in (9-(ndDO-1)+1):9){ #7:9, ndDO=4
  values = eval(parse(text = paste0("m$byw_aHO$DO",eval(i),"w[imodelDO]")))
  values[1]= max(values[1],1); #avoid avoid 0 mean and NAs in likelihood
  assign(paste0("MeanDO",eval(i)),values) } 

  #Likelihood of data
  #product over: datasets (H,DH,DO), age-groups, and weeks
  link = 1 #pars$pdm
  #Negative binomial likelihood 

  ll = sum(dnbinom(x = zd4w, size = kH, mu = MeanH4, log = T)) +
       sum(dnbinom(x = zd5w, size = kH, mu = MeanH5, log = T)) +
       sum(dnbinom(x = zd6w, size = kH, mu = MeanH6, log = T)) +
       sum(dnbinom(x = zd7w, size = kH, mu = MeanH7, log = T)) +
       sum(dnbinom(x = zd8w, size = kH, mu = MeanH8, log = T)) +
       sum(dnbinom(x = zd9w, size = kH, mu = MeanH9, log = T)) +
       ###
       sum(dnbinom(x = wd6w, size = kDH, mu = MeanDH6*link, log = T)) +
       sum(dnbinom(x = wd7w, size = kDH, mu = MeanDH7*link, log = T)) +
       sum(dnbinom(x = wd8w, size = kDH, mu = MeanDH8*link, log = T)) +
       sum(dnbinom(x = wd9w, size = kDH, mu = MeanDH9*link, log = T)) +
       ###
       sum(dnbinom(x = vd6w, size = kDO, mu = MeanDO6*link, log = T)) +
       sum(dnbinom(x = vd7w, size = kDO, mu = MeanDO7*link, log = T)) +
       sum(dnbinom(x = vd8w, size = kDO, mu = MeanDO8*link, log = T)) +
       sum(dnbinom(x = vd9w, size = kDO, mu = MeanDO9*link, log = T))
    
    return(ll)
} #Likelihood
## iplatform==0, simulated data, model(pars), pars from parameters  ##  -5593.537
## iplatform==1,     dummy data, model(pars), pars from parameters  ##  -3223.607


## Likelihood definition, parameter ranges  ####################################
niter = 120000 #3000 #30000#9000 #200000
LOWER = c(c(1, 1)/tMax,          0,                 0,                  0, pkLower, pkLower2, 0); #kDO
UPPER = c(  1, 1,       log(R0Max),log(pE0Max/pE0Min), log(adMax/adMin),   pkUpper, pkUpper2, log(pH0Max/pH0Min)); #kDO
LogLikelihood = LogLikelihood2; #rEI, rIR, R0, pE0, al, kH, kDH, kDO


#Uniform priors
  #PARSTART = 0.5*UPPER
  #nchain = 3
  #setup    = createBayesianSetup(likelihood=LogLikelihood, lower = LOWER, upper = UPPER) #parallel = T,
  #settings = list (startValue=t(array(PARSTART,dim=c(length(PARSTART),nchain))), iterations = niter, burnin = round(niter*length(LOWER)/15), message=T) #F)
#Beta priors
  #PRIOR <- createBetaPrior(3,3,lower = LOWER, upper = UPPER)
  #PRIOR <- createBetaPrior(4,5,lower = LOWER, upper = UPPER)
  PRIOR <- createBetaPrior(3,4,lower = LOWER, upper = UPPER)
  setup  = createBayesianSetup(likelihood=LogLikelihood, prior =PRIOR) #parallel = T,
  settings = list (iterations = niter, burnin = round(niter*length(LOWER)/15), message=T) #F)

## Bayesian sample
tout1 <- system.time(out <- runMCMC(bayesianSetup=setup, settings=settings) )



Resample=0 #####################################################################
if (Resample==1){
## Run with new prior based on posterior
#newPrior <- createPriorDensity(out)
newPrior = createPriorDensity(out, method = "multivariate", eps = 1e-10, lower = LOWER, upper =  UPPER, best = NULL)
#newPrior = createPriorDensity(out, method = "multivariate", eps = 1e-10, lower = rep(-10, 3), upper =  rep(10, 3), best = NULL)
setup2 = createBayesianSetup(likelihood=LogLikelihood, prior = newPrior)
## Try: message=T)
## Bayesian sample
tout2 <- system.time(out2 <- runMCMC(bayesianSetup=setup2, settings=settings) )
 #=> lots of NAN bec parameters not sensinbly bounded - would need to transform pars to allow this
out_1 <- out
out   <- out2 }


Restart=0 ######################################################################
if (Restart==1){
## Restart same sampling
#tout2 <- system.time(out2 <- runMCMC(bayesianSetup=setup, settings=settings) )
  out_1 <- out
  out   <- out2
tout3 <- system.time(out3 <- runMCMC(bayesianSetup=setup, settings=settings) )
  out_2 <- out
  out   <- out3
}


## MAP Estimates  ##############################################################
parsE     <- pars
MAPE      <- MAP(out)
### UPDATE: @@
###         proposal pars$
###         MAP      parsE$, 
###         sample   parsES$
parsE$rEI <- 1/(  MAPE$parametersMAP[1]*tMax)
parsE$rIR <- 1/(  MAPE$parametersMAP[2]*tMax)
parsE$R0  <- exp( MAPE$parametersMAP[3])#*logR0Max)#*R0Max
parsE$pE0 <- exp(-MAPE$parametersMAP[4] + logpE0Max)
parsE$ad  <- exp(-MAPE$parametersMAP[5] + logadMax)
parsE$kH  <- 1/(  MAPE$parametersMAP[6]^2) #1/pk^2
parsE$kDH <- 1/(  MAPE$parametersMAP[7]^2)
parsE$kDO <- parsE$kDH
#parsE$kDO <- 1/(      MAPE$parametersMAP[8]^2)
parsE$pH0 <- exp(-MAPE$parametersMAP[8] + logpH0Max)
#dependent
parsE$Ea0 = parsE$Na0*parsE$pE0
parsE$Ha0 = parsE$Na0*parsE$pH0
parsE$Sa0 = parsE$Na0 - parsE$Ea0 - parsE$Ia0 - parsE$Ua0 - parsE$Ha0 - parsE$Ra0 - parsE$Da0   
parsE$beta= BETA(parsE)
#predictions
mE        <- model(parsE)


## UNCERTAINTY  ################################################################
## Sample the chains
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
npar      = length(LOWER)
nsample   = 3000#100#500#1000#;
psample = getSample(out, parametersOnly = T, numSamples = nsample, start=(niter/3)/3) #parametersOnly = F
# run model for each parameter set in the sample
zsample = matrix(0,length(imodelH),nsample)
wsample = matrix(0,length(imodelH),nsample)
vsample = matrix(0,length(imodelH),nsample)
parsES  = pars
### UPDATE: @@
###         proposal pars$
###         MAP      parsE$, 
###         sample   parsES$
for(i in 1:nsample){
  parsES$rEI <- 1/(  as.vector(psample[i,1])*tMax)
  parsES$rIR <- 1/(  as.vector(psample[i,2])*tMax)
  parsES$R0  <- exp( as.vector(psample[i,3])) #*logR0Max)#*R0Max
  parsES$pE0 <- exp(-as.vector(psample[i,4]) + logpE0Max)
  parsES$ad  <- exp(-as.vector(psample[i,5]) + logadMax)
  parsES$pH0 <- exp(-as.vector(psample[i,8]) + logpH0Max)
  #Dependent
  parsES$Ea0 = parsES$Na0*parsES$pE0
  parsES$Ha0 = parsES$Na0*parsES$pH0
  parsES$Sa0 = parsES$Na0 - parsES$Ea0 - parsES$Ia0 - parsES$Ua0 - parsES$Ha0 - parsES$Ra0 - parsES$Da0 
  parsES$beta= BETA(parsES)
  outs        = model(as.vector(parsES))
  zsample[,i] = outs$byw$Hw[imodelH]
  wsample[,i] = outs$byw$DHw[imodelH]
  vsample[,i] = outs$byw$DOw[imodelH]  
} 
Weekssample = 1 + outs$byw$time[imodelH]/7 + Week_shift_model
Datessample = as.Date(paste(Weekssample, "2020", 'Mon'), '%U %Y %a') 
##95% CrI
zsample95 = matrix(0,length(imodelH),2)
wsample95 = matrix(0,length(imodelH),2)
vsample95 = matrix(0,length(imodelH),2)
for(it in 1:length(imodelH)){
  samp_it <- zsample[it,]
  zsample95[it,1] = quantile(samp_it,0.05)
  zsample95[it,2] = quantile(samp_it,0.95)
  samp_it <- wsample[it,]
  wsample95[it,1] = quantile(samp_it,0.05)
  wsample95[it,2] = quantile(samp_it,0.95)
  samp_it <- vsample[it,]
  vsample95[it,1] = quantile(samp_it,0.05)
  vsample95[it,2] = quantile(samp_it,0.95) 
}
} 


#True parameters (except last two)
thetaTrue = c(pars$rEI, pars$rIR, pars$R0, pars$pE0, pars$ad, pars$kH, pars$kDH);


#Expected parameters

### NB data
###  k empirical
  mEm  = mean(mE$byw$Hw); 
  mEm2 = mean(mE$byw$Dw)

if (!is.element(pset$iplatform,1)) { #cant estimate with dummy data (1)
  kempir_mdata  = round(1/((var(zd) - mean(zd))/(mean(zd)^2)),1); 
  kempir_mmodel = round(1/((var(zd) - mEm)/(mEm^2)),1);
  kempir_mdata2  = round(1/((var(wd) - mean(wd))/(mean(wd)^2)),1); 
  kempir_mmodel2 = round(1/((var(wd) - mEm2)/(mEm2^2)),1);
    print(paste0("k_empiricalH (data mean)  = ", kempir_mdata))
    print(paste0("k_empiricalH (model mean) = ", kempir_mmodel))
    print(paste0("k_empiricalD (data mean)  = ", kempir_mdata2))
    print(paste0("k_empiricalD (model mean) = ", kempir_mmodel2))
} else {kempir_mmodel=1; kempir_mmodel2=1; kempir_mdata =1; kempir_mdata2=1;}

### NB data - Normal residual likelihood
###  sd empirical 
dev  = sqrt(mEm  + mEm^2*abs(kempir_mmodel)) #abs() to tackle dummy data negative k
dev2 = sqrt(mEm2 + mEm2^2*abs(kempir_mmodel2))
thetaTrue[c(length(thetaTrue)-1,length(thetaTrue))] = c(dev,dev2) 

### NB data - NB likelihood
if(pset$iplatform==0){ #simulation: true parameters
  thetaTrue[c(length(thetaTrue)-1,length(thetaTrue))] = c(pars$k,pars$k)  
  } else {             #not simulation
  thetaTrue[c(length(thetaTrue)-1,length(thetaTrue))] = c(kempir_mdata, kempir_mdata2) }



##### Summary - txt output
sink(file = paste0(output_dir,"/",pset$File_fit_summary_1),append=FALSE,split=FALSE)
print(paste0("Likelihood NB"))
#cat("\n"); 
## Run time
print(out$settings$runtime)
print(paste0("Time used (sec): ", round(tout1[[3]],3)))
cat("\n")
## MAP Estimates
print(paste0("1/rEI MAP: ", round(1/parsE$rEI, 3), ". Expected/start: ", round(1/thetaTrue[1], 3))) #rEI
print(paste0("1/rIR MAP: ", round(1/parsE$rIR, 3), ". Expected/start: ", round(1/thetaTrue[2], 3))) #rIR
print(paste0("R0    MAP: ", round(parsE$R0,    3), ". Expected/start: ", round(  thetaTrue[3], 3))) #R0
print(paste0("pE0   MAP: ", round(parsE$pE0,   4), ". Expected/start: ", round(  thetaTrue[4], 3))) #pE0
print(paste0("ad    MAP: ", round(parsE$ad,    4), ". Expected/start: ", round(  thetaTrue[5], 3))) #ad
print(paste0("kH    MAP: ", round(parsE$kH,    3), ". Expected/start: ", round(  thetaTrue[6], 3))) #kH
print(paste0("kDH,kDO MAP: ", round(parsE$kDH,   3), ". Expected/start: ", round(  thetaTrue[7], 3))) #kD
#print(paste0("kDO   dep: ", round(parsE$kDO,   3), ". Expected/start: ", round(  thetaTrue[7], 3))) #kD
#print(paste0("kDO   dep: ", round(parsE$kDO,   3), ". Expected/start: ", round(  thetaTrue[8], 3))) #kD
print(paste0("beta  dep: ", round(parsE$beta,  5)))
print(paste0("Ea0   dep: ", round(sum(parsE$Ea0),   0))) #or sum(parsE$Na0*parsE$pE0)
print(paste0("Ha0   dep: ", round(sum(parsE$Ha0),   0))) #or sum(parsE$Na0*parsE$HE0)
print(paste0("Estimated proportion deaths outside hospital = ", round(parsE$ad/(1+parsE$ad),3)))
cat("\n");
print(summary(out)); 
cat("\n")
print(paste0("Mean by chain and parameter:"))
print(out$X)
sink()

sink(file = paste0(output_dir,"/",pset$File_fit_summary_2),append=FALSE,split=FALSE) #append=TRUE,split=FALSE)
#cat("\n")
## Data and model
print(paste0("#H  data pts fitted, #: ", length(idataH),   ", list: ", range(idataH)[1],  "...",range(idataH)[2]))   #41, 8...48
print(paste0("#DH data pts fitted, #: ", length(idataDH),  ", list: ", range(idataDH)[1], "...",range(idataDH)[2]))  #41, 8...48
print(paste0("#DO data pts fitted, #: ", length(idataDO),  ", list: ", range(idataDO)[1], "...",range(idataDO)[2]))  #41, 8...48
print(paste0("#H  model pts used,  #: ", length(imodelH),  ", list: ", range(imodelH)[1], "...",range(imodelH)[2]))   #41, 8...48
print(paste0("#DH model pts used,  #: ", length(imodelDH), ", list: ", range(imodelDH)[1],"...",range(imodelDH)[2]))  #41, 8...48
print(paste0("#DO model pts used,  #: ", length(imodelDO), ", list: ", range(imodelDO)[1],"...",range(imodelDO)[2]))  #41, 8...48

cat("\n")
print(names(out[[1]]))
cat("\n")
print(names(out[[2]]))
cat("\n")
print(paste0("k_empirical (data mean)   = ", kempir_mdata))
print(paste0("k_empirical (model mean)  = ", kempir_mmodel))
print(paste0("k_empiricalD (data mean)  = ", kempir_mdata2))
print(paste0("k_empiricalD (model mean) = ", kempir_mmodel2))
cat("\n")
#print("Priors")
print(out$setup$prior)
cat("\n"); 
cat("\n")
sink()



##### Plots - overall dataframes ###############################################
N  = pars$Npop
Nc = pars$Npopcoh
if(pset$iplatform<2) {weight=1} else {weight=N/Nc}

datH <- tibble(Weeks  = 1 + mE$byw$time[imodelH]/7 + Week_shift_model, #model time 0 <> week 1 + (Week1_Model-1) =week 8
               Dates  = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'),
               H_est  = mE$byw$Hw[imodelH],
               zdw    = zd*weight)
datD <- tibble(Weeks  = 1 + mE$byw$time[imodelDH]/7 + Week_shift_model,
               Dates  = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'),
               DH_est = mE$byw$DHw[imodelDH],
               DO_est = mE$byw$DOw[imodelDO],  
               wdw    = wd*weight,
               vdw    = vd*weight)

if (pset$iplatform>0) {
datH <- tibble(datH,
               Weeksz = datH_l$Weeks[idataH],
               Datesz = Dates)            #datH_l$Dates[idataH]) 
               #Justification:datX_l$Dates (X=DH, DO) has NA (dummy & OS) 
               #while datX_l$Weeks datX_l$Freq (zd, wd, vd) has no NA (by _l construction)
datD <- tibble(datD,
               Weeksw = datDH_l$Weeks[idataDH],
               Weeksv = datDO_l$Weeks[idataDO],
               Datesw = Dates,           #datDH_l$Dates[idataDH],
               Datesv = Dates)  } else { #datDO_l$Dates[idataDO])  } else {
datH <- tibble(datH,
               H_mod  = datM$H_mod[imodelH],
               Weeksz = Weeks,
               Datesz = Dates)
datD <- tibble(datD,
               DH_mod = datM$DH_mod[imodelDH],
               DO_mod = datM$DO_mod[imodelDO],
               Weeksw = Weeks,
               Weeksv = Weeks,
               Datesw = Dates,
               Datesv = Dates)  }


###### Plots - Age-profile dataframes (NOT MERGED) #############################
##OS:   datHa_l  or datHa   (l = long pivot, all 48*9 pts)
##OS:  datDHa_l  or datDHa
##OS:  datDOa_l  or datDOa

if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
  if (pset$iplatform==2){
    for (i in 1:9){
      #idataH1-idataH9 
      values =  which(!is.na(datHa_l$Week)                 &  datHa_l$ageg==i
                           & datHa_l$Week >= Week1_Fit_H   &  datHa_l$Week <= Week2_Fit_H)
      assign(paste0("idataH",eval(i)),values) 
      #idataDH1-idataDH9 
      values =  which(!is.na(datDHa_l$Week)                & datDHa_l$ageg==i
                           & datDHa_l$Week >= Week1_Fit_DH & datDHa_l$Week <= Week2_Fit_DH)
      assign(paste0("idataDH",eval(i)),values) 
      #idataDO1-idataDO9 
      values =  which(!is.na(datDOa_l$Week)                & datDOa_l$ageg==i
                           & datDOa_l$Week >= Week1_Fit_DO & datDOa_l$Week <= Week2_Fit_DO)
      assign(paste0("idataDO",eval(i)),values) 
    }}
#H  
#H estimated (MAP) - Hiw <= mE$byw_age$Hiw
  for (i in 1:9){
    values = eval(parse(text = paste0("mE$byw_age$H",eval(i),"w[imodelH]")))
    assign(paste0("H",eval(i),"w"),values) }
#H data simulated - Hid <= datM$H_modi - (actually, using true model, as the data too noisy)
  if (pset$iplatform==0){
  for (i in 1:9){
    values = eval(parse(text = paste0("datM$H_mod",eval(i),"[imodelH]")))
    assign(paste0("H",eval(i),"d"),values) } } else {
#H data os          - Hid <= datHa_l$Freq[idataHi]
  for (i in 1:9){
    values = eval(parse(text = paste0("datHa_l$Freq[idataH",eval(i),"]*weightz[",eval(i),"]")))
    assign(paste0("H",eval(i),"d"),values) } }
#DH    
#DH estimated (MAP) - DHiw <= mE$byw_aHO$DHiw
  for (i in 1:9){
    values = eval(parse(text = paste0("mE$byw_aHO$DH",eval(i),"w[imodelDH]")))
    assign(paste0("DH",eval(i),"w"),values) }
#DH data simulated  - DHid <= datM$DH_modi - (actually, using true model, as the data too noisy)
  if (pset$iplatform==0){
  for (i in 1:9){
    values = eval(parse(text = paste0("datM$DH_mod",eval(i),"[imodelDH]")))
    assign(paste0("DH",eval(i),"d"),values) } } else {
#DH data os         - DHid <= datDHa_l$Freq[idataDHi]
  for (i in 1:9){
    values = eval(parse(text = paste0("datDHa_l$Freq[idataDH",eval(i),"]*weightw[",eval(i),"]")))
    assign(paste0("DH",eval(i),"d"),values) } }
#DO
#DO estimated (MAP) - DOiw <= mE$byw_aHO$DOiw
  for (i in 1:9){
    values = eval(parse(text = paste0("mE$byw_aHO$DO",eval(i),"w[imodelDO]")))
    assign(paste0("DO",eval(i),"w"),values) }
#DO data simulated  - DOid <= datM$DO_modi - (actually, using true model, as the data too noisy)
  if (pset$iplatform==0){
  for (i in 1:9){
    values = eval(parse(text = paste0("datM$DO_mod",eval(i),"[imodelDO]")))
    assign(paste0("DO",eval(i),"d"),values) } } else {
#DO data os         - DOid <= datDOa_l$Freq[idataDOi]
  for (i in 1:9){
    values = eval(parse(text = paste0("datDOa_l$Freq[idataDO",eval(i),"]*weightv[",eval(i),"]")))
    assign(paste0("DO",eval(i),"d"),values) } }

#H
datHa  <- tibble(Weeks = 1 + mE$byw$time[imodelH]/7 + Week_shift_model,
                 Dates  = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'),
                 H1w=H1w, H2w=H2w, H3w=H3w, H4w=H4w, H5w=H5w, #estimated
                 H6w=H6w, H7w=H7w, H8w=H8w, H9w=H9w,
                 H1d=H1d, H2d=H2d, H3d=H3d, H4d=H4d, H5d=H5d, #data
                 H6d=H6d, H7d=H7d, H8d=H8d, H9d=H9d)
#DH
datDHa <- tibble(Weeks = 1 + mE$byw$time[imodelDH]/7 + Week_shift_model,
                 Dates = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'),
                 DH1w=DH1w, DH2w=DH2w, DH3w=DH3w, DH4w=DH4w, DH5w=DH5w, #estimated
                 DH6w=DH6w, DH7w=DH7w, DH8w=DH8w, DH9w=DH9w,
                 DH1d=DH1d, DH2d=DH2d, DH3d=DH3d, DH4d=DH4d, DH5d=DH5d, #data
                 DH6d=DH6d, DH7d=DH7d, DH8d=DH8d, DH9d=DH9d)
#DO
    datDOa <- tibble(Weeks = 1 + mE$byw$time[imodelDO]/7 + Week_shift_model,
                 Dates = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'),
                 DO1w=DO1w, DO2w=DO2w, DO3w=DO3w, DO4w=DO4w, DO5w=DO5w, #estimated
                 DO6w=DO6w, DO7w=DO7w, DO8w=DO8w, DO9w=DO9w,
                 DO1d=DO1d, DO2d=DO2d, DO3d=DO3d, DO4d=DO4d, DO5d=DO5d, #data
                 DO6d=DO6d, DO7d=DO7d, DO8d=DO8d, DO9d=DO9d)

} #Plots - Age profile dataframes (not merged)
                 
                    


#### pdf Plots #################################################################
#pdf(file = paste0(output_dir,"/",pset$File_fit_output))

#### svg plots #################################################################
#### Diagnostics
#1
#par(mar = c(0.5, 1, 1, 1)) #ar(mar = c(2, 2, 1, 1))  #bottom, left, top, right
p<-marginalPlot(out); invisible(print(p))
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_marginalPlot")
svglite(paste0(filenamepath,".svg")); marginalPlot(out); invisible(dev.off())
#2-3
par(mar = c(2, 2, 1, 1)) ##bottom, left, top, right
p<-plot(out); invisible(print(p))
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_plotout_lastPage")
if (pset$iplatform==0){
svglite(paste0(filenamepath,".svg")); plot(out); invisible(dev.off()) }
#4
par(mar = c(2, 2, 1, 1))
print(correlationPlot(out))
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_correlationPlot")
svglite(paste0(filenamepath,".svg")); correlationPlot(out); invisible(dev.off())


### Plot overall
#5
colors <- c(  "I_dat"  = "black",   "I_est" = "red",     "I_model" = "green",
             "H_datw"  = "black",   "H_est" = "red",     "H_model" = "pink",
            "DH_datw"  = "grey",   "DH_est" = "blue",   "DH_model" = "cyan",
            "DO_datw"  = "black",  "DO_est" = "green4", "DO_model" = "green")
p1 <- ggplot() +
          labs(x = 'Date', y = 'Hospitalisations & deaths in & outside hospital', color = "Legend") + 
          #xlim(c(0, NA)) +  ylim(c(0, NA)) + #Dont use with Dates, only with Weeks
          scale_color_manual(values = colors) +
          geom_point(data=datH, aes(x=Datesz,y=zdw,    color =  "H_datw"), size = 1.4,   pch = 19) +
          geom_point(data=datD, aes(x=Datesw,y=wdw,    color = "DH_datw"), size =   1,   pch = 16) +
          geom_point(data=datD, aes(x=Datesv,y=vdw,    color = "DO_datw"), size =   1,   pch = 1) +
          geom_line (data=datH, aes(x=Dates, y=H_est,  color =  "H_est")) +
          geom_line (data=datD, aes(x=Dates, y=DH_est, color = "DH_est")) +
          geom_line (data=datD, aes(x=Dates, y=DO_est, color = "DO_est"))
			   
    if (pset$iplatform==0) {
    p1 <- p1 + 
          geom_line (data=datH, aes(x=Dates,y=H_mod,  color =  "H_model")) +
          geom_line (data=datD, aes(x=Dates,y=DH_mod, color = "DH_model")) +
          geom_line (data=datD, aes(x=Dates,y=DO_mod, color = "DO_model"))  }
print(p1)
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_Overall")
svglite(paste0(filenamepath,".svg")); print(p1); invisible(dev.off())


## Plot posterior samples
#6
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
##
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_PosteriorSample")
##
par(mfrow = c(3,1))
par(mar = c(2, 4, 1, 1)) #bottom, left, top, right
colors <- c("Data" = 1,  "MAP" = 2, "95% CrI" = "grey70", "95% perc" = "grey70")
zMAX = rep(range(zsample)[2],length(Datessample))
#H
dzsample <- tibble(Date=Datessample, zsample05=zsample95[,1], zsample95=zsample95[,2],  # sample trajectories
                   zMAP=outs$byw$Hw[imodelH], Datez=datH$Datesz, zdw=datH$zdw, yLIM=zMAX )
p1 <- ggplot(dzsample, aes(x=Date)) + 
       geom_ribbon(aes(ymin = zsample05, ymax = zsample95), fill = "grey70") +
       geom_point(aes(x=Datez, y = zdw,       color="Data")) +
       geom_line (aes(x=Date,  y = zMAP,      color="MAP")) +
       geom_line (aes(x=Date,  y = zsample05, color="95% CrI")) +
       labs(x = 'Date', y = 'Hospitalisations', color = "Legend") + 
       #xlim(c(0, NA)) +  ylim(c(0, zMAX[1])) + #Dont use with Dates, only with Weeks
       scale_color_manual(values = colors) 
#print(p1)

#DH
dwsample <- tibble(Date=Datessample, wsample05=wsample95[,1], wsample95=wsample95[,2],  # sample trajectories
                   wMAP=outs$byw$DHw[imodelDH], Datew=datD$Datesw, wdw=datD$wdw, yLIM=zMAX ) 
p2 <- ggplot(dwsample, aes(x=Date)) + 
  geom_ribbon(aes(ymin = wsample05, ymax = wsample95), fill = "grey70") +
  geom_point(aes(x=Datew, y = wdw,       color="Data")) +
  geom_line (aes(x=Date,  y = wMAP,      color="MAP")) +
  geom_line (aes(x=Date,  y = wsample05, color="95% CrI")) +
  labs(x = 'Date', y = 'Deaths in hospital', color = "Legend") + 
  #xlim(c(0, NA)) +  ylim(c(0, zMAX[1])) + #Dont use with Dates, only with Weeks
  scale_color_manual(values = colors) 
#print(p2)

#DO
dvsample <- tibble(Date=Datessample, vsample05=vsample95[,1], vsample95=vsample95[,2],  # sample trajectories
                   vMAP=outs$byw$DOw[imodelDO], Datev=datD$Datesv, vdw=datD$vdw, yLIM=zMAX ) 
p3 <- ggplot(dvsample, aes(x=Date)) + 
  geom_ribbon(aes(ymin = vsample05, ymax = vsample95), fill = "grey70") +
  geom_point(aes(x=Datev, y = vdw,       color="Data")) +
  geom_line (aes(x=Date,  y = vMAP,      color="MAP")) +
  geom_line (aes(x=Date,  y = vsample05, color="95% CrI")) +
  labs(x = 'Date', y = 'Deaths outside hospital', color = "Legend") + 
  #xlim(c(0, NA)) +  ylim(c(0, zMAX[1])) + #Don't use with Dates, only with Weeks
  scale_color_manual(values = colors) 
#print(p3)
gridExtra::grid.arrange(p1, p2, p3, nrow = 3)

svglite(paste0(filenamepath,".svg")); 
   gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
invisible(dev.off())
}

##Plot by age profiles
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){

colors <- c("0-4" = 1, "05-11" = 2,  "12-17" = 3, "18-29" = 4, "30-39" = 5, 
            "40-49" = 6, "50-59" = 7,  "60-69" = 8, "70+" = 9)
#7 H
p1 <- ggplot() +
    labs(x = 'Date', y = 'Hospitalisations', color = "Legend") +
    #xlim(c(0, NA)) +  ylim(c(0, NA)) + #Don't use with Dates, only with Weeks
    scale_color_manual(values = colors) +
    geom_line (data=datHa, aes(x=Dates,y=H1w, color = "0-4")) +
    geom_line (data=datHa, aes(x=Dates,y=H2w, color = "05-11")) +
    geom_line (data=datHa, aes(x=Dates,y=H3w, color = "12-17")) +
    geom_line (data=datHa, aes(x=Dates,y=H4w, color = "18-29")) +
    geom_line (data=datHa, aes(x=Dates,y=H5w, color = "30-39")) +
    geom_line (data=datHa, aes(x=Dates,y=H6w, color = "40-49")) +
    geom_line (data=datHa, aes(x=Dates,y=H7w, color = "50-59")) +
    geom_line (data=datHa, aes(x=Dates,y=H8w, color = "60-69")) +
    geom_line (data=datHa, aes(x=Dates,y=H9w, color = "70+")) +
    geom_point(data=datHa, aes(x=Dates,y=H1d, color = "0-4")) +
    geom_point(data=datHa, aes(x=Dates,y=H2d, color = "05-11")) +
    geom_point(data=datHa, aes(x=Dates,y=H3d, color = "12-17")) +
    geom_point(data=datHa, aes(x=Dates,y=H4d, color = "18-29")) +
    geom_point(data=datHa, aes(x=Dates,y=H5d, color = "30-39")) +
    geom_point(data=datHa, aes(x=Dates,y=H6d, color = "40-49")) +
    geom_point(data=datHa, aes(x=Dates,y=H7d, color = "50-59")) +
    geom_point(data=datHa, aes(x=Dates,y=H8d, color = "60-69")) +
    geom_point(data=datHa, aes(x=Dates,y=H9d, color = "70+"))
print(p1)
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_AgeProfile_H")
svglite(paste0(filenamepath,".svg")); print(p1); invisible(dev.off())
#8 DH
p2 <- ggplot() +
    labs(x = 'Date', y = 'Deaths in hospital', color = "Legend") + 
    #xlim(c(0, NA)) +  ylim(c(0, NA)) + #Don't use with Dates, only with Weeks
    scale_color_manual(values = colors) +
    geom_line (data=datDHa, aes(x=Dates,y=DH1w, color = "0-4")) +
    geom_line (data=datDHa, aes(x=Dates,y=DH2w, color = "05-11")) +
    geom_line (data=datDHa, aes(x=Dates,y=DH3w, color = "12-17")) +
    geom_line (data=datDHa, aes(x=Dates,y=DH4w, color = "18-29")) +
    geom_line (data=datDHa, aes(x=Dates,y=DH5w, color = "30-39")) +
    geom_line (data=datDHa, aes(x=Dates,y=DH6w, color = "40-49")) +
    geom_line (data=datDHa, aes(x=Dates,y=DH7w, color = "50-59")) +
    geom_line (data=datDHa, aes(x=Dates,y=DH8w, color = "60-69")) +
    geom_line (data=datDHa, aes(x=Dates,y=DH9w, color = "70+")) +
    geom_point(data=datDHa, aes(x=Dates,y=DH1d, color = "0-4")) +
    geom_point(data=datDHa, aes(x=Dates,y=DH2d, color = "05-11")) +
    geom_point(data=datDHa, aes(x=Dates,y=DH3d, color = "12-17")) +
    geom_point(data=datDHa, aes(x=Dates,y=DH4d, color = "18-29")) +
    geom_point(data=datDHa, aes(x=Dates,y=DH5d, color = "30-39")) +
    geom_point(data=datDHa, aes(x=Dates,y=DH6d, color = "40-49")) +
    geom_point(data=datDHa, aes(x=Dates,y=DH7d, color = "50-59")) +
    geom_point(data=datDHa, aes(x=Dates,y=DH8d, color = "60-69")) +
    geom_point(data=datDHa, aes(x=Dates,y=DH9d, color = "70+"))  
print(p2)
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_AgeProfile_DH")
svglite(paste0(filenamepath,".svg")); print(p2); invisible(dev.off())
#9 DO
p3 <- ggplot() +
    labs(x = 'Date', y = 'Deaths outside hospital', color = "Legend") + 
    scale_color_manual(values = colors) +
    geom_line (data=datDOa, aes(x=Dates,y=DO1w, color = "0-4")) +
    geom_line (data=datDOa, aes(x=Dates,y=DO2w, color = "05-11")) +
    geom_line (data=datDOa, aes(x=Dates,y=DO3w, color = "12-17")) +
    geom_line (data=datDOa, aes(x=Dates,y=DO4w, color = "18-29")) +
    geom_line (data=datDOa, aes(x=Dates,y=DO5w, color = "30-39")) +
    geom_line (data=datDOa, aes(x=Dates,y=DO6w, color = "40-49")) +
    geom_line (data=datDOa, aes(x=Dates,y=DO7w, color = "50-59")) +
    geom_line (data=datDOa, aes(x=Dates,y=DO8w, color = "60-69")) +
    geom_line (data=datDOa, aes(x=Dates,y=DO9w, color = "70+")) +
    geom_point(data=datDOa, aes(x=Dates,y=DO1d, color = "0-4")) +
    geom_point(data=datDOa, aes(x=Dates,y=DO2d, color = "05-11")) +
    geom_point(data=datDOa, aes(x=Dates,y=DO3d, color = "12-17")) +
    geom_point(data=datDOa, aes(x=Dates,y=DO4d, color = "18-29")) +
    geom_point(data=datDOa, aes(x=Dates,y=DO5d, color = "30-39")) +
    geom_point(data=datDOa, aes(x=Dates,y=DO6d, color = "40-49")) +
    geom_point(data=datDOa, aes(x=Dates,y=DO7d, color = "50-59")) +
    geom_point(data=datDOa, aes(x=Dates,y=DO8d, color = "60-69")) +
    geom_point(data=datDOa, aes(x=Dates,y=DO9d, color = "70+"))  
print(p3)
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_AgeProfile_DO")
svglite(paste0(filenamepath,".svg")); print(p3); invisible(dev.off())  
}

##summary in text file
#10
#plot.new()
for (i in 1:2){
filenamepath =  paste0(output_dir,"/",pset$File_fit_summary0,"_",i)
txt = readLines(paste0(filenamepath,".txt"))
if(i==1){
plot.new()
gridExtra::grid.table(txt, theme=ttheme_default(base_size = 3, padding = unit(c(1, 1),"mm") ))
#p<-gridExtra::grid.table(txt, theme=ttheme_default(base_size = 4, padding = unit(c(1, 1),"mm") ))
#print(p)
}
svglite(paste0(filenamepath,".svg")); 
print(gridExtra::grid.table(txt, theme=ttheme_default(base_size = 6, padding = unit(c(1, 1),"mm") )) ); 
invisible(dev.off())
}

#need?
#pdf(file = paste0(output_dir,"/",pset$File_fit_variables), height=nrow(mE$byw)/3)
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_variables")
svglite(paste0(filenamepath,".svg")) #, height=nrow(mE$byw)/3); #os complained
   gridExtra::grid.table(round(mE$byw[c("time","St","Ht","Hw","Dt","Dw")])) 
dev.off()

#if (pset$iplatform>0){
#  
#pdf(file = paste0(output_dir,"/",pset$File_fit_data1), height=nrow(datDH)/3)
#  gridExtra::grid.table(datH[c("Weeks","zdw")])
#dev.off()
#pdf(file = paste0(output_dir,"/",pset$File_fit_data2), height=nrow(datDD)/3)
#  gridExtra::grid.table(datDH[c("Weeks","wdw")])
#dev.off()
#pdf(file = paste0(output_dir,"/",pset$File_fit_data2), height=nrow(datDD)/3)
#  gridExtra::grid.table(datDO[c("Weeks","vdw")])
#dev.off()

## pdf end




