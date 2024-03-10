
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
  
  Week1_Model  = lubridate::week("2020-01-27") #"2020-02-24")   #[1]  8 #from SEIR_contacts
  Week2_Model  = lubridate::week("2021-01-17")+52 #only days #3 #no.weeks in 2020 = 53
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
  Week1_Model  = lubridate::week("2020-01-27")    #  4 #start of "contact" data
  Week2_Study  = lubridate::week("2020-12-01")    # 48 #start of vacc & alpha
  Week2_Model  = Week1_Model + (pars$nw-1)        # 55 # model set to run pars$nw=52 weeks 
  Week1_Fit_H  = max( c(min( datHa_m_l$Week), Week1_Model), na.rm=T)              #  4 #max(c(min(1:48),4))
  Week1_Fit_DH = max( c(min(datDHa_m_l$Week), Week1_Model), na.rm=T)              #  4 #max(c(min(1:48),4))
  Week1_Fit_DO = max( c(min(datDOa_m_l$Week), Week1_Model), na.rm=T)              #  4 #max(c(min(1:48),4))
  Week2_Fit_H  = min( c(max( datHa_m_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(max(1:48),55,48))
  Week2_Fit_DH = min( c(max(datDHa_m_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(max(1:48),55,48))
  Week2_Fit_DO = min( c(max(datDOa_m_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(max(1:48),55,48))

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
  imodelH  = idataH  - Week_shift_model #1:45 = 4:48-(4-1)
  imodelDH = idataDH - Week_shift_model #1:45 = 4:48-(4-1)
  imodelDO = idataDO - Week_shift_model #1:45 = 4:48-(4-1)
  
  print(paste0("#H  data pts fitted, #: ", length(idataH),   ", list: ", range(idataH)[1],  "...",range(idataH)[2]))    #45, 4...48
  print(paste0("#DH data pts fitted, #: ", length(idataDH),  ", list: ", range(idataDH)[1], "...",range(idataDH)[2]))   #45, 4...48
  print(paste0("#DO data pts fitted, #: ", length(idataDO),  ", list: ", range(idataDO)[1], "...",range(idataDO)[2]))   #45, 4...48
  print(paste0("#H  model pts used,  #: ", length(imodelH),  ", list: ", range(imodelH)[1], "...",range(imodelH)[2]))   #45, 1...45
  print(paste0("#DH model pts used,  #: ", length(imodelDH), ", list: ", range(imodelDH)[1],"...",range(imodelDH)[2]))  #45, 1...45
  print(paste0("#DO model pts used,  #: ", length(imodelDO), ", list: ", range(imodelDO)[1],"...",range(imodelDO)[2]))  #45, 1...45
  # => Dont need specific idataH, imodelH, etc - idata and imodel suffice
  
#Initial conditions for model.cpp, each age group 1:9 - for variables with full reporting
  #for (i in 1:9){ 
  #  value = which( !is.na(datHa_l$Week)  & datHa_l$ageg==i  & datHa_l$Week==Week1_Fit_H )
  #  pars$Ha0[i] = datHa_l$Freq[value] 
  #  value = which( !is.na(datDHa_l$Week) & datDHa_l$ageg==i & datDHa_l$Week==Week1_Fit_H )
  #  pars$DHa0[i] = datDHa_l$Freq[values]
  #  value = which( !is.na(datDHa_l$Week) & datDHa_l$ageg==i & datDHa_l$Week==Week1_Fit_H )
  #  pars$DOa0[i] = datDHa_l$Freq[values] 
  #}

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


### Model IC based on data
### (no merging, as only applies to data fitting)
### can only initialise: Ha0[] and Da0[]
### can be inconsistent with other state variables, which, except for Ea0, are assumed=0
# Ea0 >0 => Ia and Ua increase afterwards (while one of Ia0 or Ua0 wont do that)
if (pset$iplatform==2 & pset$DataIC==1){ #>0
for (i in 1:9){ #assigining current incidence to  state variables (neglecting remains of state from previously)
  valueH  = which( datHa_l$ageg==i &  datHa_l$Week==Week1_Fit_H)
  valueDH = which(datDHa_l$ageg==i & datDHa_l$Week==Week1_Fit_H)
  valueDO = which(datDOa_l$ageg==i & datDOa_l$Week==Week1_Fit_H)
  pars$Ha0[i] =  datHa_l$Freq[valueH]
  pars$Da0[i] = datDHa_l$Freq[valueDH] + datDOa_l$Freq[valueDO]
}}
  

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
  weightz_m=c(    ageonsH*pars$Npop/(    agecohH*pars$Npopcoh))
  weightw_m=c(    ageonsD*pars$Npop/(    agecohD*pars$Npopcoh))
  weightv_m=c(    ageonsD*pars$Npop/(    agecohD*pars$Npopcoh))
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
## zd4w-zd9w, wd6w-wd9w, vd6w-vd9w i=4:9 or 6:9
for (i in (9-ndH+1):9){ #weekly incidence of hospitalisations
  values = eval(parse(text = paste0("zd",eval(i))))*weightz_m[i]
  assign(paste0("zd",eval(i),"w"),round(values, 0))
  assign(paste0("sd",eval(i),"w"),sd(values))       #sd4w-sd9w
  }
for (i in (9-ndDH+1):9){ #weekly incidence of deaths in hospital
  values =  round( eval(parse(text = paste0("wd",eval(i))))*weightw_m[i], 0)
  assign(paste0("wd",eval(i),"w"),values) }
for (i in (9-ndDO+1):9){ #weekly incidence of deaths in hospital
  values =  round( eval(parse(text = paste0("vd",eval(i))))*weightv_m[i], 0)
  assign(paste0("vd",eval(i),"w"),values) }



### Parameter bounds
pkMin    = 0.1  #1/k^2, NB likelihood and data
pkMax    = 5    #Assume: same kD for DH and DO

sdHMin   = 0
sdHMax   = 5 #multiplicative factor of the sd across the age-glrups fitted

tMax     = 20 #10
tMax2    = 50 #30
tMin     = 1  #implicit in LOWER and par resacaling
tMin2    = 1  #implicit in UPPER and par resacaling

yAMax    = 2
yAMin    = 0.01

ArseedMin = 0
ArseedMax = (2*84)*2 #sum(pars$rseed)*2

R0Max    = 20 #10
R0Min    = 0.01

pE0Max   = 0.10
pE0Min   = 0.00001   #pop = 565.5

adMax    = 0.99
adMin    = 0.0099

hAMax    = 2 #1
hAMin    = 0.01

hMax    = 0.99
hMin    = 0.00001


### LIKELIHOOD FUNCTION

source(file = paste0(input_dir,"/BETA.r")) #Used within Likelihood
LogLikelihood <- function(theta){
  ### UPDATE: @@
  ###         proposal pars$
  ###         MAP      parsE$, 
  ###         sample   parsES$
  ### Proposed parameters
  pars$rEI = 1/(  theta[1]*tMax)
  pars$rID = 1/(  theta[2]*tMax2)
  pars$R0  =  exp(theta[3]) #^2
  pars$pE0 =      theta[4]
  #Arseed   =      theta[5]
  yA       =      theta[5]
  hA       =      theta[6] #hAMin+theta[]*(hAMax-hAMin)
  pars$ad  =      theta[7] #adMin+theta[]*(adMax-adMin)
  sdH      =      theta[8]
  #kH     = 1/(  theta[8]*theta[8])    # pk = theta = 1/sqrt(k) => k = 1/pk^2
  kDH      = 1/(  theta[9]*theta[9])
  kDO      = kDH
  #kDO      = 1/(  theta[3]*theta[3])
  #Dependent parameters
  pars$h     = hA*pars$h
  pars$y     = yA*pars$y
  #pars$rseed = Arseed*pars$ageons
  pars$Ea0   = pars$Na0*pars$pE0
  pars$Sa0   = pars$Na0 - pars$Ea0 - pars$Ia0 - pars$Ua0 - pars$Ha0 - pars$Oa0 - pars$Ra0 - pars$Da0   
  pars$beta  = BETA(pars) 

  ### Model outputs (from Rcpp, given the above proposed parameters)
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
  values = eval(parse(text = paste0("m$byw_age$H",eval(i),"w[imodelH]"))) #Consistent: (1 + mE$byw$time[imodelH]/7) + (Week1_Model-1)
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
  #Negative binomial likelihood or
  #Normal likelihood
  #ll = sum(dnbinom(x = zd4w, size = kH, mu = MeanH4, log = T)) +
       #sum(dnbinom(x = zd5w, size = kH, mu = MeanH5, log = T)) +
       #sum(dnbinom(x = zd6w, size = kH, mu = MeanH6, log = T)) +
       #sum(dnbinom(x = zd7w, size = kH, mu = MeanH7, log = T)) +
       #sum(dnbinom(x = zd8w, size = kH, mu = MeanH8, log = T)) +
       #sum(dnbinom(x = zd9w, size = kH, mu = MeanH9, log = T)) +
  ll = sum(  dnorm(x = zd4w, sd = sdH*sd4w, mean = MeanH4, log = T)) +
       sum(  dnorm(x = zd5w, sd = sdH*sd5w, mean = MeanH5, log = T)) +
       sum(  dnorm(x = zd6w, sd = sdH*sd6w, mean = MeanH6, log = T)) +
       sum(  dnorm(x = zd7w, sd = sdH*sd7w, mean = MeanH7, log = T)) +
       sum(  dnorm(x = zd8w, sd = sdH*sd8w, mean = MeanH8, log = T)) +
       sum(  dnorm(x = zd9w, sd = sdH*sd9w, mean = MeanH9, log = T)) +
       ###
       sum(dnbinom(x = wd6w, size = kDH,    mu = MeanDH6*link, log = T)) +
       sum(dnbinom(x = wd7w, size = kDH,    mu = MeanDH7*link, log = T)) +
       sum(dnbinom(x = wd8w, size = kDH,    mu = MeanDH8*link, log = T)) +
       sum(dnbinom(x = wd9w, size = kDH,    mu = MeanDH9*link, log = T)) +
       ###
       sum(dnbinom(x = vd6w, size = kDO,    mu = MeanDO6*link, log = T)) +
       sum(dnbinom(x = vd7w, size = kDO,    mu = MeanDO7*link, log = T)) +
       sum(dnbinom(x = vd8w, size = kDO,    mu = MeanDO8*link, log = T)) +
       sum(dnbinom(x = vd9w, size = kDO,    mu = MeanDO9*link, log = T))
    
    return(ll)
} #Likelihood
## iplatform==0, simulated data, model(pars), pars from parameters  ##  -5593.537
## iplatform==1,     dummy data, model(pars), pars from parameters  ##  -3223.607


## Likelihood definition, parameter ranges  ####################################
niter = 30000#6000##3000
if (pset$iplatform==2){niter=120000} #200000

          #rEI,    rID,     R0,          pE0,    yA/Arseed,    hA,    ad,     sdH,   kD
LOWER = c(1/tMax, 1/tMax2, log(R0Min),  pE0Min,  yAMin, hAMin, adMin,  sdHMin, pkMin);
UPPER = c(1,      1,       log(R0Max),  pE0Max,  yAMax, hAMax, adMax,  sdHMax, pkMax);

#Uniform priors
  #PARSTART = 0.5*UPPER
  #nchain = 3
  #setup    = createBayesianSetup(likelihood=LogLikelihood, lower = LOWER, upper = UPPER) #parallel = T,
  #settings = list (startValue=t(array(PARSTART,dim=c(length(PARSTART),nchain))), iterations = niter, burnin = round(niter*length(LOWER)/15), message=T) #F)
#Beta priors
  Burnin = round(niter/2)+1  #round(niter/4)+1 #+1 as "burnin" is the start of effective sample
  PRIOR <- createBetaPrior(3,3,lower = LOWER, upper = UPPER)
  setup  = createBayesianSetup(likelihood=LogLikelihood, prior =PRIOR) #parallel = T,
  settings = list (iterations = niter, burnin = Burnin, message=T) #F) #round(niter*length(LOWER)/15), message=T) #F)

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
print("MAP..."); cat("\n")
parsE     <- pars     #pars setup initially
MAPE      <- MAP(out) #pasr estimated
### UPDATE: @@
###         proposal pars$
###         MAP      parsE$, 
###         sample   parsES$
#kH,     kD,       rEI,    rID,     yA/Arseed,         R0,         pE0,    #ad, h4-h9
parsE$rEI <- 1/(  as.vector(MAPE$parametersMAP[1]*tMax))
parsE$rID <- 1/(  as.vector(MAPE$parametersMAP[2]*tMax2))
parsE$R0  <-  exp(as.vector(MAPE$parametersMAP[3])) #^2)
parsE$pE0 <-      as.vector(MAPE$parametersMAP[4])
#ArseedE   <-      as.vector(MAPE$parametersMAP[5])
yAE       <-      as.vector(MAPE$parametersMAP[5])
hAE       <-      as.vector(MAPE$parametersMAP[6])
parsE$ad  <-      as.vector(MAPE$parametersMAP[7])
#parsE$kH  <- 1/(  as.vector(MAPE$parametersMAP[8]^2)) #1/pk^2
parsE$sdH <-      as.vector(MAPE$parametersMAP[8])
parsE$kDH <- 1/(  as.vector(MAPE$parametersMAP[9]^2))
parsE$kDO <- parsE$kDH
#Dependent parameters
parsE$h     = hAE*pars$h #parsE$h
parsE$y     = yAE*pars$y
#parsE$rseed =   ArseedE*pars$ageons
parsE$Ea0   = parsE$Na0*parsE$pE0
parsE$Sa0   = parsE$Na0 - parsE$Ea0 - parsE$Ia0 - parsE$Ua0 - parsE$Ha0 - parsE$Oa0 - parsE$Ra0 - parsE$Da0   
parsE$beta  = BETA(parsE)
#Predictions
mE        <- model(parsE)
#R0_week using MAP-parameters
source(file = paste0(input_dir,"/R0.r"))
ntimes = length(imodelH)
r0 = R0(parsE, GetBeta=0, GetOutput=1, Sampling=1, nt=ntimes)
R0_weekE = r0[[2]]$R0_week
#Note: R0(parsE, GetBeta=0, GetOutput=1, Sampling=1, nt=ntimes)[[1]]$R0 - same as parsE$R0


## Credible Intervals ##########################################################
## Sample the chains
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
print("Sampling..."); cat("\n")
  
npar = length(LOWER)
Thin=4#2#4
Chains=3
StartSampChainPostBurn = 1
LengtMcmcChainPostBurn = floor((niter-Burnin+1)/Chains)
LengtSampChainPostBurn = LengtMcmcChainPostBurn - StartSampChainPostBurn + 1
#nsample = 100#3000#500#1000#;
nsample = Chains*LengtMcmcChainPostBurn/Thin
#start-end - for each chain
#Note: parametersOnly = F = gives extra: Lposterior Llikelihood     Lprior
psample = getSample(out, parametersOnly = T, start=StartSampChainPostBurn, end= LengtSampChainPostBurn, thin=Thin)
#dim(psample)#  = c(nsample, npar)
# run model for each parameter set in the sample
zsample = matrix(0,ntimes,nsample)
wsample = matrix(0,ntimes,nsample)
vsample = matrix(0,ntimes,nsample)
parsES  = pars #parsE
### UPDATE: @@
###         proposal pars$
###         MAP      parsE$, 
###         sample   parsES$
for(i in 1:nsample){
  #rEI,     rID,     R0,     pE0,     yA/Arseed,     hA,     ad,     sdH,     kD
  parsES$rEI <- 1/(  as.vector(psample[i,1])*tMax)
  parsES$rID <- 1/(  as.vector(psample[i,2])*tMax2)
  parsES$R0  <-  exp(as.vector(psample[i,3])) #^2
  parsES$pE0 <-      as.vector(psample[i,4])
  #ArseedES   <-      as.vector(psample[i,5])
  yAES       <-      as.vector(psample[i,5])
  hAES       <-      as.vector(psample[i,6])
  parsES$ad  <-      as.vector(psample[i,7])
  #Dependent parameters
  parsES$h    =       hAES*pars$h #parsES$h
  parsES$y    =       yAES*pars$y
  #parsES$rseed=   ArseedES*pars$ageons
  parsES$Ea0  = parsES$Na0*parsES$pE0
  parsES$Sa0  = parsES$Na0 - parsES$Ea0 - parsES$Ia0 - parsES$Ua0 - parsES$Ha0 - parsES$Oa0 - parsES$Ra0 - parsES$Da0 
  parsES$beta = BETA(parsES)
  outs        = model(as.vector(parsES))
  zsample[,i] = outs$byw$Hw[imodelH]
  wsample[,i] = outs$byw$DHw[imodelH]
  vsample[,i] = outs$byw$DOw[imodelH]  
} 
Weekssample = 1 + outs$byw$time[imodelH]/7 + Week_shift_model
Datessample = as.Date(paste(Weekssample, "2020", 'Mon'), '%U %Y %a') #Checked: "Mon" consistent with weeks/dates def throughout 
##95% CrI
zsample95 = matrix(0,ntimes,2)
wsample95 = matrix(0,ntimes,2)
vsample95 = matrix(0,ntimes,2)
q1=0.01 #0.05
q2=0.99 #0.95
for(it in 1:ntimes){
  samp_it <- zsample[it,]
  zsample95[it,1] = quantile(samp_it,q1)[[1]]
  zsample95[it,2] = quantile(samp_it,q2)[[1]]
  samp_it <- wsample[it,]
  wsample95[it,1] = quantile(samp_it,q1)[[1]]
  wsample95[it,2] = quantile(samp_it,q2)[[1]]
  samp_it <- vsample[it,]
  vsample95[it,1] = quantile(samp_it,q1)[[1]]
  vsample95[it,2] = quantile(samp_it,q2)[[1]]
}
## R0_week
nsampleR0 = min(750,nsample) #300#100
R0weeksample = matrix(0,ntimes,nsampleR0)
for(i in 1:nsampleR0){
  #rEI,     rID,     R0,     pE0,     yA/Arseed,     hA,     ad,     sdH,     kD
  parsES$rEI <- 1/(  as.vector(psample[i,1])*tMax)
  parsES$rID <- 1/(  as.vector(psample[i,2])*tMax2)
  parsES$R0  <-  exp(as.vector(psample[i,3])) #^2
  parsES$pE0 <-      as.vector(psample[i,4])
  #ArseedES   <-      as.vector(psample[i,5])
  yAES       <-      as.vector(psample[i,5])
  hAES       <-      as.vector(psample[i,6])
  parsES$ad  <-      as.vector(psample[i,7])
  #Dependent parameters
  parsES$h    =       hAES*pars$h #parsES$h
  parsES$y    =       yAES*pars$y
  #parsES$rseed=   ArseedES*pars$ageons
  parsES$Ea0  = parsES$Na0*parsES$pE0
  parsES$Sa0  = parsES$Na0 - parsES$Ea0 - parsES$Ia0 - parsES$Ua0 - parsES$Ha0 - parsES$Oa0 - parsES$Ra0 - parsES$Da0 
  parsES$beta = BETA(parsES)
  outs        = model(as.vector(parsES))
  R0weeksample[,i] = R0(parsES, GetBeta=0, GetOutput=1, Sampling=1, nt=ntimes)[[2]]$R0_week 
}
R0weeksample95 = matrix(0,length(imodelH),2)
for(it in 1:ntimes) {
  samp_it <- R0weeksample[it,]
  R0weeksample95[it,1] = quantile(samp_it,0.01)[[1]] #min(samp_it) #quantile(samp_it,q1)[[1]]
  R0weeksample95[it,2] = quantile(samp_it,0.99)[[1]] #max(samp_it) #quantile(samp_it,q2)[[1]]
}


} 
## Credible Intervals ##########################################################



##### Summary - txt output
SCREEN=0
if(SCREEN==1){sink(file = paste0(output_dir,"/","screen.txt"),append=FALSE,split=FALSE)
  cat("\n"); print(paste0(format(Sys.Date(), "%d-%m-%Y"), " - ", format(Sys.time(),'%H.%M.%S_%d-%m-%Y'))); cat("\n")
  print("Summary 1..."); cat("\n")
sink()}

sink(file = paste0(output_dir,"/",pset$File_fit_summary_1),append=FALSE,split=FALSE)
print(paste0("Likelihood NB"))
#cat("\n"); 
## Run time
print(out$settings$runtime)
print(paste0("Time used (sec): ", round(tout1[[3]],3)))
cat("\n")
### UPDATE: @@
#Expected parameters
thetaTrue = c(pars$rEI, pars$rID, pars$R0, pars$pE0, 1, 1, pars$ad, pars$sdH, pars$kDH) #pars$h[1:9]);
## MAP Estimates
#print(paste0("kH    MAP: ", round(parsE$kH,    3), ". Expected: ", round(  pars$kH,      3))) #kH
print(paste0("sdH   MAP: ", round(parsE$sdH,   3),    ". Expected: ", round(  pars$sdH,     3))) #sdH
print(paste0("kDH,kDO MAP:",round(parsE$kDH,   3),    ". Expected: ", round(  pars$kDH,     3))) #kD
print(paste0("1/rEI MAP: ", round(1/parsE$rEI, 3),    ". Expected: ", round(1/pars$rEI,     3))) #rEI
print(paste0("1/rID MAP: ", round(1/parsE$rID, 3),    ". Expected: ", round(1/pars$rID,     3))) #rID
print(paste0("R0    MAP: ", round(parsE$R0,    3),    ". Expected: ", round(  pars$R0,      3))) #R0
print(paste0("pE0   MAP: ", round(parsE$pE0,   4),    ". Expected: ", round(  pars$pE0,     3))) #pE0
#print(paste0("rseed MAP: ", round(sum(parsE$rseed), 0), ". Expected: ", round(sum(pars$rseed), 0) )) #rseed
print(paste0("yA    MAP: ", round(parsE$y[9]/pars$y[9], 4), ". Expected: ")) #yA
print(paste0("hA    MAP: ", round(parsE$h[9]/pars$h[9], 4), ". Expected: ")) #hA
print(paste0("ad    MAP: ", round(parsE$ad,    4),    ". Expected: ", round(  pars$ad,      3))) #ad
print(paste0("beta  dep: ", round(parsE$beta,     5) ))
print(paste0("E0    dep: ", round(sum(parsE$Ea0), 0), ". Expected: ", round(sum(pars$Na0*pars$pE0)) ))
print(paste0("Estimated proportion deaths outside hospital = ", round(parsE$ad/(1+parsE$ad),3)))
cat("\n");
print(summary(out)); 
cat("\n")
print(paste0("Mean by chain and parameter:"))
print(out$X)
sink()


print("Summary 2..."); cat("\n")
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
#print("Priors")
print(out$setup$prior)
cat("\n"); 
cat("\n")
sink()



##### Plots - Overall dataframes ###############################################
print("Data frames..."); cat("\n")
N  = pars$Npop
Nc = pars$Npopcoh
if(pset$iplatform<2) {weight=1} else {weight=N/Nc}

datH <- tibble(Weeks  = 1 + mE$byw$time[imodelH]/7 + Week_shift_model,    #model time 0 <> week1 + (Week1_Model-1) = 1 + (4-1) = week4
               Dates  = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'), #Checked: "Mon" consistent with weeks/dates def throughout 
               H_est  = mE$byw$Hw[imodelH],
               zdw    = zd*weight,
               R0_week = R0_weekE[imodelH])
datD <- tibble(Weeks  = 1 + mE$byw$time[imodelDH]/7 + Week_shift_model,
               Dates  = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'), #Checked: "Mon" consistent with weeks/dates def throughout 
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

#y axis log transformation - default: linear
YLOG <- function(y,LOG=0){ if (LOG==1) {z=log10(y+1)} else {z=y}; return(z) }
LOG=1; #0 #apply scale of plotting Age Profiles

if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
#indices of data vectors
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

#Variables (Model and data)  
  for (i in 1:9){
#H Model (MAP)
    values = eval(parse(text = paste0("mE$byw_age$H",eval(i),"w[imodelH]")))      #Hiw <= mE$byw_age$Hiw
    assign(paste0("H",eval(i),"w"),  YLOG(values,LOG))
#H data: OS or simulated (actually, true model, as data too noisy)
    if (pset$iplatform>0){     #Hid <= datHa_l$Freq[idataHi]                  or  #Hid <= datM$H_modi
    values = eval(parse(text = paste0("datHa_l$Freq[idataH",eval(i),"]*weightz[",eval(i),"]")))  } else { 
    values = eval(parse(text = paste0("datM$H_mod",eval(i),"[imodelH]"))) }
    assign(paste0("H",eval(i),"d"),  YLOG(values,LOG))
#DH Model (MAP)
    values = eval(parse(text = paste0("mE$byw_aHO$DH",eval(i),"w[imodelDH]")))   #DHiw <= mE$byw_aHO$DHiw
    assign(paste0("DH",eval(i),"w"), YLOG(values,LOG))
#DH data 
    if (pset$iplatform>0){   #DHid <= datDHa_l$Freq[idataDHi]                or  #DHid <= datM$DH_modi 
    values = eval(parse(text = paste0("datDHa_l$Freq[idataDH",eval(i),"]*weightw[",eval(i),"]"))) } else {
    values = eval(parse(text = paste0("datM$DH_mod",eval(i),"[imodelDH]"))) }
    assign(paste0("DH",eval(i),"d"), YLOG(values,LOG))
#DO Model (MAP)
    values = eval(parse(text = paste0("mE$byw_aHO$DO",eval(i),"w[imodelDO]")))   #DOiw <= mE$byw_aHO$DOiw
    assign(paste0("DO",eval(i),"w"), YLOG(values,LOG))
#DO data
  if (pset$iplatform>0){     #DOid <= datDOa_l$Freq[idataDOi]                or  #DOid <= datM$DO_modi
    values = eval(parse(text = paste0("datDOa_l$Freq[idataDO",eval(i),"]*weightv[",eval(i),"]"))) } else {
    values = eval(parse(text = paste0("datM$DO_mod",eval(i),"[imodelDO]"))) }
    assign(paste0("DO",eval(i),"d"), YLOG(values,LOG))
}

#H
datHa  <- tibble(Weeks = 1 + mE$byw$time[imodelH]/7 + Week_shift_model,
                 Dates  = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'), #Checked: "Mon" consistent with weeks/dates def throughout 
                 H1w=H1w, H2w=H2w, H3w=H3w, H4w=H4w, H5w=H5w, #estimated
                 H6w=H6w, H7w=H7w, H8w=H8w, H9w=H9w,
                 H1d=H1d, H2d=H2d, H3d=H3d, H4d=H4d, H5d=H5d, #data
                 H6d=H6d, H7d=H7d, H8d=H8d, H9d=H9d)
#DH
datDHa <- tibble(Weeks = 1 + mE$byw$time[imodelDH]/7 + Week_shift_model,
                 Dates = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'), #Checked: "Mon" consistent with weeks/dates def throughout 
                 DH1w=DH1w, DH2w=DH2w, DH3w=DH3w, DH4w=DH4w, DH5w=DH5w,    #estimated
                 DH6w=DH6w, DH7w=DH7w, DH8w=DH8w, DH9w=DH9w,
                 DH1d=DH1d, DH2d=DH2d, DH3d=DH3d, DH4d=DH4d, DH5d=DH5d,    #data
                 DH6d=DH6d, DH7d=DH7d, DH8d=DH8d, DH9d=DH9d)
#DO
datDOa <- tibble(Weeks = 1 + mE$byw$time[imodelDO]/7 + Week_shift_model,   
                 Dates = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'), #Checked: "Mon" consistent with weeks/dates def throughout 
                 DO1w=DO1w, DO2w=DO2w, DO3w=DO3w, DO4w=DO4w, DO5w=DO5w,    #estimated
                 DO6w=DO6w, DO7w=DO7w, DO8w=DO8w, DO9w=DO9w,
                 DO1d=DO1d, DO2d=DO2d, DO3d=DO3d, DO4d=DO4d, DO5d=DO5d,    #data
                 DO6d=DO6d, DO7d=DO7d, DO8d=DO8d, DO9d=DO9d)

} #Plots - Age profile dataframes (not merged)
                 
                    


#### pdf Plots #################################################################
#pdf(file = paste0(output_dir,"/",pset$File_fit_output))

#### svg plots #################################################################

#### Diagnostics

#1 marginal
if(SCREEN==1){sink(file = paste0(output_dir,"/","screen.txt"),append=TRUE,split=FALSE)
  cat("\n"); print("Fig 1..."); cat("\n")
sink()}
#par(mar =c(0,0,0,0)) # c(0.5, 1, 1, 1)) #ar(mar = c(2, 2, 1, 1))  #bottom, left, top, right
#p<-marginalPlot(out); print(p)
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_marginalPlot")
svglite(paste0(filenamepath,".svg")); marginalPlot(out); invisible(dev.off())

#2-3 trace
if(SCREEN==1){sink(file = paste0(output_dir,"/","screen.txt"),append=TRUE,split=FALSE)
  cat("\n"); print("Fig 2-3..."); cat("\n")
sink()}
par(mar = c(2, 2, 1, 1)) ##bottom, left, top, right
p<-plot(out); print(p)
if (pset$iplatform==0){
  filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_plotout_lastPage")
  svglite(paste0(filenamepath,".svg")); plot(out); invisible(dev.off()) }

#4 correlations
if(SCREEN==1){sink(file = paste0(output_dir,"/","screen.txt"),append=TRUE,split=FALSE)
  cat("\n"); print("Fig 4..."); cat("\n")
sink()}
par(mar = c(0,0,0,0)) #par(mar = c(2, 2, 1, 1))
#pdf("corr.pdf")
#p<-correlationPlot(out); print(p)  #Error in plot.new() : figure margins too large  (16 par)
#dev.off()
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_correlationPlot")
svglite(paste0(filenamepath,".svg")); correlationPlot(out); invisible(dev.off())


### Results

#5 overall
if(SCREEN==1){sink(file = paste0(output_dir,"/","screen.txt"),append=TRUE,split=FALSE)
  cat("\n"); print("Fig 5..."); cat("\n")
sink()}
colors <- c(  "I_dat"  = "black",   "I_est" = "red",     "I_model" = "green",
             "H_datw"  = "black",   "H_est" = "red",     "H_model" = "pink",
            "DH_datw"  = "grey",   "DH_est" = "blue",   "DH_model" = "cyan",
            "DO_datw"  = "black",  "DO_est" = "green4", "DO_model" = "green", "R0_est" = "grey40")

coeff <- (max(datH$zdw)/max(datH$R0_week))

p1 <- ggplot() +
          #xlim(c(0, NA)) +  ylim(c(0, NA)) + #Dont use with Dates, only with Weeks
          scale_color_manual(values = colors) +
          geom_point(data=datH, aes(x=Datesz,y=zdw,    color =  "H_datw"), size = 1.4,   pch = 19) +
          geom_point(data=datD, aes(x=Datesw,y=wdw,    color = "DH_datw"), size =   1,   pch = 16) +
          geom_point(data=datD, aes(x=Datesv,y=vdw,    color = "DO_datw"), size =   1,   pch = 1) +
          geom_line (data=datH, aes(x=Dates, y=H_est,  color =  "H_est")) +
          geom_line (data=datD, aes(x=Dates, y=DH_est, color = "DH_est")) +
          geom_line (data=datD, aes(x=Dates, y=DO_est, color = "DO_est")) +
          geom_point(data=datH, aes(x=Dates, y=H_est,  color =  "H_est")) +
          geom_point(data=datD, aes(x=Dates, y=DH_est, color = "DH_est")) +
          geom_point(data=datD, aes(x=Dates, y=DO_est, color = "DO_est")) +
          geom_line (data=datH, aes(x=Dates, y=R0_week*coeff,color = "R0_est"))

if (pset$iplatform==0) {
p1 <- p1 + 
          geom_line (data=datH, aes(x=Dates,y=H_mod,  color =  "H_model")) +
          geom_line (data=datD, aes(x=Dates,y=DH_mod, color = "DH_model")) +
          geom_line (data=datD, aes(x=Dates,y=DO_mod, color = "DO_model"))  }
#secondary axis for R0
p1 <- p1 + 
          scale_y_continuous(
            name = 'Hospitalisations & deaths in & outside hospital',
            sec.axis = sec_axis(~.*(1/coeff), name="R0") ) + 
          labs(x = 'Date', color = "Variable") + #Legend") + #y = 'Hospitalisations & deaths in & outside hospital', color = "Legend") + 
          theme(axis.title.y       = element_text(color = 1),
                axis.title.y.right = element_text(color = 1) ,
                axis.text          = element_text(size = 16),
                axis.title         = element_text(size = 18, face = "bold"),
                legend.title       = element_text(size = 18),
                legend.text        = element_text(size = 14))
print(p1)

filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_Overall")
sc = 3#10
svglite(paste0(filenamepath,".svg"),width=sc*6, height=sc*3); print(p1); invisible(dev.off())



#6 Plot posterior samples
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
if(SCREEN==1){sink(file = paste0(output_dir,"/","screen.txt"),append=TRUE,split=FALSE)
  cat("\n"); print("Fig 6..."); cat("\n")
sink()}
##
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_PosteriorSample")
##
par(mfrow = c(4,1))
par(mar = c(2, 2, 1, 1)) #bottom, left, top, right
colors <- c("Data" = 1,  "MAP" = 2, "95% CrI" = "grey70", "95% perc" = "grey70")
zMAX = rep(range(zsample)[2],length(Datessample))
#H
dzsample <- tibble(Date=Datessample, zsample05=zsample95[,1], zsample95=zsample95[,2],
                   zMAP=outs$byw$Hw[imodelH], Datez=datH$Datesz, zdw=datH$zdw, yLIM=zMAX )
p1 <- ggplot(dzsample, aes(x=Date)) + 
      geom_ribbon(aes(ymin = zsample05, ymax = zsample95), fill = "grey70") +
      geom_point(aes(x=Datez, y = zdw,       color="Data")) +
      geom_line (aes(x=Date,  y = zMAP,      color="MAP")) +
      geom_line (aes(x=Date,  y = zsample05, color="95% CrI")) +
      labs(x = "", y = 'Hospitalisations',   color = "") + #Legend") + 
      scale_color_manual(values = colors) +
      theme(axis.title         = element_text(size = 12, face = "bold"))#,
           #axis.title.y       = element_text(color = 1),
           #axis.title.y.right = element_text(color = 1) ,
           #axis.text          = element_text(size = 16),
           #legend.title       = element_text(size = 16),
           #legend.text        = element_text(size = 12))

#DH
dwsample <- tibble(Date=Datessample, wsample05=wsample95[,1], wsample95=wsample95[,2],
                   wMAP=outs$byw$DHw[imodelDH], Datew=datD$Datesw, wdw=datD$wdw, yLIM=zMAX ) 
p2 <- ggplot(dwsample, aes(x=Date)) + 
      geom_ribbon(aes(ymin = wsample05, ymax = wsample95), fill = "grey70") +
      geom_point(aes(x=Datew, y = wdw,       color="Data")) +
      geom_line (aes(x=Date,  y = wMAP,      color="MAP")) +
      geom_line (aes(x=Date,  y = wsample05, color="95% CrI")) +
      labs(x = "", y = 'Deaths in hospital', color = "") + #Legend") + 
      scale_color_manual(values = colors) +
      theme(axis.title = element_text(size = 12, face = "bold"))

#DO
dvsample <- tibble(Date=Datessample, vsample05=vsample95[,1], vsample95=vsample95[,2],
                   vMAP=outs$byw$DOw[imodelDO], Datev=datD$Datesv, vdw=datD$vdw, yLIM=zMAX ) 
p3 <- ggplot(dvsample, aes(x=Date)) + 
      geom_ribbon(aes(ymin = vsample05, ymax = vsample95), fill = "grey70") +
      geom_point(aes(x=Datev, y = vdw,       color="Data")) +
      geom_line (aes(x=Date,  y = vMAP,      color="MAP")) +
      geom_line (aes(x=Date,  y = vsample05, color="95% CrI")) +
      labs(x = '', y = 'Deaths outside hospital', color = "") + 
      scale_color_manual(values = colors) +
      theme(axis.title = element_text(size = 12, face = "bold"))

### Plot R0 over time
#filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_PosteriorSampleR0")
colors <- c("Contact mtx" = 1,  "MAP" = 2, "95% CrI" = "grey70", "95% perc" = "grey70")
zMAX = rep(range(R0weeksample)[2],length(Datessample))
#H
dzsample <- tibble(Date=Datessample, R0sample05=R0weeksample95[,1], R0sample95=R0weeksample95[,2],
                   R0MAP=datH$R0_week, R0xcmMEV=r0[[2]]$R0xcmMEV, yLIM=zMAX )
pR0 <-ggplot(dzsample, aes(x=Date)) + 
      geom_ribbon(aes(ymin = R0sample05, ymax = R0sample95), fill = "grey70") +
      geom_point(aes(x=Date, y = R0xcmMEV,   color="Contact mtx")) +
      geom_line (aes(x=Date, y = R0MAP,      color="MAP")) +
      geom_line (aes(x=Date, y = R0sample05, color="95% CrI")) +
      labs(x = 'Date', y = 'R0 estimate', color = "") + 
      scale_color_manual(values = colors) +
      theme(axis.title = element_text(size = 12, face = "bold"))

gridExtra::grid.arrange(p1, p2, p3, pR0, nrow = 4)

svglite(paste0(filenamepath,".svg")); 
gridExtra::grid.arrange(p1, p2, p3, pR0, nrow = 4)
invisible(dev.off())

}


##7 age profiles
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
colors <- c("0-4" = 1, "05-11" = 2,  "12-17" = 3, "18-29" = 4, "30-39" = 5, 
            "40-49" = 6, "50-59" = 7,  "60-69" = 8, "70+" = 9)
Yname = c('Hospitalisations', 'Deaths in hospital', 'Deaths outside hospital')
if (LOG==1) {Yname = c('log Hospitalisations', 'log Deaths in hospital', 'log Deaths outside hospital')}

#H
if(SCREEN==1){sink(file = paste0(output_dir,"/","screen.txt"),append=TRUE,split=FALSE)
  cat("\n"); print("Fig 7..."); cat("\n")
sink()}
p1 <- ggplot() +
    labs(x = "", y = Yname[1], color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold"))+
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

#DH
if(SCREEN==1){sink(file = paste0(output_dir,"/","screen.txt"),append=TRUE,split=FALSE)
  cat("\n"); print("Fig 8..."); cat("\n")
sink()}
p2 <- ggplot() +
    labs(x = "", y = Yname[2], color = "Age group") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold"))+
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

#DO
if(SCREEN==1){sink(file = paste0(output_dir,"/","screen.txt"),append=TRUE,split=FALSE)
  cat("\n"); print("Fig 9..."); cat("\n")
sink()}
p3 <- ggplot() +
    labs(x = 'Date', y = Yname[3], color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold"))+
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

gridExtra::grid.arrange(p1, p2, p3, nrow = 3)

filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_AgeProfiles")
svglite(paste0(filenamepath,".svg")); 
gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
invisible(dev.off())

}

##summary in text file
#10
#if(SCREEN==1){sink(file = paste0(output_dir,"/","screen.txt"),append=TRUE,split=FALSE)
#  cat("\n"); print("Fig/Tab 10..."); cat("\n")
#sink()}
#plot.new()
#for (i in 1:2){
#filenamepath =  paste0(output_dir,"/",pset$File_fit_summary0,"_",i)
#txt = readLines(paste0(filenamepath,".txt"))
#if(i==1){
#plot.new()
#p<-gridExtra::grid.table(txt, theme=ttheme_default(base_size = 2, padding = unit(c(1, 1),"mm") )) #4, padding = unit(c(1, 1),"mm") ))
#print(p)
#}
#svglite(paste0(filenamepath,".svg")); 
#print(gridExtra::grid.table(txt, theme=ttheme_default(base_size = 2, padding = unit(c(1, 1),"mm") )) ); #6, padding = unit(c(1, 1),"mm") )) ); 
#invisible(dev.off())
#}

#need?
#pdf(file = paste0(output_dir,"/",pset$File_fit_variables), height=nrow(mE$byw)/3)
#if(SCREEN==1){ sink(file = paste0(output_dir,"/","screen.txt"),append=TRUE,split=FALSE)
#  cat("\n"); print("Fig 11..."); cat("\n")
#sink()}
#filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_variables")
#svglite(paste0(filenamepath,".svg")) #, height=nrow(mE$byw)/3); #os complained
#   plot(1:10)
#   #gridExtra::grid.table(round(mE$byw[c("time","St","Ht","Hw","Dt","Dw")])) 
#invisible(dev.off())

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




