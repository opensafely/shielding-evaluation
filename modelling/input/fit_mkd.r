
### Dataframes from model:
###   datM$  Weeks, Dataz, Dataw, Datav, Datazi, Datawi, Datavi

### Sourced from main: HDdata.R
### Dataframes with OS data:
###   names(datHa_m_l)  [1] "ageg2"  "Week" "Date" "Freq"  (shield)
###   names(datDHa_m_l) [1] "ageg2"  "Week" "Date" "Freq"  (shield)
###   names(datDOa_m_l) [1] "ageg2"  "Week" "Date" "Freq"  (shield) 


#TODO: extend to shielding: iweeksdataH40-90 or 60-90, and 41-91 or 61-91 <= iweeksdataH4-9 or 6-9

#TODO: mcmc - revise parameters used

#TODO: plots final - x-axis dates not week - or data at start

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
#datM => datH, datDH, datDO
  iweeksmodelH  = seq_along(datM$Weeks)
  iweeksmodelDH = iweeksmodelH
  iweeksmodelDO = iweeksmodelH
  iweeksdataH   = iweeksmodelH #length(datM$Dataz...Dataz9)
  iweeksdataDH  = iweeksmodelH #length(datM$Dataw...Dataw9)
  iweeksdataDO  = iweeksmodelH #length(datM$Datav...Datav9)
  ## iweekdataHi, iweekdataDHi, iweekdataDOi - age independent

  #Temporal data (weekly incidence)
  #defined in simulation.r
  #zd = datM$Dataz[iweeksdata]
  #wd = datM$Dataw[iweeksdata] 
  #vd = datM$Datav[iweeksdata] 
  
  #Replicate in simulated data the merging of ageg in the real data
  # zd4-zd9 - Hospitalisations
  # wd6-wd9 - Deaths in hospital
  # vd6-vd9 - Deaths outside hospital
  ndH  = 6  #data "4" merges 1:4
  ndDH = 4  #data "6" merges 1:6
  ndDO = 4  #data "6" merges 1:6
  zd4 = rep(0,times=length(iweeksmodelH))
  wd6 = rep(0,times=length(iweeksmodelDH))
  vd6 = rep(0,times=length(iweeksmodelDO))
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

  print(paste0("#H  data pts fitted, #: ", length(iweeksdataH),   ", list: ", range(iweeksdataH)[1],  "...",range(iweeksdataH)[2]))    #41, 1...41
  print(paste0("#DH data pts fitted, #: ", length(iweeksdataDH),  ", list: ", range(iweeksdataDH)[1], "...",range(iweeksdataDH)[2]))   #41, 1...41
  print(paste0("#DO data pts fitted, #: ", length(iweeksdataDO),  ", list: ", range(iweeksdataDO)[1], "...",range(iweeksdataDO)[2]))   #41, 1...41
  print(paste0("#H  model pts used,  #: ", length(iweeksmodelH),  ", list: ", range(iweeksmodelH)[1], "...",range(iweeksmodelH)[2]))   #41, 1...41
  print(paste0("#DH model pts used,  #: ", length(iweeksmodelDH), ", list: ", range(iweeksmodelDH)[1],"...",range(iweeksmodelDH)[2]))  #41, 1...41
  print(paste0("#DO model pts used,  #: ", length(iweeksmodelDO), ", list: ", range(iweeksmodelDO)[1],"...",range(iweeksmodelDO)[2]))  #41, 1...41
  
  
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
    values =  which(      !is.na(datHa_m_l$Week) &  datHa_m_l$ageg2==i
               & Week1_Fit_H <=  datHa_m_l$Week  &  datHa_m_l$Week <= Week2_Fit_H) #dates relative to "2020-01-01
    assign(paste0("iweeksdataH", eval(i)),values)                    #iweeskdataH4=8:48 ...  iweeskdataH9
    assign(paste0("zd",eval(i)), datHa_m_l$Freq[values]) }            #zd4 ... zd9
  #weekly incidence of deaths in hospital
  ndDH = 4 #data "6" merges 1:6
  for (i in (9-ndDH+1):9){
    values =  which(      !is.na(datDHa_m_l$Week) & datDHa_m_l$ageg2==i
               & Week1_Fit_DH <= datDHa_m_l$Week  & datDHa_m_l$Week <= Week2_Fit_DH) #dates relative to "2020-01-01
    assign(paste0("iweeksdataDH",eval(i)),values)                     #iweeskdataDH6=8:48 ... iweeskdataDH9
    assign(paste0("wd",eval(i)),datDHa_m_l$Freq[values]) }            #wd6 ... wd9 
  #weekly incidence of deaths outside hospital
  ndDO = 4  #data "6" merges 1:6
  for (i in (9-ndDO+1):9){ 
    values =  which(      !is.na(datDOa_m_l$Week) & datDOa_m_l$ageg2==i                
               & Week1_Fit_DO <= datDOa_m_l$Week  & datDOa_m_l$Week <= Week2_Fit_DO) #dates relative to "2020-01-01
    assign(paste0("iweeksdataDO",eval(i)),values)                     #iweeksdataDO6=8:48 ... iweeskdataDO9
    assign(paste0("vd",eval(i)), datDOa_m_l$Freq[values]) }           #vd6 ... vd9

#Convert selected-data-weeks (index 8:48 from Week1_Model=2020-02-24 to Week2_Study=2020-12-01) 
#to model-weeks (index 1:52 from Week1_Model=2020-02-24 - want to stop at Week2_Study=2020-12-01)
#=> used in likelihood
  iweeksmodelH  = iweeksdataH4  - (Week1_Model-1) #1:41=8:48-(8-1) #indices iweeksdataH4 are 1st in long pivot
  iweeksmodelDH = iweeksdataDH6 - (Week1_Model-1) #1:41=8:48-(8-1) #indices iweeksdataDH6 are 1st in long pivot
  iweeksmodelDO = iweeksdataDO6 - (Week1_Model-1) #1:41=8:48-(8-1) #indices iweeksdataDO6 are 1st in long pivot
  print(paste0("#H  data pts fitted, #: ", length(iweeksdataH4),  ", list: ", range(iweeksdataH4)[1], "...",range(iweeksdataH4)[2]))   #41, 8...48
  print(paste0("#DH data pts fitted, #: ", length(iweeksdataDH6), ", list: ", range(iweeksdataDH6)[1],"...",range(iweeksdataDH6)[2]))  #41, 8...48
  print(paste0("#DO data pts fitted, #: ", length(iweeksdataDO6), ", list: ", range(iweeksdataDO6)[1],"...",range(iweeksdataDO6)[2]))  #41, 8...48
  print(paste0("#H  model pts used,  #: ", length(iweeksmodelH),  ", list: ", range(iweeksmodelH)[1], "...",range(iweeksmodelH)[2]))   #41, 8...48
  print(paste0("#DH model pts used,  #: ", length(iweeksmodelDH), ", list: ", range(iweeksmodelDH)[1],"...",range(iweeksmodelDH)[2]))  #41, 8...48
  print(paste0("#DO model pts used,  #: ", length(iweeksmodelDO), ", list: ", range(iweeksmodelDO)[1],"...",range(iweeksmodelDO)[2]))  #41, 8...48
   
#Temporal data (weekly incidence)
  iweeksdataH  = iweeksdataH4  #indices iweeksdataH4 are 1st in long pivot
  iweeksdataDH = iweeksdataDH6 #indices iweeksdataH4 are 1st in long pivot
  iweeksdataDO = iweeksdataDO6 #indices iweeksdataH4 are 1st in long pivot
  zd =  datH$Freq[iweeksdataH]
  wd = datDH$Freq[iweeksdataDH]
  vd = datDO$Freq[iweeksdataDO]

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
sdMax    = sd(zd)
sdMax2   = sd(wd)
sdMax3   = sd(vd)
tMax     = 10
R0Max    = 10
adMax    = 0.99
adMin    = 0.0099
logadMax = log(adMax)
pE0Max   = 0.10
pE0Min   = 0.005    #pop = 120k
logpE0Max= log(pE0Max)
pkLower  = 0.1       #1/k^2, NB likelihood and data
pkLower2 = 0.1       #Assume: same kD for DH and DO
pkUpper  = 5
pkUpper2 = 5
pdmMin   = 0.1
pdmMax   = 2


### LIKELIHOOD FUNCTION
source(file = paste0(input_dir,"/BETA.r")) #Used within Likelihood2
LogLikelihood2 <- function(theta){
  ### Proposed parameters
  pars$rEI = 1/(  theta[1]*tMax)
  pars$rIR = 1/(  theta[2]*tMax)
  pars$R0  = exp( theta[3])             #*logR0Max)#*R0Max 
  pars$pE0 = exp(-theta[4] + logpE0Max) # exp(-r+logpE0Max), r=log(pE0Max/pE0)= 0,3 <= pE0=Max,0.005 (120k pop), fpr Max=0.1
  pars$ad  = exp(-theta[5] + logadMax)
  #pars$ad  =      theta[5]
  #pars$pdm = exp(+theta[5])            # pdm=0.1, 2 - theta=0, 2.3
  ### NB likelihood
  kH       = 1/(  theta[6]*theta[6])    # pk = theta = 1/sqrt(k) => k = 1/pk^2
  kDH      = 1/(  theta[7]*theta[7])
  #kDO      = kDH
  kDO      = 1/(  theta[8]*theta[8])
  ### Normal likelihood
  # sdHScaled  = theta[6]               #auxiliary parameter for normal noise  
  # sdDScaled  = theta[7]               #auxiliary parameter for normal noise
  #Dependent parameters
  pars$Ea0 = pars$Na0*pars$pE0
  pars$Sa0 = pars$Na0 - pars$Ra0 - pars$Ea0 - pars$Ia0
  pars$beta= BETA(pars)

  ### Model outputs (from Rcpp, given the proposed parameters)
  m <- model(pars)

  #Temporal model
  #MeanH  = m$byw$Hw[iweeksmodelH]; 
  #MeanDH = m$byw$DHw[iweeksmodelDH];
  #MeanDO = m$byw$DOw[iweeksmodelDO];
    
  #Model merged ageg - #NB: no ageons weighting: Ha (Da) are proportional to ageg (via Na)
  MeanH4  = rep(0,times=length(iweeksmodelH))
  MeanDH6 = rep(0,times=length(iweeksmodelDH))
  MeanDO6 = rep(0,times=length(iweeksmodelDO))
  for (i in 1:(9-(ndH-1))) { #1:4,  ndH=6
  MeanH4  = MeanH4  + eval(parse(text = paste0("m$byw_age$H", eval(i),"w[iweeksmodelH]")));
  MeanH4[1]= max(MeanH4[1],1) } #avoid 0 mean and NAs in likelihood
  for (i in 1:(9-(ndDH-1))){ #1:6, ndDH=4
  MeanDH6 = MeanDH6 + eval(parse(text = paste0("m$byw_aHO$DH",eval(i),"w[iweeksmodelDH]")));
  MeanDH6[1]= max(MeanDH6[1],1) } #avoid 0 mean and NAs in likelihood
  for (i in 1:(9-(ndDO-1))){ #1:6, ndDO=4
  MeanDO6 = MeanDO6 + eval(parse(text = paste0("m$byw_aHO$DO",eval(i),"w[iweeksmodelDO]")));
  MeanDO6[1]= max(MeanDO6[1],1) } #avoid 0 mean and NAs in likelihood

  #MeanH5-9
  for (i in (9-(ndH-1)+1):9) { #5:9, ndH=6
  values = eval(parse(text = paste0("m$byw_age$H",eval(i),"w[iweeksmodelH]")))
  values[1]= max(values[1],1); #avoid avoid 0 mean and NAs in likelihood
  assign(paste0("MeanH",eval(i)),values) } 
  #MeanDH7-9
  for (i in (9-(ndDH-1)+1):9){ #7:9, ndDH=
  values = eval(parse(text = paste0("m$byw_aHO$DH",eval(i),"w[iweeksmodelDH]")))
  values[1]= max(values[1],1); #avoid avoid 0 mean and NAs in likelihood
  assign(paste0("MeanDH",eval(i)),values) }
  #MeanDO7-9
  for (i in (9-(ndDO-1)+1):9){ #7:9, ndDO=4
  values = eval(parse(text = paste0("m$byw_aHO$DO",eval(i),"w[iweeksmodelDO]")))
  values[1]= max(values[1],1); #avoid avoid 0 mean and NAs in likelihood
  assign(paste0("MeanDO",eval(i)),values) } 

  #Likelihood of data
  #product over: data-types (H,DH,DO), age-groups, and weeks (sum)
  link = 1 #pars$pdm
  #for now
  #kD=1
  #kH=1
  #Normal likelihood
  #return(sum(dnorm(zd, mean = muH, sd = sdHScaled*sdMax,  log = T)) 
  #    +  sum(dnorm(wd, mean = muD, sd = sdDScaled*sdMax2, log = T)))
  #Negative binomial likelihood 

  ll = sum(dnbinom(x = zd4w, size = kH, mu = MeanH4, log = T)) +
       sum(dnbinom(x = zd5w, size = kH, mu = MeanH5, log = T)) +
       sum(dnbinom(x = zd6w, size = kH, mu = MeanH6, log = T)) +
       sum(dnbinom(x = zd7w, size = kH, mu = MeanH7, log = T)) +
       sum(dnbinom(x = zd8w, size = kH, mu = MeanH8, log = T)) +
       sum(dnbinom(x = zd9w, size = kH, mu = MeanH9, log = T)) +
       ### Assume: same kD for DH and DO
       sum(dnbinom(x = wd6w, size = kDH, mu = MeanDH6*link, log = T)) +
       sum(dnbinom(x = wd7w, size = kDH, mu = MeanDH7*link, log = T)) +
       sum(dnbinom(x = wd8w, size = kDH, mu = MeanDH8*link, log = T)) +
       sum(dnbinom(x = wd9w, size = kDH, mu = MeanDH9*link, log = T)) +

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
#LOWER = c(c(1, 1)/tMax,          0,                 0,                  0, pkLower, pkLower2)#, #log(pdmMin), 
#UPPER = c(  1, 1,       log(R0Max),log(pE0Max/pE0Min), log(pE0Max/pE0Min), pkUpper, pkUpper2)#, #log(pdmMax), 
LOWER = c(c(1, 1)/tMax,          0,                 0,                  0, pkLower, pkLower2, pkLower2); #kDO #log(pdmMin), 
UPPER = c(  1, 1,       log(R0Max),log(pE0Max/pE0Min), log(adMax/adMin),   pkUpper, pkUpper2, pkUpper2); #kDO #log(pdmMax), 
LogLikelihood = LogLikelihood2; #rEI, rIR, R0, pE0, pdm/al, sd, sd2

### Proposed parameters
#pars$rEI = 1/(  theta[1]*tMax)
#pars$rIR = 1/(  theta[2]*tMax)
#pars$R0  = exp( theta[3])             
#pars$pE0 = exp(-theta[4] + logpE0Max)
#pars$ad  = exp(-theta[5] + logadMax)
#kH       = 1/(  theta[6]*theta[6])   
#kDH      = 1/(  theta[7]*theta[7])
##kDO      = kDH
#kDO      = 1/(  theta[8]*theta[8])
##

#Normal priors
  #PRIOR <- createTruncatedNormalPrior(0,.5,lower = LOWER, upper = UPPER)
  #setup  = createBayesianSetup(likelihood=LogLikelihood, prior =PRIOR) #parallel = T,
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
parsE$rEI <- 1/(tMax*MAPE$parametersMAP[1])
parsE$rIR <- 1/(tMax*MAPE$parametersMAP[2])
parsE$R0  <- exp(    MAPE$parametersMAP[3])#*logR0Max)#*R0Max
parsE$pE0 <- exp(   -MAPE$parametersMAP[4] + logpE0Max)
parsE$ad  <- exp(   -MAPE$parametersMAP[5] + logadMax)
#pars$ad   <-         MAPE$parametersMAP[5
#parsE$pdm <- exp(MAPE$parametersMAP[5])
parsE$kH  <- 1/(      MAPE$parametersMAP[6]^2) #1/pk^2
parsE$kDH <- 1/(      MAPE$parametersMAP[7]^2)
#parsE$kDO <- parsE$kDH
parsE$kDO <- 1/(      MAPE$parametersMAP[8]^2)
#dependent
parsE$Ea0 = parsE$Na0*parsE$pE0
parsE$Sa0 = parsE$Na0-parsE$Ra0-parsE$Ea0-parsE$Ia0
parsE$beta= BETA(parsE)
#predictions
mE        <- model(parsE)


## UNCERTAINTY  ################################################################
## Sample the chains
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
Weeks     = iweeksmodelH #seq_along(zd) #length(mE$byw$time)
iweeksz   = Weeks
npar      = length(LOWER)
nsample   = 3000#500#1000#;
psample = getSample(out, parametersOnly = T, numSamples = nsample, start=(niter/3)/3) #parametersOnly = F
# run model for each parameter set in the sample
zsample = matrix(0,length(Weeks),nsample)
wsample = matrix(0,length(Weeks),nsample)
vsample = matrix(0,length(Weeks),nsample)
parsES  = pars
for(i in 1:nsample){
  parsES$rEI <- 1/(tMax*as.vector(psample[i,1]))
  parsES$rIR <- 1/(tMax*as.vector(psample[i,2]))
  parsES$R0  <- exp(    as.vector(psample[i,3])) #*logR0Max)#*R0Max
  parsES$pE0 <- exp(   -as.vector(psample[i,4]) + logpE0Max)
  parsES$ad  <- exp(   -as.vector(psample[i,5]) + logadMax)
  #parsES$ad  <-         as.vector(psample[i,5])
  #parsES$pdm <- exp(as.vector(psample[i,5]))
  #dependent
  parsES$Ea0 = parsES$Na0*parsES$pE0
  parsES$Sa0 = parsES$Na0-parsES$Ra0-parsES$Ea0-parsES$Ia0
  parsES$beta= BETA(parsES)
  outs        = model(as.vector(parsES))
  zsample[,i] = outs$byw$Hw[iweeksz]
  wsample[,i] = outs$byw$DHw[iweeksz]
  vsample[,i] = outs$byw$DOw[iweeksz]
} }


#True parameters (except last two)
thetaTrue = c(pars$rEI, pars$rIR, pars$R0, pars$pE0, pars$ad, 1, 1); #pars$phm, 1, 1); 


#Expected parameters

### Normal data - Normal residual likelihood
###  sd empirical
   #dev = round(0.05*mean(zd))

### NB data
###  k empirical
  mEm  = mean(mE$byw$Hw); 
  mEm2 = mean(mE$byw$Dw)

if (!is.element(pset$iplatform,1)) { #cant estimate  with dummy data (1)
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
iwH=iweeksmodelH; iwDH=iweeksmodelDH; iwDO=iweeksmodelDO;
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
print(paste0("ad    MAP: ", round(parsE$ad,    4), ". Expected/start: ", round(  thetaTrue[5], 3))) #ad/pdm
print(paste0("kH    MAP: ", round(parsE$kH,    3), ". Expected/start: ", round(  thetaTrue[6], 3))) #kH
print(paste0("kDH   MAP: ", round(parsE$kDH,   3), ". Expected/start: ", round(  thetaTrue[7], 3))) #kD
#print(paste0("kDO   dep: ", round(parsE$kDO,   3), ". Expected/start: ", round(  thetaTrue[7], 3))) #kD
print(paste0("kDO   dep: ", round(parsE$kDO,   3), ". Expected/start: ", round(  thetaTrue[8], 3))) #kD
print(paste0("beta  dep: ", round(parsE$beta,  5)))
print(paste0("Ea0   dep: ", round(sum(parsE$Ea0),   0))) #or sum(parsE$Na0*parsE$pE0)
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
print(paste0("#H  data pts fitted, #: ", length(iweeksdataH4),  ", list: ", range(iweeksdataH4)[1], "...",range(iweeksdataH4)[2]))   #41, 8...48
print(paste0("#DH data pts fitted, #: ", length(iweeksdataDH6), ", list: ", range(iweeksdataDH6)[1],"...",range(iweeksdataDH6)[2]))  #41, 8...48
print(paste0("#DO data pts fitted, #: ", length(iweeksdataDO6), ", list: ", range(iweeksdataDO6)[1],"...",range(iweeksdataDO6)[2]))  #41, 8...48
print(paste0("#H  model pts used,  #: ", length(iweeksmodelH),  ", list: ", range(iweeksmodelH)[1], "...",range(iweeksmodelH)[2]))   #41, 8...48
print(paste0("#DH model pts used,  #: ", length(iweeksmodelDH), ", list: ", range(iweeksmodelDH)[1],"...",range(iweeksmodelDH)[2]))  #41, 8...48
print(paste0("#DO model pts used,  #: ", length(iweeksmodelDO), ", list: ", range(iweeksmodelDO)[1],"...",range(iweeksmodelDO)[2]))  #41, 8...48
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
rE = 1#parsE$pdm #

iseqH  = iweeksmodelH
iseqDH = iweeksmodelDH
iseqDO = iweeksmodelDO

datH <- tibble(Weeks  =    mE$byw$time[iseqH]/7,  
               H_est  =    mE$byw$Hw[iseqH],
               Datazw =    zd*weight)
datD <- tibble(Weeks  =    mE$byw$time[iseqDH]/7,
               DH_est = rE*mE$byw$DHw[iseqDH],  
               Dataww =    wd*weight,
               DO_est = rE*mE$byw$DOw[iseqDO],  
               Datavw =    vd*weight)

if (pset$iplatform==0) {
  datH <- tibble(datH, 
                 H_mod  = datM$H_mod[iseqH])
  datD <- tibble(datD, 
                 DH_mod = datM$DH_mod[iseqDH],
                 DO_mod = datM$DO_mod[iseqDO]) }



###### Plots - Age-profile dataframes (NOT MERGED) #############################
##OS:   datHa_l  or datHa   (l = long pivot, all 48*9 pts)
##OS:  datDHa_l  or datDHa
##OS:  datDOa_l  or datDOa

if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
  if (pset$iplatform==2){
    for (i in 1:9){
      #iweekSdataH1-9 
      values =  which(!is.na(datHa_l$Week)                 &  datHa_l$ageg==i
                           & datHa_l$Week >= Week1_Fit_H   &  datHa_l$Week <= Week2_Fit_H)
      assign(paste0("iweeksdataH",eval(i)),values) 
      #iweeksdataDH1-9 
      values =  which(!is.na(datDHa_l$Week)                & datDHa_l$ageg==i
                           & datDHa_l$Week >= Week1_Fit_DH & datDHa_l$Week <= Week2_Fit_DH)
      assign(paste0("iweeksdataDH",eval(i)),values) 
      #iweeksdataDO1-9 
      values =  which(!is.na(datDOa_l$Week)                & datDOa_l$ageg==i
                           & datDOa_l$Week >= Week1_Fit_DO & datDOa_l$Week <= Week2_Fit_DO)
      assign(paste0("iweeksdataDO",eval(i)),values) 
    }}
#H
    datHa <- tibble(Weeks = mE$byw$time[iseqH]/7,
                    H1w   = mE$byw_age$H1w[iseqH], #estimated
                    H2w   = mE$byw_age$H2w[iseqH],
                    H3w   = mE$byw_age$H3w[iseqH],
                    H4w   = mE$byw_age$H4w[iseqH],
                    H5w   = mE$byw_age$H5w[iseqH],
                    H6w   = mE$byw_age$H6w[iseqH],
                    H7w   = mE$byw_age$H7w[iseqH],
                    H8w   = mE$byw_age$H8w[iseqH],
                    H9w   = mE$byw_age$H9w[iseqH])
    if (pset$iplatform==0){ #sim data: H model (true)
    datHa <- tibble(datHa, 
                    H1d   = datM$H_mod1[iseqH],
                    H2d   = datM$H_mod2[iseqH],       
                    H3d   = datM$H_mod3[iseqH],
                    H4d   = datM$H_mod4[iseqH],
                    H5d   = datM$H_mod5[iseqH],
                    H6d   = datM$H_mod6[iseqH],
                    H7d   = datM$H_mod7[iseqH],
                    H8d   = datM$H_mod8[iseqH],
                    H9d   = datM$H_mod9[iseqH]) } else { #iplat=2
    datHa <- tibble(datHa, #os data: H data
                    H1d   = datHa_l$Freq[iweeksdataH1]*weightz[1],
                    H2d   = datHa_l$Freq[iweeksdataH2]*weightz[2],
                    H3d   = datHa_l$Freq[iweeksdataH3]*weightz[3],
                    H4d   = datHa_l$Freq[iweeksdataH4]*weightz[4],
                    H5d   = datHa_l$Freq[iweeksdataH5]*weightz[5],
                    H6d   = datHa_l$Freq[iweeksdataH6]*weightz[6],
                    H7d   = datHa_l$Freq[iweeksdataH7]*weightz[7],
                    H8d   = datHa_l$Freq[iweeksdataH8]*weightz[8],
                    H9d   = datHa_l$Freq[iweeksdataH9]*weightz[9]) }
#DH
    datDHa <- tibble(Weeks = mE$byw$time[iseqDH]/7,
                    DH1w   = mE$byw_aHO$DH1w[iseqDH], #estimated
                    DH2w   = mE$byw_aHO$DH2w[iseqDH],
                    DH3w   = mE$byw_aHO$DH3w[iseqDH],
                    DH4w   = mE$byw_aHO$DH4w[iseqDH],
                    DH5w   = mE$byw_aHO$DH5w[iseqDH],
                    DH6w   = mE$byw_aHO$DH6w[iseqDH],
                    DH7w   = mE$byw_aHO$DH7w[iseqDH],
                    DH8w   = mE$byw_aHO$DH8w[iseqDH],
                    DH9w   = mE$byw_aHO$DH9w[iseqDH])
    if (pset$iplatform==0){ #sim data: DH model (true)
    datDHa <- tibble(datDHa, 
                    DH1d   = datM$DH_mod1[iseqDH],
                    DH2d   = datM$DH_mod2[iseqDH],
                    DH3d   = datM$DH_mod3[iseqDH],
                    DH4d   = datM$DH_mod4[iseqDH],
                    DH5d   = datM$DH_mod5[iseqDH],
                    DH6d   = datM$DH_mod6[iseqDH],
                    DH7d   = datM$DH_mod7[iseqDH],
                    DH8d   = datM$DH_mod8[iseqDH],
                    DH9d   = datM$DH_mod9[iseqDH]) } else{ #iplat=2
    datDHa <- tibble(datDHa, #os data: DH data
                    DH1d   = datDHa_l$Freq[iweeksdataDH1]*weightw[1],
                    DH2d   = datDHa_l$Freq[iweeksdataDH2]*weightw[2],
                    DH3d   = datDHa_l$Freq[iweeksdataDH3]*weightw[3],
                    DH4d   = datDHa_l$Freq[iweeksdataDH4]*weightw[4],
                    DH5d   = datDHa_l$Freq[iweeksdataDH5]*weightw[5],
                    DH6d   = datDHa_l$Freq[iweeksdataDH6]*weightw[6],
                    DH7d   = datDHa_l$Freq[iweeksdataDH7]*weightw[7],
                    DH8d   = datDHa_l$Freq[iweeksdataDH8]*weightw[8],
                    DH9d   = datDHa_l$Freq[iweeksdataDH9]*weightw[9],) }
#DO
    datDOa <- tibble(Weeks = mE$byw$time[iseqDO]/7,
                    DO1w   = mE$byw_aHO$DO1w[iseqDO], #estimated
                    DO2w   = mE$byw_aHO$DO2w[iseqDO],
                    DO3w   = mE$byw_aHO$DO3w[iseqDO],
                    DO4w   = mE$byw_aHO$DO4w[iseqDO],
                    DO5w   = mE$byw_aHO$DO5w[iseqDO],
                    DO6w   = mE$byw_aHO$DO6w[iseqDO],
                    DO7w   = mE$byw_aHO$DO7w[iseqDO],
                    DO8w   = mE$byw_aHO$DO8w[iseqDO],
                    DO9w   = mE$byw_aHO$DO9w[iseqDO])
    if (pset$iplatform==0){ #sim data: DO model (true)
    datDOa <- tibble(datDOa, 
                    DO1d   = datM$DO_mod1[iseqDO],
                    DO2d   = datM$DO_mod2[iseqDO],
                    DO3d   = datM$DO_mod3[iseqDO],
                    DO4d   = datM$DO_mod4[iseqDO],
                    DO5d   = datM$DO_mod5[iseqDO],
                    DO6d   = datM$DO_mod6[iseqDO],
                    DO7d   = datM$DO_mod7[iseqDO],
                    DO8d   = datM$DO_mod8[iseqDO],
                    DO9d   = datM$DO_mod9[iseqDO]) } else{ #iplat=2
    datDOa <- tibble(datDOa, #os data: DO data
                    DO1d   = datDOa_l$Freq[iweeksdataDO1]*weightv[1],
                    DO2d   = datDOa_l$Freq[iweeksdataDO2]*weightv[2],
                    DO3d   = datDOa_l$Freq[iweeksdataDO3]*weightv[3],
                    DO4d   = datDOa_l$Freq[iweeksdataDO4]*weightv[4],
                    DO5d   = datDOa_l$Freq[iweeksdataDO5]*weightv[5],
                    DO6d   = datDOa_l$Freq[iweeksdataDO6]*weightv[6],
                    DO7d   = datDOa_l$Freq[iweeksdataDO7]*weightv[7],
                    DO8d   = datDOa_l$Freq[iweeksdataDO8]*weightv[8],
                    DO9d   = datDOa_l$Freq[iweeksdataDO9]*weightv[9]) }
} #Plots - Age profile dataframes (not merged)
                    
                    


#### pdf Plots #################################################################
#pdf(file = paste0(output_dir,"/",pset$File_fit_output))

#### svg plots #################################################################
#### Diagnostics
#1
par(mar = c(0.5, 1, 1, 1)) #ar(mar = c(2, 2, 1, 1))  #bottom, left, top, right
p<-marginalPlot(out); print(p) #as 03feb 1.45
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_marginalPlot")
svglite(paste0(filenamepath,".svg")); marginalPlot(out); invisible(dev.off())
#2-3
par(mar = c(2, 2, 1, 1)) #as 03feb 1.45 #bottom, left, top, right
p<-plot(out); print(p)   #as 03feb 1.45
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
          labs(x = 'Weeks', y = 'Hospitalisations & deaths in & outside hospital', color = "Legend") + 
          scale_color_manual(values = colors) +
          geom_point(data=datH, aes(x=Weeks,y=Datazw, color =  "H_datw"), size = 1.4, pch = 19) +
          geom_line (data=datH, aes(x=Weeks,y=H_est,  color =  "H_est")) +
          geom_point(data=datD, aes(x=Weeks,y=Dataww, color = "DH_datw"), size =   1,   pch = 16) +
          geom_line (data=datD, aes(x=Weeks,y=DH_est, color = "DH_est")) +
          geom_point(data=datD, aes(x=Weeks,y=Datavw, color = "DO_datw"), size =   1,   pch = 1) +
          geom_line (data=datD, aes(x=Weeks,y=DO_est, color = "DO_est"))
    
    if (pset$iplatform==0) {
    p1 <- p1 + geom_line(data=datH, aes(x=Weeks,y=H_mod,  color =  "H_model")) +
               geom_line(data=datD, aes(x=Weeks,y=DH_mod, color = "DH_model")) +
               geom_line(data=datD, aes(x=Weeks,y=DO_mod, color = "DO_model"))  }
print(p1)
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_Overall")
svglite(paste0(filenamepath,".svg")); print(p1); invisible(dev.off())


## Plot posterior samples
#6
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
##
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_PosteriorSample")
svglite(paste0(filenamepath,".svg")); 
##  
par(mfrow = c(3,1))
par(mar = c(2, 4, 1, 1)) #bottom, left, top, right
##H
p <- matplot(Weeks,      zsample,       col="grey", type='l', xlab="Weeks", ylab="Hospitalisations (scaled)") # sample trajectories
points (datH$Weeks, datH$Datazw,   col="black") # data
lines  (Weeks,      mE$byw$Hw[iweeksz], col='red') # MAP estimate
legend(max(Weeks)*0.7, max(zsample), legend=c("sample", "admissions", "MAP"),
       col=c("grey", "black", "red"), lty=1:2, cex=0.8)
#DH
p <- p + 
matplot(Weeks,      wsample,       col="grey", type='l', xlab="Weeks", ylab="Deaths in hospital (scaled)", ylim=range(zsample)) # sample trajectories
points (datD$Weeks, datD$Dataww,   col="black") # data
lines  (Weeks,      mE$byw$DHw[iweeksz], col='red') # MAP estimate
legend(max(Weeks)*0.7, max(zsample), legend=c("sample", "deaths", "MAP"),
       col=c("grey", "black", "red"), lty=1:2, cex=0.8)
#DO
p <- p + 
matplot(Weeks,      vsample,       col="grey", type='l', xlab="Weeks", ylab="Deaths outside hospital (scaled)", ylim=range(zsample)) # sample trajectories
points (datD$Weeks, datD$Datavw,   col="black") # data
lines  (Weeks,      mE$byw$DOw[iweeksz], col='red') # MAP estimate
legend(max(Weeks)*0.7, max(zsample), legend=c("sample", "deaths", "MAP"),
       col=c("grey", "black", "red"), lty=1:2, cex=0.8)
print(p)
##
invisible(dev.off())
#or : svglite at top
}

##Plot by age profiles
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){

colors <- c("0-4" = 1, "05-11" = 2,  "12-17" = 3, "18-29" = 4, "30-39" = 5, 
            "40-49" = 6, "50-59" = 7,  "60-69" = 8, "70+" = 9)
#7 H
p1 <- ggplot() +
    labs(x = 'Weeks', y = 'Hospitalisations', color = "Legend") + 
    scale_color_manual(values = colors) +
    geom_line (data=datHa, aes(x=Weeks,y=H1w, color = "0-4")) +
    geom_line (data=datHa, aes(x=Weeks,y=H2w, color = "05-11")) +
    geom_line (data=datHa, aes(x=Weeks,y=H3w, color = "12-17")) +
    geom_line (data=datHa, aes(x=Weeks,y=H4w, color = "18-29")) +
    geom_line (data=datHa, aes(x=Weeks,y=H5w, color = "30-39")) +
    geom_line (data=datHa, aes(x=Weeks,y=H6w, color = "40-49")) +
    geom_line (data=datHa, aes(x=Weeks,y=H7w, color = "50-59")) +
    geom_line (data=datHa, aes(x=Weeks,y=H8w, color = "60-69")) +
    geom_line (data=datHa, aes(x=Weeks,y=H9w, color = "70+")) +
    geom_point(data=datHa, aes(x=Weeks,y=H1d, color = "0-4")) +
    geom_point(data=datHa, aes(x=Weeks,y=H2d, color = "05-11")) +
    geom_point(data=datHa, aes(x=Weeks,y=H3d, color = "12-17")) +
    geom_point(data=datHa, aes(x=Weeks,y=H4d, color = "18-29")) +
    geom_point(data=datHa, aes(x=Weeks,y=H5d, color = "30-39")) +
    geom_point(data=datHa, aes(x=Weeks,y=H6d, color = "40-49")) +
    geom_point(data=datHa, aes(x=Weeks,y=H7d, color = "50-59")) +
    geom_point(data=datHa, aes(x=Weeks,y=H8d, color = "60-69")) +
    geom_point(data=datHa, aes(x=Weeks,y=H9d, color = "70+"))
print(p1)
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_AgeProfile_H")
svglite(paste0(filenamepath,".svg")); print(p1); invisible(dev.off())
#8 DH
p2 <- ggplot() +
    labs(x = 'Weeks', y = 'Deaths in hospital', color = "Legend") + 
    scale_color_manual(values = colors) +
    geom_line (data=datDHa, aes(x=Weeks,y=DH1w, color = "0-4")) +
    geom_line (data=datDHa, aes(x=Weeks,y=DH2w, color = "05-11")) +
    geom_line (data=datDHa, aes(x=Weeks,y=DH3w, color = "12-17")) +
    geom_line (data=datDHa, aes(x=Weeks,y=DH4w, color = "18-29")) +
    geom_line (data=datDHa, aes(x=Weeks,y=DH5w, color = "30-39")) +
    geom_line (data=datDHa, aes(x=Weeks,y=DH6w, color = "40-49")) +
    geom_line (data=datDHa, aes(x=Weeks,y=DH7w, color = "50-59")) +
    geom_line (data=datDHa, aes(x=Weeks,y=DH8w, color = "60-69")) +
    geom_line (data=datDHa, aes(x=Weeks,y=DH9w, color = "70+")) +
    geom_point(data=datDHa, aes(x=Weeks,y=DH1d, color = "0-4")) +
    geom_point(data=datDHa, aes(x=Weeks,y=DH2d, color = "05-11")) +
    geom_point(data=datDHa, aes(x=Weeks,y=DH3d, color = "12-17")) +
    geom_point(data=datDHa, aes(x=Weeks,y=DH4d, color = "18-29")) +
    geom_point(data=datDHa, aes(x=Weeks,y=DH5d, color = "30-39")) +
    geom_point(data=datDHa, aes(x=Weeks,y=DH6d, color = "40-49")) +
    geom_point(data=datDHa, aes(x=Weeks,y=DH7d, color = "50-59")) +
    geom_point(data=datDHa, aes(x=Weeks,y=DH8d, color = "60-69")) +
    geom_point(data=datDHa, aes(x=Weeks,y=DH9d, color = "70+"))  
print(p2)
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_AgeProfile_DH")
svglite(paste0(filenamepath,".svg")); print(p2); invisible(dev.off())
#9 DO
p3 <- ggplot() +
    labs(x = 'Weeks', y = 'Deaths outside hospital', color = "Legend") + 
    scale_color_manual(values = colors) +
    geom_line (data=datDOa, aes(x=Weeks,y=DO1w, color = "0-4")) +
    geom_line (data=datDOa, aes(x=Weeks,y=DO2w, color = "05-11")) +
    geom_line (data=datDOa, aes(x=Weeks,y=DO3w, color = "12-17")) +
    geom_line (data=datDOa, aes(x=Weeks,y=DO4w, color = "18-29")) +
    geom_line (data=datDOa, aes(x=Weeks,y=DO5w, color = "30-39")) +
    geom_line (data=datDOa, aes(x=Weeks,y=DO6w, color = "40-49")) +
    geom_line (data=datDOa, aes(x=Weeks,y=DO7w, color = "50-59")) +
    geom_line (data=datDOa, aes(x=Weeks,y=DO8w, color = "60-69")) +
    geom_line (data=datDOa, aes(x=Weeks,y=DO9w, color = "70+")) +
    geom_point(data=datDOa, aes(x=Weeks,y=DO1d, color = "0-4")) +
    geom_point(data=datDOa, aes(x=Weeks,y=DO2d, color = "05-11")) +
    geom_point(data=datDOa, aes(x=Weeks,y=DO3d, color = "12-17")) +
    geom_point(data=datDOa, aes(x=Weeks,y=DO4d, color = "18-29")) +
    geom_point(data=datDOa, aes(x=Weeks,y=DO5d, color = "30-39")) +
    geom_point(data=datDOa, aes(x=Weeks,y=DO6d, color = "40-49")) +
    geom_point(data=datDOa, aes(x=Weeks,y=DO7d, color = "50-59")) +
    geom_point(data=datDOa, aes(x=Weeks,y=DO8d, color = "60-69")) +
    geom_point(data=datDOa, aes(x=Weeks,y=DO9d, color = "70+"))  
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
#  gridExtra::grid.table(datH[c("Weeks","Datazw")])
#dev.off()
#pdf(file = paste0(output_dir,"/",pset$File_fit_data2), height=nrow(datDD)/3)
#  gridExtra::grid.table(datDH[c("Weeks","Dataww")])
#dev.off()
#pdf(file = paste0(output_dir,"/",pset$File_fit_data2), height=nrow(datDD)/3)
#gridExtra::grid.table(datDO[c("Weeks","Datavw")])
#dev.off()

## pdf end




