
### Dataframes from model:
###   datM$  Weeks, Dataz, Dataw, Datav, Datazi, Datawi, Datavi

### Dataframes with OS data - sourced from main: HDdata.R
###   datH_l, datDH_l, datDO_l
###   datHa_l, datDHa_l, datDOa_l
### TODO: - in HDdata.R and here
###   datHas_l, datDHas_l, datDOas_l
### names: Week=weekH, Date=dateH, Freq=freqHas, Ageg=ageg2, Shield=shield

#TODO: extend to shielding: idataH40-90 or 60-90, and 41-91 or 61-91 <= idataH4-9 or 6-9


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
## Incidence data 
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
  
## Temporal Incidence data
  zd = datM$Dataz[idata]
  wd = datM$Dataw[idata] 
  vd = datM$Datav[idata]
  
## Age Incidence data
  for (i in 1:9){ assign(paste0("zd",eval(i)), datM[paste0("Dataz",i)][[1]] ) }#zd1-zd9 - Hospitalisations
  for (i in 1:9){ assign(paste0("wd",eval(i)), datM[paste0("Dataw",i)][[1]] ) }#wd1-wd9 - Deaths in hospital
  for (i in 1:9){ assign(paste0("vd",eval(i)), datM[paste0("Datav",i)][[1]] ) }#vd1-vd9 - Deaths outside hospital

## Merge age groups - Replicate in simulated data the merging of ageg in the real data
  nmH  = 4  #merges 1:4
  nmD  = 6  #merges 1:6 
  zdm4 = rep(0,times=length(idata))
  wdm6 = rep(0,times=length(idata))
  vdm6 = rep(0,times=length(idata))
  for (i in 1:nmH){ zdm4 = zdm4 + datM[paste0("Dataz",i)][[1]] } #[[1]] numeric part
  for (i in 1:nmD){ wdm6 = wdm6 + datM[paste0("Dataw",i)][[1]] } #[[1]] numeric part
  for (i in 1:nmD){ vdm6 = vdm6 + datM[paste0("Datav",i)][[1]] } #[[1]] numeric part

  print(paste0("#H  data pts fitted, #: ", length(idataH),   ", list: ", range(idataH)[1],  "...",range(idataH)[2]))    #41, 1...41
  print(paste0("#DH data pts fitted, #: ", length(idataDH),  ", list: ", range(idataDH)[1], "...",range(idataDH)[2]))   #41, 1...41
  print(paste0("#DO data pts fitted, #: ", length(idataDO),  ", list: ", range(idataDO)[1], "...",range(idataDO)[2]))   #41, 1...41
  print(paste0("#H  model pts used,  #: ", length(imodelH),  ", list: ", range(imodelH)[1], "...",range(imodelH)[2]))   #41, 1...41
  print(paste0("#DH model pts used,  #: ", length(imodelDH), ", list: ", range(imodelDH)[1],"...",range(imodelDH)[2]))  #41, 1...41
  print(paste0("#DO model pts used,  #: ", length(imodelDO), ", list: ", range(imodelDO)[1],"...",range(imodelDO)[2]))  #41, 1...41
#Date range of model  
  Week1_Model  = lubridate::week("2020-01-27")    #4  #cf contacts.r
  Week2_Model  = lubridate::week("2021-01-17")+52 #55
  Week_shift_model = (Week1_Model-1)

  
} else { #if iplatform>0

## Each age group has:
#-- weeks 1-48 in 2020
#-- freq=0 (date=NA) when events unreported
# Date range for fitting
# Convert dates to WEEKS                          #check: week("2020-01-01") or week("2020-01-07") #[1] 1
  Week1_Model  = lubridate::week("2020-01-27")    #  4 #start of "contact" data
  Week2_Study  = lubridate::week("2020-12-01")    # 48 #start of vacc & alpha
  Week2_Model  = Week1_Model + (pars$nw-1)        # 55 #model set to run 52=pars$nw weeks 
  Week1_Fit_H  = max( c(min( datHa_l$Week), Week1_Model), na.rm=T)              #  4 #max(c(min(1:48),4))
  Week1_Fit_DH = max( c(min(datDHa_l$Week), Week1_Model), na.rm=T)              #  4 #max(c(min(1:48),4))
  Week1_Fit_DO = max( c(min(datDOa_l$Week), Week1_Model), na.rm=T)              #  4 #max(c(min(1:48),4))
  Week2_Fit_H  = min( c(max( datHa_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(max(1:48),55,48))
  Week2_Fit_DH = min( c(max(datDHa_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(max(1:48),55,48))
  Week2_Fit_DO = min( c(max(datDOa_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(max(1:48),55,48))

## Temporal-Incidence data
## - long weekly data
  idataH  = which(!is.na(datH_l$Week)  & Week1_Fit_H  <=  datH_l$Week &  datH_l$Week <= Week2_Fit_H) #dates relative to "2020-01-01
  idataDH = which(!is.na(datDH_l$Week) & Week1_Fit_DH <= datDH_l$Week & datDH_l$Week <= Week2_Fit_DH)
  idataDO = which(!is.na(datDO_l$Week) & Week1_Fit_DO <= datDO_l$Week & datDO_l$Week <= Week2_Fit_DO)
  zd      =  datH_l$Freq[idataH]  
  wd      = datDH_l$Freq[idataDH] 
  vd      = datDO_l$Freq[idataDO]

## Age-Incidence data, weekly hospital admissions and deaths
##TODO: ? write as in fit_mkd_J5nDbHhmdy2(re-sub3)_01apr24
  for (i in 1:9){values = which(  !is.na(datHa_l$Week) &  datHa_l$Ageg==i   #idata= 8:48
                     & Week1_Fit_H  <=   datHa_l$Week  &  datHa_l$Week <= Week2_Fit_H) #dates relative to "2020-01-01
           assign(paste0("zd",eval(i)),  datHa_l$Freq[values])  }           #zd1 ... zd9
  for (i in 1:9){values = which( !is.na(datDHa_l$Week) & datDHa_l$Ageg==i   #idata= 8:48
                     & Week1_Fit_DH <=  datDHa_l$Week  & datDHa_l$Week <= Week2_Fit_DH) #dates relative to "2020-01-01
            assign(paste0("wd",eval(i)),datDHa_l$Freq[values])  }           #wd1 ... wd9 
  for (i in 1:9){values = which( !is.na(datDOa_l$Week) & datDOa_l$Ageg==i   #idata= 8:48              
                     & Week1_Fit_DO <=  datDOa_l$Week  & datDOa_l$Week <= Week2_Fit_DO) #dates relative to "2020-01-01
            assign(paste0("vd",eval(i)),datDOa_l$Freq[values])  }           #vd1 ... vd9
## Merge age groups, as for real data
## (do merging here rather than read merged dfs)
  nmH  = 4  #merges 1:4
  nmD  = 6  #merges 1:6 
  zdm4 = rep(0,times=length(idataH))
  wdm6 = rep(0,times=length(idataDH))
  vdm6 = rep(0,times=length(idataDO))
  for (i in 1:nmH){ zdm4 = zdm4 + eval(parse(text = paste0("zd",eval(i)))) }
  for (i in 1:nmD){ wdm6 = wdm6 + eval(parse(text = paste0("wd",eval(i)))) }
  for (i in 1:nmD){ vdm6 = vdm6 + eval(parse(text = paste0("vd",eval(i)))) }

## Convert data week-range (index 4:48, from Week1_Model=2020-01-27 to Week2_Study=2020-12-01) 
## to model-weeks          (index 1:52, from Week1_Model=2020-01-27)
## => use model in likelihood
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
  # TODO: => Dont need specific idataH, imodelH, etc - idata and imodel suffice
  
} #iplatform


#### WEIGHTS to scale data (each age group) to model demography ################
if(pset$iplatform==2){ #Different demographies between model (England) and data (tpp cohort)
  # England, ONS
  ageons = pars$ageons         #1:9
  # Cohort, TPP
  agecoh = pars$agecoh         #1:9
  #### merged groups
  ageons4m = sum(ageons[1:nmH])  #4 <= 1:4
  ageons6m = sum(ageons[1:nmD])  #6 <= 1:6
  agecoh4m = sum(agecoh[1:nmH])  #4 <= 1:4
  agecoh6m = sum(agecoh[1:nmD])  #6 <= 1:6
  #weights
  weight    = ageons*pars$Npop/(agecoh*pars$Npopcoh)
  weightz4m = ageons4m*pars$Npop/(agecoh4m*pars$Npopcoh)
  weightw6m = ageons6m*pars$Npop/(agecoh6m*pars$Npopcoh)
  weightv6m = ageons6m*pars$Npop/(agecoh6m*pars$Npopcoh)  
  
} else{
  weight    = rep(1,times=length(pars$ageons))
  weightz4m = 1
  weightw6m = 1
  weightv6m = 1
}








## WEIGHTED DATA ###############################################################
## TIME-TOTALS

#### merged groups
zdm4w   = round(zdm4*weightz4m,0)
wdm6w   = round(wdm6*weightw6m,0)
vdm6w   = round(vdm6*weightv6m,0)
sdzdm4w = sd(zdm4w)
sdwdm6w = sd(wdm6w)
sdvdm6w = sd(vdm6w)
### Other groups
for (i in 1:9){
  ## WEIGHTED incidence data
  ## -Counts for NB need be integer => round()
  assign(paste0("zd",eval(i),"w"), round( eval(parse(text = paste0("zd",eval(i))))*weight[i], 0))   #zd1w-zd9w
  assign(paste0("wd",eval(i),"w"), round( eval(parse(text = paste0("wd",eval(i))))*weight[i], 0))   #wd1w-wd9w
  assign(paste0("vd",eval(i),"w"), round( eval(parse(text = paste0("vd",eval(i))))*weight[i], 0))   #vd1w-vd9w
  #standard deviation for normal likelihood
  assign(paste0("sdzd",eval(i),"w"),  sd( eval(parse(text = paste0("zd",eval(i))))*weight[i]) )     #sdzd4w-sdzd9w
  assign(paste0("sdwd",eval(i),"w"),  sd( eval(parse(text = paste0("wd",eval(i))))*weight[i]) )     #sdwd4w-sdwd9w
  assign(paste0("sdvd",eval(i),"w"),  sd( eval(parse(text = paste0("vd",eval(i))))*weight[i]) )     #sdvd4w-sdvd9w

  ## WEIGHTED TIME-TOTALS
  assign(paste0("Tzd",eval(i),"w"),  sum( eval(parse(text = paste0("zd",eval(i),"w")))) )           #Tzd1w-Tzd9w
  assign(paste0("Twd",eval(i),"w"),  sum( eval(parse(text = paste0("wd",eval(i),"w")))) )           #Twd1w-Twd9w
  assign(paste0("Tvd",eval(i),"w"),  sum( eval(parse(text = paste0("vd",eval(i),"w")))) )           #Tvd1w-Tvd9w
  #standard deviation for normal likelihood
  assign(paste0("sdTzd",eval(i),"w"),     eval(parse(text = paste0("sdzd",eval(i),"w")))*sqrt(length(4:48)) ) #sdTzd4w-sdTzd9w #45=no. time points
  assign(paste0("sdTwd",eval(i),"w"),     eval(parse(text = paste0("sdwd",eval(i),"w")))*sqrt(length(4:48)) ) #sdTwd4w-sdTwd9w #45=no. time points
  assign(paste0("sdTvd",eval(i),"w"),     eval(parse(text = paste0("sdvd",eval(i),"w")))*sqrt(length(4:48)) ) #sdTvd4w-sdTvd9w #45=no. time points
}


######## Prevalence data #######################################################
cis<-read.csv(paste0(input_dir,"/CIS_ONS_England_03May-12Dec-2020_Positivity.csv"))
posi_av_perc = 100*cis$Pos.average.prop #100 x proportion
#
#
posi_sd_perc = 100*(cis$Pos.U95CI.prop-cis$Pos.L95CI.prop)/1.96
#round((100*(cis$Pos.U95CI.prop-cis$Pos.L95CI.prop))/1.96,2)
##Truncate sd to avoid negative variable range
posi_sd_percT = posi_sd_perc/2     #range(posi_sd_percT)  #[1] 0.007653061 0.089285714
#plot(posi_av_perc)
#lines(posi_av_perc+1.96*posi_sd_percT)
#lines(posi_av_perc-1.96*posi_sd_percT)
posi_sd_percT_mean = mean(posi_sd_percT) #0.02566481
##Absolute weeks CIS Positivity: 18-50
##Absolute weeks Model:           4-48 = 4-17 + 18-48
##=> fit index imodel = 15-45
imodel_posi=15:45
idata_posi = 1:31 #range(cis$Date[1:31]) [1] "2020-05-03" "2020-11-29" #
shift_posi = unique(imodel_posi-idata_posi)
Dates_posi = lubridate::ymd( "2020-01-27" ) + lubridate::weeks(idata_posi - 1 + shift_posi)
if (pset$iplatform==0){ #this factor used because the prevalence data is unrelated to the simulated data
  posi_d_to_m = max( 100*(mout$byw$Ct[imodel_posi]/pars$Npop) )/ 
                max(posi_av_perc[idata_posi])  #[1] 0.007730766
} else { 
  posi_d_to_m=1 }
##data to be fittted
posi_data_perc = posi_av_perc[idata_posi]



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
### -for each age group within the model
### -can only initialise: Ha0[] and Da0[]
### -can be inconsistent with other state variables, which, except for Ea0, are assumed=0
### -Ea0 >0 enough => Ia and Ua increase afterwards (but only one of Ia0 or Ua0 won't do that)
if (pset$iplatform==2 & pset$DataIC==1){ #>0
for (i in 1:9){ #assigining current incidence to state variables (neglecting remains of state from previously)
  ivalueH  = which( datHa_l$Ageg==i &  datHa_l$Week==Week1_Fit_H)
  ivalueDH = which(datDHa_l$Ageg==i & datDHa_l$Week==Week1_Fit_H)
  ivalueDO = which(datDOa_l$Ageg==i & datDOa_l$Week==Week1_Fit_H)
  pars$Ha0[i] =  datHa_l$Freq[ivalueH]
  pars$Da0[i] = datDHa_l$Freq[ivalueDH] + datDOa_l$Freq[ivalueDO]
}}
  
### Parameter transformation functions
fI <- function(theta){  return(par = theta)          } #identity
fi <- function(theta){  return(par = 1/theta)        } #inverse
fp <- function(theta){  return(par = 1/(theta*theta))} #pareto
fs <- function(theta){  return(par = theta*theta)    } #square
fe <- function(theta){  return(par = exp(theta))     } #expeonential
gI <- function(par)  {  return(theta = par)          }
gi <- function(par)  {  return(theta = 1/par)        }
gp <- function(par)  {  return(theta = 1/sqrt(par))  }
gs <- function(par)  {  return(theta = sqrt(par))    }
ge <- function(par)  {  return(theta = log(par))   }


### Parameter bounds
age  = c(mean(0:4),mean(5:11),mean(12:17),mean(18:29),mean(30:39),mean(40:49),mean(50:59),mean(60:69),mean(70:90)) #85))
age9 = age[9]
age1 = age[1]
age3 = age[3]

pkMin    = 0.1  #1/k^2, NB likelihood and data
pkMax    = 5    #Assume: same kD for DH and DO

sdHMin   = 0
sdHMax   = 5 #multiplicative factor of the sd across the age-glrups fitted

tMax     = 20 #10
tMax2    = 50 #30
tMin     = 1  #implicit in LOWER and par resacaling
tMin2    = 1  #implicit in UPPER and par resacaling

yAMax    = 1.4 #min(1/pars$y) [1] 1.449275, so that y(a)=<1 for all a
yAMin    = 0.01

fuMax    = 1
fuMin    = 0

ArseedMin = 0
ArseedMax = (2*84)*2 #sum(pars$rseed)*2

R0Max    = 20 #10
R0Min    = 0.01

pE0Max   = 0.10
pE0Min   = 0.00001   #pop = 565.5

adMax    = 0.99
adMin    = 0.0099

hAMax    = 2 #min(1/pars$y) [1] 2.103049, so that h(a)=<1 for all a
hAMin    = 0.01

hMax    = 0.99
hMin    = 0.00001

mAMax    = 2 #min(1/pars$m) [1] 2.43309, so that m(a)=<1 for all a
mAMin    = 0.01

oN = 1/pars$Npop


### LIKELIHOOD FUNCTION

source(file = paste0(input_dir,"/BETA.r")) #Used within Likelihood
LogLikelihood <- function(theta){
  ### UPDATE: @@
  ###         proposal pars$
  ###         MAP      parsE$, 
  ###         sample   parsES$
  ### Proposed parameters
  #pars$rEI = 1/(  theta[1])#*tMax)
  pars$rIR = 1/(  theta[1])#*tMax)
  pars$rOD = 1/(  theta[2])#*tMax2)
  pars$R0  =  exp(theta[3]) #^2
  pars$pE0 =      theta[4]
  #Arseed   =      theta[5]
  #yA       =      theta[5]
  pars$fu  =      theta[5]
  hA       =      theta[6] #hAMin+theta[]*(hAMax-hAMin)
  pars$ad  =      theta[7] #adMin+theta[]*(adMax-adMin)
  mA       =      theta[8]
  #sdH      =      theta[8]
  sdH      = pars$sdH #1
  kH       = pars$kH  #1 1/(  theta[8]*theta[8])    # pk = theta = 1/sqrt(k) => k = 1/pk^2
  kDH      = 1/(  theta[9]*theta[9])
  kDO      = kDH
  #kDO      = 1/(  theta[3]*theta[3])
  #Dependent parameters
  pars$h     = hA*pars$h
  #pars$y     = yA*pars$y
  pars$m     = mA*pars$m
  #pars$rseed = Arseed*pars$ageons
  pars$Ea0   = pars$Na0*pars$pE0
  pars$Sa0   = pars$Na0 - pars$Ea0 - pars$Ia0 - pars$Ua0 - pars$Ha0 - pars$Oa0 - pars$Ra0 - pars$Da0   
  pars$beta  = BETA(pars) 

  ### Model outputs (from Rcpp, given the above proposed parameters)
  m <- model(pars)
  
  ### Model predictions - positivity
  MeanPosi_perc = m$byw$Ct[imodel_posi]*oN*100

  ### Model predictions - incidence
  ### (no ageons weighing: Ha and are proportional to ageg (via Na))
  #Totals
  for (i in 1:nmH) { 
    assign(paste0("TMeanH", eval(i)), sum(eval(parse(text = paste0("m$byw_age$H", eval(i),"w[imodelH]")))))  }  # TMeanH1-4
  for (i in 1:nmD) { 
    assign(paste0("TMeanDH",eval(i)), sum(eval(parse(text = paste0("m$byw_aHO$DH",eval(i),"w[imodelDH]")))))    # TMeanDH1-6
    assign(paste0("TMeanDO",eval(i)), sum(eval(parse(text = paste0("m$byw_aHO$DO",eval(i),"w[imodelDO]"))))) }  # TMeanDO1-6
  #Merged groups
  MeanHm4  = rep(0,times=length(imodelH))
  MeanDHm6 = rep(0,times=length(imodelDH))
  MeanDOm6 = rep(0,times=length(imodelDO))
  for (i in 1:nmH) {MeanHm4  = MeanHm4  + eval(parse(text = paste0("m$byw_age$H", eval(i),"w[imodelH]")))  }
  for (i in 1:nmD) {MeanDHm6 = MeanDHm6 + eval(parse(text = paste0("m$byw_aHO$DH",eval(i),"w[imodelDH]")))
                    MeanDOm6 = MeanDOm6 + eval(parse(text = paste0("m$byw_aHO$DO",eval(i),"w[imodelDO]"))) }
  #avoid 0 mean and NAs in likelihood
  MeanHm4[1] = max(MeanHm4[1],1)
  MeanDOm6[1]= max(MeanDOm6[1],1)
  MeanDHm6[1]= max(MeanDHm6[1],1)
  #Non-merged
  for (i in 1:9) { valuesz = eval(parse(text = paste0("m$byw_age$H",eval(i),"w[imodelH]")))
                   valuesw = eval(parse(text = paste0("m$byw_aHO$DH",eval(i),"w[imodelDH]")))
                   valuesv = eval(parse(text = paste0("m$byw_aHO$DO",eval(i),"w[imodelDO]")))
                   valuesz[1]= max(valuesz[1],1);             #avoid avoid 0 mean and NAs in likelihood
                   valuesw[1]= max(valuesw[1],1); 
                   valuesv[1]= max(valuesv[1],1); 
                   assign(paste0("MeanH", eval(i)),valuesz)   #MeanH1-9
                   assign(paste0("MeanDH",eval(i)),valuesw)   #MeanDH1-9
                   assign(paste0("MeanDO",eval(i)),valuesv) } #MeanDO1-9

  #Likelihood of data
  #product over: 1) datasets (H,DH,DO), 2) age-groups, and 3) weeks
  #Negative binomial or Normal likelihood

  ## Totals - sum of 45 NB,s - but note: many are added (non-reported) zeros
  kHT  = kH*10  #45; 
  kDHT = kDH*10 #*45; 
  kDOT = kDO*10 #*45; 
  ## Merged  - sum of 4 or 6 variables
  kHm  = kH;  #kHm  = kH*4;   
  kDHm = kDH; #kDHm = kDH*6;
  kDOm = kDO; #kDOm = kDO*6; 

  ll =
  ###H 
   #(   dnbinom(x =  Tzd1w, size = kHT,        mu   = TMeanH1,  log = T)) +     #1 Totals - NB (below counts)
   #(   dnbinom(x =  Tzd2w, size = kHT,        mu   = TMeanH2,  log = T)) +
   #(   dnbinom(x =  Tzd3w, size = kHT,        mu   = TMeanH3,  log = T)) +
   #(   dnbinom(x =  Tzd4w, size = kHT,        mu   = TMeanH4,  log = T)) +
   #sum(  dnorm(x =  zdm4w, sd =   sdH*sdzdm4w,mean = MeanHm4,  log = T)) +     #2 Merged - Normal
    sum(  dnorm(x =  zd1w,  sd =   sdH*sdzd1w, mean = MeanH1,   log = T)) +     #3 Non-merged - Normal
    sum(  dnorm(x =  zd2w,  sd =   sdH*sdzd2w, mean = MeanH2,   log = T)) +
    sum(  dnorm(x =  zd3w,  sd =   sdH*sdzd3w, mean = MeanH3,   log = T)) +
    sum(  dnorm(x =  zd4w,  sd =   sdH*sdzd4w, mean = MeanH4,   log = T)) +     
    sum(  dnorm(x =  zd5w,  sd =   sdH*sdzd5w, mean = MeanH5,   log = T)) +     #Other - Normal
    sum(  dnorm(x =  zd6w,  sd =   sdH*sdzd6w, mean = MeanH6,   log = T)) +
    sum(  dnorm(x =  zd7w,  sd =   sdH*sdzd7w, mean = MeanH7,   log = T)) +
    sum(  dnorm(x =  zd8w,  sd =   sdH*sdzd8w, mean = MeanH8,   log = T)) +
    sum(  dnorm(x =  zd9w,  sd =   sdH*sdzd9w, mean = MeanH9,   log = T)) +
   #sum(  dnbinom(x= zdm4w, size = kHm,        mu   = MeanHm4,  log = T)) +     #2 Merged - NB
   #sum(  dnbinom(x= zd5w,  size = kH,         mu   = MeanH5,   log = T)) +     #Other    - NB
   #sum(  dnbinom(x= zd6w,  size = kH,         mu   = MeanH6,   log = T)) +
   #sum(  dnbinom(x= zd7w,  size = kH,         mu   = MeanH7,   log = T)) +
   #sum(  dnbinom(x= zd8w,  size = kH,         mu   = MeanH8,   log = T)) +
   #sum(  dnbinom(x =zd9w,  size = kH,         mu   = MeanH9,   log = T)) +    
  ###DH
   #(     dnbinom(x= Twd1w, size = kDHT,       mu   = TMeanDH1, log = T)) +     #1 Totals - NB (below counts)
   #(     dnbinom(x= Twd2w, size = kDHT,       mu   = TMeanDH2, log = T)) +
   #(     dnbinom(x= Twd3w, size = kDHT,       mu   = TMeanDH3, log = T)) +
   #(     dnbinom(x= Twd4w, size = kDHT,       mu   = TMeanDH4, log = T)) +
   #(     dnbinom(x= Twd5w, size = kDHT,       mu   = TMeanDH5, log = T)) +
   #(     dnbinom(x= Twd6w, size = kDHT,       mu   = TMeanDH6, log = T)) +
   #sum(  dnbinom(x= wdm6w, size = kDHm,       mu   = MeanDHm6, log = T)) +     #2 Merged - NB
    sum(  dnbinom(x = wd1w, size = kDH,        mu   = MeanDH1,  log = T)) +     #3 Non-merged - NB
    sum(  dnbinom(x = wd2w, size = kDH,        mu   = MeanDH2,  log = T)) +     
    sum(  dnbinom(x = wd3w, size = kDH,        mu   = MeanDH3,  log = T)) +     
    sum(  dnbinom(x = wd4w, size = kDH,        mu   = MeanDH4,  log = T)) +     
    sum(  dnbinom(x = wd5w, size = kDH,        mu   = MeanDH5,  log = T)) +     
    sum(  dnbinom(x = wd6w, size = kDH,        mu   = MeanDH6,  log = T)) +     
    sum(  dnbinom(x = wd7w, size = kDH,        mu   = MeanDH7,  log = T)) +     #Other - Normal
    sum(  dnbinom(x = wd8w, size = kDH,        mu   = MeanDH8,  log = T)) +
    sum(  dnbinom(x = wd9w, size = kDH,        mu   = MeanDH9,  log = T)) +
  ###DO
   #(     dnbinom(x= Tvd1w, size = kDOT,       mu   = TMeanDO1, log = T)) +     #1 Totals - NB (below counts)
   #(     dnbinom(x= Tvd2w, size = kDOT,       mu   = TMeanDO2, log = T)) +
   #(     dnbinom(x= Tvd3w, size = kDOT,       mu   = TMeanDO3, log = T)) +
   #(     dnbinom(x= Tvd4w, size = kDOT,       mu   = TMeanDO4, log = T)) +
   #(     dnbinom(x= Tvd5w, size = kDOT,       mu   = TMeanDO5, log = T)) +
   #(     dnbinom(x= Tvd6w, size = kDOT,       mu   = TMeanDO6, log = T)) +
   #sum(  dnbinom(x= vdm6w, size = kDOm,       mu   = MeanDOm6, log = T)) +     #2 Merged - NB
    sum(  dnbinom(x = vd1w, size = kDO,        mu   = MeanDO1,  log = T)) +     #3 Non-merged - NB
    sum(  dnbinom(x = vd2w, size = kDO,        mu   = MeanDO2,  log = T)) +     
    sum(  dnbinom(x = vd3w, size = kDO,        mu   = MeanDO3,  log = T)) +     
    sum(  dnbinom(x = vd4w, size = kDO,        mu   = MeanDO4,  log = T)) +     
    sum(  dnbinom(x = vd5w, size = kDO,        mu   = MeanDO5,  log = T)) +     
    sum(  dnbinom(x = vd6w, size = kDO,        mu   = MeanDO6,  log = T)) +     
    sum(  dnbinom(x = vd7w, size = kDO,        mu   = MeanDO7,  log = T)) +     #Other - Normal
    sum(  dnbinom(x = vd8w, size = kDO,        mu   = MeanDO8,  log = T)) +
    sum(  dnbinom(x = vd9w, size = kDO,        mu   = MeanDO9,  log = T)) +
 ###Prevalence
    sum(  dnorm(x = posi_data_perc*posi_d_to_m, sd = posi_sd_percT*posi_d_to_m, mean = MeanPosi_perc, log = T))

  
    return(ll)
} #Likelihood


## Likelihood definition, parameter ranges  ####################################
niter = 30000#60000#6000##3000
if (pset$iplatform==2){niter=120000} #150000} #200000

        #rIR/rEI,  rOD,     R0,        pE0,    fu/yA/Arseed,   hA,   ad,  mA/sdH,   kD
LOWER = c(1,       1,       log(R0Min),  pE0Min,  fuMin, hAMin, adMin,  mAMin, pkMin);



UPPER = c(tMax,   tMax2,    log(R0Max),  pE0Max,  fuMax, hAMax, adMax,  mAMax, pkMax);



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
#rIR/rEI,  rOD,     R0,        pE0,    fu/yA/Arseed,   hA,   ad,  mA/sdH,   kD
#parsE$rEI <- 1/(  as.vector(MAPE$parametersMAP[1]))#*tMax))
parsE$rIR <- 1/(  as.vector(MAPE$parametersMAP[1]))#*tMax))
parsE$rOD <- 1/(  as.vector(MAPE$parametersMAP[2]))#*tMax2))
parsE$R0  <-  exp(as.vector(MAPE$parametersMAP[3])) #^2)
parsE$pE0 <-      as.vector(MAPE$parametersMAP[4])
#ArseedE   <-      as.vector(MAPE$parametersMAP[5])
#yAE       <-      as.vector(MAPE$parametersMAP[5])
parsE$fu  <-      as.vector(MAPE$parametersMAP[5])
hAE       <-      as.vector(MAPE$parametersMAP[6])
parsE$ad  <-      as.vector(MAPE$parametersMAP[7])
#parsE$kH  <- 1/(  as.vector(MAPE$parametersMAP[8]^2)) #1/pk^2
#parsE$sdH <-      as.vector(MAPE$parametersMAP[8])
mAE       <-      as.vector(MAPE$parametersMAP[8])
parsE$kDH <- 1/(  as.vector(MAPE$parametersMAP[9]^2))
parsE$kDO <- parsE$kDH
#Dependent parameters
parsE$h     = hAE*pars$h #parsE$h
#parsE$y     = yAE*pars$y
parsE$m     = mAE*pars$m
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
PositivE = (mE$byw$Ct[imodelH])*oN
#Prevalence using MAP-parameters
#see datap

#Note: R0(parsE, GetBeta=0, GetOutput=1, Sampling=1, nt=ntimes)[[1]]$R0 - same as parsE$R0


## Credible Intervals ##########################################################
## Sample the chains
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
print("Sampling..."); cat("\n")

oN = 1/pars$Npop
npar = length(LOWER)
Thin=4#2#4
Chains=3
StartSampChainPostBurn = 1
LengtMcmcChainPostBurn = floor((niter-Burnin+1)/Chains)
LengtSampChainPostBurn = LengtMcmcChainPostBurn - StartSampChainPostBurn + 1
nsample = Chains*LengtMcmcChainPostBurn/Thin #nsample = 100#3000#500#1000#;
#start-end - for each chain
#Note: parametersOnly = F = gives extra: Lposterior Llikelihood     Lprior
psample = getSample(out, parametersOnly = T, start=StartSampChainPostBurn, end= LengtSampChainPostBurn, thin=Thin)
#dim(psample)#  = c(nsample, npar)
# run model for each parameter set in the sample
zsample = matrix(0,ntimes,nsample)
wsample = matrix(0,ntimes,nsample)
vsample = matrix(0,ntimes,nsample)
Psample = matrix(0,ntimes,nsample)
csample = matrix(0,ntimes,nsample)
parsES  = pars #parsE
### UPDATE: @@
###         proposal pars$
###         MAP      parsE$, 
###         sample   parsES$
for(i in 1:nsample){
  #rIR/rEI,  rOD,     R0,        pE0,    fu/yA/Arseed,   hA,   ad,  mA/sdH,   kD
  #parsES$rEI <- 1/(  as.vector(psample[i,1]))#*tMax)
  parsES$rIR <- 1/(  as.vector(psample[i,1]))#*tMax)
  parsES$rOD <- 1/(  as.vector(psample[i,2]))#*tMax2)
  parsES$R0  <-  exp(as.vector(psample[i,3])) #^2
  parsES$pE0 <-      as.vector(psample[i,4])
  #ArseedES   <-      as.vector(psample[i,5])
  #yAES       <-      as.vector(psample[i,5])
  parsES$fu  <-      as.vector(psample[i,5])
  hAES       <-      as.vector(psample[i,6])
  parsES$ad  <-      as.vector(psample[i,7])
  mAES       <-      as.vector(psample[i,8])
  #Dependent parameters
  parsES$h    =       hAES*pars$h #parsES$h
  #parsES$y    =       yAES*pars$y
  parsES$m    =       mAES*pars$m
  #parsES$rseed=   ArseedES*pars$ageons
  parsES$Ea0  = parsES$Na0*parsES$pE0
  parsES$Sa0  = parsES$Na0 - parsES$Ea0 - parsES$Ia0 - parsES$Ua0 - parsES$Ha0 - parsES$Oa0 - parsES$Ra0 - parsES$Da0 
  parsES$beta = BETA(parsES)
  
  
  
  
  
  
  
  
  
  
  outs        = model(as.vector(parsES))
  zsample[,i] = outs$byw$Hw[imodelH]
  wsample[,i] = outs$byw$DHw[imodelH]
  vsample[,i] = outs$byw$DOw[imodelH]  
  Psample[,i] = (outs$byw$It[imodelH] + outs$byw$Ut[imodelH])*oN
  csample[,i] = (outs$byw$Ct[imodelH])*oN
} 
Weekssample = 1 + outs$byw$time[imodelH]/7 + Week_shift_model
Datessample = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1) #"2020-01-06" Mon
#Datessample = as.Date(paste(Weekssample, "2020", 'Mon'), '%U %Y %a') #Checked: "Mon" consistent with weeks/dates def throughout 
##95% CrI
zsample95 = matrix(0,ntimes,2)
wsample95 = matrix(0,ntimes,2)
vsample95 = matrix(0,ntimes,2)
Psample95 = matrix(0,ntimes,2)
csample95 = matrix(0,ntimes,2)
q1=0.01 #0.05
q2=0.99 #0.95
for(it in 1:ntimes){
  samp_it <- zsample[it,]
  zsample95[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  zsample95[it,2] = quantile(samp_it,q2,na.rm=T)[[1]]
  samp_it <- wsample[it,]
  wsample95[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  wsample95[it,2] = quantile(samp_it,q2,na.rm=T)[[1]]
  samp_it <- vsample[it,]
  vsample95[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  vsample95[it,2] = quantile(samp_it,q2,na.rm=T)[[1]]
  samp_it <- Psample[it,]
  Psample95[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  Psample95[it,2] = quantile(samp_it,q2,na.rm=T)[[1]]
  samp_it <- csample[it,]
  csample95[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  csample95[it,2] = quantile(samp_it,q2,na.rm=T)[[1]]
}
## R0_week
nsampleR0 = min(750,nsample) #300#100
R0weeksample = matrix(0,ntimes,nsampleR0)
for(i in 1:nsampleR0){
  #rIR/rEI,  rOD,     R0,        pE0,    fu/yA/Arseed,   hA,   ad,  mA/sdH,   kD
  #parsES$rEI <- 1/(  as.vector(psample[i,1]))#*tMax)
  parsES$rIR <- 1/(  as.vector(psample[i,1]))#*tMax)
  parsES$rOD <- 1/(  as.vector(psample[i,2]))#*tMax2)
  parsES$R0  <-  exp(as.vector(psample[i,3])) #^2
  parsES$pE0 <-      as.vector(psample[i,4])
  #ArseedES   <-      as.vector(psample[i,5])
  #yAES       <-      as.vector(psample[i,5])
  parsES$fu  <-      as.vector(psample[i,5])
  hAES       <-      as.vector(psample[i,6])
  parsES$ad  <-      as.vector(psample[i,7])
  mAES       <-      as.vector(psample[i,8])
  #Dependent parameters
  parsES$h    =       hAES*pars$h #parsES$h
  #parsES$y    =       yAES*pars$y
  parsES$m    =       mAES*pars$m
  #parsES$rseed=   ArseedES*pars$ageons
  parsES$Ea0  = parsES$Na0*parsES$pE0
  parsES$Sa0  = parsES$Na0 - parsES$Ea0 - parsES$Ia0 - parsES$Ua0 - parsES$Ha0 - parsES$Oa0 - parsES$Ra0 - parsES$Da0 
  parsES$beta = BETA(parsES)
  
  
  
  
  
  
  
  
  
  
  #outs        = model(as.vector(parsES))
  R0weeksample[,i] = R0(parsES, GetBeta=0, GetOutput=1, Sampling=1, nt=ntimes)[[2]]$R0_week 
}
R0weeksample95 = matrix(0,length(imodelH),2)
for(it in 1:ntimes) {
  samp_it <- R0weeksample[it,]
  R0weeksample95[it,1] = quantile(samp_it,0.01,na.rm=T)[[1]] #min(samp_it) #quantile(samp_it,q1)[[1]]
  R0weeksample95[it,2] = quantile(samp_it,0.99,na.rm=T)[[1]] #max(samp_it) #quantile(samp_it,q2)[[1]]
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
## MAP Estimates
print(paste0("kH  Fixed: ", round(parsE$kH,    3),    ". Expected: ", round(  pars$kH,      3))) #kH
print(paste0("sdH Fixed: ", round(parsE$sdH,   3),    ". Expected: ", round(  pars$sdH,     3))) #sdH
print(paste0("kDH,kDO MAP:",round(parsE$kDH,   3),    ". Expected: ", round(  pars$kDH,     3), ", sdH=",pars$sdH)) #kD
print(paste0("1/rIR MAP: ", round(1/parsE$rIR, 3),    ". Expected: ", round(1/pars$rIR,     3))) #rEI
print(paste0("1/rOD MAP: ", round(1/parsE$rOD, 3),    ". Expected: ", round(1/pars$rOD,     3))) #rOD
print(paste0("R0    MAP: ", round(parsE$R0,    3),    ". Expected: ", round(  pars$R0,      3))) #R0
print(paste0("pE0   MAP: ", round(parsE$pE0,   4),    ". Expected: ", round(  pars$pE0,     3))) #pE0
print(paste0("fu    MAP: ", round(parsE$fu,    4),    ". Expected: ", round(  pars$fu,      3))) #pE0 #fu #yA
print(paste0("hA    MAP: ", round(parsE$h[9]/pars$h[9], 4), ". Expected: ", round(1,        3))) #hA
print(paste0("ad    MAP: ", round(parsE$ad,    4),    ". Expected: ", round(  pars$ad,      3))) #ad
print(paste0("mA    MAP: ", round(parsE$m[9]/pars$m[9], 4), ". Expected: ", round(1,        3))) #mA







print(paste0("beta  dep: ", round(parsE$beta,     5) ))
print(paste0("E0    dep: ", round(sum(parsE$Ea0), 0), ". Expected: ", round(sum(pars$Na0*pars$pE0)) ))
print(paste0("Estimated proportion deaths outside hospital = ", round(parsE$ad/(1+parsE$ad),3)))

cat("\n");
print(summary(out)); 
cat("\n")
print(paste0("Mean by chain and parameter:"))
print(out$X)
sink()

## Probability-age plots 
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_plots_probs")
svglite(paste0(filenamepath,".svg")); 
par(mfrow = c(4,1))
par(mar = c(4, 4, 1, 4))  #bottom, left, top, right
plot(pars$h, ylim=c(0,1),ylab="h",xlab="")
  lines(parsE$h, col=2, lwd=2)
plot(pars$y, ylim=c(0,1),ylab="y",xlab="")
  lines(parsE$y, col=2, lwd=2)
plot(pars$d*pars$ad,ylab="d",xlab="")
  lines(parsE$d*parsE$ad, col=2, lwd=2)  #parsE$ad=1 if parsE$d estimated
plot(pars$m, ylim=c(0,1),ylab="m",xlab="age group")
  lines(parsE$m, col=2, lwd=2)
invisible(dev.off())


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
cat("\n")
print("Parameters (doesn't aply to those estimated)"); cat("\n")
print(pars[-1]) #"cm"
cat("\n"); 
cat("\n"); 
sink()



##### Plots - Overall dataframes ###############################################
print("Data frames..."); cat("\n")
N  = pars$Npop
Nc = pars$Npopcoh


if(pset$iplatform<2) {weight0=1} else {weight0=N/Nc}
if(pset$iplatform==1){Weekssample = 1 + mE$byw$time[imodelH]/7 + Week_shift_model}

datp <- tibble(Weeks  = 1 + mE$byw$time[imodelH]/7 + Week_shift_model,
               Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1), #"2020-01-06" Mon
               #Dates  = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'),
               Prev   = (mE$byw$It[imodelH] + mE$byw$Ut[imodelH])*oN,
               Posi   = (mE$byw$Ct[imodelH])*oN )

datH <- tibble(Weeks  = 1 + mE$byw$time[imodelH]/7 + Week_shift_model,    #model time 0 <> week1 + (Week1_Model-1) = 1 + (4-1) = week4
               Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1), #"2020-01-06" Mon
               #Dates  = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'), #Checked: "Mon" consistent with weeks/dates def throughout 
               H_est  = mE$byw$Hw[imodelH],
               zdw    = zd*weight0,
               R0_week = R0_weekE[imodelH])
datD <- tibble(Weeks  = 1 + mE$byw$time[imodelDH]/7 + Week_shift_model,
               Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1), #"2020-01-06" Mon
               #Dates  = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'), #Checked: "Mon" consistent with weeks/dates def throughout 
               DH_est = mE$byw$DHw[imodelDH],
               DO_est = mE$byw$DOw[imodelDO],  
               wdw    = wd*weight0,
               vdw    = vd*weight0)

if (pset$iplatform>0) {
datH <- tibble(datH,
               Weeksz = datH_l$Weeks[idataH],
               Datesz = Dates)            #datH_l$Dates[idataH]) 
               #Justification:datX_l$Dates (X=DH, DO) has NA (both dummy & OS) 
               #while         datX_l$Weeks anddatX_l$Freq (zd, wd, vd) have no NA (by _l construction)
datD <- tibble(datD,
               Weeksw = datDH_l$Weeks[idataDH],
               Weeksv = datDO_l$Weeks[idataDO],
               Datesw = Dates,                          #datDH_l$Dates[idataDH],
               Datesv = Dates)  } else { #iplatform=0   #datDO_l$Dates[idataDO])  } else {
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

#y axis log transformation - default: linear
YLOG <- function(y,LOG=0){ if (LOG==1) {z=log10(y+1)} else {z=y}; return(z) }
LOG=1; #0 #apply scale of plotting Age Profiles

if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
#Indices of data vectors (pset$iplatform==2)
#=> defined in"Age-Incidence data" at the top
  
#Variables (Model and data)  
  for (i in 1:9){
#MAP
    valuesH  = eval(parse(text = paste0("mE$byw_age$H",eval(i), "w[imodelH]")))
    valuesDH = eval(parse(text = paste0("mE$byw_aHO$DH",eval(i),"w[imodelDH]")))
    valuesDO = eval(parse(text = paste0("mE$byw_aHO$DO",eval(i),"w[imodelDO]")))
    assign(paste0("H", eval(i),"w"), YLOG(valuesH, LOG))                        #H1w-H9w
    assign(paste0("DH",eval(i),"w"), YLOG(valuesDH,LOG))                        #DH1w-DH9w
    assign(paste0("DO",eval(i),"w"), YLOG(valuesDO,LOG))                        #DO1w-DO9w
#Data
    if (pset$iplatform>0){ #OS data
    ivaluesH = which( !is.na(datHa_l$Week) & datHa_l$Ageg==i  & datHa_l$Week  >= Week1_Fit_H  &  datHa_l$Week <= Week2_Fit_H)  #idataH1-idataH9 
    ivaluesDH= which(!is.na(datDHa_l$Week) & datDHa_l$Ageg==i & datDHa_l$Week >= Week1_Fit_DH & datDHa_l$Week <= Week2_Fit_DH) #idataDH1-idataDH9   
    ivaluesDO= which(!is.na(datDOa_l$Week) & datDOa_l$Ageg==i & datDOa_l$Week >= Week1_Fit_DO & datDOa_l$Week <= Week2_Fit_DO) #idataDO1-idataDO9 
    valuesH  =  datHa_l$Freq[ivaluesH]*weight[i] 
    valuesDH = datDHa_l$Freq[ivaluesDH]*weight[i]
    valuesDO = datDOa_l$Freq[ivaluesDO]*weight[i]  } else { #iplatform=0  #simulated (actually, true model, as data too noisy)
    valuesH  = eval(parse(text = paste0("datM$H_mod", eval(i),"[imodelH]"))) 
    valuesDH = eval(parse(text = paste0("datM$DH_mod",eval(i),"[imodelDH]"))) 
    valuesDO = eval(parse(text = paste0("datM$DO_mod",eval(i),"[imodelDO]"))) 
    }
    assign(paste0("H", eval(i),"d"), YLOG(valuesH, LOG))                        #H1d-H9d
    assign(paste0("DH",eval(i),"d"), YLOG(valuesDH,LOG))                        #DH1d-DH9d
    assign(paste0("DO",eval(i),"d"), YLOG(valuesDO,LOG))                        #DO1d-DO9d
}

#H
datHa  <- tibble(Weeks = 1 + mE$byw$time[imodelH]/7 + Week_shift_model,
                 Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1), #"2020-01-06" Mon
                 #Dates = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'), #Checked: "Mon" consistent with weeks/dates def throughout 
                 H1w=H1w, H2w=H2w, H3w=H3w, H4w=H4w, H5w=H5w, #estimated
                 H6w=H6w, H7w=H7w, H8w=H8w, H9w=H9w,
                 H1d=H1d, H2d=H2d, H3d=H3d, H4d=H4d, H5d=H5d, #data
                 H6d=H6d, H7d=H7d, H8d=H8d, H9d=H9d)
#DH
datDHa <- tibble(Weeks = 1 + mE$byw$time[imodelDH]/7 + Week_shift_model,
                 Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1), #"2020-01-06" Mon
                 #Dates = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'), #Checked: "Mon" consistent with weeks/dates def throughout 
                 DH1w=DH1w, DH2w=DH2w, DH3w=DH3w, DH4w=DH4w, DH5w=DH5w,    #estimated
                 DH6w=DH6w, DH7w=DH7w, DH8w=DH8w, DH9w=DH9w,
                 DH1d=DH1d, DH2d=DH2d, DH3d=DH3d, DH4d=DH4d, DH5d=DH5d,    #data
                 DH6d=DH6d, DH7d=DH7d, DH8d=DH8d, DH9d=DH9d)
#DO
datDOa <- tibble(Weeks = 1 + mE$byw$time[imodelDO]/7 + Week_shift_model,   
                 Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1), #"2020-01-06" Mon
                 #Dates = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'), #Checked: "Mon" consistent with weeks/dates def throughout 
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
#par(mar =c(0,0,0,0)) # c(0.5, 1, 1, 1)) #Par(mar = c(2, 2, 1, 1))  #bottom, left, top, right
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
if(length(UPPER)<12){
svglite(paste0(filenamepath,".svg")); correlationPlot(out); invisible(dev.off()) } else {
svglite(paste0(filenamepath,".svg")); plot(1:10); invisible(dev.off())
}


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
colors <- c("Data" = 1,  "MAP" = 2, "95% CrI" = "grey70", "95% perc" = "grey70", "MAP prev" = 2, "MAP posi" = 2)
zMAX = rep(range(zsample)[2],length(Datessample))
#H
dzsample <- tibble(Date=Datessample, zsample05=zsample95[,1], zsample95=zsample95[,2],
                   zMAP=mE$byw$Hw[imodelH], Datez=datH$Datesz, zdw=datH$zdw, yLIM=zMAX )
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
                   wMAP=mE$byw$DHw[imodelDH], Datew=datD$Datesw, wdw=datD$wdw, yLIM=zMAX ) 
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
                   vMAP=mE$byw$DOw[imodelDO], Datev=datD$Datesv, vdw=datD$vdw, yLIM=zMAX ) 
p3 <- ggplot(dvsample, aes(x=Date)) + 
      geom_ribbon(aes(ymin = vsample05, ymax = vsample95), fill = "grey70") +
      geom_point(aes(x=Datev, y = vdw,       color="Data")) +
      geom_line (aes(x=Date,  y = vMAP,      color="MAP")) +
      geom_line (aes(x=Date,  y = vsample05, color="95% CrI")) +
      labs(x = '', y = 'Deaths outside hospital', color = "") + 
      scale_color_manual(values = colors) +
      theme(axis.title = element_text(size = 12, face = "bold"))


gridExtra::grid.arrange(p1, p2, p3, nrow = 3)

svglite(paste0(filenamepath,".svg")); 
gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
invisible(dev.off())


### Plot R0 over time
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_PosteriorSample_Prev_R0")

colors <- c("Contact mtx" = 1,  "MAP" = 2, "95% CrI" = "grey70", "95% perc" = "grey70")
#R0
drsample <- tibble(Date=Datessample, R0sample05=R0weeksample95[,1], R0sample95=R0weeksample95[,2],
                   R0MAP=datH$R0_week, R0xcmMEV=r0[[2]]$R0xcmMEV)
pR0 <-ggplot(drsample, aes(x=Date)) + 
      geom_ribbon(aes(ymin = R0sample05, ymax = R0sample95), fill = "grey70") +
      geom_point(aes(x=Date, y = R0xcmMEV,   color="Contact mtx")) +
      geom_line (aes(x=Date, y = R0MAP,      color="MAP")) +
      geom_line (aes(x=Date, y = R0sample05, color="95% CrI")) +
      labs(x = 'Date', y = 'R0 estimate', color = "") + 
      scale_color_manual(values = colors) +
      theme(axis.title = element_text(size = 12, face = "bold"))

### Plot prevalence
dcsample <- tibble(Date=Datessample, p05=Psample95[,1], p95=Psample95[,2], pMAP = datp$Prev)
pp <-ggplot(dcsample, aes(x=Date)) + 
  geom_ribbon(aes(ymin = p05, ymax = p95), fill = "grey70") +
  geom_line (aes(x=Date, y = pMAP,    color="MAP")) +
  geom_line (aes(x=Date, y = p05,     color="95% CrI")) +
  labs(x = 'Date', y = 'Prevalence estimate', color = "") + 
  scale_color_manual(values = colors) +
  theme(axis.title = element_text(size = 12, face = "bold"))

### Plot positivity
posi_data_plot = 0*datp$Posi #same length as model
posi_data_plot[idata_posi + shift_posi] = (posi_data_perc/100)*posi_d_to_m #data*factor to which model fitted 

dcsample <- tibble(Date=Datessample, c05=csample95[,1], c95=csample95[,2], 
                   cMAP = datp$Posi, cdata=posi_data_plot)
pc <-ggplot(dcsample, aes(x=Date)) + 
  geom_ribbon(aes(ymin = c05, ymax = c95), fill = "grey70") +
  geom_point(aes(x=Date, y = cdata,   color="Data")) +
  geom_line (aes(x=Date, y = cMAP,    color="MAP")) +
  geom_line (aes(x=Date, y = c05,     color="95% CrI")) +
  labs(x = 'Date', y = 'Positivity estimate', color = "") + 
  scale_color_manual(values = colors) +
  theme(axis.title = element_text(size = 12, face = "bold"))

gridExtra::grid.arrange(pp, pc, pR0, nrow = 3)

svglite(paste0(filenamepath,".svg")); 
gridExtra::grid.arrange(pp, pc, pR0, nrow = 3)
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




