
### Dataframes from model:
###   datM$  Weeks, Dataz, Dataw, Datav, Datazi, Datawi, Datavi, etc

### Dataframes with OS data - sourced from HDdata.R via main:
###   datH_l,   datDH_l,   datDO_l
###   datHa_l,  datDHa_l,  datDOa_l
###   datHs_l,  datDHs_l,  datDOs_l
###   datHas_l, datDHas_l, datDOas_l
### names: Week, Date, Freq, Ageg, Shield

# TODO: => Dont need specific idataH, imodelH, etc - idata and imodel suffice - use all over?

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
  Week1_Data = lubridate::week("2020-01-01") #applied in HDdata.r, HDdata.Rmd
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
  
## Merge age groups - Replicate in simulated data the merging of ageg in the real data
  #TODO: revise merge nos.
  nmH_0  = 4  #merges 1:4
  nmD_0  = 6  #merges 1:6 
  nmH_1  = 4  #merges 1:4
  nmD_1  = 6  #merges 1:6 
  zdm4_0 = rep(0,times=length(idata))
  wdm6_0 = rep(0,times=length(idata))
  vdm6_0 = rep(0,times=length(idata))
  zdm4_1 = rep(0,times=length(idata))
  wdm6_1 = rep(0,times=length(idata))
  vdm6_1 = rep(0,times=length(idata))
  for (i in 1:nmH_0){ zdm4_0 = zdm4_0 + datM[paste0("Dataz",i,"_0")][[1]] } #[[1]] numeric part
  for (i in 1:nmD_0){ wdm6_0 = wdm6_0 + datM[paste0("Dataw",i,"_0")][[1]] }
  for (i in 1:nmD_0){ vdm6_0 = vdm6_0 + datM[paste0("Datav",i,"_0")][[1]] }
  for (i in 1:nmH_1){ zdm4_1 = zdm4_1 + datM[paste0("Dataz",i,"_1")][[1]] }
  for (i in 1:nmD_1){ wdm6_1 = wdm6_1 + datM[paste0("Dataw",i,"_1")][[1]] }
  for (i in 1:nmD_1){ vdm6_1 = vdm6_1 + datM[paste0("Datav",i,"_1")][[1]] }
  
  print(paste0("#H  data pts fitted, #: ", length(idataH),   ", list: ", range(idataH)[1],  "...",range(idataH)[2]))    #41, 1...41
  print(paste0("#DH data pts fitted, #: ", length(idataDH),  ", list: ", range(idataDH)[1], "...",range(idataDH)[2]))   #41, 1...41
  print(paste0("#DO data pts fitted, #: ", length(idataDO),  ", list: ", range(idataDO)[1], "...",range(idataDO)[2]))   #41, 1...41
  print(paste0("#H  model pts used,  #: ", length(imodelH),  ", list: ", range(imodelH)[1], "...",range(imodelH)[2]))   #41, 1...41
  print(paste0("#DH model pts used,  #: ", length(imodelDH), ", list: ", range(imodelDH)[1],"...",range(imodelDH)[2]))  #41, 1...41
  print(paste0("#DO model pts used,  #: ", length(imodelDO), ", list: ", range(imodelDO)[1],"...",range(imodelDO)[2]))  #41, 1...41
#Date range of model  
  Week1_Model  = lubridate::week("2020-01-27")    #  4 #cf contacts.r
  Week2_Model  = Week1_Model + (pars$nw-1)        # 55 #model set to run 52=pars$nw weeks
  Week_shift_model = (Week1_Model-1)

  
} else { #if iplatform>0

## Each age group has:
## - weeks 1-48 in 2020
## - freq=0 (inputted) when events unreported
## Date range for fitting
## Convert dates to WEEKS                         #check: week("2020-01-01") or week("2020-01-07") #[1] 1
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

## Merge age groups (do merging here rather than read merged dfs)
  #TODO: revise merge nos.
  nmH_0  = 4  #merges 1:4
  nmD_0  = 6  #merges 1:6 
  nmH_1  = 4  #merges 1:4
  nmD_1  = 6  #merges 1:6 
  zdm4_0 = rep(0,times=length(idataH ))
  wdm6_0 = rep(0,times=length(idataDH))
  vdm6_0 = rep(0,times=length(idataDO))
  zdm4_1 = rep(0,times=length(idataH ))
  wdm6_1 = rep(0,times=length(idataDH))
  vdm6_1 = rep(0,times=length(idataDO))
  for (i in 1:nmH_0){ zdm4_0 = zdm4_0 + eval(parse(text = paste0("zd",eval(i),"_0")))  } 
  for (i in 1:nmD_0){ wdm6_0 = wdm6_0 + eval(parse(text = paste0("wd",eval(i),"_0")))  }
  for (i in 1:nmD_0){ vdm6_0 = vdm6_0 + eval(parse(text = paste0("vd",eval(i),"_0")))  }
  for (i in 1:nmH_1){ zdm4_1 = zdm4_1 + eval(parse(text = paste0("zd",eval(i),"_1")))  }
  for (i in 1:nmD_1){ wdm6_1 = wdm6_1 + eval(parse(text = paste0("wd",eval(i),"_1")))  }
  for (i in 1:nmD_1){ vdm6_1 = vdm6_1 + eval(parse(text = paste0("vd",eval(i),"_1")))  }
  
## Convert data week-range (index 4:48, since Week1_Model=2020-01-27 to Week2_Study=2020-12-01) 
## to model-weeks          (index 1:52, since Week1_Model=2020-01-27)
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
  
} #iplatform


#### WEIGHTS to scale data (each age group) to model demography ################
if(pset$iplatform==2){ #Different demographies between model (England) and data (cohort)
  # England, ONS
  ageons = pars$ageons         #1:9
  # Cohort, TPP
  agecoh = pars$agecoh         #1:
  #### merged groups
  ageons4m_0 = sum(ageons[1:nmH_0])  #4 <= 1:4
  ageons6m_0 = sum(ageons[1:nmD_0])  #6 <= 1:6
  agecoh4m_0 = sum(agecoh[1:nmH_0])  #4 <= 1:4
  agecoh6m_0 = sum(agecoh[1:nmD_0])  #6 <= 1:6
  ageons4m_1 = sum(ageons[1:nmH_1])  #4 <= 1:4
  ageons6m_1 = sum(ageons[1:nmD_1])  #6 <= 1:6
  agecoh4m_1 = sum(agecoh[1:nmH_1])  #4 <= 1:4
  agecoh6m_1 = sum(agecoh[1:nmD_1])  #6 <= 1:6
  #weights
  weight      = ageons*pars$Npop/(agecoh*pars$Npopcoh)
  weightz4m_0 = ageons4m_0*pars$Npop/(agecoh4m_0*pars$Npopcoh)
  weightw6m_0 = ageons6m_0*pars$Npop/(agecoh6m_0*pars$Npopcoh)
  weightv6m_0 = ageons6m_0*pars$Npop/(agecoh6m_0*pars$Npopcoh)
  weightz4m_1 = ageons4m_1*pars$Npop/(agecoh4m_1*pars$Npopcoh)
  weightw6m_1 = ageons6m_1*pars$Npop/(agecoh6m_1*pars$Npopcoh)
  weightv6m_1 = ageons6m_1*pars$Npop/(agecoh6m_1*pars$Npopcoh)

} else{
  weight    = rep(1,times=length(pars$ageons))
  weightz4m_0 = 1
  weightw6m_0 = 1
  weightv6m_0 = 1
  weightz4m_1 = 1
  weightw6m_1 = 1
  weightv6m_1 = 1
}


## WEIGHTED INCIDENCE DATA #####################################################
## RAW, MERGED & TIME-TOTALS ###################################################

#### merged groups
zdm4w_0   = round(zdm4_0*weightz4m_0,0)
wdm6w_0   = round(wdm6_0*weightw6m_0,0)
vdm6w_0   = round(vdm6_0*weightv6m_0,0)
zdm4w_1   = round(zdm4_1*weightz4m_1,0)
wdm6w_1   = round(wdm6_1*weightw6m_1,0)
vdm6w_1   = round(vdm6_1*weightv6m_1,0)
sdzdm4w_0 = sd(zdm4w_0)
sdwdm6w_0 = sd(wdm6w_0)
sdvdm6w_0 = sd(vdm6w_0)
sdzdm4w_1 = sd(zdm4w_1)
sdwdm6w_1 = sd(wdm6w_1)
sdvdm6w_1 = sd(vdm6w_1)

### raw & total groups
for (i in 1:9){
  ## -Counts for NB need be integer => round()
  assign(paste0("zd",  eval(i),"w_0"), round( eval(parse(text = paste0("zd",eval(i),"_0")))*weight[i], 0)) #zd1w_0-zd9w_0
  assign(paste0("wd",  eval(i),"w_0"), round( eval(parse(text = paste0("wd",eval(i),"_0")))*weight[i], 0)) #wd1w_0-wd9w_0
  assign(paste0("vd",  eval(i),"w_0"), round( eval(parse(text = paste0("vd",eval(i),"_0")))*weight[i], 0)) #vd1w_0-vd9w_0
  assign(paste0("zd",  eval(i),"w_1"), round( eval(parse(text = paste0("zd",eval(i),"_1")))*weight[i], 0)) #zd1w_1-zd9w_1
  assign(paste0("wd",  eval(i),"w_1"), round( eval(parse(text = paste0("wd",eval(i),"_1")))*weight[i], 0)) #wd1w_1-wd9w_1
  assign(paste0("vd",  eval(i),"w_1"), round( eval(parse(text = paste0("vd",eval(i),"_1")))*weight[i], 0)) #vd1w_1-vd9w_1
  #standard deviation for normal likelihood
  assign(paste0("sdzd",eval(i),"w_0"),    sd( eval(parse(text = paste0("zd",eval(i),"_0")))*weight[i]) )   #sdzd4w_0-sdzd9w_0
  assign(paste0("sdwd",eval(i),"w_0"),    sd( eval(parse(text = paste0("wd",eval(i),"_0")))*weight[i]) )   #sdwd4w_0-sdwd9w_0
  assign(paste0("sdvd",eval(i),"w_0"),    sd( eval(parse(text = paste0("vd",eval(i),"_0")))*weight[i]) )   #sdvd4w_0-sdvd9w_0
  assign(paste0("sdzd",eval(i),"w_1"),    sd( eval(parse(text = paste0("zd",eval(i),"_1")))*weight[i]) )   #sdzd4w_1-sdzd9w_1
  assign(paste0("sdwd",eval(i),"w_1"),    sd( eval(parse(text = paste0("wd",eval(i),"_1")))*weight[i]) )   #sdwd4w_1-sdwd9w_1
  assign(paste0("sdvd",eval(i),"w_1"),    sd( eval(parse(text = paste0("vd",eval(i),"_1")))*weight[i]) )   #sdvd4w_1-sdvd9w_1
  
  ## WEIGHTED TIME-TOTALS
  assign(paste0("Tzd",eval(i),"w_0"), sum( eval(parse(text = paste0("zd",eval(i),"w_0")))) )              #Tzd1w_0-Tzd9w_0
  assign(paste0("Twd",eval(i),"w_0"), sum( eval(parse(text = paste0("wd",eval(i),"w_0")))) )              #Twd1w_0-Twd9w_0
  assign(paste0("Tvd",eval(i),"w_0"), sum( eval(parse(text = paste0("vd",eval(i),"w_0")))) )              #Tvd1w_0-Tvd9w_0
  assign(paste0("Tzd",eval(i),"w_1"), sum( eval(parse(text = paste0("zd",eval(i),"w_1")))) )              #Tzd1w_1-Tzd9w_1
  assign(paste0("Twd",eval(i),"w_1"), sum( eval(parse(text = paste0("wd",eval(i),"w_1")))) )              #Twd1w_1-Twd9w_1
  assign(paste0("Tvd",eval(i),"w_1"), sum( eval(parse(text = paste0("vd",eval(i),"w_1")))) )              #Tvd1w_01Tvd9w_1
  #standard deviation if normal likelihood
  assign(paste0("sdTzd",eval(i),"w_0"),    eval(parse(text = paste0("sdzd",eval(i),"w_0")))*sqrt(length(4:48)) ) #sdTzd4w_0-sdTzd9w_0 #45 time points
  assign(paste0("sdTwd",eval(i),"w_0"),    eval(parse(text = paste0("sdwd",eval(i),"w_0")))*sqrt(length(4:48)) ) #sdTwd4w_0-sdTwd9w_0
  assign(paste0("sdTvd",eval(i),"w_0"),    eval(parse(text = paste0("sdvd",eval(i),"w_0")))*sqrt(length(4:48)) ) #sdTvd4w_0-sdTvd9w_0
  assign(paste0("sdTzd",eval(i),"w_1"),    eval(parse(text = paste0("sdzd",eval(i),"w_1")))*sqrt(length(4:48)) ) #sdTzd4w_1-sdTzd9w_1 #45 time points
  assign(paste0("sdTwd",eval(i),"w_1"),    eval(parse(text = paste0("sdwd",eval(i),"w_1")))*sqrt(length(4:48)) ) #sdTwd4w_1-sdTwd9w_1
  assign(paste0("sdTvd",eval(i),"w_1"),    eval(parse(text = paste0("sdvd",eval(i),"w_1")))*sqrt(length(4:48)) ) #sdTvd4w_1-sdTvd9w_1
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
  posi_d_to_m = max( 100*((mout$byw_0$Ct[imodel_posi] + mout$byw_1$Ct[imodel_posi])/pars$Npop) )/ 
                max(posi_av_perc[idata_posi])  #[1] 0.007730766
} else { #fitting real data
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
sourceCpp(file = paste0(input_dir,"/",pset$File_modelas_choice)) }
model <- SEIUHRD


### Model IC based on data
### -for each age group within the model
### -can only initialise: Ha0[] and Da0[]
### -can be inconsistent with other state variables, which, except for Ea0, are assumed=0
### -Ea0 >0 is enough => Ia and Ua increase afterwards (but if initialised only Ia0 or Ua0 wouldn't do that)
if (pset$iplatform==2 & pset$DataIC==1){ #>0
for (i in 1:9){ #assigining current incidence to state variables (neglecting remains of state from previously)
  ivalueH  = which( datHa_l$Ageg==i &  datHa_l$Week==Week1_Fit_H)
  ivalueDH = which(datDHa_l$Ageg==i & datDHa_l$Week==Week1_Fit_H)
  ivalueDO = which(datDOa_l$Ageg==i & datDOa_l$Week==Week1_Fit_H)
  pars$Ha0[i] =  datHa_l$Freq[ivalueH]                                #age i on week 1
  pars$Da0[i] = datDHa_l$Freq[ivalueDH] + datDOa_l$Freq[ivalueDO]     #age i on week 1
  #model_: breaks down into H1_0(ia,0) = Ha0[ia]*pa_0; and H1_1(ia,0) = Ha0[ia]*pa_1;
}}
  
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

#pars$rIR
f1  <- function(theta) { fi(theta)}; 
g1  <- function(par)   { gi(par)}
#pars$rIH
f2  <- function(theta) { fi(theta)};
g2  <- function(par)   { gi(par)}
#pars$rOD
f3  <- function(theta) { fi(theta)};
g3  <- function(par)   { gi(par)}
#pars$pE0
f4  <- function(theta) { fe(theta)}; #s - keep e: marginal is narrow
g4  <- function(par)   { ge(par)}    #s
#hM_0
f5  <- function(theta) { fI(theta)}; #e
g5  <- function(par)   { gI(par)}
#hM_1
f6  <- function(theta) { fI(theta)}; #e
g6  <- function(par)   { gI(par)}   
#hR_0
f7  <- function(theta) { fI(theta)}; #l
g7  <- function(par)   { gI(par)}
#hR_1
f8  <- function(theta) { fI(theta)}; #l
g8  <- function(par)   { gI(par)}
#ad_0  #dM_0  
f9  <- function(theta) { fI(theta)}; #fe(theta)}; #I
g9  <- function(par)   { gI(par)}    #ge(par)}    #I
#ad_1  #dM_1  
f10 <- function(theta) { fI(theta)}; #fe(theta)}; #I
g10 <- function(par)   { gI(par)}    #ge(par)}
#dR_0  #R0    #yR0_0
f11 <- function(theta) { fI(theta)}; #e #I 
g11 <- function(par)   { gI(par)}
#dR_1  #kDH   #yR0_1
f12 <- function(theta) { fI(theta)}; #e #I 
g12 <- function(par)   { gI(par)}
#yM1_0
f13 <- function(theta) { fI(theta)};  #e 
g13 <- function(par)   { gI(par)}
#yM1_1
f14 <- function(theta) { fI(theta)};  #e
g14 <- function(par)   { gI(par)}
#yR1_0
f15 <- function(theta) { fI(theta)}; #l #e
g15 <- function(par)   { gI(par)}    #l
#yR1_1
f16 <- function(theta) { fI(theta)}; #l #e
g16 <- function(par)   { gI(par)}    #l


### Parameter bounds
age  = c(mean(0:4),mean(5:11),mean(12:17),mean(18:29),mean(30:39),mean(40:49),mean(50:59),mean(60:69),mean(70:90)) #85))
age9 = age[9]
age1 = age[1]
age3 = age[3]

tMax     = 10 #20 #10
tMax2    = 30 #50 #30
tMin     = 1  #
tMin2    = 1  #

R0Max    = 5 #20 #10
R0Min    = 0.1 #0.01

pE0Max   = 0.05 #0.10
pE0Min   = 10/pars$Npop #0.00001   #pop = 565.5

fuMax    = 1
fuMin    = 0.1 #0.001

hMMax  = 1
hMMin  = 0.01
hRMax  = 1.5*max(1/abs(age[1:8]-age9))  #[1] 0.09677419
hRMin  = 0.01*max(1/abs(age[1:8]-age9)) #[1] 0.0006451613

yM1Max = 1
yM1Min = 0.01
yR1Max = 1.25*max(1/abs(age[3:8]-age9)) #[1] 0.08064516
yR1Min = 0.05*max(1/abs(age[3:8]-age9)) #[1] 0.003225806

yR0Max = 0.15*max(1/(age3-age[1:2]))    #[1] 0.02307692
yR0Min = 0.001*max(1/(age3-age[1:2]))   #[1] 0.0001538462

adMax  = 1
adMin  = 0.01

sdHMin = 0.01
sdHMax = 5    #multiplicative factor of the sd across the age-groups fitted

pkMin  = 0.1  #1/k^2, NB likelihood and data
pkMax  = 5    #Assume: same kD for DH and DO

oN = 1/pars$Npop


### LIKELIHOOD FUNCTION

source(file = paste0(input_dir,"/BETA_0.r")) #BETA <- function(pars) - Used within Likelihood
LogLikelihood <- function(theta){
  ### UPDATE: @@
  ###         proposal pars$
  ###         MAP      parsE$, 
  ###         sample   parsES$
  ### Proposed parameters
  pars$rIR = f1(theta[1])
  pars$rIH = f2(theta[2])
  pars$rOD = f3(theta[3]) #pars$rODa= f3(theta[3])
  pars$pE0 = f4(theta[4])
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
  sdH =      pars$sdH #0.67 #theta[8]
  kH  =      pars$kH  #1
  kDH =      pars$kDH
  kDO =      pars$kDO #kDH
  #Dependent parameters
  pars$h_0 = hM_0*exp((age-age9)*hR_0)
  pars$h_1 = hM_1*exp((age-age9)*hR_1)
  pars$d_0 = dM_0*exp((age-age9)*dR_0) #0.2*pars$h_0*pars$m_0  *pars$ad (default=1)
  pars$d_1 = dM_1*exp((age-age9)*dR_0) #0.2*pars$h_1*pars$m_1  *pars$ad (default=1)
  pars$y_0[3:9] = yM1_0*exp((age[3:9]-age9)*yR1_0)
  pars$y_1[3:9] = yM1_1*exp((age[3:9]-age9)*yR1_1)
  pars$Ea0   = pars$Na0*pars$pE0
  pars$Sa0   = pars$Na0 - pars$Ea0 - pars$Ia0 - pars$Ua0 - pars$Ha0 - pars$Oa0 - pars$Ra0 - pars$Da0   
  pars$beta  = BETA(pars) #BETA_0(pars)
  #pars$R0  = fs(theta[3])
  #pars$fu  = fs(theta[5])
  #kH =       fp(theta[8])  #pk = theta = 1/sqrt(k) => k = 1/pk^2
  #kDH =      fp(theta[24])
  #kDO =      fp(theta[3])

  ### Model outputs (from Rcpp, given the above proposed parameters)
  m <- model(pars)
  
  ### Model predictions - positivity
  MeanPosi_perc = (m$byw_0$Ct[imodel_posi] + m$byw_1$Ct[imodel_posi] )*oN*100

  ### Model predictions - incidence
  ### (Note: Has, DHas, DOas and are proportional to ageons (via Na))
  #Totals
  for (i in 1:nmH_0) { 
    assign(paste0("TMeanH", eval(i),"_0"), sum(eval(parse(text = paste0("m$byw_ageH_0$H", eval(i),"w[imodelH]")))))  }  # TMeanH1_0-4_0
  for (i in 1:nmH_1) { 
    assign(paste0("TMeanH", eval(i),"_1"), sum(eval(parse(text = paste0("m$byw_ageH_1$H", eval(i),"w[imodelH]")))))  }  # TMeanH1_0-4_0
  for (i in 1:nmD_0) { 
    assign(paste0("TMeanDH",eval(i),"_0"), sum(eval(parse(text = paste0("m$byw_ageD_0$DH",eval(i),"w[imodelDH]")))))    # TMeanDH1_0-6_0
    assign(paste0("TMeanDO",eval(i),"_0"), sum(eval(parse(text = paste0("m$byw_ageD_0$DO",eval(i),"w[imodelDO]"))))) }  # TMeanDO1_0-6_0
  for (i in 1:nmD_1) { 
    assign(paste0("TMeanDH",eval(i),"_1"), sum(eval(parse(text = paste0("m$byw_ageD_1$DH",eval(i),"w[imodelDH]")))))    # TMeanDH1_0-6_0
    assign(paste0("TMeanDO",eval(i),"_1"), sum(eval(parse(text = paste0("m$byw_ageD_1$DO",eval(i),"w[imodelDO]"))))) }  # TMeanDO1_0-6_0
  #Merged groups
  MeanHm4_0  = rep(0,times=length(imodelH))
  MeanHm4_1  = rep(0,times=length(imodelH))
  MeanDHm6_0 = rep(0,times=length(imodelDH))
  MeanDHm6_1 = rep(0,times=length(imodelDH))
  MeanDOm6_0 = rep(0,times=length(imodelDO))
  MeanDOm6_1 = rep(0,times=length(imodelDO))
  for (i in 1:nmH_0){MeanHm4_0  = MeanHm4_0  + eval(parse(text = paste0("m$byw_ageH_0$H", eval(i),"w[imodelH]")))  }
  for (i in 1:nmH_1){MeanHm4_1  = MeanHm4_1  + eval(parse(text = paste0("m$byw_ageH_1$H", eval(i),"w[imodelH]")))  }
  for (i in 1:nmD_0){MeanDHm6_0 = MeanDHm6_0 + eval(parse(text = paste0("m$byw_ageD_0$DH",eval(i),"w[imodelDH]")))
                     MeanDOm6_0 = MeanDOm6_0 + eval(parse(text = paste0("m$byw_ageD_0$DO",eval(i),"w[imodelDO]"))) }
  for (i in 1:nmD_1){MeanDHm6_1 = MeanDHm6_1 + eval(parse(text = paste0("m$byw_ageD_1$DH",eval(i),"w[imodelDH]")))
                     MeanDOm6_1 = MeanDOm6_1 + eval(parse(text = paste0("m$byw_ageD_1$DO",eval(i),"w[imodelDO]"))) }
  #avoid 0 mean and NAs in likelihood
  MeanHm4_0[1] = max(MeanHm4_0[1], 1)
  MeanDOm6_0[1]= max(MeanDOm6_0[1],1)
  MeanDHm6_0[1]= max(MeanDHm6_0[1],1)
  MeanHm4_1[1] = max(MeanHm4_1[1], 1)
  MeanDOm6_1[1]= max(MeanDOm6_1[1],1)
  MeanDHm6_1[1]= max(MeanDHm6_1[1],1)
  #Non-merged
  for (i in 1:9) { valuesz_0 = eval(parse(text = paste0("m$byw_ageH_0$H", eval(i),"w[imodelH]")))
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
                   assign(paste0("MeanH", eval(i),"_0"),valuesz_0)   #MeanH1_0-9_0
                   assign(paste0("MeanDH",eval(i),"_0"),valuesw_0)   #MeanDH1_0-9_0
                   assign(paste0("MeanDO",eval(i),"_0"),valuesv_0)   #MeanDO1_0-9_0
                   assign(paste0("MeanH", eval(i),"_1"),valuesz_1)   #MeanH1_1-9_1
                   assign(paste0("MeanDH",eval(i),"_1"),valuesw_1)   #MeanDH1_1-9_1
                   assign(paste0("MeanDO",eval(i),"_1"),valuesv_1) } #MeanDO1_1-9_1

  #Likelihood of data
  #product over: 1) datasets (H,DH,DO), 2) age-groups, and 3) weeks
  #Negative binomial or Normal likelihood

  ## Totals - sum of 45 NB,s - but note: many are inputted (non-reported) zeros
  kHT  = kH*10  #45; 
  kDHT = kDH*10 #*45; 
  kDOT = kDO*10 #*45; 
  ## Merged  - sum of 4 or 6 variables
  kHm  = kH;   #kH*4;   
  kDHm = kDH;  #kDH*6;
  kDOm = kDO;  #kDO*6; 

  ll_0 =
  ###H 
    (     dnbinom(x= Tzd1w_0, size = kHT,        mu   = TMeanH1_0,  log = T)) +     #1 Totals - NB (below counts)
    (     dnbinom(x= Tzd2w_0, size = kHT,        mu   = TMeanH2_0,  log = T)) +
    (     dnbinom(x= Tzd3w_0, size = kHT,        mu   = TMeanH3_0,  log = T)) +
    (     dnbinom(x= Tzd4w_0, size = kHT,        mu   = TMeanH4_0,  log = T)) +
   #sum(  dnorm(x =  zdm4w_0, sd = sdH*sdzdm4w_0,mean = MeanHm4_0,  log = T)) +     #2 Merged - Normal
   #sum(  dnbinom(x= zdm4w_0, size = kHm,        mu   = MeanHm4_0,  log = T)) +     #2 Merged - NB
   #sum(  dnbinom(x = zd1w_0, size = kH,         mu   = MeanH1_0,   log = T)) +     #3 Non-merged - NB
   #sum(  dnbinom(x = zd2w_0, size = kH,         mu   = MeanH2_0,   log = T)) +
   #sum(  dnbinom(x = zd3w_0, size = kH,         mu   = MeanH3_0,   log = T)) +
   #sum(  dnbinom(x = zd4w_0, size = kH,         mu   = MeanH4_0,   log = T)) +
   #sum(  dnorm(x =  zd1w_0,  sd = sdH*sdzd1w_0, mean = MeanH1_0,   log = T)) +     #3 Non-merged - Normal
   #sum(  dnorm(x =  zd2w_0,  sd = sdH*sdzd2w_0, mean = MeanH2_0,   log = T)) +        #(not ideal - low counts)
   #sum(  dnorm(x =  zd3w_0,  sd = sdH*sdzd3w_0, mean = MeanH3_0,   log = T)) +
   #sum(  dnorm(x =  zd4w_0,  sd = sdH*sdzd4w_0, mean = MeanH4_0,   log = T)) +     
   #sum(  dnorm(x =  zd5w_0,  sd = sdH*sdzd5w_0, mean = MeanH5_0,   log = T)) +     #Other - Normal
   #sum(  dnorm(x =  zd6w_0,  sd = sdH*sdzd6w_0, mean = MeanH6_0,   log = T)) +
   #sum(  dnorm(x =  zd7w_0,  sd = sdH*sdzd7w_0, mean = MeanH7_0,   log = T)) +
   #sum(  dnorm(x =  zd8w_0,  sd = sdH*sdzd8w_0, mean = MeanH8_0,   log = T)) +
   #sum(  dnorm(x =  zd9w_0,  sd = sdH*sdzd9w_0, mean = MeanH9_0,   log = T)) +
    sum(  dnbinom(x= zd5w_0,  size = kH,         mu   = MeanH5_0,   log = T)) +     #Other - NB
    sum(  dnbinom(x= zd6w_0,  size = kH,         mu   = MeanH6_0,   log = T)) +
    sum(  dnbinom(x= zd7w_0,  size = kH,         mu   = MeanH7_0,   log = T)) +
    sum(  dnbinom(x= zd8w_0,  size = kH,         mu   = MeanH8_0,   log = T)) +
    sum(  dnbinom(x =zd9w_0,  size = kH,         mu   = MeanH9_0,   log = T)) +    
  ###DH
    (     dnbinom(x= Twd1w_0, size = kDHT,       mu   = TMeanDH1_0, log = T)) +     #1 Totals - NB (below counts)
    (     dnbinom(x= Twd2w_0, size = kDHT,       mu   = TMeanDH2_0, log = T)) +
    (     dnbinom(x= Twd3w_0, size = kDHT,       mu   = TMeanDH3_0, log = T)) +
    (     dnbinom(x= Twd4w_0, size = kDHT,       mu   = TMeanDH4_0, log = T)) +
    (     dnbinom(x= Twd5w_0, size = kDHT,       mu   = TMeanDH5_0, log = T)) +
    (     dnbinom(x= Twd6w_0, size = kDHT,       mu   = TMeanDH6_0, log = T)) +
   #sum(  dnbinom(x= wdm6w_0, size = kDHm,       mu   = MeanDHm6_0, log = T)) +     #2 Merged - NB
   #sum(  dnbinom(x = wd1w_0, size = kDH,        mu   = MeanDH1_0,  log = T)) +     #3 Non-merged - NB
   #sum(  dnbinom(x = wd2w_0, size = kDH,        mu   = MeanDH2_0,  log = T)) +     
   #sum(  dnbinom(x = wd3w_0, size = kDH,        mu   = MeanDH3_0,  log = T)) +     
   #sum(  dnbinom(x = wd4w_0, size = kDH,        mu   = MeanDH4_0,  log = T)) +     
   #sum(  dnbinom(x = wd5w_0, size = kDH,        mu   = MeanDH5_0,  log = T)) +     
   #sum(  dnbinom(x = wd6w_0, size = kDH,        mu   = MeanDH6_0,  log = T)) +     
    sum(  dnbinom(x = wd7w_0, size = kDH,        mu   = MeanDH7_0,  log = T)) +     #Other - Normal
    sum(  dnbinom(x = wd8w_0, size = kDH,        mu   = MeanDH8_0,  log = T)) +
    sum(  dnbinom(x = wd9w_0, size = kDH,        mu   = MeanDH9_0,  log = T)) +
  ###DO
    (     dnbinom(x= Tvd1w_0, size = kDOT,       mu   = TMeanDO1_0, log = T)) +     #1 Totals - NB (below counts)
    (     dnbinom(x= Tvd2w_0, size = kDOT,       mu   = TMeanDO2_0, log = T)) +
    (     dnbinom(x= Tvd3w_0, size = kDOT,       mu   = TMeanDO3_0, log = T)) +
    (     dnbinom(x= Tvd4w_0, size = kDOT,       mu   = TMeanDO4_0, log = T)) +
    (     dnbinom(x= Tvd5w_0, size = kDOT,       mu   = TMeanDO5_0, log = T)) +
    (     dnbinom(x= Tvd6w_0, size = kDOT,       mu   = TMeanDO6_0, log = T)) +
   #sum(  dnbinom(x= vdm6w_0, size = kDOm,       mu   = MeanDOm6_0, log = T)) +     #2 Merged - NB
   #sum(  dnbinom(x = vd1w_0, size = kDO,        mu   = MeanDO1_0,  log = T)) +     #3 Non-merged - NB
   #sum(  dnbinom(x = vd2w_0, size = kDO,        mu   = MeanDO2_0,  log = T)) +     
   #sum(  dnbinom(x = vd3w_0, size = kDO,        mu   = MeanDO3_0,  log = T)) +     
   #sum(  dnbinom(x = vd4w_0, size = kDO,        mu   = MeanDO4_0,  log = T)) +     
   #sum(  dnbinom(x = vd5w_0, size = kDO,        mu   = MeanDO5_0,  log = T)) +     
   #sum(  dnbinom(x = vd6w_0, size = kDO,        mu   = MeanDO6_0,  log = T)) +     
    sum(  dnbinom(x = vd7w_0, size = kDO,        mu   = MeanDO7_0,  log = T)) +     #Other - Normal
    sum(  dnbinom(x = vd8w_0, size = kDO,        mu   = MeanDO8_0,  log = T)) +
    sum(  dnbinom(x = vd9w_0, size = kDO,        mu   = MeanDO9_0,  log = T))

  ll_1 =
  ###H 
  #(     dnbinom(x= Tzd1w_1, size = kHT,        mu   = TMeanH1_1,  log = T)) +     #1 Totals - NB (below counts)
  #(     dnbinom(x= Tzd2w_1, size = kHT,        mu   = TMeanH2_1,  log = T)) +
  #(     dnbinom(x= Tzd3w_1, size = kHT,        mu   = TMeanH3_1,  log = T)) +
  #(     dnbinom(x= Tzd4w_1, size = kHT,        mu   = TMeanH4_1,  log = T)) +
  #sum(  dnorm(x =  zdm4w_1, sd = sdH*sdzdm4w_1,mean = MeanHm4_1,  log = T)) +     #2 Merged - Normal
  #sum(  dnbinom(x= zdm4w_1, size = kHm,        mu   = MeanHm4_1,  log = T)) +     #2 Merged - NB
   sum(  dnbinom(x = zd1w_1, size = kH,         mu   = MeanH1_1,   log = T)) +     #3 Non-merged - NB
   sum(  dnbinom(x = zd2w_1, size = kH,         mu   = MeanH2_1,   log = T)) +
   sum(  dnbinom(x = zd3w_1, size = kH,         mu   = MeanH3_1,   log = T)) +
   sum(  dnbinom(x = zd4w_1, size = kH,         mu   = MeanH4_1,   log = T)) +
  #sum(  dnorm(x =  zd1w_1,  sd = sdH*sdzd1w_1, mean = MeanH1_1,   log = T)) +     #3 Non-merged - Normal
  #sum(  dnorm(x =  zd2w_1,  sd = sdH*sdzd2w_1, mean = MeanH2_1,   log = T)) +        #(not ideal - low counts)
  #sum(  dnorm(x =  zd3w_1,  sd = sdH*sdzd3w_1, mean = MeanH3_1,   log = T)) +
  #sum(  dnorm(x =  zd4w_1,  sd = sdH*sdzd4w_1, mean = MeanH4_1,   log = T)) +     
  #sum(  dnorm(x =  zd5w_1,  sd = sdH*sdzd5w_1, mean = MeanH5_1,   log = T)) +     #Other - Normal
  #sum(  dnorm(x =  zd6w_1,  sd = sdH*sdzd6w_1, mean = MeanH6_1,   log = T)) +
  #sum(  dnorm(x =  zd7w_1,  sd = sdH*sdzd7w_1, mean = MeanH7_1,   log = T)) +
  #sum(  dnorm(x =  zd8w_1,  sd = sdH*sdzd8w_1, mean = MeanH8_1,   log = T)) +
  #sum(  dnorm(x =  zd9w_1,  sd = sdH*sdzd9w_1, mean = MeanH9_1,   log = T)) +
   sum(  dnbinom(x= zd5w_1,  size = kH,         mu   = MeanH5_1,   log = T)) +     #Other - NB
   sum(  dnbinom(x= zd6w_1,  size = kH,         mu   = MeanH6_1,   log = T)) +
   sum(  dnbinom(x= zd7w_1,  size = kH,         mu   = MeanH7_1,   log = T)) +
   sum(  dnbinom(x= zd8w_1,  size = kH,         mu   = MeanH8_1,   log = T)) +
   sum(  dnbinom(x =zd9w_1,  size = kH,         mu   = MeanH9_1,   log = T)) +    
  ###DH
  #(     dnbinom(x= Twd1w_1, size = kDHT,       mu   = TMeanDH1_1, log = T)) +     #1 Totals - NB (below counts)
  #(     dnbinom(x= Twd2w_1, size = kDHT,       mu   = TMeanDH2_1, log = T)) +
  #(     dnbinom(x= Twd3w_1, size = kDHT,       mu   = TMeanDH3_1, log = T)) +
  #(     dnbinom(x= Twd4w_1, size = kDHT,       mu   = TMeanDH4_1, log = T)) +
  #(     dnbinom(x= Twd5w_1, size = kDHT,       mu   = TMeanDH5_1, log = T)) +
  #(     dnbinom(x= Twd6w_1, size = kDHT,       mu   = TMeanDH6_1, log = T)) +
  #sum(  dnbinom(x= wdm6w_1, size = kDHm,       mu   = MeanDHm6_1, log = T)) +     #2 Merged - NB
   sum(  dnbinom(x = wd1w_1, size = kDH,        mu   = MeanDH1_1,  log = T)) +     #3 Non-merged - NB
   sum(  dnbinom(x = wd2w_1, size = kDH,        mu   = MeanDH2_1,  log = T)) +     
   sum(  dnbinom(x = wd3w_1, size = kDH,        mu   = MeanDH3_1,  log = T)) +     
   sum(  dnbinom(x = wd4w_1, size = kDH,        mu   = MeanDH4_1,  log = T)) +     
   sum(  dnbinom(x = wd5w_1, size = kDH,        mu   = MeanDH5_1,  log = T)) +     
   sum(  dnbinom(x = wd6w_1, size = kDH,        mu   = MeanDH6_1,  log = T)) +     
   sum(  dnbinom(x = wd7w_1, size = kDH,        mu   = MeanDH7_1,  log = T)) +     #Other - Normal
   sum(  dnbinom(x = wd8w_1, size = kDH,        mu   = MeanDH8_1,  log = T)) +
   sum(  dnbinom(x = wd9w_1, size = kDH,        mu   = MeanDH9_1,  log = T)) +
  ###DO
  #(     dnbinom(x= Tvd1w_1, size = kDOT,       mu   = TMeanDO1_1, log = T)) +     #1 Totals - NB (below counts)
  #(     dnbinom(x= Tvd2w_1, size = kDOT,       mu   = TMeanDO2_1, log = T)) +
  #(     dnbinom(x= Tvd3w_1, size = kDOT,       mu   = TMeanDO3_1, log = T)) +
  #(     dnbinom(x= Tvd4w_1, size = kDOT,       mu   = TMeanDO4_1, log = T)) +
  #(     dnbinom(x= Tvd5w_1, size = kDOT,       mu   = TMeanDO5_1, log = T)) +
  #(     dnbinom(x= Tvd6w_1, size = kDOT,       mu   = TMeanDO6_1, log = T)) +
  #sum(  dnbinom(x= vdm6w_1, size = kDOm,       mu   = MeanDOm6_1, log = T)) +     #2 Merged - NB
   sum(  dnbinom(x = vd1w_1, size = kDO,        mu   = MeanDO1_1,  log = T)) +     #3 Non-merged - NB
   sum(  dnbinom(x = vd2w_1, size = kDO,        mu   = MeanDO2_1,  log = T)) +     
   sum(  dnbinom(x = vd3w_1, size = kDO,        mu   = MeanDO3_1,  log = T)) +     
   sum(  dnbinom(x = vd4w_1, size = kDO,        mu   = MeanDO4_1,  log = T)) +     
   sum(  dnbinom(x = vd5w_1, size = kDO,        mu   = MeanDO5_1,  log = T)) +     
   sum(  dnbinom(x = vd6w_1, size = kDO,        mu   = MeanDO6_1,  log = T)) +     
   sum(  dnbinom(x = vd7w_1, size = kDO,        mu   = MeanDO7_1,  log = T)) +     #Other - Normal
   sum(  dnbinom(x = vd8w_1, size = kDO,        mu   = MeanDO8_1,  log = T)) +
   sum(  dnbinom(x = vd9w_1, size = kDO,        mu   = MeanDO9_1,  log = T))
  
 ll = ll_0 + ll_1 +
 ###Prevalence
    0.25*sum(  dnorm(x = posi_data_perc*posi_d_to_m, sd = posi_sd_percT*posi_d_to_m, mean = MeanPosi_perc, log = T))

    return(ll)
} #Likelihood


## Likelihood definition, parameter ranges  ####################################
niter = 30000#120000#6000#60000#15000#6000#3000#30000#60000
if (pset$iplatform==2){niter=200000} #150000} #120000}

        #1/rIR,     1/rIH,       1/rOD,       pE0,          hM,                         hR,                
LOWER = c(tMin,     tMin2,       tMin2,       g4(pE0Min),   g5(hMMin),    g6(hMMin),    g7(hRMin),    g8(hRMin),
        #dM,                     dR,                        yM1,                        yR1,
        g9(hMMin),  g10(hMMin),  g11(hRMin),  g12(hRMin),   g13(yM1Min),  g14(yM1Min),  g15(yR1Min),  g16(yR1Min));

        #1/rIR,     1/rIH,       1/rOD,       pE0,          hM,                         hR,                
UPPER = c(tMax,     tMax2,       tMax2,       g4(pE0Max),   g5(hMMax),    g6(hMMax),    g7(hRMax),    g8(hRMax),
        #dM,                     dR,                        yM1,                       yR1,
        g9(hMMax),  g10(hMMax),  g11(hRMax),  g12(hRMax),   g13(yM1Max),  g14(yM1Max),  g15(yR1Max),  g16(yR1Max));

######### parameters_J6_24apr24_rev
hMbest_0= pars$hM_0 #0.3796 #0.3068 #0.4755 #0.3567 #pars$h[9]=0.4755
hMbest_1= pars$hM_1 #0.9235 #0.9096 #0.4755 #0.3567
hRbest_0= pars$hR_0 #0.0416 #0.0398 #0.0684
hRbest_1= pars$hR_1 #0.0011 #9e-04  #0.0684

dMbest_0= pars$dM_0 #0.0253 #0.0219 #0.0213 #assumes 2*0.1*h*m (ad=1)
dMbest_1= pars$dM_1 #0.0857 #0.1036 #0.0296
dRbest_0= pars$dR_0 #0.0909 #0.0903 #0.1219
dRbest_1= pars$dR_1 #0.0011+0.0778# hR_1+mR_1 #0.3923#0.6061#0.1157

yMbest_0= pars$yM1_0 #0.4326 #0.6498 #0.69   #0.7689 #ageg 3:9 #pars$y[9]=0.69
yMbest_1= pars$yM1_1 #0.1738 #0.1609 #0.69   #0.7689
yRbest_0= pars$yR1_0 #0.0105 #0.016  #0.019
yRbest_1= pars$yR1_1 #0.0151 #0.0115 #0.019

#adbest_0= 0.2 #pars$ad_0
#adbest_1= 0.2 #pars$ad_1
#R0best  = pars$R0
#rEIbest = pars$rEI
#kDHbest = pars$kDH
#kDObest = pars$kDO
BEST0 = c(1/pars$rIR, 1/pars$rIH, 1/pars$rOD, pars$pE0, #1/pars$rODa
         hMbest_0, hMbest_1, hRbest_0, hRbest_1,
         dMbest_0, dMbest_1, dRbest_0, dRbest_1, 
         yMbest_0, yMbest_1, yRbest_0, yRbest_1)

BEST = c( g1(pars$rIR),  g2(pars$rIH),  g3(pars$rOD),  g4(pars$pE0),
          g5(hMbest_0),  g6(hMbest_1),  g7(hRbest_0),  g8(hRbest_1),
          g9(dMbest_0), g10(dMbest_1), g11(dRbest_0), g12(dRbest_1), 
         g13(yMbest_0), g14(yMbest_1), g15(yRbest_0), g16(yRbest_1))
         
#Uniform priors
  #PARSTART = 0.5*UPPER
  #nchain = 3
  #setup    = createBayesianSetup(likelihood=LogLikelihood, lower = LOWER, upper = UPPER) #parallel = T,
  #settings = list (startValue=t(array(PARSTART,dim=c(length(PARSTART),nchain))), iterations = niter, burnin = round(niter*length(LOWER)/15), message=T) #F)
#Beta priors
  Burnin = round(niter/2)+1  #round(niter/4)+1 #+1 as "burnin" is the start of effective sample
  PRIOR <- createBetaPrior(4,4,lower = LOWER, upper = UPPER) #createBetaPrior(3,3,lower = LOWER, upper = UPPER
  setup  = createBayesianSetup(likelihood=LogLikelihood, prior =PRIOR) #, best=BEST) #NULL) #parallel = T,
  settings = list (iterations = niter, burnin = Burnin, message=T) #F) 

## Bayesian sample
tout1 <- system.time(out <- runMCMC(bayesianSetup=setup, settings=settings) )



Resample=0 #####################################################################
if (Resample==1){
## Run with new prior based on posterior
#newPrior <- createPriorDensity(out)
newPrior = createPriorDensity(out, method = "multivariate", eps = 1e-10, lower = LOWER, upper =  UPPER, best = NULL) #lower = rep(-10, 3), upper =  rep(10, 3)
setup2 = createBayesianSetup(likelihood=LogLikelihood, prior = newPrior)
## Try: message=T)
## Bayesian sample
tout2 <- system.time(out2 <- runMCMC(bayesianSetup=setup2, settings=settings) )
 #=> lots of NAN bec parameters not sensibly bounded - would need to transform pars to allow this
out_1 <- out
out   <- out2 }


Restart=0 ######################################################################
if (Restart==1){
## Restart same sampling
  out_1 <- out
  out   <- out2
tout3 <- system.time(out3 <- runMCMC(bayesianSetup=setup, settings=settings) )
  out_2 <- out
  out   <- out3
}


## SAVE MCMC chain objects for OC and for outside reconstruction
## -thinned (x7)
library(coda)
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_mcmcChain")
# thinned indices
y  <- out$chain[[1]]
i7 <- seq(seq_along(y[,1])[1]+6,seq_along(y[,1])[length(y[,1])],7)
# thinned mcmc object and csv writing
out2=out
for (ic in 1:3) { #default: nchain=3
  out2$chain[[ic]] <- coda::as.mcmc(out$chain[[ic]][i7,])
  write.csv(data.frame(out2$chain[[ic]]), file=paste0(filenamepath,ic,".csv")) 
}

## CHAINS diagnostics -split in 4-par sets
npar = length(LOWER)

# pdf filenamepathAll - start ##################################################
filenamepathAll = paste0(output_dir,"/",pset$File_fit_output0,"_All")
if (pset$iplatform<2) {pdf(paste0(filenamepathAll,".pdf"))}

# parname=paste0("p",1:npar)
# column sets
J=unique(ceiling((1:npar)/4))
Ja=unique(4*(floor((1:npar-1)/4))+1)  #npar external
Jb=unique(4*(ceiling((1:npar)/4)))    #npar external

# narrower-column mcmc object
out3=out2
filenamepath    = paste0(output_dir,"/",pset$File_fit_output0,"_marginalTrace")
for (icolset in seq_along(J)){
  for(ic in 1:3) {
    out3$chain[[ic]] <- coda::as.mcmc(out2$chain[[ic]][,c(Ja[icolset]:Jb[icolset])]) }
# object plotting and svg writing
  par(mar = c(2, 2, 1, 1)) ##bottom, left, top, right #default: par("mar") #[1] 5.1 4.1 4.1 2.1
  plot(out3$chain); 
  svg(paste0(filenamepath,icolset,".svg")); plot(out3$chain); dev.off() }
#if (pset$iplatform<2) {dev.off()}; #dev.off()
## MAP Estimates  ##############################################################
print("MAP..."); cat("\n")
parsE     <- pars     #pars setup initially
MAPE      <- MAP(out) #pars estimated
### UPDATE: @@
###         proposal pars$
###         MAP      parsE$, 
###         sample   parsES$
parsE$rIR <- f1(as.vector(MAPE$parametersMAP[1]))
parsE$rIH <- f2(as.vector(MAPE$parametersMAP[2]))
parsE$rOD <- f3(as.vector(MAPE$parametersMAP[3])) #parsE$rODa
parsE$pE0 <- f4(as.vector(MAPE$parametersMAP[4]))
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
#kHE       <- pars$kH  #1
#kDHE      <- pars$kDH #1
#kDOE      <- pars$kDO #1
#Dependent parameters
parsE$h_0 = hME_0*exp((age-age9)*hRE_0)
parsE$h_1 = hME_1*exp((age-age9)*hRE_1)
parsE$d_0 = dME_0*exp((age-age9)*dRE_0) #2*parsE$h_0*pars$m_0
parsE$d_1 = dME_1*exp((age-age9)*dRE_1) #2*parsE$h_1*pars$m_1
parsE$y_0[3:9] = yM1E_0*exp((age[3:9]-age9)*yR1E_0)
parsE$y_1[3:9] = yM1E_1*exp((age[3:9]-age9)*yR1E_1)
parsE$Ea0   = parsE$Na0*parsE$pE0
parsE$Sa0   = parsE$Na0 - parsE$Ea0 - parsE$Ia0 - parsE$Ua0 - parsE$Ha0 - parsE$Oa0 - parsE$Ra0 - parsE$Da0   
parsE$beta  = BETA(parsE) #BETA_0(parsE)
#Predictions
mE        <- model(parsE)
#R0_week using MAP-parameters
source(file = paste0(input_dir,"/R0_0.r"))
ntimes = length(imodelH)
r0 = R0_0(parsE, GetBeta=0, GetOutput=1, Sampling=1, nt=ntimes)
R0_weekE = r0[[2]]$R0_week
#PositivE = (mE$byw_0$Ct[imodelH] + mE$byw_1$Ct[imodelH])*oN
#Prevalence using MAP-parameters
#see datp

#Note: R0(parsE, GetBeta=0, GetOutput=1, Sampling=1, nt=ntimes)[[1]]$R0 - same as parsE$R0


## Credible Intervals ##########################################################

## from full sample
# quantiles of full sample
# getCredibleIntervals(getSample(out),c(0.025,0.975))
# round(getCredibleIntervals(getSample(out),c(0.025,0.975)) ,3)
# median of full sample
#	getCredibleIntervals(getSample(out),c(0.5,0.5))[1,]


## Sample the chains
if (!is.element(pset$iplatform,1) & length(idataH)==length(idataDH) ){
print("Sampling..."); cat("\n")

oN = 1/pars$Npop
npar = length(LOWER)
Thin = 4#2
Chains = 3
StartSampChainPostBurn = 1
LengtMcmcChainPostBurn = floor((niter-Burnin+1)/Chains)
LengtSampChainPostBurn = LengtMcmcChainPostBurn - StartSampChainPostBurn + 1
nsample = Chains*LengtMcmcChainPostBurn/Thin #nsample = 100#3000#500#1000
#start-end - for each chain
#Note: parametersOnly = F => gives extra: Lposterior Llikelihood     Lprior
psample = getSample(out, parametersOnly = T, start=StartSampChainPostBurn, end= LengtSampChainPostBurn, thin=Thin)
#dim(psample) #= c(nsample, npar)
#
# Parameter stats
Pname=c("rIR  ",  "rIH  ",  "rOD  ", "pE0  ",
        "hM_0 ",  "hM_1 ",  "hR_0 ", "hR_1 ",  
        "dM_0 ",  "dM_1 ",  "dR_0 ", "dR_1 ",
        "yM1_0",  "yM1_1",  "yR1_0", "yR1_1")
Pnamei=c("1/rIR", "1/rIH",  "1/rOD",  Pname[4:npar])

##Full sample quantiles
Pq=matrix(0,npar,3)
PMAP=rep(0,npar)
q1=0.025
q2=0.975
Pq <- t(getCredibleIntervals(getSample(out),c(q1,0.5,q2))) #Pq[ip,1:3] #getCredibleIntervals[1:3,ip]
##transformations
#1:3 - time - keep as is
Pq[4,]=f4(Pq[4,])
##rounding
Pr=round(Pq,3)

##MAP
##MAPE are already transformed
for (ip in 1:npar) {
  PMAP[ip] = eval(parse(text = paste0("f",eval(ip),"(MAPE[[1]][[",eval(ip),"]])")))
  if(ip<=3) PMAP[ip] = 1/PMAP[ip] #1/rate as MAPE are transformed
}
##rounding
PMAPr=round(PMAP,3)

# run model for each parameter set in psample
zsample_0 = matrix(0,ntimes,nsample)
wsample_0 = matrix(0,ntimes,nsample)
vsample_0 = matrix(0,ntimes,nsample)
zsample_1 = matrix(0,ntimes,nsample)
wsample_1 = matrix(0,ntimes,nsample)
vsample_1 = matrix(0,ntimes,nsample)
Psample   = matrix(0,ntimes,nsample)
csample   = matrix(0,ntimes,nsample)
### probs
y12_0 = pars$y_0[1:2]
y12_1 = pars$y_1[1:2]
na = pars$na
ysample_0 = matrix(0,na,nsample)
ysample_1 = matrix(0,na,nsample)
hsample_0 = matrix(0,na,nsample)
hsample_1 = matrix(0,na,nsample)
yhsample_0= matrix(0,na,nsample)
yhsample_1= matrix(0,na,nsample)
dsample_0 = matrix(0,na,nsample)
dsample_1 = matrix(0,na,nsample)
###
parsES    = pars #parsE
### beta - function of other parameters
betasample= rep(0,times=nsample)
### scenario b - Averted amounts
dHosp_0   = rep(0,times=nsample)
dHosp_1   = rep(0,times=nsample)
dHosp     = rep(0,times=nsample)
dMort_0   = rep(0,times=nsample)
dMort_1   = rep(0,times=nsample)
dMort     = rep(0,times=nsample)
#zsample_0_b = matrix(0,ntimes,nsample)
#wsample_0_b = matrix(0,ntimes,nsample)
#vsample_0_b = matrix(0,ntimes,nsample)
#zsample_1_b = matrix(0,ntimes,nsample)
#wsample_1_b = matrix(0,ntimes,nsample)
#vsample_1_b = matrix(0,ntimes,nsample)
### UPDATE: @@
###         proposal pars$
###         MAP      parsE$, 
###         sample   parsES$
for(i in 1:nsample){
  parsES$rIR <- f1(as.vector(psample[i,1])) 
  parsES$rIH <- f2(as.vector(psample[i,2]))
  parsES$rOD <- f3(as.vector(psample[i,3])) #parsES$rODa
  parsES$pE0 <- f4(as.vector(psample[i,4]))
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
  #kHES       <- pars$kH  #1
  #kDHES      <- pars$kDH #1
  #kDOES      <- pars$kDO #1
  #Dependent parameters
  parsES$h_0  = hMES_0*exp((age-age9)*hRES_0)
  parsES$h_1  = hMES_1*exp((age-age9)*hRES_1)
  parsES$d_0  = dMES_0*exp((age-age9)*dRES_0) #2*parsES$h_0*pars$m_0
  parsES$d_1  = dMES_1*exp((age-age9)*dRES_1) #2*parsES$h_1*pars$m_1
  parsES$y_0[3:9] = yM1ES_0*exp((age[3:9]-age9)*yR1ES_0)
  parsES$y_1[3:9] = yM1ES_1*exp((age[3:9]-age9)*yR1ES_1)
  parsES$yh_0 = parsES$y_0*parsES$h_0
  parsES$yh_1 = parsES$y_1*parsES$h_1
  parsES$Ea0  = parsES$Na0*parsES$pE0
  parsES$Sa0  = parsES$Na0 - parsES$Ea0 - parsES$Ia0 - parsES$Ua0 - parsES$Ha0 - parsES$Oa0 - parsES$Ra0 - parsES$Da0 
  parsES$beta = BETA(parsES) #BETA_0(parsES)
  betasample[i] = parsES$beta
  outs        = model(as.vector(parsES))
  zsample_0[,i] = outs$byw_0$Hw[imodelH]
  wsample_0[,i] = outs$byw_0$DHw[imodelH]
  vsample_0[,i] = outs$byw_0$DOw[imodelH]
  zsample_1[,i] = outs$byw_1$Hw[imodelH]
  wsample_1[,i] = outs$byw_1$DHw[imodelH]
  vsample_1[,i] = outs$byw_1$DOw[imodelH] 
  Psample[,i]   = (outs$byw_0$It[imodelH] + outs$byw_1$It[imodelH] + outs$byw_0$Ut[imodelH] + outs$byw_1$Ut[imodelH])*oN
  csample[,i]   = (outs$byw_0$Ct[imodelH] + outs$byw_1$Ct[imodelH])*oN
  ### probs
  ysample_0[,i] = c(y12_0, parsES$y_0[3:9])
  ysample_1[,i] = c(y12_1, parsES$y_1[3:9])
  hsample_0[,i] = parsES$h_0
  hsample_1[,i] = parsES$h_1
  yhsample_0[,i]= parsES$yh_0
  yhsample_1[,i]= parsES$yh_1
  dsample_0[,i] = parsES$d_0
  dsample_1[,i] = parsES$d_1
  ### scenario b
  parsES_b      = parsES
  parsES_b$cm_1 = parsES_b$cm_0
  outs_b        = model(as.vector(parsES_b))
  #zsample_0_b[,i] = outs_b$byw_0$Hw[imodelH]
  #wsample_0_b[,i] = outs_b$byw_0$DHw[imodelH]
  #vsample_0_b[,i] = outs_b$byw_0$DOw[imodelH]
  #zsample_1_b[,i] = outs_b$byw_1$Hw[imodelH]
  #wsample_1_b[,i] = outs_b$byw_1$DHw[imodelH]
  #vsample_1_b[,i] = outs_b$byw_1$DOw[imodelH]
  ### scenario difference
  #dHosp_0[i] = sum(zsample_0[,i] - zsample_0_b[,i]) 
  #dHosp_1[i] = sum(zsample_1[,i] - zsample_1_b[,i])
  #dMort_0[i] = sum(wsample_0[,i] - wsample_0_b[,i] + vsample_0[,i] - vsample_0_b[,i])
  #dMort_1[i] = sum(wsample_1[,i] - wsample_1_b[,i] + vsample_1[,i] - vsample_1_b[,i])
  dHosp_0[i] = sum(outs_b$byw_0$Hw[imodelH] - outs$byw_0$Hw[imodelH]) 
  dHosp_1[i] = sum(outs_b$byw_1$Hw[imodelH] - outs$byw_1$Hw[imodelH])
  dMort_0[i] = sum(outs_b$byw_0$DHw[imodelH]- outs$byw_0$DHw[imodelH] + outs_b$byw_0$DOw[imodelH]- outs$byw_0$DOw[imodelH])
  dMort_1[i] = sum(outs_b$byw_1$DHw[imodelH]- outs$byw_1$DHw[imodelH] + outs_b$byw_1$DOw[imodelH]- outs$byw_1$DOw[imodelH])
  dHosp[i]   = dHosp_0[i] + dHosp_1[i]
  dMort[i]   = dMort_0[i] + dMort_1[i]
} 
Weekssample = 1 + outs$byw_0$time[imodelH]/7 + Week_shift_model
Datessample = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1) #"2020-01-06" Mon
##95% CrI
zsample95_0 = matrix(0,ntimes,3)
zsample95_1 = matrix(0,ntimes,3)
wsample95_0 = matrix(0,ntimes,3)
wsample95_1 = matrix(0,ntimes,3)
vsample95_0 = matrix(0,ntimes,3)
vsample95_1 = matrix(0,ntimes,3)
Psample95   = matrix(0,ntimes,3)
csample95   = matrix(0,ntimes,3)
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
dHosp95_0   = rep(0,times=3)
dHosp95_1   = rep(0,times=3)
dHosp95     = rep(0,times=3)
dMort95_0   = rep(0,times=3)
dMort95_1   = rep(0,times=3)
dMort95     = rep(0,times=3)
q1=0.01 #0.025
q2=0.99 #0.975
#beta
betasample95[1] = quantile(betasample,q1,na.rm=T)[[1]]
betasample95[2] = quantile(betasample,0.5,na.rm=T)[[1]]
betasample95[3] = quantile(betasample,q2,na.rm=T)[[1]]
#averted_0, _1 q1
dHosp95_0[1] = quantile(dHosp_0,q1,na.rm=T)[[1]]
dHosp95_1[1] = quantile(dHosp_1,q1,na.rm=T)[[1]]
dMort95_0[1] = quantile(dMort_0,q1,na.rm=T)[[1]]
dMort95_1[1] = quantile(dMort_1,q1,na.rm=T)[[1]]
#averted_0, _1 med
dHosp95_0[2] = quantile(dHosp_0,0.5,na.rm=T)[[1]]
dHosp95_1[2] = quantile(dHosp_1,0.5,na.rm=T)[[1]]
dMort95_0[2] = quantile(dMort_0,0.5,na.rm=T)[[1]]
dMort95_1[2] = quantile(dMort_1,0.5,na.rm=T)[[1]]
#averted_0, _1 q2
dHosp95_0[3] = quantile(dHosp_0,q2,na.rm=T)[[1]]
dHosp95_1[3] = quantile(dHosp_1,q2,na.rm=T)[[1]]
dMort95_0[3] = quantile(dMort_0,q2,na.rm=T)[[1]]
dMort95_1[3] = quantile(dMort_1,q2,na.rm=T)[[1]]
#averted q1, med, q2
dHosp95[1]   = quantile(dHosp,q1,na.rm=T)[[1]]
dMort95[1]   = quantile(dMort,q1,na.rm=T)[[1]]
dHosp95[2]   = quantile(dHosp,0.5,na.rm=T)[[1]]
dMort95[2]   = quantile(dMort,0.5,na.rm=T)[[1]]
dHosp95[3]   = quantile(dHosp,q2,na.rm=T)[[1]]
dMort95[3]   = quantile(dMort,q2,na.rm=T)[[1]]
for(ia in 1:na){
  ### probs
  #y
  samp_ia <- ysample_0[ia,]
  ysample95_0[ia,1] = quantile(samp_ia,q1,na.rm=T)[[1]]
  ysample95_0[ia,2] = quantile(samp_ia,0.5,na.rm=T)[[1]]
  ysample95_0[ia,3] = quantile(samp_ia,q2,na.rm=T)[[1]]
  samp_ia <- ysample_1[ia,]
  ysample95_1[ia,1] = quantile(samp_ia,q1,na.rm=T)[[1]]
  ysample95_1[ia,2] = quantile(samp_ia,0.5,na.rm=T)[[1]]
  ysample95_1[ia,3] = quantile(samp_ia,q2,na.rm=T)[[1]]
  #h
  samp_ia <- hsample_0[ia,]
  hsample95_0[ia,1] = quantile(samp_ia,q1,na.rm=T)[[1]]
  hsample95_0[ia,2] = quantile(samp_ia,0.5,na.rm=T)[[1]]
  hsample95_0[ia,3] = quantile(samp_ia,q2,na.rm=T)[[1]]
  samp_ia <- hsample_1[ia,]
  hsample95_1[ia,1] = quantile(samp_ia,q1,na.rm=T)[[1]]
  hsample95_1[ia,2] = quantile(samp_ia,0.5,na.rm=T)[[1]]
  hsample95_1[ia,3] = quantile(samp_ia,q2,na.rm=T)[[1]]
  #yh
  samp_ia <- yhsample_0[ia,]
  yhsample95_0[ia,1]= quantile(samp_ia,q1,na.rm=T)[[1]]
  yhsample95_0[ia,2]= quantile(samp_ia,0.5,na.rm=T)[[1]]
  yhsample95_0[ia,3]= quantile(samp_ia,q2,na.rm=T)[[1]]
  samp_ia <- yhsample_1[ia,]
  yhsample95_1[ia,1]= quantile(samp_ia,q1,na.rm=T)[[1]]
  yhsample95_1[ia,2]= quantile(samp_ia,0.5,na.rm=T)[[1]]
  yhsample95_1[ia,3]= quantile(samp_ia,q2,na.rm=T)[[1]]
  #d
  samp_ia <- dsample_0[ia,]
  dsample95_0[ia,1] = quantile(samp_ia,q1,na.rm=T)[[1]]
  dsample95_0[ia,2] = quantile(samp_ia,0.5,na.rm=T)[[1]]
  dsample95_0[ia,3] = quantile(samp_ia,q2,na.rm=T)[[1]]   
  samp_ia <- dsample_1[ia,]
  dsample95_1[ia,1] = quantile(samp_ia,q1,na.rm=T)[[1]]
  dsample95_1[ia,2] = quantile(samp_ia,0.5,na.rm=T)[[1]]
  dsample95_1[ia,3] = quantile(samp_ia,q2,na.rm=T)[[1]]   
}
for(it in 1:ntimes){
  #z
  samp_it <- zsample_0[it,]
  zsample95_0[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  zsample95_0[it,2] = quantile(samp_it,0.5,na.rm=T)[[1]]
  zsample95_0[it,3] = quantile(samp_it,q2,na.rm=T)[[1]]
  samp_it <- zsample_1[it,]
  zsample95_1[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  zsample95_1[it,2] = quantile(samp_it,0.5,na.rm=T)[[1]]
  zsample95_1[it,3] = quantile(samp_it,q2,na.rm=T)[[1]]
  #w
  samp_it <- wsample_0[it,]
  wsample95_0[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  wsample95_0[it,2] = quantile(samp_it,0.5,na.rm=T)[[1]]
  wsample95_0[it,3] = quantile(samp_it,q2,na.rm=T)[[1]]
  samp_it <- wsample_1[it,]
  wsample95_1[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  wsample95_1[it,2] = quantile(samp_it,0.5,na.rm=T)[[1]]
  wsample95_1[it,3] = quantile(samp_it,q2,na.rm=T)[[1]]
  #v
  samp_it <- vsample_0[it,]
  vsample95_0[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  vsample95_0[it,2] = quantile(samp_it,0.5,na.rm=T)[[1]]
  vsample95_0[it,3] = quantile(samp_it,q2,na.rm=T)[[1]]
  samp_it <- vsample_1[it,]
  vsample95_1[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  vsample95_1[it,2] = quantile(samp_it,0.5,na.rm=T)[[1]]
  vsample95_1[it,3] = quantile(samp_it,q2,na.rm=T)[[1]]
  #P
  samp_it <- Psample[it,]
  Psample95[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  Psample95[it,2] = quantile(samp_it,0.5,na.rm=T)[[1]]
  Psample95[it,3] = quantile(samp_it,q2,na.rm=T)[[1]]
  #C
  samp_it <- csample[it,]
  csample95[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  csample95[it,2] = quantile(samp_it,0.5,na.rm=T)[[1]]
  csample95[it,3] = quantile(samp_it,q2,na.rm=T)[[1]]
}

## R0_week
### UPDATE: @@
###         proposal pars$
###         MAP      parsE$, 
###         sample   parsES$
nsampleR0 = min(750,nsample) #shorter as calling R0_0()
R0weeksample = matrix(0,ntimes,nsampleR0)
R0sample     = rep(0,times=nsampleR0) #in case R0 is estimated
for(i in 1:nsampleR0){
  parsES$rIR <- f1(as.vector(psample[i,1])) 
  parsES$rIH <- f2(as.vector(psample[i,2]))
  parsES$rOD <- f3(as.vector(psample[i,3])) #parsES$rODa
  parsES$pE0 <- f4(as.vector(psample[i,4]))
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
  #kHES       <- pars$kH  #1
  #kDHES      <- pars$kDH #1
  #kDOES      <- pars$kDO #1
  #Dependent parameters
  parsES$h_0  = hMES_0*exp((age-age9)*hRES_0)
  parsES$h_1  = hMES_1*exp((age-age9)*hRES_1)
  parsES$d_0  = dMES_0*exp((age-age9)*dRES_0) #2*parsES$h_0*pars$m_0
  parsES$d_1  = dMES_1*exp((age-age9)*dRES_1) #2*parsES$h_1*pars$m_1
  parsES$y_0[3:9]  = yM1ES_0*exp((age[3:9]-age9)*yR1ES_0) #pars$y = c(liny[1:2],expy[1:7])
  parsES$y_1[3:9]  = yM1ES_1*exp((age[3:9]-age9)*yR1ES_1)
  parsES$yh_0 = parsES$y_0*parsES$h_0
  parsES$yh_1 = parsES$y_1*parsES$h_1
  parsES$Ea0  = parsES$Na0*parsES$pE0
  parsES$Sa0  = parsES$Na0 - parsES$Ea0 - parsES$Ia0 - parsES$Ua0 - parsES$Ha0 - parsES$Oa0 - parsES$Ra0 - parsES$Da0 
  parsES$beta = BETA(parsES) #BETA_0(parsES)
  #outs        = model(as.vector(parsES))
  r0 = R0_0(parsES, GetBeta=0, GetOutput=1, Sampling=1, nt=ntimes)
  R0weeksample[,i] = r0$WeekDateR0$R0_week
  R0sample[i]      = r0$pars$R0
}
R0weeksample95 = matrix(0,length(imodelH),2)
R0sample95     = rep(0,times=3)
R0sample95[1]  = quantile(R0sample,q1,na.rm=T)[[1]]
R0sample95[2]  = quantile(R0sample,0.5,na.rm=T)[[1]]
R0sample95[3]  = quantile(R0sample,q2,na.rm=T)[[1]]
for(it in 1:ntimes) {
  samp_it <- R0weeksample[it,]
  R0weeksample95[it,1] = quantile(samp_it,q1,na.rm=T)[[1]]
  R0weeksample95[it,2] = quantile(samp_it,q2,na.rm=T)[[1]]
}


} 
## Credible Intervals ##########################################################



##Averted events
if (!is.element(pset$iplatform,1) & length(idataH)==length(idataDH) ){
cat("\n");
cat("Averted events \n")
print(paste0("Averted Hosp_1: ", round(dHosp95_1[2],0), "[",round(dHosp95_1[1],0),",",round(dHosp95_1[3],0),"]"))
print(paste0("Averted Hosp:   ", round(dHosp95[2],0),   "[",round(dHosp95[1],  0),",",round(dHosp95[3],  0),"]"))
print(paste0("Averted Mort_1: ", round(dMort95_1[2],0), "[",round(dMort95_1[1],0),",",round(dMort95_1[3],0),"]"))
print(paste0("Averted Mort:   ", round(dMort95[2],0),   "[",round(dMort95[1],  0),",",round(dMort95[3],  0),"]"))
cat("\n");
}
##Averted events


##### Summary - txt output
sink(file = paste0(output_dir,"/",pset$File_fit_summary_1),append=FALSE,split=FALSE)
print(paste0("Likelihood NB"))
#cat("\n"); 
## Run time
print(out$settings$runtime)
print(paste0("Time used (sec): ", round(tout1[[3]],3)))
cat("\n")
### UPDATE: @@
## MAP and quantile estimates
#print(paste0("sdH Fixed: ", round(parsE$sdH,   3),    ". Expected: ", round(  pars$sdH,     3))) #sdH
print(paste0("kH  Fixed: ", round(parsE$kH,    3),    ". Expected: ", round(  pars$kH,      3))) #kH
print(paste0("kDH Fixed: ", round(parsE$kDH,   3),    ". Expected: ", round(  pars$kDH,     3))) #kD
print(paste0("kDO Fixed: ", round(parsE$kDO,   3),    ". Expected: ", round(  pars$kDO,     3))) #kD
print(paste0("R0    fix: ", round(parsE$R0,    3),    ". Expected: ", round(  pars$R0,      3))) #R0
print(paste0("fu    fix: ", round(parsE$fu,    3),    ". Expected: ", round(  pars$fu,      3))) #pE0 #fu #yA

for(i in 1:npar){
  Expect=BEST0[i] #eval(parse(text = paste0("pars$",eval(parse(text = paste0("Pname[",i,"]"))))))
  #if (i<=3) Expect=1/Expect #rates
  print(paste0(Pnamei[i],", MAP: ", PMAPr[[i]],", Med: ", Pr[i,2],", CI: [",Pr[i,1],",",Pr[i,3],"]",   
        ". Expected: ", round(Expect,3))) }
print(paste0("beta dep: ",", MAP: ", round(parsE$beta,3),", Med: ", round(betasample95[2],3),", CI: [",round(betasample95[1],3),",",round(betasample95[3],3),"]",
        ". Expected: ", round(pars$beta,3)))
print(paste0("R0 fixed: ",", MAP: ", round(parsE$R0,3),  ", Med: ", round(R0sample95[2],3),  ", CI: [",round(R0sample95[1],3),  ",",round(R0sample95[3],3),"]",
        ". Expected: ", round(pars$R0,3)))
print(paste0("E0   dep: ", round(sum(parsE$Ea0), 0), ". Expected: ", round(sum(pars$Na0*pars$pE0)) ))
print(paste0("Estimated proportion deaths outside hospital = ", round(parsE$ad/(1+parsE$ad),3)))
print(paste0("Ha0_sum - input: ", sum(pars$Ha0)))
print(paste0("Da0_sum - input: ", sum(pars$Da0)))


#for(i in 1:npar){
#  print(paste0(i,"BEST=",BEST[i]))
#  print(paste0("   par=",eval(parse(text = paste0("pars$",eval(parse(text = paste0("Pname[",i,"]"))))))) ) }

  
#print(paste0("1/rIR MAP: ", round(1/parsE$rIR, 3),    ". Expected: ", round(1/pars$rIR,     3))) #rEI
#print(paste0("1/rOD MAP: ", round(1/parsE$rOD, 3),    ". Expected: ", round(1/pars$rOD,     3))) #rOD
#print(paste0("1/rIH MAP: ", round(1/parsE$rIH, 3),    ". Expected: ", round(1/pars$rIH,     3))) #rIH
#print(paste0("R0    fix: ", round(parsE$R0,    3),    ". Expected: ", round(  pars$R0,      3))) #R0
#print(paste0("pE0   MAP: ", round(parsE$pE0,   4),    ". Expected: ", round(  pars$pE0,     3))) #pE0
#print(paste0("fu    fix: ", round(parsE$fu,    3),    ". Expected: ", round(  pars$fu,      3))) #pE0 #fu #yA
#print(paste0("hM_0  MAP: ", round(parsE$h_0[9], 4),    ". Expected: ", BEST[5])) #round(  pars$h_0[9],    3))) #h9
#print(paste0("hM_1  MAP: ", round(parsE$h_1[9], 4),    ". Expected: ", BEST[6])) #round(  pars$h_1[9],    3))) #h9
#print(paste0("hR_0  MAP: ", round(log(parsE$h_0[8]/parsE$h_0[9])/(age[8]-age9),    4),    ". Expected: ",BEST[7]))
#print(paste0("hR_1  MAP: ", round(log(parsE$h_1[8]/parsE$h_1[9])/(age[8]-age9),    4),    ". Expected: ",BEST[8]))
#print(paste0("dM_0  MAP: ", round(parsE$d_0[9], 4),    ". Expected: ", BEST[9])) #round(  pars$h_0[9],    3))) #h9
#print(paste0("dM_1  MAP: ", round(parsE$d_1[9], 4),    ". Expected: ", BEST[10])) #round(  pars$h_1[9],    3))) #h9
#print(paste0("dR_0  MAP: ", round(log(parsE$d_0[8]/parsE$d_0[9])/(age[8]-age9),    4),    ". Expected: ",BEST[11]))
#print(paste0("dR_1  MAP: ", round(log(parsE$d_1[8]/parsE$d_1[9])/(age[8]-age9),    4),    ". Expected: ",BEST[12]))
##print(paste0("ad_0  MAP: ", round(parsE$ad_0,  3),    ". Expected: ", round(  pars$ad_0,    3))) #m9
##print(paste0("ad_1  MAP: ", round(parsE$ad_1,  3),    ". Expected: ", round(  pars$ad_1,    3))) #m9
##print(paste0("mM_0  MAP: ", round(parsE$m_0[9],  4),    ". Expected: ", round(  pars$m_0[9],    3))) #d9
##print(paste0("mM_1  MAP: ", round(parsE$m_1[9],  4),    ". Expected: ", round(  pars$m_1[9],    3))) #d9
##print(paste0("mR_0  MAP: ", round(log(parsE$m_0[8]/parsE$m_0[9])/(age[8]-age9),    4),    ". Expected: "))
##print(paste0("mR_1  MAP: ", round(log(parsE$m_1[8]/parsE$m_1[9])/(age[8]-age9),    4),    ". Expected: "))
##print(paste0("yR0_0 MAP: ", round((parsE$y_0[1]  - parsE$y_0[2])/(age[2]-age1),    4), ". Expected: ", pars$y_0[1]))
##print(paste0("yR0_1 MAP: ", round((parsE$y_1[1]  - parsE$y_1[2])/(age[2]-age1),    4), ". Expected: ", pars$y_1[1]))
#print(paste0("yM1_0 MAP: ", round( parsE$y_0[9],  4),   ". Expected: ", BEST[13])) #round(  pars$y_0[9],    3))) #y9
#print(paste0("yM1_1 MAP: ", round( parsE$y_1[9],  4),   ". Expected: ", BEST[14])) #round(  pars$y_1[9],    3))) #y9
#print(paste0("yR1_0 MAP: ", round((log(parsE$y_0[4]/parsE$y_0[3]))/(age[4]-age3),  4),  ". Expected: ", BEST[15]))
#print(paste0("yR1_1 MAP: ", round((log(parsE$y_1[4]/parsE$y_1[3]))/(age[4]-age3),  4),  ". Expected: ", BEST[16]))
##print(paste0("yM0_0 dep: ", round( parsE$y_0[3],  4),    ". Expected: ", round( pars$y_0[3],    3))) #y3
##print(paste0("yM0_1 dep: ", round( parsE$y_1[3],  4),    ". Expected: ", round( pars$y_1[3],    3))) #y3
#print(paste0("beta  dep: ", round(parsE$beta,     5) ))
#print(paste0("E0    dep: ", round(sum(parsE$Ea0), 0), ". Expected: ", round(sum(pars$Na0*pars$pE0)) ))
#print(paste0("Estimated proportion deaths outside hospital = ", round(parsE$ad/(1+parsE$ad),3)))

if (!is.element(pset$iplatform,1) & length(idataH)==length(idataDH) ){
cat("\n");
print(paste0("Averted Hosp_1: ", round(dHosp95_1[2],0), "[",round(dHosp95_1[1],0),",",round(dHosp95_1[3],0),"]"))
print(paste0("Averted Hosp:   ", round(dHosp95[2],0),   "[",round(dHosp95[1],  0),",",round(dHosp95[3],  0),"]"))
print(paste0("Averted Mort_1: ", round(dMort95_1[2],0), "[",round(dMort95_1[1],0),",",round(dMort95_1[3],0),"]"))
print(paste0("Averted Mort:   ", round(dMort95[2],0),   "[",round(dMort95[1],  0),",",round(dMort95[3],  0),"]"))
}
cat("\n");
print(summary(out)); 
cat("\n")
print(paste0("Mean by chain and parameter:"))
print(out$X)
sink()

#0 Probability-age plots 
#filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_plots_probs")
#svglite(paste0(filenamepath,".svg")); 
#par(mfrow = c(5,2))
#par(mar = c(4, 4, 1, 4))  #bottom, left, top, right
##h
#plot(pars$h, ylim=c(0,1),ylab="h_0",xlab=""); lines(parsE$h_0, col=2, lwd=2) #plot(pars$h_0,
#plot(pars$h, ylim=c(0,1),ylab="h_1",xlab=""); lines(parsE$h_1, col=2, lwd=2) #plot(pars$h_1
##y  
#plot(pars$y, ylim=c(0,1),ylab="y_0",xlab=""); lines(parsE$y_0, col=2, lwd=2) #plot(pars$y_0
#plot(pars$y, ylim=c(0,1),ylab="y_1",xlab=""); lines(parsE$y_1, col=2, lwd=2) #plot(pars$y_1
##yh  
#plot(pars$yh, ylim=c(0,1),ylab="yh_0",xlab=""); lines(parsE$yh_0, col=2, lwd=2) #plot(pars$y_0
#plot(pars$yh, ylim=c(0,1),ylab="yh_1",xlab=""); lines(parsE$yh_1, col=2, lwd=2) #plot(pars$y_1
##d
#plot(0.2*pars$h*pars$m_0, ylim=c(0,1),ylab="d_0",xlab=""); lines(parsE$d_0*parsE$ad, col=2, lwd=2)  #plot(pars$d_0*pars$ad
#plot(0.2*pars$h*pars$m_1, ylim=c(0,1),ylab="d_1",xlab=""); lines(parsE$d_1*parsE$ad, col=2, lwd=2)  #plot(pars$d_1*pars$ad
##m
#plot(pars$m_0, ylim=c(0,1),ylab="m_0",xlab="age group"); lines(parsE$m_0, col=2, lwd=2)
#plot(pars$m_1, ylim=c(0,1),ylab="m_1",xlab="age group"); lines(parsE$m_1, col=2, lwd=2)
#invisible(dev.off())

#0 Probability-age plots - log-lin
#filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_plots_probs_log")
#svglite(paste0(filenamepath,".svg")); 
#par(mfrow = c(4,2))
#par(mar = c(4, 4, 1, 4))  #bottom, left, top, right
##h
#eps=0.0001
#plot(log10(eps+pars$h_0), ylim=log(c(eps,1)),ylab="log10 h_0",xlab=""); lines(log10(eps+parsE$h_0), col=2, lwd=2)
#plot(log10(eps+pars$h_1), ylim=log(c(eps,1)),ylab="log10 h_1",xlab=""); lines(log10(eps+parsE$h_1), col=2, lwd=2)
##y  
#plot(log10(eps+pars$y_0), ylim=log(c(eps,1)),ylab="log10 y_0",xlab=""); lines(log10(eps+parsE$y_0), col=2, lwd=2)
#plot(log10(eps+pars$y_1), ylim=log(c(eps,1)),ylab="log10 y_1",xlab=""); lines(log10(eps+parsE$y_1), col=2, lwd=2)
##d
#plot(log10(eps+pars$d_0*pars$ad), ylim=log(c(eps,1)),ylab="log10 d_0",xlab=""); lines(log10(eps+parsE$d_0*parsE$ad), col=2, lwd=2)  #parsE$ad=1 if parsE$d estimated
#plot(log10(eps+pars$d_1*pars$ad), ylim=log(c(eps,1)),ylab="log10 d_1",xlab=""); lines(log10(eps+parsE$d_1*parsE$ad), col=2, lwd=2)  #parsE$ad=1 if parsE$d estimated
##m
#plot(log10(eps+pars$m_0), ylim=log(c(eps,1)),ylab="log10 m_0",xlab="age group"); lines(log10(eps+parsE$m_0), col=2, lwd=2)
#plot(log10(eps+pars$m_1), ylim=log(c(eps,1)),ylab="log10 m_1",xlab="age group");  lines(log10(eps+parsE$m_1), col=2, lwd=2)
#invisible(dev.off())
##screen
par(mfrow = c(4,2))
par(mar = c(4, 4, 1, 4))  #bottom, left, top, right
#h
plot(pars$h, ylim=c(0,1),ylab="h_0",xlab=""); lines(parsE$h_0, col=2, lwd=2) #plot(pars$h_0,
plot(pars$h, ylim=c(0,1),ylab="h_1",xlab=""); lines(parsE$h_1, col=2, lwd=2) #plot(pars$h_1

#y  
plot(pars$y, ylim=c(0,1),ylab="y_0",xlab=""); lines(parsE$y_0, col=2, lwd=2) #plot(pars$y_0
plot(pars$y, ylim=c(0,1),ylab="y_1",xlab=""); lines(parsE$y_1, col=2, lwd=2) #plot(pars$y_1

#d
plot(0.2*pars$h*pars$m_0, ylim=c(0,1),ylab="d_0",xlab=""); lines(parsE$d_0*parsE$ad, col=2, lwd=2)  #plot(pars$d_0*pars$ad
plot(0.2*pars$h*pars$m_1, ylim=c(0,1),ylab="d_1",xlab=""); lines(parsE$d_1*parsE$ad, col=2, lwd=2)  #plot(pars$d_1*pars$ad
#m
plot(pars$m_0, ylim=c(0,1),ylab="m_0",xlab="age group"); lines(parsE$m_0, col=2, lwd=2)
plot(pars$m_1, ylim=c(0,1),ylab="m_1",xlab="age group"); lines(parsE$m_1, col=2, lwd=2)



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
sink()



##### Plots - Overall dataframes ###############################################
print("Data frames..."); cat("\n")
N  = pars$Npop
Nc = pars$Npopcoh


if(pset$iplatform<2) {weight0=1} else {weight0=N/Nc}
if(pset$iplatform==1){Weekssample = 1 + mE$byw_0$time[imodelH]/7 + Week_shift_model}

datp <- tibble(Weeks  = 1 + mE$byw_0$time[imodelH]/7 + Week_shift_model,
               Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1), #"2020-01-06" Mon
               #Dates  = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'),
               Prev   = (mE$byw_0$It[imodelH] + mE$byw_0$Ut[imodelH] + mE$byw_1$It[imodelH] + mE$byw_1$Ut[imodelH])*oN,
               Posi   = (mE$byw_0$Ct[imodelH] + mE$byw_1$Ct[imodelH])*oN )

datH <- tibble(Weeks  = 1 + mE$byw_0$time[imodelH]/7 + Week_shift_model,    #model time 0 <> week1 + (Week1_Model-1) = 1 + (4-1) = week4
               Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1), #"2020-01-06" Mon
               #Dates  = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'), #Checked: "Mon" consistent with weeks/dates def throughout 
               H_est_0= mE$byw_0$Hw[imodelH],
               H_est_1= mE$byw_1$Hw[imodelH],
               H_est  = H_est_0 + H_est_1,
               zdw    = zd*weight0,
               zdw_0  = zd_0*weight0,
               zdw_1  = zd_1*weight0,
               R0_week = R0_weekE[imodelH])
datD <- tibble(Weeks  = 1 + mE$byw_0$time[imodelDH]/7 + Week_shift_model,
               Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1), #"2020-01-06" Mon
               #Dates  = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'), #Checked: "Mon" consistent with weeks/dates def throughout 
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

if (pset$iplatform>0) {
datH <- tibble(datH,
               Weeksz = datH_l$Week[idataH], #$Weeks[idataH],
               Datesz = Dates)            #datH_l$Dates[idataH]) 
               #Justification:datX_l$Dates (X=DH, DO) has NA (both dummy & OS) 
               #while         datX_l$Weeks and datX_l$Freq (zd, wd, vd) have no NA (by _l construction)
datD <- tibble(datD,
               Weeksw = datDH_l$Week[idataDH],  #$Weeks[idataDH],
               Weeksv = datDO_l$Week[idataDO],  #$Weeks[idataDO],
               Datesw = Dates,            #datDH_l$Dates[idataDH]
               Datesv = Dates)            #datDO_l$Dates[idataDO]
} else { #iplatform=0 
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
YEXP <- function(y,LOG=0){ if (LOG==1) {z=10^(y)-1} else {z=y}; return(z) }
LOG=1; #0 #apply scale of Age Profile plotting

if (!is.element(pset$iplatform,1) & length(idataH)==length(idataDH) ){
#Indices of data vectors (pset$iplatform==2)
#=> defined in "## Indices" at the top
  
#Variables (Model and data)  
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
    if (pset$iplatform>0){ #OS data
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
    } else { #iplatform=0  #simulated (actually, true model, as simulated data too noisy)
  #for (i in 1:9){ assign(paste0("zd",eval(i),"_0"), datM[paste0("Dataz",i,"_0")][[1]] ) }#zd1-zd9 - Hospitalisations
    valuesH_0  = eval(parse(text = paste0( "datM$H_mod",eval(i),"_0","[imodelH]"))) 
    valuesDH_0 = eval(parse(text = paste0("datM$DH_mod",eval(i),"_0","[imodelDH]"))) 
    valuesDO_0 = eval(parse(text = paste0("datM$DO_mod",eval(i),"_0","[imodelDO]"))) 
    valuesH_1  = eval(parse(text = paste0( "datM$H_mod",eval(i),"_1","[imodelH]"))) 
    valuesDH_1 = eval(parse(text = paste0("datM$DH_mod",eval(i),"_1","[imodelDH]"))) 
    valuesDO_1 = eval(parse(text = paste0("datM$DO_mod",eval(i),"_1","[imodelDO]"))) }
    assign(paste0("H", eval(i),"d_0"), YLOG(valuesH_0, LOG))                        #H1d_0-H9d_0
    assign(paste0("DH",eval(i),"d_0"), YLOG(valuesDH_0,LOG))                        #DH1d_0-DH9d_0
    assign(paste0("DO",eval(i),"d_0"), YLOG(valuesDO_0,LOG))                        #DO1d_0-DO9d_0
    assign(paste0("H", eval(i),"d_1"), YLOG(valuesH_1, LOG))                        #H1d_1-H9d_1
    assign(paste0("DH",eval(i),"d_1"), YLOG(valuesDH_1,LOG))                        #DH1d_1-DH9d_1
    assign(paste0("DO",eval(i),"d_1"), YLOG(valuesDO_1,LOG))                        #DO1d_1-DO9d_1    
}
#Time
#Considering that imodelH = imodelDH = imodelDO
datT    <- tibble(Weeks = 1 + mE$byw_0$time[imodelH]/7 + Week_shift_model,
                 Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1)) #"2020-01-06" Mon
                 #Dates = as.Date(paste(Weeks, "2020", 'Mon'), '%U %Y %a'), #Checked: "Mon" consistent with weeks/dates def throughout 
#H
datHa_0 <- tibble(datT,
                  H1w=H1w_0, H2w=H2w_0, H3w=H3w_0, H4w=H4w_0, H5w=H5w_0, #estimated
                  H6w=H6w_0, H7w=H7w_0, H8w=H8w_0, H9w=H9w_0,
                  H1d=H1d_0, H2d=H2d_0, H3d=H3d_0, H4d=H4d_0, H5d=H5d_0, #data
                  H6d=H6d_0, H7d=H7d_0, H8d=H8d_0, H9d=H9d_0)
datHa_1 <- tibble(datT,
                  H1w=H1w_1, H2w=H2w_1, H3w=H3w_1, H4w=H4w_1, H5w=H5w_1, #estimated
                  H6w=H6w_1, H7w=H7w_1, H8w=H8w_1, H9w=H9w_1,
                  H1d=H1d_1, H2d=H2d_1, H3d=H3d_1, H4d=H4d_1, H5d=H5d_1, #data
                  H6d=H6d_1, H7d=H7d_1, H8d=H8d_1, H9d=H9d_1)
#DH
datDHa_0<- tibble(datT,
                  DH1w=DH1w_0, DH2w=DH2w_0, DH3w=DH3w_0, DH4w=DH4w_0, DH5w=DH5w_0,    #estimated
                  DH6w=DH6w_0, DH7w=DH7w_0, DH8w=DH8w_0, DH9w=DH9w_0,
                  DH1d=DH1d_0, DH2d=DH2d_0, DH3d=DH3d_0, DH4d=DH4d_0, DH5d=DH5d_0,    #data
                  DH6d=DH6d_0, DH7d=DH7d_0, DH8d=DH8d_0, DH9d=DH9d_0)
datDHa_1<- tibble(datT,
                  DH1w=DH1w_1, DH2w=DH2w_1, DH3w=DH3w_1, DH4w=DH4w_1, DH5w=DH5w_1,    #estimated
                  DH6w=DH6w_1, DH7w=DH7w_1, DH8w=DH8w_1, DH9w=DH9w_1,
                  DH1d=DH1d_1, DH2d=DH2d_1, DH3d=DH3d_1, DH4d=DH4d_1, DH5d=DH5d_1,    #data
                  DH6d=DH6d_1, DH7d=DH7d_1, DH8d=DH8d_1, DH9d=DH9d_1)
#DO
datDOa_0<- tibble(datT,
                 DO1w=DO1w_0, DO2w=DO2w_0, DO3w=DO3w_0, DO4w=DO4w_0, DO5w=DO5w_0,    #estimated
                 DO6w=DO6w_0, DO7w=DO7w_0, DO8w=DO8w_0, DO9w=DO9w_0,
                 DO1d=DO1d_0, DO2d=DO2d_0, DO3d=DO3d_0, DO4d=DO4d_0, DO5d=DO5d_0,    #data
                 DO6d=DO6d_0, DO7d=DO7d_0, DO8d=DO8d_0, DO9d=DO9d_0)
datDOa_1<- tibble(datT,
                 DO1w=DO1w_1, DO2w=DO2w_1, DO3w=DO3w_1, DO4w=DO4w_1, DO5w=DO5w_1,    #estimated
                 DO6w=DO6w_1, DO7w=DO7w_1, DO8w=DO8w_1, DO9w=DO9w_1,
                 DO1d=DO1d_1, DO2d=DO2d_1, DO3d=DO3d_1, DO4d=DO4d_1, DO5d=DO5d_1,    #data
                 DO6d=DO6d_1, DO7d=DO7d_1, DO8d=DO8d_1, DO9d=DO9d_1)

} #Plots - Age profile dataframes (not merged)
                 
                    


#### pdf Plots #################################################################
#pdf(file = paste0(output_dir,"/",pset$File_fit_output))

#### svg plots #################################################################

#### Diagnostics

#1 marginal
par(mar=c(0.5, 1, 1, 1)) #shows axis scale better #par(mar =c(0,0,0,0)) # c(0.5, 1, 1, 1)) #Par(mar = c(2, 2, 1, 1))  #bottom, left, top, right
#p<-marginalPlot(out); print(p)
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_marginalPlot")
#if (pset$iplatform<2){ 
#  pdf(paste0(filenamepath,".pdf")); marginalPlot(out); invisible(dev.off())     }
if(length(UPPER)<17){
  svglite(paste0(filenamepath,".svg")); marginalPlot(out); invisible(dev.off()) } else {
  svglite(paste0(filenamepath,".svg")); plot(1:10); invisible(dev.off())        }

#2-3 trace #NB replaced by out2
#par(mar = c(2, 2, 1, 1)) ##bottom, left, top, right
#p<-plot(out); print(p)
#filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_plotout_lastPage")
#if (pset$iplatform==0){
#  svglite(paste0(filenamepath,".svg")); plot(out); invisible(dev.off())         }

#4 correlations
par(mar = c(0,0,0,0)) #par(mar = c(2, 2, 1, 1))
#pdf("corr.pdf")
#p<-correlationPlot(out); print(p)  #Error in plot.new() : figure margins too large  (16 par)
#dev.off()
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_correlationPlot")
if(length(UPPER)<12){
  svglite(paste0(filenamepath,".svg")); correlationPlot(out); invisible(dev.off()) } else {
  svglite(paste0(filenamepath,".svg")); plot(1:10); invisible(dev.off())        }


### Results
Title_0 = c("Other")
Title_1 = c("Shielding")

#5 overall
colors <- c(  "I_dat"  = "black",   "I_est"  = "red",     "I_model" = "green",
             "H_datw"  = "red",     "H_est"  = "red",     "H_model" = "pink",
            "DH_datw"  = "blue",    "DH_est" = "blue",   "DH_model" = "cyan",
            "DO_datw"  = "green4",  "DO_est" = "green4", "DO_model" = "green", "R0_est" = "grey40")

coeff <- (max(datH$zdw)/max(datH$R0_week))

pO <- ggplot() +
          #xlim(c(0, NA)) +  ylim(c(0, NA)) + #Dont use with Dates, only with Weeks
          scale_color_manual(values = colors) +
          geom_point(data=datH, aes(x=Datesz,y=zdw,    color =  "H_datw"), size = 1.4,   pch = 19) +
          geom_point(data=datD, aes(x=Datesw,y=wdw,    color = "DH_datw"), size = 1.4,   pch = 16) +
          geom_point(data=datD, aes(x=Datesv,y=vdw,    color = "DO_datw"), size = 1.4,   pch = 16) + #1) +
          geom_line (data=datH, aes(x=Dates, y=H_est,  color =  "H_est")) +
          geom_line (data=datD, aes(x=Dates, y=DH_est, color = "DH_est")) +
          geom_line (data=datD, aes(x=Dates, y=DO_est, color = "DO_est")) +
          #geom_point(data=datH, aes(x=Dates, y=H_est,  color =  "H_est")) +
          #geom_point(data=datD, aes(x=Dates, y=DH_est, color = "DH_est")) +
          #geom_point(data=datD, aes(x=Dates, y=DO_est, color = "DO_est")) +
          geom_line (data=datH, aes(x=Dates, y=R0_week*coeff,color = "R0_est"))

if (pset$iplatform==0) {
pO <- pO + 
          geom_line (data=datH, aes(x=Dates,y=H_mod,  color =  "H_model")) +
          geom_line (data=datD, aes(x=Dates,y=DH_mod, color = "DH_model")) +
          geom_line (data=datD, aes(x=Dates,y=DO_mod, color = "DO_model"))  }
#secondary axis for R0
pO <- pO + 
          scale_y_continuous(
            name = 'Hospitalisations & deaths in & outside hospital (whole population)',
            sec.axis = sec_axis(~.*(1/coeff), name="R0") ) + 
          labs(x = 'Date', color = "Variable") + #Legend") + #y = 'Hospitalisations & deaths in & outside hospital', color = "Legend") + 
          theme(axis.title.y       = element_text(color = 1),
                axis.title.y.right = element_text(color = 1) ,
                axis.text          = element_text(size = 16),
                axis.title         = element_text(size = 18, face = "bold"),
                legend.title       = element_text(size = 18),
                legend.text        = element_text(size = 14))
print(pO)

filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_Overall")
sc = 3#10
svglite(paste0(filenamepath,".svg"),width=sc*6, height=sc*3); print(pO); invisible(dev.off())



#6 Plot posterior samples
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
##
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_PosteriorSample")
##
par(mfrow = c(4,1))
par(mar = c(2, 2, 1, 1)) #bottom, left, top, right
colors <- c("Data" = 1,  "MAP" = 2, "95% CrI" = "grey70", "95% perc" = "grey70", "MAP prev" = 2, "MAP posi" = 2)
zMAX = rep(range(c(zsample_0,zsample_1))[2],length(Datessample))

#H
dzsample_0<- tibble(Date=Datessample, Datez=datH$Datesz, zsample05=zsample95_0[,1],
                    zsample95=zsample95_0[,3], zMAP=mE$byw_0$Hw[imodelH], zdw=datH$zdw_0)
dzsample_1<- tibble(Date=Datessample, Datez=datH$Datesz, zsample05=zsample95_1[,1], 
                    zsample95=zsample95_1[,3], zMAP=mE$byw_1$Hw[imodelH], zdw=datH$zdw_1)

#breaks=seq(min(as.POSIXct(Datessample)), max(as.POSIXct(Datessample)), length=6)
breaks=seq(min((Datessample)), max((Datessample)), length=6)
date_labels = "%d/%m/%y" # "%D") # "%M-%Y")

pz_0<-ggplot(dzsample_0, aes(x=Date)) + 
      geom_ribbon(aes(ymin = zsample05, ymax = zsample95), fill = "grey70") +
      geom_point(aes(x=Datez, y = zdw,       color="Data")) +
      geom_line (aes(x=Date,  y = zMAP,      color="MAP")) +
      geom_line (aes(x=Date,  y = zsample05, color="95% CrI")) +
      labs(x = "", y = 'Hospitalisations',   color = "") + #Legend") + 
      scale_color_manual(values = colors) +
      theme(axis.title = element_text(size = 12, face = "bold")) +  ggtitle(Title_0) +
      scale_x_date(breaks= breaks, date_labels= date_labels) 
           #axis.title.y       = element_text(color = 1),
           #axis.title.y.right = element_text(color = 1) ,
           #axis.text          = element_text(size = 16),
           #legend.title       = element_text(size = 16),
           #legend.text        = element_text(size = 12))
pz_1<-ggplot(dzsample_1, aes(x=Date)) + 
      geom_ribbon(aes(ymin = zsample05, ymax = zsample95), fill = "grey70") +
      geom_point(aes(x=Datez, y = zdw,       color="Data")) +
      geom_line (aes(x=Date,  y = zMAP,      color="MAP")) +
      geom_line (aes(x=Date,  y = zsample05, color="95% CrI")) +
      labs(x = "", y = 'Hospitalisations',   color = "") + #Legend") + 
      scale_color_manual(values = colors) +
      theme(axis.title = element_text(size = 12, face = "bold")) +  ggtitle(Title_1) +
      scale_x_date(breaks= breaks, date_labels= date_labels)
#DH
dwsample_0<- tibble(Date=Datessample, Datew=datD$Datesw, wsample05=wsample95_0[,1], 
                    wsample95=wsample95_0[,3], wMAP=mE$byw_0$DHw[imodelDH], wdw=datD$wdw_0)
dwsample_1<- tibble(Date=Datessample, Datew=datD$Datesw, wsample05=wsample95_1[,1], 
                    wsample95=wsample95_1[,3], wMAP=mE$byw_1$DHw[imodelDH], wdw=datD$wdw_1)

pw_0 <-ggplot(dwsample_0, aes(x=Date)) + 
       geom_ribbon(aes(ymin = wsample05, ymax = wsample95), fill = "grey70") +
       geom_point(aes(x=Datew, y = wdw,       color="Data")) +
       geom_line (aes(x=Date,  y = wMAP,      color="MAP")) +
       geom_line (aes(x=Date,  y = wsample05, color="95% CrI")) +
       labs(x = "", y = 'Deaths in hospital', color = "") + #Legend") + 
       scale_color_manual(values = colors) +
       theme(axis.title = element_text(size = 12, face = "bold")) +
       scale_x_date(breaks= breaks, date_labels= date_labels)

pw_1 <-ggplot(dwsample_1, aes(x=Date)) + 
       geom_ribbon(aes(ymin = wsample05, ymax = wsample95), fill = "grey70") +
       geom_point(aes(x=Datew, y = wdw,       color="Data")) +
       geom_line (aes(x=Date,  y = wMAP,      color="MAP")) +
       geom_line (aes(x=Date,  y = wsample05, color="95% CrI")) +
       labs(x = "", y = 'Deaths in hospital', color = "") + #Legend") + 
       scale_color_manual(values = colors) +
       theme(axis.title = element_text(size = 12, face = "bold")) +
       scale_x_date(breaks= breaks, date_labels= date_labels)
#DO
dvsample_0<- tibble(Date=Datessample, Datev=datD$Datesv, vsample05=vsample95_0[,1], 
                    vsample95=vsample95_0[,3], vMAP=mE$byw_0$DOw[imodelDO], vdw=datD$vdw_0)
dvsample_1<- tibble(Date=Datessample, Datev=datD$Datesv, vsample05=vsample95_1[,1], 
                    vsample95=vsample95_1[,3], vMAP=mE$byw_1$DOw[imodelDO], vdw=datD$vdw_1)

pv_0 <-ggplot(dvsample_0, aes(x=Date)) + 
       geom_ribbon(aes(ymin = vsample05, ymax = vsample95), fill = "grey70") +
       geom_point(aes(x=Datev, y = vdw,       color="Data")) +
       geom_line (aes(x=Date,  y = vMAP,      color="MAP")) +
       geom_line (aes(x=Date,  y = vsample05, color="95% CrI")) +
       labs(x = '', y = 'Deaths outside hospital', color = "") + 
       scale_color_manual(values = colors) +
       theme(axis.title = element_text(size = 12, face = "bold")) +
       scale_x_date(breaks= breaks, date_labels= date_labels)

pv_1 <-ggplot(dvsample_1, aes(x=Date)) + 
       geom_ribbon(aes(ymin = vsample05, ymax = vsample95), fill = "grey70") +
       geom_point(aes(x=Datev, y = vdw,       color="Data")) +
       geom_line (aes(x=Date,  y = vMAP,      color="MAP")) +
       geom_line (aes(x=Date,  y = vsample05, color="95% CrI")) +
       labs(x = '', y = 'Deaths outside hospital', color = "") + 
       scale_color_manual(values = colors) +
       theme(axis.title = element_text(size = 12, face = "bold")) +
       scale_x_date(breaks= breaks, date_labels= date_labels)

gridExtra::grid.arrange(pz_0, pz_1, pw_0, pw_1, pv_0, pv_1, nrow = 3, ncol = 2)

svglite(paste0(filenamepath,".svg")); 
gridExtra::grid.arrange(pz_0, pz_1, pw_0, pw_1, pv_0, pv_1, nrow = 3, ncol = 2)
invisible(dev.off())



#6b Plot posterior samples - DATA ROUNDED TO MID 6
#roundmid_any <- function(x, to=6){
#  # like round_any, but centers on (integer) midpoint of the rounding points
#  ceiling(x/to)*to - (floor(to/2)*(x!=0)) }
rm6 <- function(x){ ceiling(x/6)*6 - (floor(6/2)*(x!=0))   }
r5  <- function(x){ return( round(x/5)*5)   }
#redact <-function(x){ x[which(x<8)]="REDACTED"; return(x)}
##assuming we revert "REDACTED" to 4 (sort of intermediate between 0 and 8)
#redact2<-function(x){ x[which(x<8)]=4; return(x)}

  ##
  filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_PosteriorSample_rounded")
  ##
  par(mfrow = c(4,1))
  par(mar = c(2, 2, 1, 1)) #bottom, left, top, right
  colors <- c("Data" = 1,  "MAP" = 2, "95% CrI" = "grey70", "95% perc" = "grey70", "MAP prev" = 2, "MAP posi" = 2)
  zMAX = rep(range(c(zsample_0,zsample_1))[2],length(Datessample))
  
  #H
  #zdw_0_r5 = redact2(datH$zdw_0)  #redact
  #zdw_0_r5 = r5(zdw_0_r5)         #round
  #zdw_1_r5 = redact2(datH$zdw_1)  #redact
  #zdw_1_r5 = r5(zdw_1_r5)         #round
  zdw_0_r6 = rm6(datH$zdw_0)       #round mid-6
  zdw_1_r6 = rm6(datH$zdw_1)       #round mid-6
  dzsample_0<- tibble(Date=Datessample, Datez=datH$Datesz, zsample05=zsample95_0[,1],
                      zsample95=zsample95_0[,3], zMAP=mE$byw_0$Hw[imodelH], zdw=zdw_0_r6)
  dzsample_1<- tibble(Date=Datessample, Datez=datH$Datesz, zsample05=zsample95_1[,1], 
                      zsample95=zsample95_1[,3], zMAP=mE$byw_1$Hw[imodelH], zdw=zdw_1_r6)
  
  pz_0<-ggplot(dzsample_0, aes(x=Date)) + 
    geom_ribbon(aes(ymin = zsample05, ymax = zsample95), fill = "grey70") +
    geom_point(aes(x=Datez, y = zdw,       color="Data")) +
    geom_line (aes(x=Date,  y = zMAP,      color="MAP")) +
    geom_line (aes(x=Date,  y = zsample05, color="95% CrI")) +
    labs(x = "", y = 'Hospitalisations',   color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  ggtitle(Title_0) +
    scale_x_date(breaks= breaks, date_labels= date_labels) 
  #axis.title.y       = element_text(color = 1),
  #axis.title.y.right = element_text(color = 1) ,
  #axis.text          = element_text(size = 16),
  #legend.title       = element_text(size = 16),
  #legend.text        = element_text(size = 12))
  pz_1<-ggplot(dzsample_1, aes(x=Date)) + 
    geom_ribbon(aes(ymin = zsample05, ymax = zsample95), fill = "grey70") +
    geom_point(aes(x=Datez, y = zdw,       color="Data")) +
    geom_line (aes(x=Date,  y = zMAP,      color="MAP")) +
    geom_line (aes(x=Date,  y = zsample05, color="95% CrI")) +
    labs(x = "", y = 'Hospitalisations',   color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  ggtitle(Title_1) +
    scale_x_date(breaks= breaks, date_labels= date_labels) 
  #DH
  #wdw_0_r5 = redact2(datD$wdw_0)  #redact
  #wdw_0_r5 = r5(wdw_0_r5)         #round
  #wdw_1_r5 = redact2(datD$wdw_1)  #redact
  #wdw_1_r5 = r5(wdw_1_r5)         #round
  wdw_0_r6 = rm6(datD$wdw_0)       #round mid-6
  wdw_1_r6 = rm6(datD$wdw_1)       #round mid-6
  dwsample_0<- tibble(Date=Datessample, Datew=datD$Datesw, wsample05=wsample95_0[,1], 
                      wsample95=wsample95_0[,3], wMAP=mE$byw_0$DHw[imodelDH], wdw=wdw_0_r6)
  dwsample_1<- tibble(Date=Datessample, Datew=datD$Datesw, wsample05=wsample95_1[,1], 
                      wsample95=wsample95_1[,3], wMAP=mE$byw_1$DHw[imodelDH], wdw=wdw_1_r6)
  
  pw_0 <-ggplot(dwsample_0, aes(x=Date)) + 
    geom_ribbon(aes(ymin = wsample05, ymax = wsample95), fill = "grey70") +
    geom_point(aes(x=Datew, y = wdw,       color="Data")) +
    geom_line (aes(x=Date,  y = wMAP,      color="MAP")) +
    geom_line (aes(x=Date,  y = wsample05, color="95% CrI")) +
    labs(x = "", y = 'Deaths in hospital', color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +#+  ggtitle(Title_0)
    scale_x_date(breaks= breaks, date_labels= date_labels) 
  
  pw_1 <-ggplot(dwsample_1, aes(x=Date)) + 
    geom_ribbon(aes(ymin = wsample05, ymax = wsample95), fill = "grey70") +
    geom_point(aes(x=Datew, y = wdw,       color="Data")) +
    geom_line (aes(x=Date,  y = wMAP,      color="MAP")) +
    geom_line (aes(x=Date,  y = wsample05, color="95% CrI")) +
    labs(x = "", y = 'Deaths in hospital', color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) + #+  ggtitle(Title_1)
    scale_x_date(breaks= breaks, date_labels= date_labels) 
  #DO
  #vdw_0_r5 = redact2(datD$vdw_0)  #redact
  #vdw_0_r5 = r5(vdw_0_r5)         #round
  #vdw_1_r5 = redact2(datD$vdw_1)  #redact
  #vdw_1_r5 = r5(vdw_1_r5)         #round
  vdw_0_r6 = rm6(datD$vdw_0)       #round mid-6
  vdw_1_r6 = rm6(datD$vdw_1)       #round mid-6
  dvsample_0<- tibble(Date=Datessample, Datev=datD$Datesv, vsample05=vsample95_0[,1], 
                      vsample95=vsample95_0[,3], vMAP=mE$byw_0$DOw[imodelDO], vdw=vdw_0_r6)
  dvsample_1<- tibble(Date=Datessample, Datev=datD$Datesv, vsample05=vsample95_1[,1], 
                      vsample95=vsample95_1[,3], vMAP=mE$byw_1$DOw[imodelDO], vdw=vdw_1_r6)
  
  pv_0 <-ggplot(dvsample_0, aes(x=Date)) + 
    geom_ribbon(aes(ymin = vsample05, ymax = vsample95), fill = "grey70") +
    geom_point(aes(x=Datev, y = vdw,       color="Data")) +
    geom_line (aes(x=Date,  y = vMAP,      color="MAP")) +
    geom_line (aes(x=Date,  y = vsample05, color="95% CrI")) +
    labs(x = '', y = 'Deaths outside hospital', color = "") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) + #+  ggtitle(Title_0)
    scale_x_date(breaks= breaks, date_labels= date_labels)
  
  pv_1 <-ggplot(dvsample_1, aes(x=Date)) + 
    geom_ribbon(aes(ymin = vsample05, ymax = vsample95), fill = "grey70") +
    geom_point(aes(x=Datev, y = vdw,       color="Data")) +
    geom_line (aes(x=Date,  y = vMAP,      color="MAP")) +
    geom_line (aes(x=Date,  y = vsample05, color="95% CrI")) +
    labs(x = '', y = 'Deaths outside hospital', color = "") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +#+  ggtitle(Title_1)
    scale_x_date(breaks= breaks, date_labels= date_labels) 
  
  gridExtra::grid.arrange(pz_0, pz_1, pw_0, pw_1, pv_0, pv_1, nrow = 3, ncol = 2)
  
  svglite(paste0(filenamepath,".svg")); 
  gridExtra::grid.arrange(pz_0, pz_1, pw_0, pw_1, pv_0, pv_1, nrow = 3, ncol = 2)
  invisible(dev.off())
  


#7 plot prev, positivity, R0

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
dcsample <- tibble(Date=Datessample, p05=Psample95[,1], p95=Psample95[,3], pMAP = datp$Prev)
pp  <-ggplot(dcsample, aes(x=Date)) + 
      geom_ribbon(aes(ymin = p05, ymax = p95), fill = "grey70") +
      geom_line (aes(x=Date, y = pMAP,    color="MAP")) +
      geom_line (aes(x=Date, y = p05,     color="95% CrI")) +
      labs(x = 'Date', y = 'Prevalence estimate', color = "") + 
      scale_color_manual(values = colors) +
      theme(axis.title = element_text(size = 12, face = "bold"))

### Plot positivity
posi_data_plot = 0*datp$Posi #same length as model
posi_data_plot[idata_posi + shift_posi] = (posi_data_perc/100)*posi_d_to_m #data*factor to which model fitted 

dcsample <- tibble(Date=Datessample, c05=csample95[,1], c95=csample95[,3], 
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


#8 probs - prob plots
    ##
    filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_plots_probs")
    #par(mfrow = c(5,2))
    #par(mar = c(4, 4, 1, 4))  #bottom, left, top, right
    colors <- c("Data" = 1,  "MAP" = 2, "95% CrI" = "grey70", "Period 1" = 1, "Period 2" = 2)
    Age <- c("0-4", "05-11",  "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70+")
    
    #breaks=seq(min(as.POSIXct(Datessample)), max(as.POSIXct(Datessample)), length=6)
    #breaks=seq(min((Datessample)), max((Datessample)), length=6)
    #date_labels = "%d/%m/%y" # "%D") # "%M-%Y")

    #y   #ysample95_0[2] median
    DF_0 <- tibble(Age=Age, s05=ysample95_0[,1], s95=ysample95_0[,3], MAP=parsE$y_0, DAT=pars$y_0)
    DF_1 <- tibble(Age=Age, s05=ysample95_1[,1], s95=ysample95_1[,3], MAP=parsE$y_1, DAT=pars$y_1)
    py_0<-ggplot(data=DF_0, aes(x=Age, group=1)) +   #"group=1" because x var is charactrer/factor
      geom_ribbon(aes(ymin = s05, ymax = s95), fill = "grey70") +
      geom_point(aes( y = DAT,      color="Data")) +
      geom_line (aes( y = MAP,      color="MAP")) +
      geom_line (aes( y = s05,      color="95% CrI")) +
      labs(x = "", y = 'prob y_0',   color = "") + #Legend") + 
      scale_color_manual(values = colors) + ylim(0, 1) + 
      theme(axis.title = element_text(size = 12, face = "bold")) +  ggtitle(Title_0) #+
    py_1<-ggplot(data=DF_1, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95), fill = "grey70") +
      geom_point(aes( y = DAT,      color="Data")) +
      geom_line (aes( y = MAP,      color="MAP")) +
      geom_line (aes( y = s05,      color="95% CrI")) +
      labs(x = "", y = 'prob y_1',   color = "") + #Legend") + 
      scale_color_manual(values = colors) + ylim(0, 1) + 
      theme(axis.title = element_text(size = 12, face = "bold")) +  ggtitle(Title_1) #+

    #h   #hsample95_0[2] median
    DF_0 <- tibble(Age=Age, s05=hsample95_0[,1], s95=hsample95_0[,3], MAP=parsE$h_0, DAT=pars$h_0)
    DF_1 <- tibble(Age=Age, s05=hsample95_1[,1], s95=hsample95_1[,3], MAP=parsE$h_1, DAT=pars$h_1)
    ph_0<-ggplot(data=DF_0, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95), fill = "grey70") +
      geom_point(aes( y = DAT,      color="Data")) +
      geom_line (aes( y = MAP,      color="MAP")) +
      geom_line (aes( y = s05,      color="95% CrI")) +
      labs(x = "", y = 'prob h_0',   color = "") + #Legend") + 
      scale_color_manual(values = colors) + ylim(0, 1) + 
      theme(axis.title = element_text(size = 12, face = "bold")) #+  ggtitle(Title_0) #+
    ph_1<-ggplot(data=DF_1, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95), fill = "grey70") +
      geom_point(aes( y = DAT,      color="Data")) +
      geom_line (aes( y = MAP,      color="MAP")) +
      geom_line (aes( y = s05,      color="95% CrI")) +
      labs(x = "", y = 'prob h_1',   color = "") + #Legend") + 
      scale_color_manual(values = colors) + ylim(0, 1) + 
      theme(axis.title = element_text(size = 12, face = "bold")) #+  ggtitle(Title_0) #+

    #yh  #yhsample95_0[2] median
    DF_0 <- tibble(Age=Age, s05=yhsample95_0[,1], s95=yhsample95_0[,3], MAP=parsE$yh_0, DAT=pars$yh_0)
    DF_1 <- tibble(Age=Age, s05=yhsample95_1[,1], s95=yhsample95_1[,3], MAP=parsE$yh_1, DAT=pars$yh_1)
    pyh_0<-ggplot(data=DF_0, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95), fill = "grey70") +
      geom_point(aes( y = DAT,      color="Data")) +
      geom_line (aes( y = MAP,      color="MAP")) +
      geom_line (aes( y = s05,      color="95% CrI")) +
      labs(x = "", y = 'prob yh_0',   color = "") + #Legend") + 
      scale_color_manual(values = colors) + ylim(0, 1) + 
      theme(axis.title = element_text(size = 12, face = "bold")) #+  ggtitle(Title_0) #+
    pyh_1<-ggplot(data=DF_1, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95), fill = "grey70") +
      geom_point(aes( y = DAT,      color="Data")) +
      geom_line (aes( y = MAP,      color="MAP")) +
      geom_line (aes( y = s05,      color="95% CrI")) +
      labs(x = "", y = 'prob yh_1',   color = "") + #Legend") + 
      scale_color_manual(values = colors) + ylim(0, 1) + 
      theme(axis.title = element_text(size = 12, face = "bold")) #+   ggtitle(Title_0) #+
    
    #d   #dample95_0[2] median
    DF_0 <- tibble(Age=Age, s05=dsample95_0[,1], s95=dsample95_0[,3], MAP=parsE$d_0, DAT=pars$d_0)
    DF_1 <- tibble(Age=Age, s05=dsample95_1[,1], s95=dsample95_1[,3], MAP=parsE$d_1, DAT=pars$d_1)
    pd_0<-ggplot(data=DF_0, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95), fill = "grey70") +
      geom_point(aes( y = DAT,      color="Data")) +
      geom_line (aes( y = MAP,      color="MAP")) +
      geom_line (aes( y = s05,      color="95% CrI")) +
      labs(x = "", y = 'prob d_0',   color = "") + #Legend") + 
      scale_color_manual(values = colors) + ylim(0, 1) + 
      theme(axis.title = element_text(size = 12, face = "bold")) #+   ggtitle(Title_0) #+
    pd_1<-ggplot(data=DF_1, aes(x=Age, group=1)) + 
      geom_ribbon(aes(ymin = s05, ymax = s95), fill = "grey70") +
      geom_point(aes( y = DAT,      color="Data")) +
      geom_line (aes( y = MAP,      color="MAP")) +
      geom_line (aes( y = s05,      color="95% CrI")) +
      labs(x = "", y = 'prob d_1',   color = "") + #Legend") + 
      scale_color_manual(values = colors) + ylim(0, 1) + 
      theme(axis.title = element_text(size = 12, face = "bold")) #+   ggtitle(Title_0) #+
    
    #m  
    DF_0 <- tibble(Age=Age, DAT=pars$m_0, DATa=pars$ma_0, DATb=pars$mb_0)
    DF_1 <- tibble(Age=Age, DAT=pars$m_1, DATa=pars$ma_1, DATb=pars$mb_1)
    pm_0<-ggplot(data=DF_0, aes(x=Age, group=1)) + 
      geom_point(aes(x=Age,  y = DATa,     color="Period 1")) +
      geom_point(aes(x=Age,  y = DATb,     color="Period 2")) +
      geom_line (aes(x=Age,  y = DATa,     color="Period 1")) +
      geom_line (aes(x=Age,  y = DATb,     color="Period 2")) +
      labs(x = "", y = 'prob m_0',   color = "") + #Legend") + 
      scale_color_manual(values = colors) +
      theme(axis.title = element_text(size = 12, face = "bold")) #+   ggtitle(Title_0) #+
    pm_1<-ggplot(data=DF_1, aes(x=Age, group=1)) + 
      geom_point(aes(x=Age,  y = DATa,     color="Period 1")) +
      geom_point(aes(x=Age,  y = DATb,     color="Period 2")) +
      geom_line (aes(x=Age,  y = DATa,     color="Period 1")) +
      geom_line (aes(x=Age,  y = DATb,     color="Period 2")) +
      labs(x = "", y = 'prob m_1',   color = "") + #Legend") + 
      scale_color_manual(values = colors) +
      theme(axis.title = element_text(size = 12, face = "bold")) #+   ggtitle(Title_1) #+

    gridExtra::grid.arrange(py_0, py_1, ph_0, ph_1, pyh_0, pyh_1, pd_0, pd_1, pm_0, pm_1, nrow = 5, ncol = 2)
    
    svglite(paste0(filenamepath,".svg")); 
    gridExtra::grid.arrange(py_0, py_1, ph_0, ph_1, pyh_0, pyh_1, pd_0, pd_1, pm_0, pm_1, nrow = 5, ncol = 2)
    invisible(dev.off())
    

    
##9 age profiles
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
colors <- c("0-4" = 1, "05-11" = 2,  "12-17" = 3, "18-29" = 4, "30-39" = 5, 
            "40-49" = 6, "50-59" = 7,  "60-69" = 8, "70+" = 9)
Yname = c('Hospitalisations', 'Deaths in hospital', 'Deaths outside hospital')
if (LOG==1) {Yname = c('log10 Hospitalisations', 'log10 Deaths in hospital', 'log10 Deaths outside hospital')}

#H
pH_0 <- ggplot() +
    labs(x = "", y = Yname[1], color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  ggtitle(Title_0) +
    scale_x_date(breaks= breaks, date_labels= date_labels) +
    geom_line (data=datHa_0, aes(x=Dates,y=H1w, color = "0-4")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H2w, color = "05-11")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H3w, color = "12-17")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H4w, color = "18-29")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H5w, color = "30-39")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H6w, color = "40-49")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H7w, color = "50-59")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H8w, color = "60-69")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H9w, color = "70+")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H1d, color = "0-4")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H2d, color = "05-11")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H3d, color = "12-17")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H4d, color = "18-29")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H5d, color = "30-39")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H6d, color = "40-49")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H7d, color = "50-59")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H8d, color = "60-69")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H9d, color = "70+"))
pH_1 <- ggplot() +
    labs(x = "", y = Yname[1], color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  ggtitle(Title_1) +
    scale_x_date(breaks= breaks, date_labels= date_labels) +
    geom_line (data=datHa_1, aes(x=Dates,y=H1w, color = "0-4")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H2w, color = "05-11")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H3w, color = "12-17")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H4w, color = "18-29")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H5w, color = "30-39")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H6w, color = "40-49")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H7w, color = "50-59")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H8w, color = "60-69")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H9w, color = "70+")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H1d, color = "0-4")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H2d, color = "05-11")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H3d, color = "12-17")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H4d, color = "18-29")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H5d, color = "30-39")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H6d, color = "40-49")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H7d, color = "50-59")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H8d, color = "60-69")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H9d, color = "70+"))

#DH
pDH_0 <- ggplot() +
    labs(x = "", y = Yname[2], color = "Age group") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  #ggtitle(Title_0) +
    scale_x_date(breaks= breaks, date_labels= date_labels) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH1w, color = "0-4")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH2w, color = "05-11")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH3w, color = "12-17")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH4w, color = "18-29")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH5w, color = "30-39")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH6w, color = "40-49")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH7w, color = "50-59")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH8w, color = "60-69")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH9w, color = "70+")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH1d, color = "0-4")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH2d, color = "05-11")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH3d, color = "12-17")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH4d, color = "18-29")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH5d, color = "30-39")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH6d, color = "40-49")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH7d, color = "50-59")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH8d, color = "60-69")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH9d, color = "70+"))  
pDH_1 <- ggplot() +
    labs(x = "", y = Yname[2], color = "Age group") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  #ggtitle(Title_1) +
    scale_x_date(breaks= breaks, date_labels= date_labels) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH1w, color = "0-4")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH2w, color = "05-11")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH3w, color = "12-17")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH4w, color = "18-29")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH5w, color = "30-39")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH6w, color = "40-49")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH7w, color = "50-59")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH8w, color = "60-69")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH9w, color = "70+")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH1d, color = "0-4")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH2d, color = "05-11")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH3d, color = "12-17")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH4d, color = "18-29")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH5d, color = "30-39")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH6d, color = "40-49")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH7d, color = "50-59")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH8d, color = "60-69")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH9d, color = "70+"))  

#DO
pDO_0 <- ggplot() +
    labs(x = 'Date', y = Yname[3], color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  #ggtitle(Title_0) +
    scale_x_date(breaks= breaks, date_labels= date_labels) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO1w, color = "0-4")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO2w, color = "05-11")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO3w, color = "12-17")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO4w, color = "18-29")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO5w, color = "30-39")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO6w, color = "40-49")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO7w, color = "50-59")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO8w, color = "60-69")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO9w, color = "70+")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO1d, color = "0-4")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO2d, color = "05-11")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO3d, color = "12-17")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO4d, color = "18-29")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO5d, color = "30-39")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO6d, color = "40-49")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO7d, color = "50-59")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO8d, color = "60-69")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO9d, color = "70+"))  
pDO_1 <- ggplot() +
    labs(x = 'Date', y = Yname[3], color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  #ggtitle(Title_1 +
    scale_x_date(breaks= breaks, date_labels= date_labels) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO1w, color = "0-4")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO2w, color = "05-11")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO3w, color = "12-17")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO4w, color = "18-29")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO5w, color = "30-39")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO6w, color = "40-49")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO7w, color = "50-59")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO8w, color = "60-69")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO9w, color = "70+")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO1d, color = "0-4")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO2d, color = "05-11")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO3d, color = "12-17")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO4d, color = "18-29")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO5d, color = "30-39")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO6d, color = "40-49")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO7d, color = "50-59")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO8d, color = "60-69")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO9d, color = "70+"))

gridExtra::grid.arrange(pH_0, pH_1, pDH_0, pDH_1, pDO_0, pDO_1, nrow = 3, ncol = 2)

filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_AgeProfiles")
svglite(paste0(filenamepath,".svg")); 
gridExtra::grid.arrange(pH_0, pH_1, pDH_0, pDH_1, pDO_0, pDO_1, nrow = 3, ncol = 2)
invisible(dev.off())



##7b age profiles - DATA ROUNDED TO MID 6
  Yname = c('Hospitalisations *', 'Deaths in hospital *', 'Deaths outside hospital *')
  if (LOG==1) {Yname = c('log10 Hospitalisations *', 'log10 Deaths in hospital *', 'log10 Deaths outside hospital *')}
  #* data rounded to mid-6
  #H
  #H1d_0_r5 = redact2(YEXP(datHa_0$H1d,1))  #redact after reversing LOG10
  #H1d_1_r5 = redact2(YEXP(datHa_1$H1d,1))  #redact
  #H2d_0_r5 = redact2(YEXP(datHa_0$H2d,1))  #redact
  #H2d_1_r5 = redact2(YEXP(datHa_1$H2d,1))  #redact
  #H3d_0_r5 = redact2(YEXP(datHa_0$H3d,1))  #redact
  #H3d_1_r5 = redact2(YEXP(datHa_1$H3d,1))  #redact
  #H4d_0_r5 = redact2(YEXP(datHa_0$H4d,1))  #redact
  #H4d_1_r5 = redact2(YEXP(datHa_1$H4d,1))  #redact
  #H5d_0_r5 = redact2(YEXP(datHa_0$H5d,1))  #redact
  #H5d_1_r5 = redact2(YEXP(datHa_1$H5d,1))  #redact
  #H6d_0_r5 = redact2(YEXP(datHa_0$H6d,1))  #redact
  #H6d_1_r5 = redact2(YEXP(datHa_1$H6d,1))  #redact
  #H7d_0_r5 = redact2(YEXP(datHa_0$H7d,1))  #redact
  #H7d_1_r5 = redact2(YEXP(datHa_1$H7d,1))  #redact
  #H8d_0_r5 = redact2(YEXP(datHa_0$H8d,1))  #redact
  #H8d_1_r5 = redact2(YEXP(datHa_1$H8d,1))  #redact
  #H9d_0_r5 = redact2(YEXP(datHa_0$H9d,1))  #redact
  #H9d_1_r5 = redact2(YEXP(datHa_1$H9d,1))  #redact
  #H1d_0_r5 = YLOG(r5(H1d_0_r5),1)          #round and LOG
  #H1d_1_r5 = YLOG(r5(H1d_1_r5),1)          #round
  #H2d_0_r5 = YLOG(r5(H2d_0_r5),1)          #round
  #H2d_1_r5 = YLOG(r5(H2d_1_r5),1)          #round
  #H3d_0_r5 = YLOG(r5(H3d_0_r5),1)          #round
  #H3d_1_r5 = YLOG(r5(H3d_1_r5),1)          #round
  #H4d_0_r5 = YLOG(r5(H4d_0_r5),1)          #round
  #H4d_1_r5 = YLOG(r5(H4d_1_r5),1)          #round
  #H5d_0_r5 = YLOG(r5(H5d_0_r5),1)          #round
  #H5d_1_r5 = YLOG(r5(H5d_1_r5),1)          #round
  #H6d_0_r5 = YLOG(r5(H6d_0_r5),1)          #round
  #H6d_1_r5 = YLOG(r5(H6d_1_r5),1)          #round
  #H7d_0_r5 = YLOG(r5(H7d_0_r5),1)          #round
  #H7d_1_r5 = YLOG(r5(H7d_1_r5),1)          #round
  #H8d_0_r5 = YLOG(r5(H8d_0_r5),1)          #round
  #H8d_1_r5 = YLOG(r5(H8d_1_r5),1)          #round
  #H9d_0_r5 = YLOG(r5(H9d_0_r5),1)          #round
  #H9d_1_r5 = YLOG(r5(H9d_1_r5),1)          #round
  H1d_0_r6 = YLOG(rm6(YEXP(datHa_0$H1d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H2d_0_r6 = YLOG(rm6(YEXP(datHa_0$H2d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H3d_0_r6 = YLOG(rm6(YEXP(datHa_0$H3d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H4d_0_r6 = YLOG(rm6(YEXP(datHa_0$H4d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H5d_0_r6 = YLOG(rm6(YEXP(datHa_0$H5d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H6d_0_r6 = YLOG(rm6(YEXP(datHa_0$H6d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H7d_0_r6 = YLOG(rm6(YEXP(datHa_0$H7d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H8d_0_r6 = YLOG(rm6(YEXP(datHa_0$H8d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H9d_0_r6 = YLOG(rm6(YEXP(datHa_0$H9d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H1d_1_r6 = YLOG(rm6(YEXP(datHa_1$H1d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H2d_1_r6 = YLOG(rm6(YEXP(datHa_1$H2d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H3d_1_r6 = YLOG(rm6(YEXP(datHa_1$H3d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H4d_1_r6 = YLOG(rm6(YEXP(datHa_1$H4d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H5d_1_r6 = YLOG(rm6(YEXP(datHa_1$H5d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H6d_1_r6 = YLOG(rm6(YEXP(datHa_1$H6d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H7d_1_r6 = YLOG(rm6(YEXP(datHa_1$H7d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H8d_1_r6 = YLOG(rm6(YEXP(datHa_1$H8d,1)),1) #reverse LOG10, round mid-6, and LOG10
  H9d_1_r6 = YLOG(rm6(YEXP(datHa_1$H9d,1)),1) #reverse LOG10, round mid-6, and LOG10

  pH_0 <- ggplot() +
    labs(x = "", y = Yname[1], color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  ggtitle(Title_0) +
    scale_x_date(breaks= breaks, date_labels= date_labels) +
    geom_line (data=datHa_0, aes(x=Dates,y=H1w, color = "0-4")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H2w, color = "05-11")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H3w, color = "12-17")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H4w, color = "18-29")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H5w, color = "30-39")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H6w, color = "40-49")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H7w, color = "50-59")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H8w, color = "60-69")) +
    geom_line (data=datHa_0, aes(x=Dates,y=H9w, color = "70+")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H1d_0_r6, color = "0-4")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H2d_0_r6, color = "05-11")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H3d_0_r6, color = "12-17")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H4d_0_r6, color = "18-29")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H5d_0_r6, color = "30-39")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H6d_0_r6, color = "40-49")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H7d_0_r6, color = "50-59")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H8d_0_r6, color = "60-69")) +
    geom_point(data=datHa_0, aes(x=Dates,y=H9d_0_r6, color = "70+"))
  pH_1 <- ggplot() +
    labs(x = "", y = Yname[1], color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  ggtitle(Title_1) +
    scale_x_date(breaks= breaks, date_labels= date_labels) +
    geom_line (data=datHa_1, aes(x=Dates,y=H1w, color = "0-4")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H2w, color = "05-11")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H3w, color = "12-17")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H4w, color = "18-29")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H5w, color = "30-39")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H6w, color = "40-49")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H7w, color = "50-59")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H8w, color = "60-69")) +
    geom_line (data=datHa_1, aes(x=Dates,y=H9w, color = "70+")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H1d_1_r6, color = "0-4")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H2d_1_r6, color = "05-11")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H3d_1_r6, color = "12-17")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H4d_1_r6, color = "18-29")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H5d_1_r6, color = "30-39")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H6d_1_r6, color = "40-49")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H7d_1_r6, color = "50-59")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H8d_1_r6, color = "60-69")) +
    geom_point(data=datHa_1, aes(x=Dates,y=H9d_1_r6, color = "70+"))
  
  #DH
  #DH1d_0_r5 = redact2(YEXP(datDHa_0$DH1d,1))  #redact after reversing LOG10
  #DH1d_1_r5 = redact2(YEXP(datDHa_1$DH1d,1))  #redact
  #DH2d_0_r5 = redact2(YEXP(datDHa_0$DH2d,1))  #redact
  #DH2d_1_r5 = redact2(YEXP(datDHa_1$DH2d,1))  #redact
  #DH3d_0_r5 = redact2(YEXP(datDHa_0$DH3d,1))  #redact
  #DH3d_1_r5 = redact2(YEXP(datDHa_1$DH3d,1))  #redact
  #DH4d_0_r5 = redact2(YEXP(datDHa_0$DH4d,1))  #redact
  #DH4d_1_r5 = redact2(YEXP(datDHa_1$DH4d,1))  #redact
  #DH5d_0_r5 = redact2(YEXP(datDHa_0$DH5d,1))  #redact
  #DH5d_1_r5 = redact2(YEXP(datDHa_1$DH5d,1))  #redact
  #DH6d_0_r5 = redact2(YEXP(datDHa_0$DH6d,1))  #redact
  #DH6d_1_r5 = redact2(YEXP(datDHa_1$DH6d,1))  #redact
  #DH7d_0_r5 = redact2(YEXP(datDHa_0$DH7d,1))  #redact
  #DH7d_1_r5 = redact2(YEXP(datDHa_1$DH7d,1))  #redact
  #DH8d_0_r5 = redact2(YEXP(datDHa_0$DH8d,1))  #redact
  #DH8d_1_r5 = redact2(YEXP(datDHa_1$DH8d,1))  #redact
  #DH9d_0_r5 = redact2(YEXP(datDHa_0$DH9d,1))  #redact
  #DH9d_1_r5 = redact2(YEXP(datDHa_1$DH9d,1))  #redact
  #DH1d_0_r5 = YLOG(r5(DH1d_0_r5),1)          #round and LOG
  #DH1d_1_r5 = YLOG(r5(DH1d_1_r5),1)          #round
  #DH2d_0_r5 = YLOG(r5(DH2d_0_r5),1)          #round
  #DH2d_1_r5 = YLOG(r5(DH2d_1_r5),1)          #round
  #DH3d_0_r5 = YLOG(r5(DH3d_0_r5),1)          #round
  #DH3d_1_r5 = YLOG(r5(DH3d_1_r5),1)          #round
  #DH4d_0_r5 = YLOG(r5(DH4d_0_r5),1)          #round
  #DH4d_1_r5 = YLOG(r5(DH4d_1_r5),1)          #round
  #DH5d_0_r5 = YLOG(r5(DH5d_0_r5),1)          #round
  #DH5d_1_r5 = YLOG(r5(DH5d_1_r5),1)          #round
  #DH6d_0_r5 = YLOG(r5(DH6d_0_r5),1)          #round
  #DH6d_1_r5 = YLOG(r5(DH6d_1_r5),1)          #round
  #DH7d_0_r5 = YLOG(r5(DH7d_0_r5),1)          #round
  #DH7d_1_r5 = YLOG(r5(DH7d_1_r5),1)          #round
  #DH8d_0_r5 = YLOG(r5(DH8d_0_r5),1)          #round
  #DH8d_1_r5 = YLOG(r5(DH8d_1_r5),1)          #round
  #DH9d_0_r5 = YLOG(r5(DH9d_0_r5),1)          #round
  #DH9d_1_r5 = YLOG(r5(DH9d_1_r5),1)          #round
  #DH1d_0_r5 = redact2(YEXP(datDHa_0$DH1d,1))  #redact after reversing LOG10
  DH1d_0_r6 = YLOG(rm6(YEXP(datDHa_0$DH1d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH2d_0_r6 = YLOG(rm6(YEXP(datDHa_0$DH2d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH3d_0_r6 = YLOG(rm6(YEXP(datDHa_0$DH3d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH4d_0_r6 = YLOG(rm6(YEXP(datDHa_0$DH4d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH5d_0_r6 = YLOG(rm6(YEXP(datDHa_0$DH5d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH6d_0_r6 = YLOG(rm6(YEXP(datDHa_0$DH6d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH7d_0_r6 = YLOG(rm6(YEXP(datDHa_0$DH7d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH8d_0_r6 = YLOG(rm6(YEXP(datDHa_0$DH8d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH9d_0_r6 = YLOG(rm6(YEXP(datDHa_0$DH9d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH1d_1_r6 = YLOG(rm6(YEXP(datDHa_1$DH1d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH2d_1_r6 = YLOG(rm6(YEXP(datDHa_1$DH2d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH3d_1_r6 = YLOG(rm6(YEXP(datDHa_1$DH3d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH4d_1_r6 = YLOG(rm6(YEXP(datDHa_1$DH4d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH5d_1_r6 = YLOG(rm6(YEXP(datDHa_1$DH5d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH6d_1_r6 = YLOG(rm6(YEXP(datDHa_1$DH6d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH7d_1_r6 = YLOG(rm6(YEXP(datDHa_1$DH7d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH8d_1_r6 = YLOG(rm6(YEXP(datDHa_1$DH8d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DH9d_1_r6 = YLOG(rm6(YEXP(datDHa_1$DH9d,1)),1) #reverse LOG10, round mid-6, and LOG10
 
  pDH_0 <- ggplot() +
    labs(x = "", y = Yname[2], color = "Age group") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  #ggtitle(Title_0) +
    scale_x_date(breaks= breaks, date_labels= date_labels) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH1w, color = "0-4")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH2w, color = "05-11")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH3w, color = "12-17")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH4w, color = "18-29")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH5w, color = "30-39")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH6w, color = "40-49")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH7w, color = "50-59")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH8w, color = "60-69")) +
    geom_line (data=datDHa_0, aes(x=Dates,y=DH9w, color = "70+")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH1d_0_r6, color = "0-4")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH2d_0_r6, color = "05-11")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH3d_0_r6, color = "12-17")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH4d_0_r6, color = "18-29")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH5d_0_r6, color = "30-39")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH6d_0_r6, color = "40-49")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH7d_0_r6, color = "50-59")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH8d_0_r6, color = "60-69")) +
    geom_point(data=datDHa_0, aes(x=Dates,y=DH9d_0_r6, color = "70+"))  
  pDH_1 <- ggplot() +
    labs(x = "", y = Yname[2], color = "Age group") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  #ggtitle(Title_1) +
    scale_x_date(breaks= breaks, date_labels= date_labels) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH1w, color = "0-4")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH2w, color = "05-11")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH3w, color = "12-17")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH4w, color = "18-29")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH5w, color = "30-39")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH6w, color = "40-49")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH7w, color = "50-59")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH8w, color = "60-69")) +
    geom_line (data=datDHa_1, aes(x=Dates,y=DH9w, color = "70+")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH1d_1_r6, color = "0-4")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH2d_1_r6, color = "05-11")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH3d_1_r6, color = "12-17")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH4d_1_r6, color = "18-29")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH5d_1_r6, color = "30-39")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH6d_1_r6, color = "40-49")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH7d_1_r6, color = "50-59")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH8d_1_r6, color = "60-69")) +
    geom_point(data=datDHa_1, aes(x=Dates,y=DH9d_1_r6, color = "70+"))  
  
  #DO
  #DO1d_0_r5 = redact2(YEXP(datDOa_0$DO1d,1))  #redact after reversing LOG10
  #DO1d_1_r5 = redact2(YEXP(datDOa_1$DO1d,1))  #redact
  #DO2d_0_r5 = redact2(YEXP(datDOa_0$DO2d,1))  #redact
  #DO2d_1_r5 = redact2(YEXP(datDOa_1$DO2d,1))  #redact
  #DO3d_0_r5 = redact2(YEXP(datDOa_0$DO3d,1))  #redact
  #DO3d_1_r5 = redact2(YEXP(datDOa_1$DO3d,1))  #redact
  #DO4d_0_r5 = redact2(YEXP(datDOa_0$DO4d,1))  #redact
  #DO4d_1_r5 = redact2(YEXP(datDOa_1$DO4d,1))  #redact
  #DO5d_0_r5 = redact2(YEXP(datDOa_0$DO5d,1))  #redact
  #DO5d_1_r5 = redact2(YEXP(datDOa_1$DO5d,1))  #redact
  #DO6d_0_r5 = redact2(YEXP(datDOa_0$DO6d,1))  #redact
  #DO6d_1_r5 = redact2(YEXP(datDOa_1$DO6d,1))  #redact
  #DO7d_0_r5 = redact2(YEXP(datDOa_0$DO7d,1))  #redact
  #DO7d_1_r5 = redact2(YEXP(datDOa_1$DO7d,1))  #redact
  #DO8d_0_r5 = redact2(YEXP(datDOa_0$DO8d,1))  #redact
  #DO8d_1_r5 = redact2(YEXP(datDOa_1$DO8d,1))  #redact
  #DO9d_0_r5 = redact2(YEXP(datDOa_0$DO9d,1))  #redact
  #DO9d_1_r5 = redact2(YEXP(datDOa_1$DO9d,1))  #redact
  #DO1d_0_r5 = YLOG(r5(DO1d_0_r5),1)          #round and LOG
  #DO1d_1_r5 = YLOG(r5(DO1d_1_r5),1)          #round
  #DO2d_0_r5 = YLOG(r5(DO2d_0_r5),1)          #round
  #DO2d_1_r5 = YLOG(r5(DO2d_1_r5),1)          #round
  #DO3d_0_r5 = YLOG(r5(DO3d_0_r5),1)          #round
  #DO3d_1_r5 = YLOG(r5(DO3d_1_r5),1)          #round
  #DO4d_0_r5 = YLOG(r5(DO4d_0_r5),1)          #round
  #DO4d_1_r5 = YLOG(r5(DO4d_1_r5),1)          #round
  #DO5d_0_r5 = YLOG(r5(DO5d_0_r5),1)          #round
  #DO5d_1_r5 = YLOG(r5(DO5d_1_r5),1)          #round
  #DO6d_0_r5 = YLOG(r5(DO6d_0_r5),1)          #round
  #DO6d_1_r5 = YLOG(r5(DO6d_1_r5),1)          #round
  #DO7d_0_r5 = YLOG(r5(DO7d_0_r5),1)          #round
  #DO7d_1_r5 = YLOG(r5(DO7d_1_r5),1)          #round
  #DO8d_0_r5 = YLOG(r5(DO8d_0_r5),1)          #round
  #DO8d_1_r5 = YLOG(r5(DO8d_1_r5),1)          #round
  #DO9d_0_r5 = YLOG(r5(DO9d_0_r5),1)          #round
  #DO9d_1_r5 = YLOG(r5(DO9d_1_r5),1)          #round
  DO1d_0_r6 = YLOG(rm6(YEXP(datDOa_0$DO1d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO2d_0_r6 = YLOG(rm6(YEXP(datDOa_0$DO2d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO3d_0_r6 = YLOG(rm6(YEXP(datDOa_0$DO3d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO4d_0_r6 = YLOG(rm6(YEXP(datDOa_0$DO4d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO5d_0_r6 = YLOG(rm6(YEXP(datDOa_0$DO5d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO6d_0_r6 = YLOG(rm6(YEXP(datDOa_0$DO6d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO7d_0_r6 = YLOG(rm6(YEXP(datDOa_0$DO7d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO8d_0_r6 = YLOG(rm6(YEXP(datDOa_0$DO8d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO9d_0_r6 = YLOG(rm6(YEXP(datDOa_0$DO9d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO1d_1_r6 = YLOG(rm6(YEXP(datDOa_1$DO1d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO2d_1_r6 = YLOG(rm6(YEXP(datDOa_1$DO2d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO3d_1_r6 = YLOG(rm6(YEXP(datDOa_1$DO3d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO4d_1_r6 = YLOG(rm6(YEXP(datDOa_1$DO4d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO5d_1_r6 = YLOG(rm6(YEXP(datDOa_1$DO5d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO6d_1_r6 = YLOG(rm6(YEXP(datDOa_1$DO6d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO7d_1_r6 = YLOG(rm6(YEXP(datDOa_1$DO7d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO8d_1_r6 = YLOG(rm6(YEXP(datDOa_1$DO8d,1)),1) #reverse LOG10, round mid-6, and LOG10
  DO9d_1_r6 = YLOG(rm6(YEXP(datDOa_1$DO9d,1)),1) #reverse LOG10, round mid-6, and LOG10

  pDO_0 <- ggplot() +
    labs(x = 'Date', y = Yname[3], color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  #ggtitle(Title_0) +
    scale_x_date(breaks= breaks, date_labels= date_labels) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO1w, color = "0-4")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO2w, color = "05-11")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO3w, color = "12-17")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO4w, color = "18-29")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO5w, color = "30-39")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO6w, color = "40-49")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO7w, color = "50-59")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO8w, color = "60-69")) +
    geom_line (data=datDOa_0, aes(x=Dates,y=DO9w, color = "70+")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO1d_0_r6, color = "0-4")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO2d_0_r6, color = "05-11")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO3d_0_r6, color = "12-17")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO4d_0_r6, color = "18-29")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO5d_0_r6, color = "30-39")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO6d_0_r6, color = "40-49")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO7d_0_r6, color = "50-59")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO8d_0_r6, color = "60-69")) +
    geom_point(data=datDOa_0, aes(x=Dates,y=DO9d_0_r6, color = "70+"))  
  pDO_1 <- ggplot() +
    labs(x = 'Date', y = Yname[3], color = "") + #Legend") + 
    scale_color_manual(values = colors) +
    theme(axis.title = element_text(size = 12, face = "bold")) +  #ggtitle(Title_1 +
    scale_x_date(breaks= breaks, date_labels= date_labels) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO1w, color = "0-4")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO2w, color = "05-11")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO3w, color = "12-17")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO4w, color = "18-29")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO5w, color = "30-39")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO6w, color = "40-49")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO7w, color = "50-59")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO8w, color = "60-69")) +
    geom_line (data=datDOa_1, aes(x=Dates,y=DO9w, color = "70+")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO1d_1_r6, color = "0-4")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO2d_1_r6, color = "05-11")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO3d_1_r6, color = "12-17")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO4d_1_r6, color = "18-29")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO5d_1_r6, color = "30-39")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO6d_1_r6, color = "40-49")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO7d_1_r6, color = "50-59")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO8d_1_r6, color = "60-69")) +
    geom_point(data=datDOa_1, aes(x=Dates,y=DO9d_1_r6, color = "70+"))
  
  gridExtra::grid.arrange(pH_0, pH_1, pDH_0, pDH_1, pDO_0, pDO_1, nrow = 3, ncol = 2)
  
  filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_AgeProfiles_rounded")
  svglite(paste0(filenamepath,".svg")); 
  gridExtra::grid.arrange(pH_0, pH_1, pDH_0, pDH_1, pDO_0, pDO_1, nrow = 3, ncol = 2)
  invisible(dev.off())
  
}

##summary in text file
#10
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


#pdf filenamepathAll
if (pset$iplatform<2) {dev.off()} #dev.off()



