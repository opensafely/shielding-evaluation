
#Time range (week-dates) and subsets of datasets for model fitting
if (pset$iplatform==0) {
  iweeksmodel = seq_along(datM$Weeks)
  iweeksdata  = iweeksmodel
  #Data within the time range
  zd = datM$Dataz[iweeksdata]     #weekly incidence in model
  if (pset$imodel==2) {
  wd = datM$Dataw[iweeksdata] 
  print(paste0("#H data pts fitted: ", length(iweeksmodel)))
  print(paste0("#D data pts fitted: ", length(iweeksmodel))) 
  } else { 
  print(paste0("#data pts fitted: ", length(iweeksmodel))) }

} else {
  if (pset$iplatform==1) Week2ofStudy = Week2OfModel #Dummy data: use max of contact matrix range
  if (pset$iplatform==2) Week2ofStudy = "2020-12-01" #Real data: use start of vacc & alpha
  if (pset$imodel==1) {
    Week1OfFit  = max(c(unique(datD$Week1OfData),  Week1OfModel)) #Week#Ofmodel from SEIR_contacts
    Week2OfFit  = min(c(unique(datD$Week2OfData),  Week2OfModel, Week2ofStudy)) } else {
    Week1OfFitH = max(c(unique(datDH$Week1OfData), Week1OfModel)) #Week#Ofmodel from SEIR_contcats
    Week2OfFitH = min(c(unique(datDH$Week2OfData), Week2OfModel, Week2ofStudy))
    Week1OfFitD = max(c(unique(datDD$Week1OfData), Week1OfModel))
    Week2OfFitD = min(c(unique(datDD$Week2OfData), Week2OfModel, Week2ofStudy)) }
  #Dates within the time range
  if (pset$imodel==1) {
    iweeksdata  = which(Week1OfFit  <= datDH$Dates & datDH$Dates <= Week2OfFitH)
    Dates = datDH$Dates[iweeksdataH] } else {
    iweeksdataH = which(Week1OfFitH <= datDH$Dates & datDH$Dates <= Week2OfFitH)
    iweeksdataD = which(Week1OfFitD <= datDD$Dates & datDD$Dates <= Week2OfFitD)
    DatesH = datDH$Dates[iweeksdataH]
    DatesD = datDD$Dates[iweeksdataD] }
  #Data within the time range
  if(pset$imodel==1) {
    zd = datD$Dataz [iweeksdata]  } else {
    zd = datDH$Dataz[iweeksdataH]           #weekly incidence of hospital admissions 
    wd = datDD$Dataw[iweeksdataD] }         #weekly incidence of deaths  

  #convert Dates-weeks to model-weeks (indices 1:52) counted from start week of model
  #=> used in likelihood
  if(pset$imodel==1) {
    iweeksmodel  = ceiling(as.numeric(difftime(Dates,  Week1OfModel, units = "weeks")))
    print(paste0("#data pts fitted: ", length(iweeksmodel)))
  } else if (pset$imodel==2) {
    iweeksmodelH = ceiling(as.numeric(difftime(DatesH, Week1OfModel, units = "weeks")))
    iweeksmodelD = ceiling(as.numeric(difftime(DatesD, Week1OfModel, units = "weeks")))
    print(paste0("#H data pts fitted: ", length(iweeksmodelH)))
    print(paste0("#D data pts fitted: ", length(iweeksmodelD)))     }

  } #iplatform #Time range 


######## 1 BayesianTools #####################
### reproducibility of MCMC sample
set.seed(7777)

if (pset$iplatform>0) {
   ## Compile & run model (weekly points)
   sourceCpp(file = paste0(input_dir,"/",pset$File_model_choice))  
  }
### Multiple model options
if (pset$imodel==1) {model <- SEIR}
if (pset$imodel==2) {model <- SEIUHRD} #; sd2osd1 = mean(wd)/mean(zd)}

### Likelihood in R and model in Rcpp
sdUpper = 1            #p   #binomial likelihood, and data
sdUpper  = 0.4*mean(zd) #sd  #normal likelihood, any data
if (pset$imodel==2) {
sdUpper2 = 0.4*mean(wd)}#sd  #normal likelihood, any data

#Model 1
LogLikelihood1 <- function(theta){
  #Proposed
  pars$rEI = theta[1] 
  pars$rIR = theta[2] 
  pars$R0  = theta[3] 
  pars$pE0 = theta[4] 
  #pars$pdm = theta[5]
  #p        = theta[5] #200#         #auxiliary parameter for bin or NB noise
  sd       = theta[5]                #auxiliary parameter for normal noise
  #Dependent
  pars$Ea0 = pars$Na0*pars$pE0
  pars$Sa0 = pars$Na0-pars$Ra0-pars$Ea0-pars$Ia0
  pars$beta= pars$R0*(pars$rIR+pars$d)/pars$u_mean
  #Model in Rcpp with proposed parameters
  m <- model(pars)
  #likelihood of (weekly) data
  if (pset$iplatform==0) {
    Mean=m$Iw              } else {
    Mean=m$Iw[iweeksmodel] }; 
  if (Mean[1]==0) Mean[1]=1; #avoid NAs
  mu = Mean  #pars$pdm*Mean
  #Negative binomial likelihood - product over weeks
    #return( sum(dnbinom(x = zd, size = 1/p, mu = mu, log=T))) #expect k=1/p=200=> p=0.005
  #Normal likelihood - product over weeks
    return(sum(dnorm(zd, mean = mu, sd = sd, log = T)))
}

#Model 2
source(file = paste0(input_dir,"/SEIUHRD_BETA.r")) #Used within Likelihood2
LogLikelihood2 <- function(theta){
  #Proposed
  pars$rEI = theta[1] 
  pars$rIR = theta[2] 
  pars$R0  = theta[3] 
  pars$pE0 = theta[4] 
  #pars$pdm = theta[5]
  #p        = theta[5] #200#         #auxiliary parameter for bin or NB noise
  sdH      = theta[5]                #auxiliary parameter for normal noise  
  sdD      = theta[6]                #auxiliary parameter for normal noise
  #Dependent
  pars$Ea0 = pars$Na0*pars$pE0
  pars$Sa0 = pars$Na0-pars$Ra0-pars$Ea0-pars$Ia0
  pars$beta= BETA(pars)
  #Model in Rcpp with proposed parameters
  m <- model(pars)
  #likelihood of (weekly) data
  if (pset$iplatform==0) {
    MeanH = m$Hw; 
    MeanD = m$Dw;               } else {
    MeanH = m$Hw[iweeksmodelH]; 
    MeanD = m$Dw[iweeksmodelD]  }; 
  MeanH[1]= max(MeanH[1],1); #avoid NAs
  MeanD[1]= max(MeanD[1],1); #avoid NAs
  muH = MeanH                #pars$pdm*MeanH #
  muD = MeanD #pars$pdm*MeanD       #
  #sdD = sd2osd1*sdH
  #Negative binomial likelihood - product over weeks
    #return( sum(dnbinom(x = zd, size = 1/p, mu = mu, log=T))) #expect k=1/p=200=> p=0.005
  #Normal likelihood - product over weeks
    return(sum(dnorm(zd, mean = muH, sd = sdH, log = T))+  sum(dnorm(wd, mean = muD, sd = sdD, log = T)))
}

## Likelihood definition, parameter ranges
niter = 30000 #200000 #30000 #120000 #90000 #60000 #150000 #30000 #50000 #40000
if (pset$imodel==1) {
  LogLikelihood = LogLikelihood1; Lower=c(1,1,1,1,1)*0.0001; Upper = c(1,1,30,1,sdUpper)   } else {
  LogLikelihood = LogLikelihood2; Lower=c(1,1,1,1,1,1)*0.0001; Upper = c(1,1,30,1,sdUpper,sdUpper2)} #rEI, rIR, R0, pE0, sd or p# pdm,
setup    = createBayesianSetup(likelihood=LogLikelihood, lower = Lower, upper = Upper) #parallel = T,
settings = list (iterations = niter, burnin = round(niter*length(Lower)/15), message=F)
## Bayesian sample
tout1 <- system.time(out <- runMCMC(bayesianSetup=setup, settings=settings) )
## Estimates
parsE     <- pars
MAPE      <- MAP(out)
parsE$rEI <- MAPE$parametersMAP[1]
parsE$rIR <- MAPE$parametersMAP[2]
parsE$R0  <- MAPE$parametersMAP[3]
parsE$pE0 <- MAPE$parametersMAP[4]
#parsE$pdm <- MAPE$parametersMAP[5]
mE        <- model(parsE)

#binomial fit or normal fit
if(pset$iplatform>0) {
   pbin = 1/pars$k;             #for neg binomial data 
   dev0 = 1          }          #for normal likelihood
#Expected parameter estimates
thetaTrue = c(pars$rEI, pars$rIR, pars$R0, pars$pE0, dev0, dev0); #pars$pdm, 


##sd of normal likelihood if normal data
  #dev = round(0.05*mean(zd))
##sd of normal likelihood for NB data
if (pset$imodel==1) {
  mEm = mean(mE$Iw)} else { 
  mEm = mean(mE$Hw); mEm2 = mean(mE$Hw);} 
dev  = sqrt(mEm  + mEm^2*pbin)
dev2 = sqrt(mEm2 + mEm2^2*pbin)
thetaTrue[c(length(thetaTrue)-1,length(thetaTrue))] = c(dev,dev2)

if (!is.element(pset$iplatform,1)) {
  kest_mdata  = round(1/((var(zd) - mean(zd))/(mean(zd)^2)),1); 
  kest_mmodel = round(1/((var(zd) - mEm)/(mEm^2)),1); 
  print(paste0("k_est (data mean)  = ", kest_mdata))
  print(paste0("k_est (model mean) = ", kest_mmodel))
}

N  = pars$Npop
rE = 1#parsE$pdm #
rM = 1#pars$pdm  #
if(pset$iplatform==0) { 
  iseq  = seq_along(mE$time) 
  iseqH = iseq
  iseqD = iseq                    } else { 
  if (pset$imodel==1) {
  iseq = iweeksmodel  } else { 
  iseqH= iweeksmodelH 
  iseqD= iweeksmodelD }           }


if (pset$imodel==1) { 
  dat  <- tibble(Weeks  = mE$time[iseq]/7,
                 Iwe    = rE* mE$Iw[iseq],   #treat PIwe (PHwe) as mu (muH) = pdm*Mean
                 DataIw = zd) 
  if (pset$iplatform==0) {
  dat  <- tibble(dat, 
                 Iw     = rM* datM$Iw[iseq]) }
    
 } else if (pset$imodel==2) {
  datH <- tibble(Weeks  = mE$time[iseqH]/7,  
                 Hwe    =     mE$Hw[iseqH],  
                 DataHw = zd)
  datD <- tibble(Weeks  = mE$time[iseqD]/7,  
                 Dwe    = rE* mE$Dw[iseqD],  
                 DataDw = wd)

  if (pset$iplatform==0) {
  datH <- tibble(datH, 
                 Hw    = datM$Hw[iseqH])
  datD <- tibble(datD, 
                 Dw    = datM$Dw[iseqD])    }
  }



## Plots
pdf(file = paste0(output_dir,"/",pset$File_fit_output))
par(mfrow = c(1,2))
marginalPlot(out)
par(mar = c(1, 1, 1, 1))
plot(out)
correlationPlot(out)

colors <- c("DIw"     = "black", "Iw_est" = "red",   "Iw_mod" = "green",
            "DHw"     = "black", "Hw_est" = "red",   "Hw_mod" = "green",
            "DDw"     = "grey",  "Dw_est" = "orange","Dw_mod" = "blue")

par(mfrow = c(1,2))

if (pset$imodel==1) {
    p1 <- ggplot(dat, aes(x= Weeks)) +
          labs(x = 'Weeks', y = 'Infections per week', color = "Legend") + 
          scale_color_manual(values = colors) +
          geom_point( aes(y=DataIw,   color = "DIw")) +
          geom_line ( aes(y=Iwe,      color = "Iw_est")) +
    if (pset$iplatform==0) {
    p1 <- p1 + geom_line (aes(x=Weeks, y=Iw,  color = "Iw_mod"))  }          
}
  
if(pset$imodel==2) {
    p1 <- ggplot() +
          labs(x = 'Weeks', y = 'hospitalisations & deaths per week', color = "Legend") + 
          scale_color_manual(values = colors) +
          geom_point(data=datH, aes(x=Weeks,y=DataHw, color = "DHw"), size = 1.4, pch = 19) +
          geom_line (data=datH, aes(x=Weeks,y=Hwe,    color = "Hw_est")) +
          geom_point(data=datD, aes(x=Weeks,y=DataDw, color = "DDw"), size =   1,   pch = 16) +
          geom_line (data=datD, aes(x=Weeks,y=Dwe,    color = "Dw_est"))
    if (pset$iplatform==0) {
    p1 <- p1 + geom_line(data=datH, aes(x=Weeks,y=Hw, color = "Hw_mod")) +
               geom_line(data=datD, aes(x=Weeks,y=Dw, color = "Dw_mod"))  }
}
print(p1)

dev.off()

## pdf: data frame
pdf(file = paste0(output_dir,"/",pset$File_fit_variables), height=nrow(mE)/3)
if (pset$imodel==1) {gridExtra::grid.table(round(mE[c("time","St","It","Iw","Rt"     )])) }
if (pset$imodel==2) {gridExtra::grid.table(round(mE[c("time","St","Ht","Hw","Dt","Dw")])) }
dev.off()

pdf(file = paste0(output_dir,"/",pset$File_fit_data1), height=nrow(datDH)/3)
if (pset$imodel==1) {
  gridExtra::grid.table(datD[c("Weeks","Dataz")]) }
if (pset$imodel==2) {
  gridExtra::grid.table(datDH[c("Weeks","Dataz")]) }
dev.off()

if (pset$imodel==2) {
pdf(file = paste0(output_dir,"/",pset$File_fit_data2), height=nrow(datDD)/3)
  gridExtra::grid.table(datDD[c("Weeks","Dataw")])
dev.off() }

## Summary - output
sink(file = paste0(output_dir,"/",pset$File_fit_summary),append=TRUE,split=FALSE)
cat("\n"); 
if (pset$imodel==1) {
  print(paste0("#data pts fitted: ", length(iweeksmodel))) 
} else if (pset$imodel==2) {
  if(pset$iplatform==0) { iwH=iweeksmodel; iwD=iweeksmodel} else {iwH=iweeksmodelH; iwD=iweeksmodelD}
  print(paste0("#H data pts fitted: ", length(iwH)))
  print(paste0("#D data pts fitted: ", length(iwD))) }
print(paste0("Expected: ", c("rEI = ","rIR = ", "R0 = ", "pE0 = ", "varH = ", "varD = "), round(thetaTrue,3))) #"pdm = ",
cat("\n");
print(summary(out)); cat("\n")
print(paste0("Mean by chain and parameter:"))
print(out$X)
print(paste0("Time used (sec):"))
print(tout1[[3]])
print(names(out[[1]]))
print(names(out[[2]]))
cat("\n")
print(paste0("k_est (data mean)  = ", kest_mdata))
print(paste0("k_est (model mean) = ", kest_mmodel))
cat("\n"); cat("\n")
sink()



######## 2 Basic Metropolis Hastings MCMC ####
