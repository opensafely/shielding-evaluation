
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
### number of estimated parameters
if(pset$iplatform==0) {
   thetaTrue = c(pars$rEI, pars$rIR, pars$R0, pars$pE0, pars$pdm, dev0);
   npar = length(thetaTrue)}
if (pset$iplatform>0) {
   npar = 6
   ## Compile & run model (weekly points)
   sourceCpp(file = paste0(input_dir,"/",pset$File_model_choice))  
  }
### Multiple model options
if (pset$imodel==1) {model <- SEIR}
if (pset$imodel==2) {model <- SEIUHRD; sd2osd1 = mean(wd)/mean(zd)}

### Likelihood in R and model in Rcpp
sdUpper = 1            #p #binomial likelihood, and data
sdUpper = 0.4*mean(zd) #sd  #normal likelihood, any data

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
  pars$pdm = theta[5]
  #p        = theta[5] #200#         #auxiliary parameter for bin or NB noise
  sdH      = theta[6]                #auxiliary parameter for normal noise  
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
  muD = pars$pdm*MeanD       #MeanD #
  sdD = sd2osd1*sdH
  #Negative binomial likelihood - product over weeks
    #return( sum(dnbinom(x = zd, size = 1/p, mu = mu, log=T))) #expect k=1/p=200=> p=0.005
  #Normal likelihood - product over weeks
    return(sum(dnorm(zd, mean = muH, sd = sdH, log = T))+  sum(dnorm(wd, mean = muD, sd = sdD, log = T)))
}

## Likelihood definition, parameter ranges
niter = 200000 #30000 #120000 #90000 #60000 #150000 #30000 #50000 #40000
if (pset$imodel==1) {
  LogLikelihood = LogLikelihood1; Lower=c(1,1,1,1,1)*0.0001;   Upper = c(1,1,30,1,sdUpper)   } else {
  LogLikelihood = LogLikelihood2; Lower=c(1,1,1,1,1,1)*0.0001; Upper = c(1,1,30,1,2,sdUpper)} #rEI, rIR, R0, pE0, pdm, sd or p
setup    = createBayesianSetup(likelihood=LogLikelihood, lower = Lower, upper = Upper) #parallel = T,
settings = list (iterations = niter, burnin = round(niter*npar/15), message=F)
## Bayesian sample
tout1 <- system.time(out <- runMCMC(bayesianSetup=setup, settings=settings) )
## summary
if (pset$iplatform==0) {
  mT <- model(pars) #out.df parameters including contacts (data fit), all parameters (simulation)
  tout1
  round(thetaTrue,3)
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

#binomial fit to binomial data or normal fit to normal data
if(pset$iplatform>0) {
   pbin = 1/pars$k;             #for neg binomial data 
   dev0 = 1 }                   #for normal H likelihood - in simulation: #round(0.05*mean(zm)) 
#Expected parameter estimates
thetaTrue = c(pars$rEI, pars$rIR, pars$R0, pars$pE0, pars$pdm, dev0); #

##sd of normal likelihood for normal data
#dev = round(0.05*mean(zd))
##sd of normal likelihood for NB data
if (pset$imodel==1) {
  mEmean = mean(mE$Iw)} else { 
  mEmean = mean(mE$Hw)} 
dev = sqrt(mEmean + mEmean^2*pbin)
thetaTrue[length(thetaTrue)] = dev

N  = pars$Npop
rE = parsE$pdm #1#
rM = pars$pdm  #1#
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
                 PIwe   = rE* mE$Iw[iseq]/N,   #treat PIwe (PHwe) as mu (muH) = pdm*Mean
                 DataIw = zd/N) 
  if (pset$iplatform==0) {
  dat  <- tibble(dat, 
                 PIw    = rM* datM$PIw[iseq]) }
    
 } else if (pset$imodel==2) {
  datH <- tibble(Weeks  = mE$time[iseqH]/7,  
                 PHwe   =     mE$Hw[iseqH]/N,  
                 DataHw = zd/N)
  datD <- tibble(Weeks  = mE$time[iseqD]/7,  
                 PDwe   = rE* mE$Dw[iseqD]/N,  
                 DataDw = wd/N)                

  if (pset$iplatform==0) {
  datH <- tibble(datH, 
                 PHw    = datM$PHw[iseqH])
  datD <- tibble(datD, 
                 PDw    = datM$PDw[iseqD])    }
  }



## Plots
pdf(file = paste0(output_dir,"/",pset$File_fit_output))
par(mfrow = c(1,2))
marginalPlot(out)
par(mar = c(1, 1, 1, 1))
plot(out)
correlationPlot(out)

colors <- c("DIw"     = "black", "PIw_est" = "red",   "PIw_mod" = "green",
            "DHw"     = "black", "PHw_est" = "red",   "PHw_mod" = "green",
            "DDw"     = "grey", "PDw_est" = "orange","PDw_mod" = "blue")

par(mfrow = c(1,2))

if (pset$imodel==1) {
    p1 <- ggplot(dat, aes(x= Weeks)) +
          labs(x = 'Weeks', y = 'Infections per week', color = "Legend") + 
          scale_color_manual(values = colors) +
          geom_point( aes(y=DataIw,   color = "DIw")) +
          geom_line ( aes(y=PIwe,     color = "PIw_est")) +
    if (pset$iplatform==0) {
    p1 <- p1 + geom_line (aes(x=Weeks, y=PIw,  color = "PIw_mod"))  }          
}
  
if(pset$imodel==2) {
    p1 <- ggplot() +
          labs(x = 'Weeks', y = 'hospitalisations & deaths per week', color = "Legend") + 
          scale_color_manual(values = colors) +
          geom_point(data=datH, aes(x=Weeks,y=DataHw, color = "DHw"), size = 1.4, pch = 19) +
          geom_line (data=datH, aes(x=Weeks,y=PHwe,   color = "PHw_est")) +
          geom_point(data=datD, aes(x=Weeks,y=DataDw, color = "DDw"), size =   1,   pch = 16) +
          geom_line (data=datD, aes(x=Weeks,y=PDwe,   color = "PDw_est"))
    if (pset$iplatform==0) {
    p1 <- p1 + geom_line(data=datH, aes(x=Weeks,y=PHw, color = "PHw_mod")) +
               geom_line(data=datD, aes(x=Weeks,y=PDw, color = "PDw_mod"))  }
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
if (pset$imodel==1) {
  print(paste0("#data pts fitted: ", length(iweeksmodel))) 
} else if (pset$imodel==2) {
  if(pset$iplatform==0) { iwH=iweeksmodel; iwD=iweeksmodel} else {iwH=iweeksmodelH; iwD=iweeksmodelD}
  print(paste0("#H data pts fitted: ", length(iwH)))
  print(paste0("#D data pts fitted: ", length(iwD))) }
print(summary(out)); cat("\n")
if(pset$iplatform==0) print(paste0("Expected: ", c("rEI = ","rIR = ", "R0 = ", "pE0 = ", "pdm = ", "var = "), round(thetaTrue,3)))
#if(pset$iplatform==0) print(paste0("Expected: ", c("rEI = ","rIR = ", "R0 = ", "pE0 = ", "var = "), round(thetaTrue,3)))
print(paste0("Mean by chain and parameter:"))
print(out$X)
print(paste0("Time used (sec):"))
print(tout1[[3]])
print(names(out[[1]]))
print(names(out[[2]]))
cat("\n"); cat("\n")
sink()




######## 2 Basic Metropolis Hastings MCMC ####
