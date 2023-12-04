
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

} else { #if iplatform>0
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
### model options
model <- SEIUHRD

### Likelihood in R and model in Rcpp
R0Max    = 10
sdMax    = sd(zd)
sdMax2   = sd(wd)
pkLower  = 0.1       #k  NB likelihood and data
pkLower2 = 0.1
pkUpper  = 5
pkUpper2 = 5
tMax     = 10
pE0Max   = 0.10
pE0Min   = 0.005    #pop = 120k
logpE0Max = log(pE0Max)
pdmMin = 0.1
pdmMax = 2

#Model 2
source(file = paste0(input_dir,"/SEIUHRD_BETA.r")) #Used within Likelihood2
LogLikelihood2 <- function(theta){
  #Proposed
  tEI = theta[1]*tMax
  tIR = theta[2]*tMax
  pars$rEI = 1/tEI
  pars$rIR = 1/tIR 
  pars$R0  = exp(theta[3])#*logR0Max)#*R0Max 
  pars$pE0 = exp(-theta[4] + logpE0Max) # exp(-r+logpE0Max), r=log(pE0Max/pE0)= 0,3 <= pE0=Max,0.005 (120k pop), fpr Max=0.1
  #pars$pdm = exp(+theta[5]) #pdm=0.1,2 - theta=0,2.3
  pars$ad  = theta[5]
  #sdHScaled  = theta[6]                #auxiliary parameter for normal noise  
  #sdDScaled  = theta[7]                #auxiliary parameter for normal noise
  pkH       = theta[6]; #kempir_mmodel #pars$k; #sdH; 
  pkD       = theta[7]; #kempir_mmodel2 #pars$k; #sdD;
  kH=1/(pkH*pkH) # pk = 1/sqrt(k) => k = 1/pk^2
  kD=1/(pkD*pkD)
  #Dependent
  pars$Ea0 = pars$Na0*pars$pE0
  pars$Sa0 = pars$Na0-pars$Ra0-pars$Ea0-pars$Ia0
  pars$beta= BETA(pars)
  #Model in Rcpp with proposed parameters
  m <- model(pars)
  #likelihood of (weekly) data
  if (pset$iplatform==0) {
    MeanH = m$byw$Hw; 
    MeanD = m$byw$Dw;               } else {
    MeanH = m$byw$Hw[iweeksmodelH]; 
    MeanD = m$byw$Dw[iweeksmodelD]  }; 
  MeanH[1]= max(MeanH[1],1); #avoid NAs
  MeanD[1]= max(MeanD[1],1); #avoid NAs
  muH = MeanH                #pars$pdm*MeanH #
  muD = MeanD #pars$pdm*MeanD 
  #Negative binomial likelihood - product over weeks
    return( sum(dnbinom(x = zd, size = kH, mu = muH, log = T)) +
            sum(dnbinom(x = wd, size = kD, mu = muD, log = T)))
  #Normal likelihood - product over weeks
    #return(sum(dnorm(zd, mean = muH, sd = sdHScaled*sdMax,  log = T)) 
    #    +  sum(dnorm(wd, mean = muD, sd = sdDScaled*sdMax2, log = T)))
}

## Likelihood definition, parameter ranges
niter = 200000 #30000 #120000 #90000 #60000 #150000 #30000 #50000 #40000
LOWER = c(c(1,1)/tMax,0,        0,                  0, pkLower, pkLower2); #log(pdmMin), 
UPPER = c(1,1,        log(R0Max),log(pE0Max/pE0Min),1, pkUpper, pkUpper2); #log(pdmMax), 
LogLikelihood = LogLikelihood2; #rEI, rIR, R0, pE0, pdm/al, sd, sd2

#Beta priors
#PRIOR <- createBetaPrior(3,3,lower = LOWER, upper = UPPER)
#PRIOR <- createBetaPrior(4,5,lower = LOWER, upper = UPPER)
PRIOR <- createBetaPrior(3,4,lower = LOWER, upper = UPPER)
setup    = createBayesianSetup(likelihood=LogLikelihood, prior =PRIOR) #parallel = T,
#Normal priors
#PRIOR <- createTruncatedNormalPrior(0,.5,lower = LOWER, upper = UPPER)
#setup    = createBayesianSetup(likelihood=LogLikelihood, prior =PRIOR) #parallel = T,
#Uniform priors
   #setup    = createBayesianSetup(likelihood=LogLikelihood, lower = LOWER, upper = UPPER) #parallel = T,
   PARSTART = 0.5*UPPER
   nchain = 3
#settings = list (startValue=t(array(PARSTART,dim=c(length(PARSTART),nchain))), iterations = niter, burnin = round(niter*length(LOWER)/15), message=T) #F)
settings = list (iterations = niter, burnin = round(niter*length(LOWER)/15), message=T) #F)
## Bayesian sample
tout1 <- system.time(out <- runMCMC(bayesianSetup=setup, settings=settings) )


Resample=0
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

Restart=0
if (Restart==1){
## Restart same sampling
#tout2 <- system.time(out2 <- runMCMC(bayesianSetup=setup, settings=settings) )
  out_1 <- out
  out   <- out2
tout3 <- system.time(out3 <- runMCMC(bayesianSetup=setup, settings=settings) )
  out_2 <- out
  out   <- out3
}

## MAP Estimates
parsE     <- pars
MAPE      <- MAP(out)
parsE$rEI <- 1/(tMax*MAPE$parametersMAP[1])
parsE$rIR <- 1/(tMax*MAPE$parametersMAP[2])
parsE$R0  <- exp(MAPE$parametersMAP[3])#*logR0Max)#*R0Max
parsE$pE0 <- exp(-MAPE$parametersMAP[4] + logpE0Max)
parsE$ad  <- MAPE$parametersMAP[5]
#parsE$pdm <- exp(MAPE$parametersMAP[5])
#dependent
parsE$Ea0 = parsE$Na0*parsE$pE0
parsE$Sa0 = parsE$Na0-parsE$Ra0-parsE$Ea0-parsE$Ia0
parsE$beta= BETA(parsE)
mE        <- model(parsE)

## UNCERTAINTY
## Sample the chains
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
Weeks     = seq_along(zd) #length(mE$byw$time)
iweeksz   = Weeks
npar      = length(LOWER)
nsample   = 1000#500#1000#3000;
psample = getSample(out, parametersOnly = T, numSamples = nsample, start=(niter/3)/3) #parametersOnly = F
# run model for each parameter set in the sample
zsample = matrix(0,length(Weeks),nsample)
wsample = matrix(0,length(Weeks),nsample)
parsES  = pars
for(i in 1:nsample){
  parsES$rEI <- 1/(tMax*as.vector(psample[i,1]))
  parsES$rIR <- 1/(tMax*as.vector(psample[i,2]))
  parsES$R0  <- exp(as.vector(psample[i,3]))#*logR0Max)#*R0Max
  parsES$pE0 <- exp(-as.vector(psample[i,4]) + logpE0Max)
  #parsES$pdm <- exp(as.vector(psample[i,5]))
  parsES$ad  <- as.vector(psample[i,5])
  #dependent
  parsES$Ea0 = parsES$Na0*parsES$pE0
  parsES$Sa0 = parsES$Na0-parsES$Ra0-parsES$Ea0-parsES$Ia0
  parsES$beta= BETA(parsES)
  outs        = model(as.vector(parsES))
  zsample[,i] = outs$byw$Hw[iweeksz]
  wsample[,i] = outs$byw$Dw[iweeksz]#/parsES$pdm
} }


#True parameters (except last two)
thetaTrue = c(pars$rEI, pars$rIR, pars$R0, pars$pE0, pars$ad, 1, 1); #pars$phm, 1, 1); 


#Expected parameters

##Normal data - Normal residual likelihood
##sd empirical
  #dev = round(0.05*mean(zd))

##NB data
##k empirical
if (pset$imodel==1) {
  mEm = mean(mE$byw$Iw) } else { 
  mEm = mean(mE$byw$Hw); mEm2 = mean(mE$byw$Dw) }

if (!is.element(pset$iplatform,1)) { #cant estimate  with dummy data (1)
  kempir_mdata  = round(1/((var(zd) - mean(zd))/(mean(zd)^2)),1); 
  kempir_mmodel = round(1/((var(zd) - mEm)/(mEm^2)),1);
  print(paste0("k_empirical (data mean)  = ", kempir_mdata))
  print(paste0("k_empirical (model mean) = ", kempir_mmodel))
  if (pset$imodel==2) {
    kempir_mdata2  = round(1/((var(wd) - mean(wd))/(mean(wd)^2)),1); 
    kempir_mmodel2 = round(1/((var(wd) - mEm2)/(mEm2^2)),1);
    print(paste0("k_empiricalD (data mean)  = ", kempir_mdata2))
    print(paste0("k_empiricalD (model mean) = ", kempir_mmodel2)) }
} else {kempir_mmodel=1; kempir_mmodel2=1; kempir_mdata =1; kempir_mdata2=1;}

##NB data - Normal residual likelihood
##sd empirical 
dev  = sqrt(mEm  + mEm^2*abs(kempir_mmodel)) #abs() to tackle dummy data negative k
if (pset$imodel==2) {
dev2 = sqrt(mEm2 + mEm2^2*abs(kempir_mmodel2)) }
thetaTrue[c(length(thetaTrue)-1,length(thetaTrue))] = c(dev,dev2) 

##NB data - NB likelihood
if(pset$iplatform==0){ #simulation: true parameters
  thetaTrue[c(length(thetaTrue)-1,length(thetaTrue))] = c(pars$k,pars$k)  
  } else {             #not simulation
  thetaTrue[c(length(thetaTrue)-1,length(thetaTrue))] = c(kempir_mdata, kempir_mdata2) }
    
## Summary - output
sink(file = paste0(output_dir,"/",pset$File_fit_summary),append=FALSE,split=FALSE) #append=TRUE,split=FALSE)
cat("\n"); 
if (pset$imodel==1) {
  print(paste0("#data pts fitted: ", length(iweeksmodel))) 
} else if (pset$imodel==2) {
  if(pset$iplatform==0) { iwH=iweeksmodel; iwD=iweeksmodel} else {iwH=iweeksmodelH; iwD=iweeksmodelD}
  print(paste0("#H data pts fitted: ", length(iwH)))
  print(paste0("#D data pts fitted: ", length(iwD))) }
print(paste0("Expected: ", c("rEI = ","rIR = ", "R0 = ", "pE0 = ", "pdm/ad =", "kH = ", "kD = "), round(thetaTrue,3))) #"pdm = ",
print(paste0("Likelihood NB. Estimated proportion deaths outside hospital = ", round(pars$ad/(1+pars$ad),3)))
cat("\n");
print(summary(out)); cat("\n")
print(paste0("Mean by chain and parameter:"))
print(out$X)
print(paste0("Time used (sec):"))
print(tout1[[3]])
print(names(out[[1]]))
print(names(out[[2]]))
cat("\n")
print(paste0("k_empirical (data mean)   = ", kempir_mdata))
print(paste0("k_empirical (model mean)  = ", kempir_mmodel))
if (pset$imodel==2) {
  print(paste0("k_empiricalD (data mean)  = ", kempir_mdata2))
  print(paste0("k_empiricalD (model mean) = ", kempir_mmodel2)) }

cat("\n"); cat("\n")
sink()
  

## Plots - dataframe
N  = pars$Npop
rE = 1#parsE$pdm #

if(pset$iplatform==0) { 
  iseq  = iweeksz #seq_along(mE$byw$time) 
  iseqH = iseq
  iseqD = iseq                    } else { 

  iseqH= iweeksmodelH #iweeksdataH ?
  iseqD= iweeksmodelD             }


  datH <- tibble(Weeks  = mE$byw$time[iseqH]/7,  
                 Hwe    =     mE$byw$Hw[iseqH],  #treat PIwe (PHwe) as mu (muH) = pdm*Mean
                 DataHw = zd)
  datD <- tibble(Weeks  = mE$byw$time[iseqD]/7,  
                 Dwe    = rE* mE$byw$Dw[iseqD],  
                 DataDw = wd)

  if (pset$iplatform==0) {
  datH <- tibble(datH, 
                 Hw    = datM$Hw[iseqH])
  datD <- tibble(datD, 
                 Dw    = datM$Dw[iseqD])    }


##Age profiles
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
  if(pset$imodel==2) {
    datHa <- tibble(Weeks = mE$byw$time[iseqH]/7, #[iseq]/7,
                    H1w   = mE$byw_age$H1w[iseqH],
                    H2w   = mE$byw_age$H2w[iseqH],
                    H3w   = mE$byw_age$H3w[iseqH],
                    H4w   = mE$byw_age$H4w[iseqH],
                    H5w   = mE$byw_age$H5w[iseqH],
                    H6w   = mE$byw_age$H6w[iseqH],
                    H7w   = mE$byw_age$H7w[iseqH],
                    H8w   = mE$byw_age$H8w[iseqH],
                    H9w   = mE$byw_age$H9w[iseqH])
    datDa <- tibble(Weeks = mE$byw$time[iseqD]/7, #[iseq]/7,
                    D1w   = mE$byw_age$D1w[iseqD],
                    D2w   = mE$byw_age$D2w[iseqD],
                    D3w   = mE$byw_age$D3w[iseqD],
                    D4w   = mE$byw_age$D4w[iseqD],
                    D5w   = mE$byw_age$D5w[iseqD],
                    D6w   = mE$byw_age$D6w[iseqD],
                    D7w   = mE$byw_age$D7w[iseqD],
                    D8w   = mE$byw_age$D8w[iseqD],
                    D9w   = mE$byw_age$D9w[iseqD])    }}

## pdf Plots
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

## posterior sample plot
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
par(mfrow = c(2,1))
##
matplot(Weeks,      zsample,       col="grey", type='l', xlab="Weeks", ylab="Hospitalisations per week") # sample trajectories
points (datH$Weeks, datH$DataHw,   col="black") # data
lines  (Weeks,      mE$byw$Hw[iweeksz], col='red') # MAP estimate
legend(max(Weeks)*0.7, max(zsample), legend=c("sample", "data", "MAP"),
       col=c("grey", "black", "red"), lty=1:2, cex=0.8)

#labels()
matplot(Weeks,      wsample,       col="grey", type='l', xlab="Weeks", ylab="Deaths per week", ylim=range(zsample)) # sample trajectories
points (datD$Weeks, datD$DataDw,   col="black") # data
lines  (Weeks,      mE$byw$Dw[iweeksz], col='red') # MAP estimate
legend(max(Weeks)*0.7, max(zsample), legend=c("sample", "data", "MAP"),
       col=c("grey", "black", "red"), lty=1:2, cex=0.8)
}

##age profiles
if (!is.element(pset$iplatform,1) & length(zd)==length(wd) ){
  if (pset$imodel==2){
    colors <- c("0-4" = 1, "5-11" = 2,  "12-17" = 3, "18-29" = 4, "30-39" = 5, 
                "40-49" = 6, "50-59" = 7,  "60-69" = 8, "70+" = 9)
    par(mfrow = c(2,1))
  p1 <- ggplot() +
    labs(x = 'Weeks', y = 'hospitalisations per week', color = "Legend") + 
    scale_color_manual(values = colors) +
    geom_line (data=datHa, aes(x=Weeks,y=H1w, color = "0-4")) +
    geom_line (data=datHa, aes(x=Weeks,y=H2w, color = "5-11")) +
    geom_line (data=datHa, aes(x=Weeks,y=H3w, color = "12-17")) +
    geom_line (data=datHa, aes(x=Weeks,y=H4w, color = "18-29")) +
    geom_line (data=datHa, aes(x=Weeks,y=H5w, color = "30-39")) +
    geom_line (data=datHa, aes(x=Weeks,y=H6w, color = "40-49")) +
    geom_line (data=datHa, aes(x=Weeks,y=H7w, color = "50-59")) +
    geom_line (data=datHa, aes(x=Weeks,y=H8w, color = "60-69")) +
    geom_line (data=datHa, aes(x=Weeks,y=H9w, color = "70+"))
  print(p1)
  p2 <- ggplot() +
    labs(x = 'Weeks', y = 'deaths per week', color = "Legend") + 
    scale_color_manual(values = colors) +
    geom_line (data=datDa, aes(x=Weeks,y=D1w, color = "0-4")) +
    geom_line (data=datDa, aes(x=Weeks,y=D2w, color = "5-11")) +
    geom_line (data=datDa, aes(x=Weeks,y=D3w, color = "12-17")) +
    geom_line (data=datDa, aes(x=Weeks,y=D4w, color = "18-29")) +
    geom_line (data=datDa, aes(x=Weeks,y=D5w, color = "30-39")) +
    geom_line (data=datDa, aes(x=Weeks,y=D6w, color = "40-49")) +
    geom_line (data=datDa, aes(x=Weeks,y=D7w, color = "50-59")) +
    geom_line (data=datDa, aes(x=Weeks,y=D8w, color = "60-69")) +
    geom_line (data=datDa, aes(x=Weeks,y=D9w, color = "70+"))
  print(p2)
}}

##summary in text file
txt=readLines(paste0(output_dir,"/",pset$File_fit_summary))
plot.new()
gridExtra::grid.table(txt, theme=ttheme_default(base_size = 5, padding = unit(c(1, 1),"mm") ))

dev.off()
##


## pdf: data frame
pdf(file = paste0(output_dir,"/",pset$File_fit_variables), height=nrow(mE$byw)/3)
if (pset$imodel==1) {gridExtra::grid.table(round(mE$byw[c("time","St","It","Iw","Rt"     )])) }
if (pset$imodel==2) {gridExtra::grid.table(round(mE$byw[c("time","St","Ht","Hw","Dt","Dw")])) }
dev.off()

if (pset$iplatform>0){
  
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

}



######## 2 Basic Metropolis Hastings MCMC ####
