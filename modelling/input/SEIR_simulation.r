## SEIR or SEIUHRD model

## Compile & run model (weekly points)
sourceCpp(file = paste0(input_dir,"/",pset$File_model_choice))

if (pset$imodel==1){ t1<- system.time( out.df <- SEIR(pars) )}
if (pset$imodel==2){ t1<- system.time( out.df <- SEIUHRD(pars) )}

## Test Rcpp code:
t1
print("should be zero...")
sum(cm_weekmean- out.df$cmdtmean)

## output model results
sink(file = paste0(output_dir,"/",pset$File_run), append=TRUE, split=FALSE)
  print("Model run time")
  t1
  cat("\n")
  print("Test Rcpp code - should be zero...")
  sum(cm_weekmean- out.df$cmdtmean)
  cat("\n \n")
sink()


## Simulated data

#### True model each week
#### ym = infected, zm = incidence, wm = incidence
if (pset$imodel==1){ ym = out.df$It;           zm = out.df$Iw}
if (pset$imodel==2){ ym = out.df$It+out.df$Ut; zm = out.df$Hw; wm = out.df$Dw}

#### True parameters - to be estimated
thetaTrue = c(pars$rEI, pars$rIR, pars$R0, pars$pE0, pars$pdm, round(0.05*mean(ym))); #sd of noise

#### Simulated Data each week
set.seed(7777)    ### for reproducibility
yd = ym + rnorm(n=nd,mean=0,sd=thetaTrue[length(thetaTrue)])
zd = zm + rnorm(n=nd,mean=0,sd=thetaTrue[length(thetaTrue)])

for (i in seq_along(yd)) {yd[i] = max(0,yd[i]); zd[i] = max(0,zd[i])} #truncate to zero at low end

#### Plot model and R0
datM <- tibble(Weeks = out.df$time, 
               PInf = ym/pars$Npop, Datay = yd/pars$Npop, 
               PSus = out.df$St/pars$Npop, 
               PRec = out.df$Rt/pars$Npop, 
               R0   = R0_week,
               PIw  = zm/pars$Npop, Dataz = zd/pars$Npop,
               PRw  = out.df$Rw/pars$Npop,  
               Week1OfModel = Week1OfModel, 
               Week2OfModel = Week2OfModel) 

xx = quantile(datM$Weeks,0.6)[[1]]; yy = max(datM$PInf)*0.9; y2 = max(datM$PInf)*0.6;

p1  <- ggplot(datM, aes(x = Weeks)) +
    geom_line (aes(y = PInf), col = 'red') +
    geom_line (aes(y = PRec/100), col = 'green') +
    geom_line (aes(y = PSus/100), col = 'blue') +
    geom_point(aes(y = Datay), size = 1.2, pch = 1) +
    annotate("text", size=3, x=xx, y=yy, label= paste0("beta=",round(pars$beta,3),", R0unif=",round(pars$R0,2),", R0=",round(max(R0_week),2))) +
    annotate("text", size=3, x=xx, y=y2, label= paste0("pinf2(log10)=",round(pars$logPI0,2),", IP=",round(1/pars$rIR,2))) +
    labs(x = 'Weeks', y = 'PInfected') + #scale_y_continuous(trans = "log10") +
    if (pset$imodel==2){geom_line(aes(y =out.df$Ht/Npop),col="blue")} #[id]/Npop),col="blue")}

p2 <-  ggplot(datM, aes(x= Weeks)) +
    geom_point(aes(y=R0)) +
    geom_line (aes(y=rep(1,nd)),col='red') +
    labs(x = 'Weeks', y = 'R0') 

p3 <- ggplot(datM, aes(x = Weeks)) +
    geom_point(aes(y = PIw), col = 'red') +
    geom_line (aes(y = PIw), col = 'red') +
    geom_line (aes(y = PInf), col = 'blue') +
    geom_point(aes(y = PRw), col = 'green') +
    geom_line (aes(y = PRw), col = 'green') +
    labs(x = 'Weeks', y = 'Pinf, Prec')

if (pset$iplatform==0) {
  gridExtra::grid.arrange(p1, p2, p3, nrow = 3) 
}

pdf(file = paste0(output_dir,"/",pset$File_model_plots))
  gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
dev.off()

#### Output simulated data
if (pset$DOfit==1) {
  datout <- data.frame(Weeks=datM$Weeks, DataInfected=yd, DataInfectedPerWeek=zd)
  write.csv(datout, row.names = T, file = paste0(output_dir,"/",pset$File_model_simulation))
}