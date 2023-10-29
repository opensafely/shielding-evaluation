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


## Simulate data

#### True model each week
#### ym = infected, zm = incidence, wm = incidence
if (pset$imodel==1){ ym = out.df$It;            zm = out.df$Iw};
if (pset$imodel==2){ ym = out.df$It+out.df$Ut;  zm = out.df$Hw;  wm = out.df$Dw;  if(wm[1]==0) wm[1]=1;}
if(zm[1]==0) zm[1]=1;

#### True parameters - to be estimated
pbin = 1/pars$k #1/200 #0.5  #for neg binomial data (size=k=200 Davies 2020 Nat Med) or binomial noise
sd   = round(0.05*mean(zm))  #for normal data
dev0 = sd #pbin #sd #noise parameter

#### Simulated Data each week
set.seed(777)#7)    ### for reproducibility
#zd = zm + rnorm(n=nd,mean=0,sd=sd)
zd = rnbinom(nd, size=1/pbin, mu=zm) #size=k=200 Davies Nat Med 2020
#for (i in seq_along(zd)) {zd[i] = max(1,zd[i])} #truncate at 1 at low end
if (pset$imodel==2){ wd = rnbinom(nd, size=1/pbin, mu=wm)  }
#if (pset$imodel==2){ for (i in seq_along(wd)) {wd[i] = max(1,wd[i])}  }

iz1=which(zd>0); iw1=which(wd>0);
print(paste0("mean zd/zm (zd>0):", mean(zd[iz1]/zm[iz1]) ))
print(paste0("mean wd/wm (wd>0):", mean(wd[iw1]/wm[iw1]) ))

plot  (iz1,zd[iz1]); lines(iz1,zd[iz1],col=1); points(iz1,zm[iz1],col=2)
points(1:nd,wd); lines(1:nd,wd,col=1); lines(1:nd,wm,col=3)



#### Plot model and R0
N  <- pars$Npop
datM <- tibble( Weeks = out.df$time, 
                PInf  = ym/N, 
                PSus  = out.df$St/N, 
                PRec  = out.df$Rt/N, 
                R0    = R0_week, 
                Week1OfModel = Week1OfModel, 
                Week2OfModel = Week2OfModel )

xx <- quantile(datM$Weeks,0.6)[[1]]; yy = max(datM$PInf)*0.9; y2 = max(datM$PInf)*0.6;
colors <- c("Pinf" = "red", "Psus/100" = "green", "Prec/100" = "blue", 
            "Dinf" = "black", "PHw" = "orange", "PDw" = "magenta")

p1  <- ggplot(datM, aes(x = Weeks)) +
  geom_line (aes(y = PInf,     color = 'Pinf')) +
  geom_line (aes(y = PSus/100, color = 'Psus/100')) +
  geom_line (aes(y = PRec/100, color = 'Prec/100')) +
  annotate("text", size=3, x=xx, y=yy, label= paste0("beta=",round(pars$beta,3),", R0unif=",round(pars$R0,2),", R0=",round(max(R0_week),2))) +
  annotate("text", size=3, x=xx, y=y2, label= paste0("pinf2(log10)=",round(pars$logPI0,2),", IP=",round(1/pars$rIR,2))) +
  labs(x = 'Weeks', y = 'PInfected', color = "Legend") +
  scale_color_manual(values = colors)

p2 <-  ggplot(datM, aes(x= Weeks)) +
  geom_point(aes(y=R0)) +
  geom_line (aes(y=rep(1,nd)),col='red') +
  labs(x = 'Weeks', y = 'R0') 


if (pset$imodel==1){
datM <- tibble( datM,
                Dataz = zd,                 
                Iw    = zm,
                PIw   = zm/N,
                PRw   = out.df$Rw/N)

colors <- c("DIw" = "black", "PIw" = "green", 
            "Pinf" = "red",  "PRw" = "blue")

p3 <- ggplot(datM, aes(x = Weeks)) +
  geom_point(aes(y = Dataz/N,  color = 'DIw'), size = 1.2, pch = 1) +
  geom_line (aes(y = PIw,      color = 'PIw')) +
  geom_line (aes(y = PInf,     color = 'Pinf')) +
  geom_line (aes(y = PRw,      color = 'PRw')) +
  labs(x = 'Weeks', y = 'PInfected', color = "Legend") + 
  scale_color_manual(values = colors)
}

if (pset$imodel==2){ 
datM <- tibble( datM, 
                Dataz = zd,                 
                Hw    = zm,
                PHw   = zm/N,
                Dataw = wd,
                Dw    = wm,
                PDw   = wm/N ) 

colors <- c("DHw" = "black", "PHw" = "green", 
            "DDw" = "black", "PDw" = "blue",
            "Pinf/100" = "red")

p3 <- ggplot(datM, aes(x = Weeks)) +
  geom_point(aes(y = Dataz/N,  color = 'DHw'), size = 1.2, pch = 1) +
  geom_line (aes(y = PHw,      color = 'PHw')) +
  geom_point(aes(y = Dataw/N,  color = 'DDw'), size = 1.2, pch = 2) +
  geom_line (aes(y = PDw,      color = 'PDw')) +
  geom_line (aes(y = PInf/100,  color = 'Pinf/100')) +
  #geom_line (aes(y = PRec/100,  color = 'PRec/100')) +
  labs(x = 'Weeks', y = 'PInfected', color = "Legend") + 
  scale_color_manual(values = colors)

}

if (pset$iplatform==0) {
  gridExtra::grid.arrange(p1, p2, p3, nrow = 3) 
}

pdf(file = paste0(output_dir,"/",pset$File_model_sim_plots))
  gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
dev.off()


#### Output simulated data
if (pset$DOfit==1) {
  datout <- data.frame(Weeks=datM$Weeks, DataInfectedPerWeek=zd)
  write.csv(datout, row.names = T, file = paste0(output_dir,"/",pset$File_model_sim_data))
}