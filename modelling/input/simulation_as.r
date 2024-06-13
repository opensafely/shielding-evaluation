## SEIUHRD model

## Compile & run model (weekly points) #########################################
sourceCpp(file = paste0(input_dir,"/",pset$File_modelas_choice))
# Arrays obtained from "contacts" via "main":
# -cm_0, cm_1 - to run model_as
# -cm_0       - for R0, BETA to estimate beta or R0 from current parameters

## Get beta for given R0
print(paste0("R0 = ", pars$R0, ",  pars$beta = ", round(pars$beta,2)))
source(file = paste0(input_dir,"/R0_0.r"))
r0 = R0_0(pars,GetBeta=1,GetOutput=1) #Defaults used: Sampling=0, nt=pars$cmdim3
pars$beta = r0[[1]]$beta
print(paste0("R0 = ", pars$R0, ",  R0_0_beta = ", round(pars$beta,2)))
source(file = paste0(input_dir,"/BETA_0.r")) #BETA <- function(pars)
pars$beta = BETA(pars) #[1] 2.372594 
print(paste0("R0 = ", pars$R0, ",  beta = ", round(pars$beta,2)))

## run model
t1<- system.time( mout <- SEIUHRD(pars) )

## Test Rcpp code:
print("Test Rccp code:")
print(t1)
print(paste0("_0 Close to machine zero?...", sum(cm_weekmean_0- mout$byw_0$cmdtmean_0) ))
print(paste0("_1 Close to machine zero?...", sum(cm_weekmean_1- mout$byw_0$cmdtmean_1) ))

#library(bench)
#sourceCpp("SEIUHRD_model.cpp")
#tt1 <- bench::mark(mout <- SEIUHRD(pars))$total_time
#sourceCpp("SEIUHRD3_model.cpp") #sourceCpp("SEIUHRDwl_model.cpp")
#tt2 <- bench::mark(mout <- SEIUHRD(pars))$total_time
#print("_model")
#tt1
#print("2_model")
#tt2

## output model results (after output from main)
sink(file = paste0(output_dir,"/",pset$File_run), append=FALSE, split=FALSE)
  cat("\n")
  cat("\n")
  print("Simulation ##########################")
  cat("\n")
  print("Model run time")
  print(t1)
  cat("\n")
  print("Test Rcpp code - close to machine zero? ")
  print(sum(cm_weekmean- mout$byw_0$cmdtmean))
  cat("\n")
sink()


## Simulate data ###############################################################
na = pars$na
nd = pset$nw
#### True model each week
## Incidence overall
ym_0 = mout$byw_0$It + mout$byw_0$Ut; #infected 
zm_0 = mout$byw_0$Hw;                 #incidence
wm_0 = mout$byw_0$DHw;                #incidence in hospital
vm_0 = mout$byw_0$DOw;                #incidence outside hospital
ym_1 = mout$byw_1$It + mout$byw_1$Ut; #infected 
zm_1 = mout$byw_1$Hw;                 #incidence
wm_1 = mout$byw_1$DHw;                #incidence in hospital
vm_1 = mout$byw_1$DOw;                #incidence outside hospital
ym = ym_0 + ym_1                      #infected 
zm = zm_0 + zm_1                      #incidence
wm = wm_0 + wm_1                      #incidence in hospital
vm = vm_0 + vm_1                      #incidence outside hospital

#zm[1]=max(zm[1],1); wm[1]=max(wm[1],1); vm[1]=max(vm[1],1); #Normal: avoid 0's when sampling

#Incidence by age
zam <- as.data.frame(matrix(rep(0,nd*na), ncol = na)); names(zam)=c(paste0("z", 1:na,"_0"))
wam <- as.data.frame(matrix(rep(0,nd*na), ncol = na)); names(wam)=c(paste0("w", 1:na,"_0"))
vam <- as.data.frame(matrix(rep(0,nd*na), ncol = na)); names(vam)=c(paste0("v", 1:na,"_0"))

for (i in 1:9){ #by age #need keep order of cols im zam etc => separate loops
  zam[paste0("z",i,"_0")] = eval(parse(text = paste0("mout$byw_ageH_0$H", i,"w"))) #
  wam[paste0("w",i,"_0")] = eval(parse(text = paste0("mout$byw_ageD_0$DH",i,"w")))
  vam[paste0("v",i,"_0")] = eval(parse(text = paste0("mout$byw_ageD_0$DO",i,"w"))) }
for (i in 1:9){ #by age
  zam[paste0("z",i,"_1")] = eval(parse(text = paste0("mout$byw_ageH_1$H", i,"w")))
  wam[paste0("w",i,"_1")] = eval(parse(text = paste0("mout$byw_ageD_1$DH",i,"w")))
  vam[paste0("v",i,"_1")] = eval(parse(text = paste0("mout$byw_ageD_1$DO",i,"w"))) }
for (i in 1:9){ #by age
  zam[paste0("z",i)]      = zam[paste0("z",i,"_0")]  + zam[paste0("z",i,"_1")]
  wam[paste0("w",i)]      = wam[paste0("w",i,"_0")]  + wam[paste0("w",i,"_1")]
  vam[paste0("v",i)]      = vam[paste0("v",i,"_0")]  + vam[paste0("v",i,"_1")]   }

#zam[[i]][1]=max(zam[[i]][1],1); wam[[i]][1]=max(wam[[i]][1],1); vam[[i]][1]=max(vam[[i]][1],1) } #Normal


#### True parameters - to be estimated
sd   = round(0.05*mean(zm))  #for normal data
sd2  = round(0.05*mean(wm))  #for normal data
sd3  = round(0.05*mean(vm))  #for normal data
dev0 = sd #1/sqrt(par$k)     #noise parameter

#### Simulated Data each week
set.seed(77)#7)

#Normal noise
#zd = zm + rnorm(n=nd,mean=0,sd=sd)
#wd = wm + rnorm(n=nd,mean=0,sd=sd) 
#vd = vm + rnorm(n=nd,mean=0,sd=sd) 
#Neg Binomial noise
zd   = rnbinom(nd, size=pars$kH,  mu=zm)
wd   = rnbinom(nd, size=pars$kDH, mu=wm)
vd   = rnbinom(nd, size=pars$kDO, mu=vm)
zd_0 = rnbinom(nd, size=pars$kH,  mu=zm_0)
wd_0 = rnbinom(nd, size=pars$kDH, mu=wm_0)
vd_0 = rnbinom(nd, size=pars$kDO, mu=vm_0)
zd_1 = rnbinom(nd, size=pars$kH,  mu=zm_1)
wd_1 = rnbinom(nd, size=pars$kDH, mu=wm_1)
vd_1 = rnbinom(nd, size=pars$kDO, mu=vm_1)

#dim(zam) [1] 52 27
zad <- zam;  names(zad)=names(zam)
wad <- wam;  names(wad)=names(wam)
vad <- vam;  names(vad)=names(vam)
for (i in 1:9){
    zad[paste0("z",i,"_0")] = rnbinom(nd, size=pars$kH,  mu=zam[[i]]) #doesnt work: zam[paste0("z",i,"_0")]
    wad[paste0("w",i,"_0")] = rnbinom(nd, size=pars$kDH, mu=wam[[i]])
    vad[paste0("v",i,"_0")] = rnbinom(nd, size=pars$kDO, mu=vam[[i]]) }
for (i in 1:9){
    zad[paste0("z",i,"_1")] = rnbinom(nd, size=pars$kH,  mu=zam[[9+i]])
    wad[paste0("w",i,"_1")] = rnbinom(nd, size=pars$kDH, mu=wam[[9+i]])
    vad[paste0("v",i,"_1")] = rnbinom(nd, size=pars$kDO, mu=vam[[9+i]]) }
for (i in 1:9){
  zad[paste0("z",i)] = rnbinom(nd, size=pars$kH,  mu=zam[[18+i]])
  wad[paste0("w",i)] = rnbinom(nd, size=pars$kDH, mu=wam[[18+i]])
  vad[paste0("v",i)] = rnbinom(nd, size=pars$kDO, mu=vam[[18+i]]) }


#Test-data option
Option_agesTest=0
if (Option_agesTest==1){
zmt<-zm
zdt<-zd
for (iw in 1:nd){
  zdt[iw]=0; zmt[iw]=0
  for (i in 1:9){
    zdt[iw] = zdt[iw] + zad[[i]][iw] 
    zmt[iw] = zmt[iw] + zam[[i]][iw] }}
#=> zmt = zm, as expected
#=> sum(zdt-zd)<0, probably randomness, adding 9 NB variates .ne. 1 NB variate with mean=sum means
#=> could simulate fit zdt rather than zd - is it worth it?
#=> Maybe not, for plot illustration only; for real data dont need construct noise
plot(1:nd,zd,type="p")
for(i in 1:9){points(1:nd,zad[[i]],col=i+1)}
lines(1:nd,zm,type="l")
for(i in 1:9){lines(1:nd,zam[[i]],col=i+1)}
}

#Plot assessment of variation in data
iz1=which(zd>0); iw1=which(wd>0); iv1=which(vd>0);
print(paste0("mean zd/zm (zd>0):", mean(zd[iz1]/zm[iz1]) ))
print(paste0("mean wd/wm (wd>0):", mean(wd[iw1]/wm[iw1]) ))
print(paste0("mean vd/vm (vd>0):", mean(vd[iv1]/vm[iv1]) ))
## output model results (after output from main)
sink(file = paste0(output_dir,"/",pset$File_run), append=TRUE, split=FALSE)
cat("\n")
print(paste0("mean zd/zm (zd>0):", mean(zd[iz1]/zm[iz1]) ))
print(paste0("mean wd/wm (wd>0):", mean(wd[iw1]/wm[iw1]) ))
print(paste0("mean vd/vm (vd>0):", mean(vd[iv1]/vm[iv1]) ))
cat("\n \n")
sink()
plot  (iz1,zd[iz1], pch=1, col=2, ylab="Simulated & true incidence (z, w, v)", xlab="Week");  lines (iz1,zm[iz1],col=2)
points(1:nd,wd, pch=20, col=3); lines(1:nd,wm,col=3)
points(1:nd,vd, pch=21, col=4); lines(1:nd,vm,col=4)



#### Save data in df
cat("\n")
print("Simulation plots")
cat("\n")

#### Plot model and R0
#### (must have sourced R0.r and run r0=R0_0(pars=pars,GetBeta=1,GetOutput=1))
R0_week = r0[[2]]$R0_week


#### dataframes
N <- pars$Npop
#panel 1, 2
datM <- tibble( Weeks   = mout$byw_0$time, 
                PInf    = ym/N, 
                PInf_0  = ym_0/N, 
                PInf_1  = ym_1/N, 
                PSus    = mout$byw_0$St/N + mout$byw_1$St/N, 
                PSus_0  = mout$byw_0$St/N,
                PSus_1  = mout$byw_1$St/N,
                PRec    = mout$byw_0$Rt/N + mout$byw_1$Rt/N,
                PRec_0  = mout$byw_0$Rt/N,
                PRec_1  = mout$byw_1$Rt/N,
                R0      = R0_week)
#panel 3
datM <- tibble( datM, 
                Dataz    = zd,                 
                Dataz_0  = zd_0,                 
                Dataz_1  = zd_1,                 
                Dataw    = wd,
                Dataw_0  = wd_0,
                Dataw_1  = wd_1,
                Datav    = vd,
                Datav_0  = vd_0,
                Datav_1  = vd_1,
                H_mod    = zm,    
                H_mod_0  = zm_0,   
                H_mod_1  = zm_1, 
                DH_mod   = wm,    
                DH_mod_0 = wm_0,    
                DH_mod_1 = wm_1,  
                DO_mod   = vm, 
                DO_mod_0 = vm_0, 
                DO_mod_1 = vm_1,  
                PH_mod   = zm/N, 
                PH_mod_0 = zm_0/N, 
                PH_mod_1 = zm_1/N, 
                PDH_mod  = wm/N, 
                PDH_mod_0= wm_0/N, 
                PDH_mod_1= wm_1/N, 
                PDO_mod  = vm/N, 
                PDO_mod_0= vm_0/N, 
                PDO_mod_1= vm_0/N) 

#panel 4,5,6 by age
#age profiles to fit: Datazi, Datawi, Datavi
for (i in 1:na){
  datM[paste0("Dataz",i)]      = zad[[18+i]]
  datM[paste0("Dataz",i,"_0")] = zad[[i]]
  datM[paste0("Dataz",i,"_1")] = zad[[9+i]]
  datM[paste0("H_mod",i)]      = zam[[18+i]] 
  datM[paste0("H_mod",i,"_0")] = zam[[i]]  
  datM[paste0("H_mod",i,"_1")] = zam[[9+i]] 
  
  datM[paste0("Dataw",i)]      = wad[[18+i]]
  datM[paste0("Dataw",i,"_0")] = wad[[i]]
  datM[paste0("Dataw",i,"_1")] = wad[[9+i]]
  datM[paste0("DH_mod",i)]     = wam[[18+i]] 
  datM[paste0("DH_mod",i,"_0")]= wam[[i]] 
  datM[paste0("DH_mod",i,"_1")]= wam[[9+i]] 
  
  datM[paste0("Datav",i)]      = vad[[18+i]]
  datM[paste0("Datav",i,"_0")] = vad[[i]]
  datM[paste0("Datav",i,"_1")] = vad[[9+i]]
  datM[paste0("DO_mod",i)]     = vam[[18+i]] 
  datM[paste0("DO_mod",i,"_0")]= vam[[i]] 
  datM[paste0("DO_mod",i,"_1")]= vam[[9+i]] 
}


### Figures
Title_a = c("All")
Title_0 = c("Other")
Title_1 = c("Shielding")

#panel 1
colors <- c( "H_data"  = "red",    "H_pred"  = "red", 
             "DH_data" = "green",  "DH_pred" = "green", 
             "DO_data" = "blue",   "DO_pred" = "blue")

p1_0 <- ggplot(datM, aes(x = Weeks)) +
  geom_point(aes(y = Dataz_0,    color =  'H_data'), size = 1.2, pch = 1) +
  geom_line (aes(y = PH_mod_0*N, color =  'H_pred')) +
  geom_point(aes(y = Dataw_0,    color = 'DH_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = PDH_mod_0*N,color = 'DH_pred')) +
  geom_point(aes(y = Datav_0,    color = 'DO_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = PDO_mod_0*N,color = 'DO_pred')) +
  #geom_line (aes(y = PInf_0/100, color = 'Pinf/100')) +
  labs(x = 'Weeks', y = 'Weekly incidence', color = "") + 
  theme(legend.position = "none") +
  scale_color_manual(values = colors) + ylim(0,max(datM$PH_mod*N)*1.2) + 
  ggtitle(Title_0)

p1_1 <- ggplot(datM, aes(x = Weeks)) +
  geom_point(aes(y = Dataz_1,    color =  'H_data'), size = 1.2, pch = 1) +
  geom_line (aes(y = PH_mod_1*N, color =  'H_pred')) +
  geom_point(aes(y = Dataw_1,    color = 'DH_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = PDH_mod_1*N,color = 'DH_pred')) +
  geom_point(aes(y = Datav_1,    color = 'DO_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = PDO_mod_1*N,color = 'DO_pred')) +
  #geom_line (aes(y = PInf_0/100, color = 'Pinf/100')) +
  labs(x = 'Weeks', y = 'Weekly incidence', color = "") +
  theme(legend.position = "none") +
  scale_color_manual(values = colors) + ylim(0,max(datM$PH_mod_1*N)*1.2) + 
  ggtitle(Title_1)

p1 <- ggplot(datM, aes(x = Weeks)) +
  geom_point(aes(y = Dataz,    color =  'H_data'), size = 1.2, pch = 1) +
  geom_line (aes(y = PH_mod*N, color =  'H_pred')) +
  geom_point(aes(y = Dataw,    color = 'DH_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = PDH_mod*N,color = 'DH_pred')) +
  geom_point(aes(y = Datav,    color = 'DO_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = PDO_mod*N,color = 'DO_pred')) +
  #geom_line (aes(y = PInf/100, color = 'Pinf/100')) +
  labs(x = 'Weeks', y = 'Weekly incidence', color = "") +  
  #theme(legend.position = "none") +
  scale_color_manual(values = colors) + ylim(0,max(datM$PH_mod*N)*1.2) + 
  ggtitle(Title_a)

#panel 2 - log of panel 1
LOGY<-function(y){log10(y+1)}

pl1_0 <- ggplot(datM, aes(x = Weeks)) +
  #geom_point(aes(y = LOGY(Dataz_0),    color =  'H_data'), size = 1.2, pch = 1) +
  geom_line (aes(y = LOGY(PH_mod_0*N), color =  'H_pred')) +
  #geom_point(aes(y = LOGY(Dataw_0),    color = 'DH_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = LOGY(PDH_mod_0*N),color = 'DH_pred')) +
  #geom_point(aes(y = LOGY(Datav_0),    color = 'DO_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = LOGY(PDO_mod_0*N),color = 'DO_pred')) +
  #geom_line (aes(y = PInf_0/100, color = 'Pinf/100')) +
  labs(x = 'Weeks', y = 'Weekly incidence', color = "") + 
  theme(legend.position = "none") +
  scale_color_manual(values = colors) + ylim(0,LOGY(max(datM$PH_mod*N))*1.2) + 
  ggtitle(Title_0)

pl1_1 <- ggplot(datM, aes(x = Weeks)) +
  #geom_point(aes(y = LOGY(Dataz_1),    color =  'H_data'), size = 1.2, pch = 1) +
  geom_line (aes(y = LOGY(PH_mod_1*N), color =  'H_pred')) +
  #geom_point(aes(y = LOGY(Dataw_1),    color = 'DH_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = LOGY(PDH_mod_1*N),color = 'DH_pred')) +
  #geom_point(aes(y = LOGY(Datav_1),    color = 'DO_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = LOGY(PDO_mod_1*N),color = 'DO_pred')) +
  #geom_line (aes(y = PInf_0/100, color = 'Pinf/100')) +
  labs(x = 'Weeks', y = 'Weekly incidence', color = "") +
  theme(legend.position = "none") +
  scale_color_manual(values = colors) + ylim(0,LOGY(max(datM$PH_mod_1*N))*1.2) + 
  ggtitle(Title_1)

pl1 <- ggplot(datM, aes(x = Weeks)) +
  #geom_point(aes(y = LOGY(Dataz),    color =  'H_data'), size = 1.2, pch = 1) +
  geom_line (aes(y = LOGY(PH_mod*N), color =  'H_pred')) +
  #geom_point(aes(y = LOGY(Dataw),    color = 'DH_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = LOGY(PDH_mod*N),color = 'DH_pred')) +
  #geom_point(aes(y = LOGY(Datav),    color = 'DO_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = LOGY(PDO_mod*N),color = 'DO_pred')) +
  #geom_line (aes(y = PInf/100, color = 'Pinf/100')) +
  labs(x = 'Weeks', y = 'Weekly incidence', color = "") +  
  #theme(legend.position = "none") +
  scale_color_manual(values = colors) + ylim(0,LOGY(max(datM$PH_mod*N))*1.2) + 
  ggtitle(Title_a)


#panel 3
xx <- quantile(datM$Weeks,0.6)[[1]]; yy = max(datM$PInf)*0.9; y2 = max(datM$PInf)*0.6;
colors <- c("Pinf" = "red", "Psus/100" = "green", "Prec/100" = "blue", 
            "Dinf" = "black", "PH_mod" = "orange", "PDH_mod" = "magenta")

p2_0  <- ggplot(datM, aes(x = Weeks)) +
  geom_line (aes(y = PInf_0,     color = 'Pinf')) +
  geom_line (aes(y = PSus_0/100, color = 'Psus/100')) +
  geom_line (aes(y = PRec_0/100, color = 'Prec/100')) +
  #annotate("text", size=3, x=xx, y=yy, label= paste0("beta=",round(pars$beta,3),", R0unif=",round(pars$R0,2),", R0=",round(max(R0_week),2))) +
  #annotate("text", size=3, x=xx, y=y2, label= paste0("pinf2(log10)=",round(pars$logPI0,2),", IP=",round(1/pars$rIR,2))) +
  labs(x = 'Weeks', y = 'PInfected, not shielding', color = "") +
  theme(legend.position = "none") +
  scale_color_manual(values = colors) + ylim(0,max(datM$PInf)*1.2)

p2_1  <- ggplot(datM, aes(x = Weeks)) +
  geom_line (aes(y = PInf_1,     color = 'Pinf')) +
  geom_line (aes(y = PSus_1/100, color = 'Psus/100')) +
  geom_line (aes(y = PRec_1/100, color = 'Prec/100')) +
  #annotate("text", size=3, x=xx, y=yy, label= paste0("beta=",round(pars$beta,3),", R0unif=",round(pars$R0,2),", R0=",round(max(R0_week),2))) +
  #annotate("text", size=3, x=xx, y=y2, label= paste0("pinf2(log10)=",round(pars$logPI0,2),", IP=",round(1/pars$rIR,2))) +
  labs(x = 'Weeks', y = 'PInfected, shielding', color = "") + #Legend") +
  theme(legend.position = "none") +
  scale_color_manual(values = colors) + ylim(0,max(datM$PInf_1)*1.2)

p2  <- ggplot(datM, aes(x = Weeks)) +
  geom_line (aes(y = PInf,     color = 'Pinf')) +
  geom_line (aes(y = PSus/100, color = 'Psus/100')) +
  geom_line (aes(y = PRec/100, color = 'Prec/100')) +
  annotate("text", size=3, x=xx, y=yy, label= paste0("beta=",round(pars$beta,3),", R0unif=",round(pars$R0,2),", R0=",round(max(R0_week),2))) +
  annotate("text", size=3, x=xx, y=y2, label= paste0("pinf2(log10)=",round(pars$logPI0,2),", IP=",round(1/pars$rIR,2))) +
  labs(x = 'Weeks', y = 'PInfected', color = "") +
  #theme(legend.position = "none") +
  scale_color_manual(values = colors) + ylim(0,max(datM$PInf)*1.2)


#panel 3 (R0) - replaced - use the other sgv
#
#p3_0 <-  ggplot(datM, aes(x= Weeks)) +
#  geom_point(aes(y=R0)) +
#  geom_line (aes(y=rep(1,nd)),col='red') +
#  labs(x = 'Weeks', y = 'R0')
#
#p3_1 <-  ggplot(datM, aes(x= Weeks)) +
#  geom_point(aes(y=R0)) +
#  geom_line (aes(y=rep(1,nd)),col='red') +
#  labs(x = 'Weeks', y = 'R0') 
#
#p3 <-  ggplot(datM, aes(x= Weeks)) +
#  geom_point(aes(y=R0)) +
#  geom_line (aes(y=rep(1,nd)),col='red') +
#  labs(x = 'Weeks', y = 'R0') 

#gridExtra::grid.arrange(p1, p1_0, p1_1, p2, p2_0, p2_1, p3, p3_0, p3_1, nrow = 3, ncol=3)


##By age
colors <- c("0-4" = "yellow", "5-11" = "pink",  "12-17" = "grey", "18-29" = "orange", "30-39" = "magenta", 
            "40-49" = "blue", "50-59" = "green",  "60-69" = "red", "70+" = "black")
#H
p4_0 <- ggplot(datM, aes(x = Weeks)) 
for (i in 1:9){  p4_0 <- p4_0 +
  geom_point(aes(y = .data[[paste0("Dataz",i,"_0")]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))][[1]], size = 1.2, pch = 1) +
  geom_line (aes(y = .data[[paste0("H_mod",i,"_0")]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))][[1]] )}
p4_0 <- p4_0 + 
  labs(x = 'Weeks', y = 'Hospital admissions by age', color = "Legend") + 
  scale_color_manual(values = colors) + ylim(0,max(datM$H_mod9)*1.2) + 
  ggtitle(Title_0)

p4_1 <- ggplot(datM, aes(x = Weeks)) 
for (i in 1:9){  p4_1 <- p4_1 +
  geom_point(aes(y = .data[[paste0("Dataz",i,"_1")]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))][[1]], size = 1.2, pch = 1) +
  geom_line (aes(y = .data[[paste0("H_mod",i,"_1")]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))][[1]] )}
p4_1<- p4_1 + 
  labs(x = 'Weeks', y = 'Hospital admissions by age', color = "Legend") + 
  scale_color_manual(values = colors) + ylim(0,max(datM$H_mod9_1)*1.2) + 
  ggtitle(Title_1)

p4 <- ggplot(datM, aes(x = Weeks)) 
for (i in 1:9){  p4 <- p4 +
  #geom_point(aes(y = .data[[paste0("Dataz",i)]]), color=names(colors)[i], size = 1.2, pch = 1) +
  #geom_line (aes(y = .data[[paste0("H_mod",i)]]), color=names(colors)[i]) }
  geom_point(aes(y = .data[[paste0("Dataz",i)]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))], size = 1.2, pch = 1) +
  geom_line (aes(y = .data[[paste0("H_mod",i)]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))] )}
p4 <- p4 + 
  labs(x = 'Weeks', y = 'Hospital admissions by age', color = "Legend") + 
  scale_color_manual(values = colors) + ylim(0,max(datM$H_mod9)*1.2) + 
  ggtitle(Title_a)

#DH
p5_0 <- ggplot(datM, aes(x = Weeks)) 
for (i in 1:9){  p5_0 <- p5_0 +
  geom_point(aes(x = Weeks, y = .data[[paste0("Dataw",i,"_0") ]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))], size = 1.2, pch = 1) +
  geom_line (aes(x = Weeks, y = .data[[paste0("DH_mod",i,"_0")]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))] )}
p5_0 <- p5_0 + labs(x = 'Weeks', y = 'Deaths in hospital by age', color = "")+ #Legend") +  
  scale_color_manual(values = colors) + ylim(0,max(datM$DH_mod9)*1.2)

p5_1 <- ggplot(datM, aes(x = Weeks)) 
for (i in 1:9){  p5_1 <- p5_1 +
  geom_point(aes(x = Weeks, y = .data[[paste0("Dataw",i,"_1") ]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))], size = 1.2, pch = 1) +
  geom_line (aes(x = Weeks, y = .data[[paste0("DH_mod",i,"_1")]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))] )}
p5_1 <- p5_1 + labs(x = 'Weeks', y = 'Deaths in hospital by age', color = "")+ #Legend") +  
  scale_color_manual(values = colors) + ylim(0,max(datM$DH_mod9_1)*1.2)

p5 <- ggplot(datM, aes(x = Weeks)) 
for (i in 1:9){  p5 <- p5 +
  geom_point(aes(x = Weeks, y = .data[[paste0("Dataw",i)      ]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))], size = 1.2, pch = 1) +
  geom_line (aes(x = Weeks, y = .data[[paste0("DH_mod",i)     ]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))] )}
p5 <- p5 + labs(x = 'Weeks', y = 'Deaths in hospital by age', color = "")+ #Legend") +  
  scale_color_manual(values = colors) + ylim(0,max(datM$DH_mod9)*1.2)

#DO
p6_0 <- ggplot(datM, aes(x = Weeks)) 
for (i in 1:9){  p6_0 <- p6_0 +
  geom_point(aes(x = Weeks, y = .data[[paste0("Datav",i,"_0")]]),  color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))], size = 1.2, pch = 1) +
  geom_line (aes(x = Weeks, y = .data[[paste0("DO_mod",i,"_0")]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))] )}
p6_0 <- p6_0 + labs(x = 'Weeks', y = 'Deaths outside hospital by age', color = "")+ #Legend") + 
  scale_color_manual(values = colors) + ylim(0,max(datM$DO_mod9)*1.2)

p6_1 <- ggplot(datM, aes(x = Weeks)) 
for (i in 1:9){  p6_1 <- p6_1 +
  geom_point(aes(x = Weeks, y = .data[[paste0("Datav",i,"_1")]]),  color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))], size = 1.2, pch = 1) +
  geom_line (aes(x = Weeks, y = .data[[paste0("DO_mod",i,"_1")]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))] )}
p6_1 <- p6_1 + labs(x = 'Weeks', y = 'Deaths outside hospital by age', color = "")+ #Legend") + 
  scale_color_manual(values = colors) + ylim(0,max(datM$DO_mod9_1)*1.2)

p6 <- ggplot(datM, aes(x = Weeks)) 
for (i in 1:9){  p6 <- p6 +
  geom_point(aes(x = Weeks, y = .data[[paste0("Datav",i)     ]]),  color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))], size = 1.2, pch = 1) +
  geom_line (aes(x = Weeks, y = .data[[paste0("DO_mod",i)     ]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))] )}
p6 <- p6 + labs(x = 'Weeks', y = 'Deaths outside hospital by age', color = "")+ #Legend") + 
  scale_color_manual(values = colors) + ylim(0,max(datM$DO_mod9)*1.2)

###pdf
#pdf(file = paste0(output_dir,"/",pset$File_model_sim_plots))
#  gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
#  gridExtra::grid.arrange(p4, p5, p6, nrow = 3)
#dev.off()
#svg
svglite(file = paste0(output_dir,"/",pset$File_model_sim_plots,"_1-3.svg")); 
  gridExtra::grid.arrange(p1_0, p1_1, p1, 
                          pl1_0,pl1_1,pl1,
                          p2_0, p2_1, p2, nrow = 3, ncol=3)
invisible(dev.off())
svglite(file = paste0(output_dir,"/",pset$File_model_sim_plots,"_4-6.svg")); 
  gridExtra::grid.arrange(p4_0, p4_1, p4, 
                          p5_0, p5_1, p5,
                          p6_0, p6_1, p6, nrow = 3, ncol=3)
invisible(dev.off())
