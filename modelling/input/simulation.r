## SEIUHRD model

## Compile & run model (weekly points) #########################################
sourceCpp(file = paste0(input_dir,"/",pset$File_model_choice))

## Get beta for given R0
source(file = paste0(input_dir,"/R0.r"))
r0 = R0(pars,GetBeta=1,GetOutput=1) #, Sampling=0, 45)  #Defaults used: Sampling=0, nt=pars$cmdim3
pars$beta = r0[[1]]$beta
#pars$beta=1
print(paste0("R0 = ", pars$R0, ",  beta = ", round(pars$beta,2)))

## run model
t1<- system.time( mout <- SEIUHRD(pars) )

## Test Rcpp code:
print("Test Rccp code:")
print(t1)
print(paste0("Close to machine zero?...", sum(cm_weekmean- mout$byw$cmdtmean) ))

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
  print(sum(cm_weekmean- mout$byw$cmdtmean))
  cat("\n")
sink()


## Simulate data ###############################################################
na = pars$na
nd = pset$nw
#### True model each week
ym = mout$byw$It + mout$byw$Ut;   #infected 
zm = mout$byw$Hw;                 #incidence
wm = mout$byw$DHw;                #incidence in hospital
vm = mout$byw$DOw;                #incidence outside hospital

#zm[1]=max(zm[1],1); wm[1]=max(wm[1],1); vm[1]=max(vm[1],1); #Normal: avoid 0's when sampling

zam <- as.data.frame(matrix(rep(0,nd*na), ncol = na)); names(zam)=c(paste0("z", 1:na))
wam <- as.data.frame(matrix(rep(0,nd*na), ncol = na)); names(wam)=c(paste0("w", 1:na)) #length can differ from zam
vam <- as.data.frame(matrix(rep(0,nd*na), ncol = na)); names(vam)=c(paste0("v", 1:na)) #length can differ from zam

for (i in 1:9){
  zam[paste0("z",i)] = eval(parse(text = paste0("mout$byw_age$H",i,"w"))) 
  wam[paste0("w",i)] = eval(parse(text = paste0("mout$byw_aHO$DH",i,"w")))
  vam[paste0("v",i)] = eval(parse(text = paste0("mout$byw_aHO$DO",i,"w"))) }
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
zd = rnbinom(nd, size=pars$kH, mu=zm) #size=k=10-200 Davies Nat Med 2020
wd = rnbinom(nd, size=pars$kDH, mu=wm)
vd = rnbinom(nd, size=pars$kDO, mu=vm)

zad <- zam;  names(zad)=c(paste0("z", 1:na))
wad <- wam;  names(wad)=c(paste0("w", 1:na))
vad <- vam;  names(vad)=c(paste0("v", 1:na))
for (i in 1:9){
    zad[paste0("z",i)] = rnbinom(nd, size=pars$kH,  mu=zam[[i]])
    wad[paste0("w",i)] = rnbinom(nd, size=pars$kDH, mu=wam[[i]])
    vad[paste0("v",i)] = rnbinom(nd, size=pars$kDO, mu=vam[[i]]) }

#Test option
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
#### (must have sourced R0.r and run r0=R0(pars=pars,GetBeta=1,GetOutput=1))
R0_week = r0[[2]]$R0_week

#### dataframes
N <- pars$Npop
#panel 1, 2
datM <- tibble( Weeks = mout$byw$time, 
                PInf  = ym/N, 
                PSus  = mout$byw$St/N, 
                PRec  = mout$byw$Rt/N, 
                R0    = R0_week, 
                Week1_Model = Week1_Model, 
                Week2_Model = Week2_Model )
#panel 3
datM <- tibble( datM, 
                Dataz  = zd,                 
                Dataw  = wd,
                Datav  = vd,
                H_mod  = zm,   #Hw    = zm,
                DH_mod = wm,   #Dw    = wm,
                DO_mod = vm,   #Dv    = vm,
                PH_mod = zm/N, #PHw
                PDH_mod= wm/N, #PDw
                PDO_mod= vm/N) #PDv

#panel 4,5,6 by age
#age profiles to fit: Datazi, Datawi, Datavi
for (i in 1:na){
  datM[paste0("Dataz",i)] = zad[[i]]
  datM[paste0("H_mod",i)] = zam[[i]] #Hw
  datM[paste0("Dataw",i)] = wad[[i]]
  datM[paste0("DH_mod",i)]= wam[[i]] #Dw
  datM[paste0("Datav",i)] = vad[[i]]
  datM[paste0("DO_mod",i)]= vam[[i]] #Dv
}

#panel 1
xx <- quantile(datM$Weeks,0.6)[[1]]; yy = max(datM$PInf)*0.9; y2 = max(datM$PInf)*0.6;
colors <- c("Pinf" = "red", "Psus/100" = "green", "Prec/100" = "blue", 
            "Dinf" = "black", "PH_mod" = "orange", "PDH_mod" = "magenta")

p1  <- ggplot(datM, aes(x = Weeks)) +
  geom_line (aes(y = PInf,     color = 'Pinf')) +
  geom_line (aes(y = PSus/100, color = 'Psus/100')) +
  geom_line (aes(y = PRec/100, color = 'Prec/100')) +
  annotate("text", size=3, x=xx, y=yy, label= paste0("beta=",round(pars$beta,3),", R0unif=",round(pars$R0,2),", R0=",round(max(R0_week),2))) +
  annotate("text", size=3, x=xx, y=y2, label= paste0("pinf2(log10)=",round(pars$logPI0,2),", IP=",round(1/pars$rIR,2))) +
  labs(x = 'Weeks', y = 'PInfected', color = "Legend") +
  scale_color_manual(values = colors)

#panel 2
p2 <-  ggplot(datM, aes(x= Weeks)) +
  geom_point(aes(y=R0)) +
  geom_line (aes(y=rep(1,nd)),col='red') +
  labs(x = 'Weeks', y = 'R0') 

#panel 3
colors <- c( "H_data"  = "green",  "H_pred"  = "green", 
             "DH_data" = "blue",   "DH_pred" = "blue", 
             "DO_data" = "orange", "DO_pred" = "orange", 
            "Pinf/100" = "red")

p3 <- ggplot(datM, aes(x = Weeks)) +
  geom_point(aes(y = Dataz/N,  color =  'H_data'), size = 1.2, pch = 1) +
  geom_line (aes(y = PH_mod,   color =  'H_pred')) +
  geom_point(aes(y = Dataw/N,  color = 'DH_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = PDH_mod,  color = 'DH_pred')) +
  geom_point(aes(y = Datav/N,  color = 'DO_data'), size = 1.2, pch = 2) +
  geom_line (aes(y = PDO_mod,  color = 'DO_pred')) +
  geom_line (aes(y = PInf/100, color = 'Pinf/100')) +
  labs(x = 'Weeks', y = 'Proportion', color = "Legend") + 
  scale_color_manual(values = colors)

##By age
#colors <- c("0-4" = 9, "5-11" = 8,  "12-17" = 7, "18-29" = 6, "30-39" = 5, 
#            "40-49" = 4, "50-59" = 3,  "60-69" = 2, "70+" = 1)
colors <- c("0-4" = "yellow", "5-11" = "pink",  "12-17" = "grey", "18-29" = "orange", "30-39" = "magenta", 
            "40-49" = "blue", "50-59" = "green",  "60-69" = "red", "70+" = "black")
#H
p4 <- ggplot(datM) #, aes(x = Weeks)) 
for (i in 1:9){  p4 <- p4 +
  #geom_point(aes(y = .data[[paste0("Dataz",i)]]), color=names(colors)[i], size = 1.2, pch = 1) +
  #geom_line (aes(y = .data[[paste0("H_mod",i)]]), color=names(colors)[i]) }
  geom_point(aes(x = Weeks, y = .data[[paste0("Dataz",i)]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))], size = 1.2, pch = 1) +
  geom_line (aes(x = Weeks, y = .data[[paste0("H_mod",i)]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))] )}
p4 <- p4 + labs(x = 'Weeks', y = 'Hospital admissions by age', color = "Legend") + 
           scale_color_manual(values = colors)
if (pset$iplatform==0) print(p4)

#DH
p5 <- ggplot(datM, aes(x = Weeks)) 
for (i in 1:9){  p5 <- p5 +
  geom_point(aes(x = Weeks, y = .data[[paste0("Dataw",i)]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))], size = 1.2, pch = 1) +
  geom_line (aes(x = Weeks, y = .data[[paste0("DH_mod",i)]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))] )}
p5 <- p5 + labs(x = 'Weeks', y = 'Deaths in hospital by age', color = "Legend") + 
          scale_color_manual(values = colors)
if (pset$iplatform==0) print(p5)

#DO
p6 <- ggplot(datM, aes(x = Weeks)) 
for (i in 1:9){  p6 <- p6 +
  geom_point(aes(x = Weeks, y = .data[[paste0("Datav",i)]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))], size = 1.2, pch = 1) +
  geom_line (aes(x = Weeks, y = .data[[paste0("DO_mod",i)]]), color=colors[eval(parse(text = paste0("names(colors)[",eval(i),"]")))] )}
p6 <- p6 + labs(x = 'Weeks', y = 'Deaths outside hospital by age', color = "Legend") + 
  scale_color_manual(values = colors)
if (pset$iplatform==0) print(p6)

###pdf
#pdf(file = paste0(output_dir,"/",pset$File_model_sim_plots))
#  gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
#  gridExtra::grid.arrange(p4, p5, p6, nrow = 3)
#dev.off()
#svg
svglite(file = paste0(output_dir,"/",pset$File_model_sim_plots,"_1-3.svg")); 
  gridExtra::grid.arrange(p1, p2, p3, nrow = 3)
invisible(dev.off())
svglite(file = paste0(output_dir,"/",pset$File_model_sim_plots,"_4-6.svg")); 
  gridExtra::grid.arrange(p4, p5, p6, nrow = 3)
invisible(dev.off())
