### Binomial CIs for estimated HFRs or m
### Data: reading OS rounded data
### platform=2 only

library(Hmisc)

INSIDECODE=0#1 #0 (self-standing) #1 (sourced)
if(SOURCE==0) INSIDECODE=0
if(SOURCE==1) INSIDECODE=1


if (INSIDECODE==0){ #SELFSTANDING ############################################
### Dataframes with OS data - sourced from HDdata.R via main:
###   datH_l,   datDH_l,   datDO_l
###   datHs_l,  datDHs_l,  datDOs_l
###   datHas_l, datDHas_l, datDOas_l
### names: Week, Date, Freq, Ageg, Shield

# 0-SETUP ######################################################################
TIME  = format(Sys.time(),'%H.%M.%S_%d-%m-%Y')
TODAY <- format(Sys.Date(), "%d-%m-%Y")
source(file = paste0(input_code,"/setup.r"))
# 1-OS rounded DATA ############################################################
if (pset$iplatform==2) {
source(file = paste0(input_code,"/HDdata.R")) }
# 2-MODEL ######################################################################
source(file = paste0(input_code,"/","parameters.r"))
Week1_Model="2020-01-27"
Week1_Model="2020-12-01"

###### Output settings and parameters
sink(file = paste0(output_dir,"/","Fig4_input_",TODAY,".txt"), append=TRUE, split=FALSE)
cat(" \n")
print(format(Sys.time(),'%H.%M.%S_%d-%m-%Y'))
cat(" \n")
print("pset: \n ")
print(rev(pset))
cat("\n")
print(paste0("Date range in contact-data:    ", Week1_Model, ", ", Week2_Model)); cat("\n")
  Week1_Data = lubridate::week("2020-01-01") 
  Week2_Data = lubridate::week("2020-12-01")
  print(paste0("Date range in H, D data:      ", "2020-01-01", " - ", "2020-12-01")); cat("\n")
  print(paste0("Week range in H, D data:      ", Week1_Data,   " - ", Week2_Data  )); cat("\n")
cat(" \n")
print("pars: \n ")
print(rev(pars[-1])); #exclude cm
cat("\n \n")
sink()

######## Week range & dataset subsets for fitting ##############################

## Each age group has:
## - weeks 1-48 in 2020
## - freq=0 (inputted) when events unreported
## Date range for fitting
## Convert dates to WEEKS
  Week1_Model  = lubridate::week("2020-01-27")    #  4 #start of "contact" data
  Week2_Model  = Week1_Model + (pars$nw-1)        # 55 #model set to run 52=pars$nw weeks 
  Week2_Study  = lubridate::week("2020-12-01")    # 48 #start of vacc & alpha
  Week1_Fit_H  = max( c(min( datHas_l$Week), Week1_Model), na.rm=T)              #  4 #max(c(min(1:48),4))
  Week1_Fit_DH = max( c(min(datDHas_l$Week), Week1_Model), na.rm=T)              #  4 #max(c(min(1:48),4))
  Week1_Fit_DO = max( c(min(datDOas_l$Week), Week1_Model), na.rm=T)              #  4 #max(c(min(1:48),4))
  Week2_Fit_H  = min( c(max( datHas_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(max(1:48),55,48))
  Week2_Fit_DH = min( c(max(datDHas_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(max(1:48),55,48))
  Week2_Fit_DO = min( c(max(datDOas_l$Week), Week2_Model, Week2_Study), na.rm=T) # 48 #min(c(max(1:48),55,48))

  
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


#y axis log transformation - default: linear
YLOG <- function(y,LOG=0){ if (LOG==1) {z=log10(y+1)} else {z=y}; return(z) }
YEXP <- function(y,LOG=0){ if (LOG==1) {z=10^(y)-1} else {z=y}; return(z) }
LOG=1; #0 #apply scale of Age Profile plot

#Data
  for (i in 1:9){
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
    } 
    assign(paste0("H", eval(i),"d_0"), YLOG(valuesH_0, LOG))                        #H1d_0-H9d_0
    assign(paste0("DH",eval(i),"d_0"), YLOG(valuesDH_0,LOG))                        #DH1d_0-DH9d_0
    assign(paste0("DO",eval(i),"d_0"), YLOG(valuesDO_0,LOG))                        #DO1d_0-DO9d_0
    assign(paste0("H", eval(i),"d_1"), YLOG(valuesH_1, LOG))                        #H1d_1-H9d_1
    assign(paste0("DH",eval(i),"d_1"), YLOG(valuesDH_1,LOG))                        #DH1d_1-DH9d_1
    assign(paste0("DO",eval(i),"d_1"), YLOG(valuesDO_1,LOG))                        #DO1d_1-DO9d_1    
}
#Time
Weekssample=4:48
datT <- tibble(Dates  = lubridate::ymd( "2020-01-06" ) + lubridate::weeks(Weekssample - 1)) #"2020-01-06" Mon
		
} #SELFSTANDING ##############################################################



### Data counts for binomial CIs

### Test m_est from Data here vs pars$m_
md_0 <- vector()
md_1 <- vector()
for (i in 1:9){
#_0
 num=eval(parse(text = paste0("DH",eval(i),"d_0")))
 den=eval(parse(text = paste0("H",eval(i),"d_0")))
 md_0[i] = sum(YEXP( num, LOG))/sum(YEXP( den, LOG)) #DH1d_0/H1d_0... DH9d_0/H9d_0
#_1
 num=eval(parse(text = paste0("DH",eval(i),"d_1")))
 den=eval(parse(text = paste0("H",eval(i),"d_1")))
 md_1[i] = sum(YEXP( num, LOG))/sum(YEXP( den, LOG)) #DH1d_1/H1d_1... DH9d_1/H9d_1
}

##periods
DATES=datT$Dates #[1] "2020-01-27"... [45]"2020-11-30"
ida=which(DATES<="2020-06-30" ) # [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
idb=which(DATES> "2020-06-30" ) # [1] 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45

#DH and D data for Binomial testing
Da_0 <- vector()
Da_1 <- vector()
Ha_0 <- vector()
Ha_1 <- vector()
Db_0 <- vector()
Db_1 <- vector()
Hb_0 <- vector()
Hb_1 <- vector()

for (i in 1:9){
 num_0=eval(parse(text = paste0("DH",eval(i),"d_0"))) #DH1d_0... DH9d_0
 num_1=eval(parse(text = paste0("DH",eval(i),"d_1"))) #DH1d_1... DH9d_1
 den_0=eval(parse(text = paste0("H", eval(i),"d_0"))) #H1d_0... H9d_0
 den_1=eval(parse(text = paste0("H", eval(i),"d_1"))) #H1d_1... H9d_1
#period 1
 Da_0[i] = sum(YEXP( num_0[ida], LOG)) 
 Da_1[i] = sum(YEXP( num_1[ida], LOG)) 
 Ha_0[i] = sum(YEXP( den_0[ida], LOG)) 
 Ha_1[i] = sum(YEXP( den_1[ida], LOG)) 
#period 2 
 Db_0[i] = sum(YEXP( num_0[idb], LOG))
 Db_1[i] = sum(YEXP( num_1[idb], LOG))
 Hb_0[i] = sum(YEXP( den_0[idb], LOG))
 Hb_1[i] = sum(YEXP( den_1[idb], LOG))
}

#Binomial CIs	
#library(Hmisc)

#rm6
ma_0<-pars$ma_0 #c(0,     0, 0.026, 0.004, 0.025, 0.050, 0.122, 0.249, 0.498) #derived
mb_0<-pars$mb_0 #c(0, 0.027,     0, 0.003, 0.010, 0.027, 0.039, 0.108, 0.292) #derived
ma_1<-pars$ma_1 #c(0,     0,     0, 0.030, 0.019, 0.104, 0.148, 0.257, 0.450) #derived
mb_1<-pars$mb_1 #c(0,     0,     0, 0.034, 0.020, 0.074, 0.126, 0.181, 0.375) #derived


x=binconf(x=ma_0*Ha_0, n=Ha_0, alpha=.05)
ma_0list <- list()
ma_0list[[1]] <- x[1:9]		#
ma_0list[[2]] <- x[10:18]	#
ma_0list[[3]] <- x[19:27]	#

x=binconf(x=ma_1*Ha_1, n=Ha_1, alpha=.05)
ma_1list <- list()
ma_1list[[1]] <- x[1:9]		#
ma_1list[[2]] <- x[10:18]	#
ma_1list[[3]] <- x[19:27]	#

x=binconf(x=mb_0*Hb_0, n=Hb_0, alpha=.05)
mb_0list <- list()
mb_0list[[1]] <- x[1:9]		#
mb_0list[[2]] <- x[10:18]	#
mb_0list[[3]] <- x[19:27]	#

x=binconf(x=mb_1*Hb_1, n=Hb_1, alpha=.05)
mb_1list <- list()
mb_1list[[1]] <- x[1:9]		#
mb_1list[[2]] <- x[10:18]	#
mb_1list[[3]] <- x[19:27]	#


#INSIDECODE=0

##Figure data input	(to be read by Fig4_readdata.r)
READ=0
if (READ==1){
DAT <-read.csv(input_data,"Fig4_data.csv")
DFyh_0       <-DAT[c("DFyh_0.Age",   "DFyh_0.s05",    "DFyh_0.s95",    "DFyh_0.Expect")]
DFyh_1       <-DAT[c("DFyh_1.Age",   "DFyh_1.s05",    "DFyh_1.s95",    "DFyh_1.Expect")]
DFd_0        <-DAT[c("DFd_0.Age",    "DFd_0.s05",     "DFd_0.s95",     "DFd_0.Expect")]
DFd_1        <-DAT[c("DFd_1.Age",    "DFd_1.s05",     "DFd_1.s95",     "DFd_1.Expect")]
DFma_0       <-DAT[c("DFma_0.Age",   "DFma_0.s05",    "DFma_0.s95",    "DFma_0.Expect")]
DFma_1       <-DAT[c("DFma_1.Age",   "DFma_1.s05",    "DFma_1.s95",    "DFma_1.Expect")]
DFmb_0       <-DAT[c("DFmb_0.Age",   "DFmb_0.s05",    "DFmb_0.s95",    "DFmb_0.Expect")]
DFmb_1       <-DAT[c("DFmb_1.Age",   "DFmb_1.s05",    "DFmb_1.s95",    "DFmb_1.Expect")]
names(DFyh_0)<-c("Age","s05","s95","Expect")
names(DFyh_1)<-c("Age","s05","s95","Expect")
names(DFd_0) <-c("Age","s05","s95","Expect")
names(DFd_1) <-c("Age","s05","s95","Expect")
names(DFma_0)<-c("Age","s05","s95","Expect")
names(DFma_1)<-c("Age","s05","s95","Expect")
names(DFmb_0)<-c("Age","s05","s95","Expect")
names(DFmb_1)<-c("Age","s05","s95","Expect")
} else {
    Age <- c("0-4", "05-11",  "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70+")
    #yh, IFR - more identifiable than y or h alone
    DFyh_0 <- tibble(Age=Age, s05=yhsample95_0[,1], s95=yhsample95_0[,3], Expect=yhsample95_0[,2]) 
    DFyh_1 <- tibble(Age=Age, s05=yhsample95_1[,1], s95=yhsample95_1[,3], Expect=yhsample95_1[,2])
	#d - death outside hospital probability
    DFd_0  <- tibble(Age=Age, s05=dsample95_0[,1], s95=dsample95_0[,3], Expect=dsample95_0[,2])
    DFd_1  <- tibble(Age=Age, s05=dsample95_1[,1], s95=dsample95_1[,3], Expect=dsample95_1[,2])
    #m, HFR
    DFma_0 <- tibble(Age=Age, s05=ma_0list[[2]], s95=ma_0list[[3]], Expect=ma_0list[[1]])
    DFma_1 <- tibble(Age=Age, s05=ma_1list[[2]], s95=ma_1list[[3]], Expect=ma_1list[[1]])
    DFmb_0 <- tibble(Age=Age, s05=mb_0list[[2]], s95=mb_0list[[3]], Expect=mb_0list[[1]])
    DFmb_1 <- tibble(Age=Age, s05=mb_1list[[2]], s95=mb_1list[[3]], Expect=mb_1list[[1]])
}

if (INSIDECODE==0) {
    ##pdf of figure panels
	pdf(paste0(output_dir,"/","Fig4_",TODAY,".pdf")) }

	colors <- c("shielding"               = 2,       "non-shielding" = "royalblue1",            
	            "shielding, period 1"     = "red4",  "shielding, period 2"     = 2,             
	            "non-shielding, period 1" = "blue4", "non-shielding, period 2" = "royalblue1",  
				"95% CI" = "grey70",  "95% CrI" = "grey70")

	Linewd  = 0.8 #0.5 default
    ptsize  = 1.5 #1.5 default
	tags    = c("a","b","c","d","e","f","g","h","i","j")
    tagpos  = c(0.01,1.00)
    tagsize = 18
    YNAME=c("Infection hospitalisation risk", "Death out-of-hospital risk", "Hospitalisation fataility risk")

	legendxpos_a=c(0.13,0.23,0.23)
	legendxpos_b=c(0.13,0.23,0.23)
	legendxpos_c=c(0.19,0.33,0.33)
	legendypos_a=c(0.75,0.75,0.75)
	legendypos_b=c(0.75,0.75,0.75)
	legendypos_c=c(0.60,0.70,0.65)
	
	ynamsize = c(9, 10, 9)
	ylabsize = c(9, 8, 8)
	xlabsize = c(9, 7, 7)

for (i in 1:3){ #1 col wide or 2 col or 3 col narrow
	  
    #yh, IFR
	pyh<-ggplot() + 
      geom_ribbon(data=DFyh_0, aes(x=Age, group=1, ymin = s05, ymax = s95, color="95% CrI"), fill = "grey70", alpha=0.3) +
      geom_ribbon(data=DFyh_1, aes(x=Age, group=1, ymin = s05, ymax = s95, color="95% CrI"), fill = "grey70", alpha=0.3) +
      geom_line  (data=DFyh_0, aes(x=Age, group=1,  y = Expect,            color="non-shielding"), linewidth=Linewd) +
      geom_line  (data=DFyh_1, aes(x=Age, group=1,  y = Expect,            color="shielding"),     linewidth=Linewd) +
      labs(x = "", y = YNAME[1],   color = "") +
      scale_color_manual(values = colors) + 
	  ylim(0, 0.5) +
	  theme_bw() +
	  #theme(legend.position="none") +
	  	  theme(legend.title=element_blank(), 
	        legend.position = "inside", 
			legend.position.inside = c(legendxpos_a[i], legendypos_a[i]), #x, y
            legend.key.size    = unit(1.0, 'lines'),			
            legend.box.spacing = unit(0, "pt"))  + 			
      theme(axis.title = element_text(size = ynamsize[i])) +
      theme(axis.text.x= element_text(size = xlabsize[i])) +
      theme(axis.text.y= element_text(size = ylabsize[i])) +
	  labs(tag=tags[1]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos)
	  
	#d - death outside hospital probability
	pd<-ggplot() + 
      geom_ribbon(data=DFd_0, aes(x=Age, group=1, ymin = s05, ymax = s95, color="95% CrI"), fill = "grey70", alpha=0.3) +
      geom_ribbon(data=DFd_1, aes(x=Age, group=1, ymin = s05, ymax = s95, color="95% CrI"), fill = "grey70", alpha=0.3) +
      geom_line  (data=DFd_0, aes(x=Age, group=1,  y = Expect,            color="non-shielding"), linewidth=Linewd) +
      geom_line  (data=DFd_1, aes(x=Age, group=1,  y = Expect,            color="shielding"),     linewidth=Linewd) +
      labs(x = "", y = YNAME[2],   color = "") +
      scale_color_manual(values = colors) + 
	  ylim(0, 0.5) +
	  theme_bw() +
	  #theme(legend.position="none") +
	  	  theme(legend.title=element_blank(), 
	        legend.position = "inside", 
		    legend.position.inside = c(legendxpos_b[i], legendypos_b[i]),  #x, y
            legend.key.size    = unit(1.0, 'lines'),			
            legend.box.spacing = unit(0, "pt"))  + 	
      theme(axis.title = element_text(size = ynamsize[i])) +
      theme(axis.text.x= element_text(size = xlabsize[i])) +
      theme(axis.text.y= element_text(size = ylabsize[i])) +
	  labs(tag=tags[2]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos)
	      
    #m - death in hospital probability
    #m, HFR
    Ymax=max(c(max(ma_0list[[3]]), max(ma_1list[[3]]), max(mb_0list[[3]]),  max(mb_1list[[3]])))

    pm<-ggplot() + #"group=1" because x var is charactrer/factor
	  geom_ribbon(data=DFma_0, aes(x=Age, group=1, ymin = s05, ymax = s95, color="95% CI"), fill = "grey70", alpha=0.3) +
      geom_ribbon(data=DFma_1, aes(x=Age, group=1, ymin = s05, ymax = s95, color="95% CI"), fill = "grey70", alpha=0.3) +
      geom_ribbon(data=DFmb_0, aes(x=Age, group=1, ymin = s05, ymax = s95, color="95% CI"), fill = "grey70", alpha=0.3) +
      geom_ribbon(data=DFmb_1, aes(x=Age, group=1, ymin = s05, ymax = s95, color="95% CI"), fill = "grey70", alpha=0.3) +
      geom_line  (data=DFma_0, aes(x=Age, group=1, y = Expect,             color="non-shielding, period 1"), linewidth=Linewd) +
      geom_line  (data=DFma_1, aes(x=Age, group=1, y = Expect,             color="shielding, period 1"),     linewidth=Linewd) +	  
      geom_line  (data=DFmb_0, aes(x=Age, group=1, y = Expect,             color="non-shielding, period 2"), linewidth=Linewd) +
      geom_line  (data=DFmb_1, aes(x=Age, group=1, y = Expect,             color="shielding, period 2"),     linewidth=Linewd) +
	  geom_point (data=DFma_0, aes(x=Age, group=1, y = Expect,             color="non-shielding, period 1"), size=ptsize) + 
	  geom_point (data=DFma_1, aes(x=Age, group=1, y = Expect,             color="shielding, period 1"),     size=ptsize) + 
	  geom_point (data=DFmb_0, aes(x=Age, group=1, y = Expect,             color="non-shielding, period 2"), size=ptsize) + 
	  geom_point (data=DFmb_1, aes(x=Age, group=1, y = Expect,             color="shielding, period 2"),     size=ptsize) + 
      labs(x = "", y = YNAME[3],    color = "") +
      scale_color_manual(values = colors) + 
	  ylim(0, Ymax) +
	  theme_bw() +
	  #theme(legend.position="none") + 
	  theme(legend.title=element_blank(), 
	        legend.position = "inside", 
			legend.position.inside = c(legendxpos_c[i], legendypos_c[i]), #x, y
            legend.key.size    = unit(1.0, 'lines'),		
            legend.box.spacing = unit(0, "pt"))  + 
      theme(axis.title = element_text(size = ynamsize[i])) +
      theme(axis.text.x= element_text(size = xlabsize[i]))   +
      theme(axis.text.y= element_text(size = ylabsize[i]))   +
	  labs(tag=tags[3]) +
	  theme(plot.tag =element_text(size=tagsize, face="bold"), plot.tag.position=tagpos)

#void
    pv <- ggplot() + theme_void()
	  	  
	if (i==2){ gridExtra::grid.arrange(pyh, pd, pm, ncol = i) }
	if (i==3){ gridExtra::grid.arrange(pyh, pv, pd, pv, pm, pv, ncol = 2) }
	#if (i<3)   gridExtra::grid.arrange(pyh, pd, pm, ncol = i)
	#if (i==3){ gridExtra::grid.arrange(pyh, pv, pd, pv, pm, pv, ncol = 2) }
}

##Figure data output (to be read by Fig4_readdata.r) 
WRITE=0#1#
if (WRITE==1){
options(scipen=999) #turn off sci notation
DF_Fig4 <- list()
Age2 <- c("'0-4", "'05-11",  "'12-17", "'18-29", "'30-39", "'40-49", "'50-59", "'60-69", "'70+")
DFyh_0["Age"]=Age2
DFyh_1["Age"]=Age2
DFd_0 ["Age"]=Age2
DFd_1 ["Age"]=Age2
DFma_0["Age"]=Age2
DFma_1["Age"]=Age2
DFmb_0["Age"]=Age2
DFmb_1["Age"]=Age2
DF_Fig4[[1]] <- DFyh_0
DF_Fig4[[2]] <- DFyh_1
DF_Fig4[[3]] <- DFd_0
DF_Fig4[[4]] <- DFd_1
DF_Fig4[[5]] <- DFma_0
DF_Fig4[[6]] <- DFma_1
DF_Fig4[[7]] <- DFmb_0
DF_Fig4[[8]] <- DFmb_1
names(DF_Fig4) <- c("DFyh_0", "DFyh_1", "DFd_0", "DFd_1", "DFma_0", "DFma_1", "DFmb_0", "DFmb_1")
write.csv(DF_Fig4,paste0("Fig4_data_",TODAY,".csv"),row.names=FALSE)
options(scipen=0) #back to sci notation
}


if (INSIDECODE==0) dev.off()    