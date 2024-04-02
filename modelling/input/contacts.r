## Contact matrix
### Start of contact data and model
Week1_Model = "2020-01-27" #"2020-02-24"
Week2_Model = "2021-01-18" #"2021-02-15"
print(paste0("Date range of contact data, ", Week1_Model, ", ", Week2_Model))

### max length (weeks) of contacts (pars$cmdim3) and model run (pset$nw)
nd = pset$nw


#### Read & build contact matrix
if (pset$DOcmREAD==1 & pset$iplatform==0) {
#m = mean of contacts over 250 MCMC samples
  #### file names with relevant weeks
  #files <- list.files(path = "./m_9x9matrices_19feb24", pattern="Eng")
  #files <- list.files(path = "./m_9x9matrices_07feb24", pattern="Eng")
  files   <- list.files(path = "./m_9x9matrices_08feb24", pattern="Eng")
  files_0 <- list.files(path = "./mn_9x9matrices_08feb24", pattern="Eng")
  files_1 <- list.files(path = "./ms_9x9matrices_08feb24", pattern="Eng")
  cdate <- sort(substring(files,8,17)) #8th to 17th character
  cdate <- cdate[1:nd] #### Select 52 weeks from 1st week (2020-01-27) to 2021-01-18") 
  cl   = list()
  cl_0 = list()
  cl_1 = list()
  for (i in seq_along(cdate)) {
    #cl[[i]]=read.csv(paste0("./m_9x9matrices_19feb24/England",cdate[i],"all.csv"),header=T) }
    #cl[[i]]=read.csv(paste0("./m_9x9matrices_07feb24/England",cdate[i],"all.csv"),header=T) }
    cl[[i]]  =read.csv(paste0("./m_9x9matrices_08feb24/England",cdate[i],"all.csv"),header=T) 
    cl_0[[i]]=read.csv(paste0("./mn_9x9matrices_08feb24/England",cdate[i],"all.csv"),header=T)
    cl_1[[i]]=read.csv(paste0("./ms_9x9matrices_08feb24/England",cdate[i],"all.csv"),header=T)  }
## Transition to shielding policy
## Linear transition from n to s
## 27-01-2020                        i=1   - m_1=mn
## fortnight 02-03-2020, 09-03-2020  i=6:7 - m_1 = mn
## fortnight 16-03-2020, 23-03-2020  i=8:9 - m_1 = mean(mn,ms)
## 30-03-2020                        i=10  - m_1 = ms - start of policy
for (i in 1:7) { 
  cl_1[[i]] = cl_0[[i]] #sh  = ns
  cl[[i]]   = cl_0[[i]] #gen = ns bec sh=ns}
}
for (i in 8:9) { 
  cl_1[[i]] = (cl_1[[i]]+cl_0[[i]])*0.5 #mean at mid point bet start of policy and a month earlier when shielding behaviour started  
  cl[[i]]   = (cl[[i]]  +cl_0[[i]])*0.5 #approximation - done properly would be at end point of contacts regression
}
#### Build matrix
  cm   <- array(0,dim=c(9,9,nd))            #9 age groups, 52 weeks
  cm_0 <- array(0,dim=c(9,9,nd))            
  cm_1 <- array(0,dim=c(9,9,nd))            
  for (i in seq_along(cdate))          {
    cm[,,i]   = as.matrix(cl[[i]])          #By week
    cm_0[,,i] = as.matrix(cl_0[[i]])          
    cm_1[,,i] = as.matrix(cl_1[[i]])   }
  ##Symmetrisation of odd non-symmetric fortnights ######
  ##i=51:52 (last fornight) - gives complex EVs for cm_0
  #=> need symmetrise
  for (i in 51:52) {cm_0[,,i] = (cm_0[,,i] + t(cm_0[,,i]))/2 }
  #=> worked, no more "complex" EV warnings
  ####################
  maxEV1   = max(eigen(cm[,,1])[[1]])       #maximum EV in week 1
  maxEV1_0 = max(eigen(cm_0[,,1])[[1]])     
  maxEV1_1 = max(eigen(cm_1[,,1])[[1]])     
  cm   = cm/maxEV1                          #Normalise by max EV of CoMix in week 1 (strongest contacts)
  cm_0 = cm_0/maxEV1_0                     
  cm_1 = cm_1/maxEV1_1                    
#### write matrix as vector (c(cm),...)
  dfcm = data.frame(cm=as.vector(cm), cm_0=as.vector(cm_0), cm_1=as.vector(cm_1))
  #write.csv(c(cm),   row.names = F, file = paste0(output_dir,"/",pset$File_contact_data))
  #write.csv(c(cm_0), row.names = F, file = paste0(output_dir,"/",pset$File_contact_data_0))
  #write.csv(c(cm_1), row.names = F, file = paste0(output_dir,"/",pset$File_contact_data_1))
  write.csv(dfcm, file = paste0(output_dir,"/",pset$File_contact_datax3))
  
} else {
  
  ####get normalised contact vector
  vcmomaxEV1x3   <- read.csv(file = paste0(input_dir,"/",pset$File_contact_datax3))
  cm   <- array(vcmomaxEV1x3$cm,  dim=c(9,9,nd))
  cm_0 <- array(vcmomaxEV1x3$cm_0,dim=c(9,9,nd))
  cm_1 <- array(vcmomaxEV1x3$cm_1,dim=c(9,9,nd))
  if (pset$DOcmONE==1) { cm = cm*0 + 1/9; cm_0 = cm_0*0 + 1/9; cm_1 = cm_1*0 + 1/9; }
}


### Summary variables
cm_weekmean    <- vector() #average contact (group-to-group) each week
cm_weekmean_0  <- vector()
cm_weekmean_1  <- vector()
cm_colsumw1    <- vector() #total contact (group-all-groups) - week 1
cm_colsumw1_0  <- vector()
cm_colsumw1_1  <- vector()
cm_colsumw10   <- vector() #total contact (group-all-groups) - week 10
cm_colsumw10_0 <- vector()
cm_colsumw10_1 <- vector()
cm_weekmaxev   <- vector() #max eigenvalue each week
cm_weekmaxev_0 <- vector()
cm_weekmaxev_1 <- vector()

#### Statistics of contacts
for (i in 1:nd) {
  #### average contact over all cells
  cm_weekmean[i]    = sum(  cm[,,i])/(dim(cm)[1]*dim(cm)[2])
  cm_weekmean_0[i]  = sum(cm_0[,,i])/(dim(cm)[1]*dim(cm)[2])
  cm_weekmean_1[i]  = sum(cm_1[,,i])/(dim(cm)[1]*dim(cm)[2]) }
  #### Max eigenvalue of cm in week 1
for (i in 1:nd) { cm_weekmaxev[i]   = max(eigen(  cm[,,i])[[1]]) }
for (i in 1:nd) { cm_weekmaxev_0[i] = max(eigen(cm_0[,,i])[[1]]) }
for (i in 1:nd) { cm_weekmaxev_1[i] = max(eigen(cm_1[,,i])[[1]]) }
for (i in 1:9) { 
  #### total contacts of each age group in week 1
  cm_colsumw1[i]    = sum(  cm[,i,1])
  cm_colsumw1_0[i]  = sum(cm_0[,i,1])
  cm_colsumw1_1[i]  = sum(cm_1[,i,1]) 
  cm_colsumw10[i]   = sum(  cm[,i,10])
  cm_colsumw10_0[i] = sum(cm_0[,i,10])
  cm_colsumw10_1[i] = sum(cm_1[,i,10])  }

#### output
sink(file = paste0(output_dir,"/",pset$File_contact_summary), append=FALSE, split=FALSE)
  print(paste0("Date range of contact-data  ", Week1_Model, ", ", Week2_Model))
  print("week 1 - total contacts per day of each person in each age group (general, ns, sh)")
  print(cm_colsumw1)
  print(cm_colsumw1_0)
  print(cm_colsumw1_1)
  cat("\n");
  print("week 10 (start of policy) - total contacts per day of each person in each age group")
  print(cm_colsumw10)
  print(cm_colsumw10_0)
  print(cm_colsumw10_1)
  cat("\n");
  print("weeks 1 to 52 - max eigenvalue of cm (general, ns, sh)")
  print(cm_weekmaxev)
  print(cm_weekmaxev_0)
  print(cm_weekmaxev_1)
  cat("\n");
  print("week 1 to 52 - mean of all pair contacts (general, ns, sh)")
  print(cm_weekmean)
  print(cm_weekmean_0)
  print(cm_weekmean_1)
sink()