## Contact matrix
### Start of contact data and model
Week1_Model = "2020-02-24"
Week2_Model = "2021-02-15"
print(paste0("Date range of contact data, ", Week1_Model, ", ", Week2_Model))

### max length (weeks) of contacts (pars$cmdim3) and model run (pset$nw)
nd = pset$nw

### Summary variables
cm_weekmean  <- vector() #average contact (group-to-group) each week
cm_colsumw1  <- vector() #total contact (group-all-groups) each week
cm_weekmaxev <- vector() #max eigenvalue each week

#### Read & build contact matrix
if (pset$DOcmREAD==1 & pset$iplatform==0) {
  #### file names with relevant weeks
  files <- list.files(path = "./m_9x9matrices", pattern="Eng")
  cdate <- sort(substring(files,8,17))
  cdate <- cdate[(1+3):(nd+3)] #### Select 52 weeks from 1st week showing change (4th week, 2020-02-24, to 2021-02-15") 
  cl = list()
  for (i in seq_along(cdate)) {
    cl[[i]]=read.csv(paste0("./m_9x9matrices/England",cdate[i],"all.csv"),header=T) }
  #### Build matrix
  cm <- array(0,dim=c(9,9,nd))            #9 age groups, 52 weeks
  for (i in seq_along(cdate)) {
    cm[,,i] = as.matrix(cl[[i]]) }       #By week
  maxEV1 = max(eigen(cm[,,1])[[1]])       #maximum EV in week 1
  cm = cm/maxEV1                          #Normalise by max EV of CoMix in week 1 (strongest contacts)
  #### write matrix
  write.csv(c(cm), row.names = F, file = paste0(output_dir,"/",pset$File_contact_data))
  
} else {
  
  ####get normalised contact vector
  cmomax <- read.csv(file = paste0(input_dir,"/",pset$File_contact_data))
  cm <- array(cmomax[[1]],dim=c(9,9,nd))
  if (pset$DOcmONE==1) cm = cm*0 + 1/9
}

#### Statistics of contacts
for (i in 1:nd) {
  #### average contact over all cells
  cm_weekmean[i]  = sum(cm[,,i])/(dim(cm)[1]*dim(cm)[2])
  #### Max eigenvalue of cm in week 1
  cm_weekmaxev[i] = max(eigen(cm[,,i])[[1]])    }
for (i in 1:9) { 
  #### total contacts of each age group
  cm_colsumw1[i]   = sum(cm[,i,1]) }

#### output
sink(file = paste0(output_dir,"/",pset$File_contact_summary), append=FALSE, split=FALSE)
  print(paste0("Date range of contact-data  ", Week1_Model, ", ", Week2_Model))
  print("total contacts per day of each person in each age group, week 1")
  cm_colsumw1
  print("max eigenvalue of cm, weeks 1 to 52")
  cm_weekmaxev
  print("mean of all pair contacts, week 1 yo 52")
  cm_weekmean
sink()