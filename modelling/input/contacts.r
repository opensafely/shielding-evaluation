## Contact matrix
### Start of contact data and model
Week1_Model = "2020-01-27"
Week2_Model = "2020-12-01"
print(paste0("Date range of model run, ", Week1_Model, ", ", Week2_Model))

### max length (weeks) of model run (pset$nw, pars$nw) and contacts (pars$cmdim3) 
nd = pset$nw


#### Read contact matrix
  vcmx3   <- read.csv(file = paste0(input_dir,"/Contact_matrix_year-from-27Jan20_vector-lenght-9x9x52_spsynv.csv")) #setup has options for other cm files
  cm   <- array(vcmx3$cm,  dim=c(9,9,nd))
  cm_0 <- array(vcmx3$cm_0,dim=c(9,9,nd))
  cm_1 <- array(vcmx3$cm_1,dim=c(9,9,nd))


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