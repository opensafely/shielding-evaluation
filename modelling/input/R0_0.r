## R0 function outputs: 
##   parameter:   beta or R0
##   time series: R0_week, R0xcmMEV, Week, Date
## Assumptions:
##   R0 (whole pop) ~ R0_0
##   beta_1 ~ beta_0
##   Calculate beta_0 or R0_0 from the _0 population parameters: y_0, h_0, m_0, d_0, ad_0
## parameters updated here:
##   * pars$R0 = R0_0
##   * pars$beta = beta_0
##

R0_0 <- function(pars,GetBeta,GetOutput,Sampling=0, nt=pars$cmdim3 ){

R0_week   = vector()
cmMEV     = vector()
rIR       = pars$rIR
u         = pars$u     #u_1 = u_0 = u => don't need specify whether contacts are _0 or _1
ngm       = cm_0       #array from "contacts"
beta0     = pars$beta 
ntcontact = nt

## cmMEV
for (i in 1:ntcontact) { #1:52
  cmMEV[i] =max(eigen(cm_0[,,i])[[1]]) } #max eigenvalue of cm_0[[i]]

## NGM MEV
  rIH = pars$rIH
  rIO = pars$rIO #est
  rOD = pars$rOD #est
  orOD= 1/rOD
  rUR = pars$rUR
  fu  = pars$fu
  ad  = pars$ad_0
  for (i in 1:ntcontact) { #1:52
    for (k in 1:9){ 
      y_k=pars$y_0[k]; 
      h_k=pars$h_0[k]; 
      d_k=pars$d_0[k]*ad; 
      for (j in 1:9) {
            ngm[j,k,i] = beta0*u[j]*cm_0[j,k,i]*( y_k*( (1+fu*(rIO*d_k*orOD))/( rIR*(1-h_k-d_k) + rIH*(h_k) + rIO*(d_k) )) + fu*(1-y_k)/rUR) }}
  R0_week[i] = max(eigen(ngm[,,i])[[1]]) } #max eigenvalue of NGM

## if pars$R0 is given
## and want pars$beta
## (the current beta0 is inconsistent with R0 & NGM bec, 1) beta had not been calculated, or 2) other parameters have changed)
## (if beta0 was correct and there were no other parameters changes, then the new beta = beta0)
if(GetBeta==1){
  #calculate new beta based on input R0 and NGM, i.e. how must scale NGM to achieve R0 in week 1 (max contacts)
  beta_0 = pars$R0/(R0_week[1]/beta0)
  #store new beta in pars
  pars$beta = beta_0
  #calculate R0 over time based on the new beta
  R0_week = beta_0*(R0_week/beta0) 
  
## if pars$beta is given
## want new pars$R0
} else {
  #calculate R0 based on input beta0=pars$beta, as maxEV of NGM in week 1 (max contacts)
  R0 = R0_week[1]
  #store R0 in pars
  pars$R0 = R0
  #R0 over time has already been calculated as R0_week
}


if (GetOutput>0) {
#save R0 over time
Week = week("2020-01-27")-1+1:ntcontact  #contacts.r: Week1_Model = "2020-01-27" 
if (nt==52){
  Date = c(as.Date(paste(Week[1:49], "2020", 'Mon'), '%U %Y %a'), as.Date(paste(Week[50:52]-52, "2021", 'Mon'), '%U %Y %a'))
} else { #if nt<50, up to "2020-12-28"
  Date = c(as.Date(paste(Week[1:ntcontact], "2020", 'Mon'), '%U %Y %a')) }
WeekDateR0 <- tibble(Week=Week, Date=Date, R0_week=R0_week, R0xcmMEV=pars$R0*cmMEV,)

if(Sampling==0){
#csv
  write.csv(WeekDateR0, row.names = F, file = paste0(output_dir,"/",pset$File_R0_week))
#svg
  colors <- c("R0-cm" = "cyan", "R0-NGM"  = "red")
  p  <- ggplot(WeekDateR0, aes(x = Date)) +
    geom_line (aes(x=Date, y = R0xcmMEV, color = "R0-cm")) +
    geom_line (aes(x=Date, y = R0_week,  color = "R0-NGM")) +
    labs(x = 'Date', y = 'R0', color = "Legend") +
    scale_color_manual(values = colors, breaks=c("R0-NGM", "R0-cm"))
    #scale_fill_discrete(breaks=c('R0-NGM', 'R0-cm'))

  if (pset$iplatform<2){ print(p)}

  filenamepath = paste0(output_dir,"/",pset$File_R0_week)
  svglite(paste0(filenamepath,".svg")); print(p); invisible(dev.off())
}

r0=list(pars,WeekDateR0); r0<-setNames(r0,c("pars","WeekDateR0"))


} else if(GetOutput==0)  {
  
r0=list(pars); r0<-setNames(r0,c("pars"))  }


return(r0)

} #GetOutput