## R0 by week

R0_week <- vector()

ngm <- cm
for (i in 1:nd) { 
  for (j in 1:9){ for (k in 1:9) { 
    if (pset$imodel==1){
      ngm[j,k,i] = pars$beta * pars$u[j]*cm[j,k,i]*( 1/pars$rIR ) }
    if (pset$imodel==2){
      ngm[j,k,i] = pars$beta * pars$u[j]*cm[j,k,i]*( pars$y[k]/(pars$rIR*(1-pars$h[k]) + pars$rHR*(pars$h[k])) + pars$fu*mean(1-pars$y[k])/pars$rUR) }
  }}
  R0_week[i] =max(eigen(ngm[,,i])[[1]]) } #max eigenvalue of NGM
write.csv(R0_week, row.names = F, file = paste0(output_dir,"/",pset$File_R0_week))
