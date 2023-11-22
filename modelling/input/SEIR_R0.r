## R0 by week

R0_week = vector()
rIR = pars$rIR
u   = pars$u
ngm = cm

if (pset$imodel==1){
for (i in 1:pars$cmdim3) { #1:52
  for (j in 1:9){ u_j = u[j]; for (k in 1:9) {
    ngm[j,k,i] = u_j*cm[j,k,i]*( 1/rIR )    }}
  #pre-beta R0
  R0_week[i] =max(eigen(ngm[,,i])[[1]]) } #max eigenvalue of NGM
}

if (pset$imodel==2){
  rIH = pars$rIH
  rID = pars$rID
  rUR = pars$rUR
  fu  = pars$fu
  ad  = pars$ad
  for (i in 1:pars$cmdim3) { #1:52
    for (k in 1:9){ y_k=pars$y[k]; h_k=pars$h[k]; d_k=pars$d[k]*ad; for (j in 1:9) {
            ngm[j,k,i] = u[j]*cm[j,k,i]*( y_k/( rIR*(1-h_k-d_k) + rIH*(h_k) + rID*(d_k)) + fu*(1-y_k)/rUR) }}
  R0_week[i] =max(eigen(ngm[,,i])[[1]]) } #max eigenvalue of NGM
}

#calculate beta based on input R0 and NGM, i.e. how must scale NGM to achieve R0 in week 1 (max contacts)
beta = pars$R0/R0_week[1]
#calculate R0 over time based on beta
R0_week = beta*R0_week
#store beta in pars
pars$beta = beta
#save R0 values over time
write.csv(R0_week, row.names = F, file = paste0(output_dir,"/",pset$File_R0_week))
