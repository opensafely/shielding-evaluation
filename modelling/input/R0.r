## R0 by week
## cmMEV = cm maxEV by week

R0_week = vector()
cmMEV = vector()
rIR = pars$rIR
u   = pars$u
ngm = cm

## cmMEV
for (i in 1:pars$cmdim3) { #1:52
  cmMEV[i] =max(eigen(cm[,,i])[[1]]) } #max eigenvalue of cm[[i]]

## NGM MEV
if (pset$imodel==1){
for (i in 1:pars$cmdim3) { #1:52
  for (j in 1:9){ u_j = u[j]; for (k in 1:9) {
    ngm[j,k,i] = u_j*cm[j,k,i]*( 1/rIR )    }}
  #pre-beta R0
  R0_week[i] =max(eigen(ngm[,,i])[[1]]) } #max eigenvalue of NGM
}
## NGM MEV
if (pset$imodel==2){
  rIH = pars$rIH
  rIO = 6*pars$rID  #tIO = 1/rIO = (1/6)*(1/rID)
  rOD = (1/5)*rIO   #tOD = 1/rOD = (5/6)*(1/rID), corresponds to 5 out of 6 compartments
  orOD= 1/rOD
  rUR = pars$rUR
  fu  = pars$fu
  ad  = pars$ad
  for (i in 1:pars$cmdim3) { #1:52
    for (k in 1:9){ y_k=pars$y[k]; h_k=pars$h[k]; d_k=pars$d[k]*ad; for (j in 1:9) {
            ngm[j,k,i] = u[j]*cm[j,k,i]*( y_k*( (1+fu*(rIO*d_k*orOD))/( rIR*(1-h_k-d_k) + rIH*(h_k) + rIO*(d_k) )) + fu*(1-y_k)/rUR) }}
            #ngm[j,k,i] = u[j]*cm[j,k,i]*( y_k/( rIR*(1-h_k-d_k) + rIH*(h_k) + (rIO + fu*rOD)*(d_k)) + fu*(1-y_k)/rUR) }}
  #pre-beta R0
  #assumes beta=1
  R0_week[i] =max(eigen(ngm[,,i])[[1]]) } #max eigenvalue of NGM
}

#calculate beta based on input R0 and NGM, i.e. how must scale NGM to achieve R0 in week 1 (max contacts)
beta = pars$R0/R0_week[1]
#calculate R0 over time based on beta
R0_week = beta*R0_week
#store beta in pars
pars$beta = beta
#save R0 values over time
df <- tibble(Week=1:pars$cmdim3, R0_week=R0_week, R0xcmMEV=pars$R0*cmMEV,)
write.csv(df, row.names = F, file = paste0(output_dir,"/",pset$File_R0_week))

colors <- c("R0-cm" = "cyan", "R0-NGM"  = "red")
p  <- ggplot(df, aes(x = Week)) +
  geom_line (aes(x=Week, y = R0xcmMEV, color = "R0-cm")) +
  geom_line (aes(x=Week, y = R0_week,  color = "R0-NGM")) +
  labs(x = 'Week', y = 'R0', color = "Legend") +
  scale_color_manual(values = colors, breaks=c("R0-NGM", "R0-cm"))
  #scale_fill_discrete(breaks=c('R0-NGM', 'R0-cm'))
if (pset$iplatform<2){ print(p)}
filenamepath = paste0(output_dir,"/",pset$File_R0_week)
svglite(paste0(filenamepath,".svg")); print(p); invisible(dev.off())
