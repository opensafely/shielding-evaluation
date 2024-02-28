## R0 in week 1 (max contacts) - to calculate beta in likelihood of SEIUHRD

BETA <- function(pars) {
cm1  = cm[,,1]   #main variable
ngm1 = cm1
u    = pars$u
fu   = pars$fu
ad   = pars$ad  #est
rIR  = pars$rIR #est
rIH  = pars$rIH
rIO  = 6*pars$rID  #tIO = 1/rIO = (1/6)*(1/rID)
rOD  = (1/5)*rIO   #tOD = 1/rOD = (5/6)*(1/rID), corresponds to 5 out of 6 compartments
orOD = 1/rOD
rUR  = pars$rUR
for (k in 1:9){ h_k = pars$h[k]; y_k = pars$y[k]; d_k=pars$d[k]*ad; for (j in 1:9) {
      #ngm1[j,k] = u[j]*cm1[j,k]*( y_k/( rIR*(1-h_k-d_k) + rIH*(h_k) + (rIO + fu*rOD)*(d_k) ) + fu*(1-y_k)/rUR) }}
      ngm1[j,k] = u[j]*cm1[j,k]*( y_k*( (1+fu*(rIO*d_k*orOD))/( rIR*(1-h_k-d_k) + rIH*(h_k) + rIO*(d_k) )) + fu*(1-y_k)/rUR) }}

#calculate beta based on proposed R0 and max ev of NGM in week 1, i.e. how must scale NGM to achieve this R0 
#beta = pars$R0/max(eigen(ngm1[,])[[1]])
beta = pars$R0/max(c(eigen(ngm1[,])[[1]],0.1))

return (beta)
}