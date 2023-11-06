## R0 in week 1 (max contacts) - to calculate beta in likelihood of SEIUHRD

BETA <- function(pars) {
cm1  = cm[,,1]   #main variable
ngm1 = cm1
u    = pars$u
fu   = pars$fu
rIR  = pars$rIR
rHR  = pars$rHR
rUR  = pars$rUR
for (k in 1:9){ h_k = pars$h[k]; y_k = pars$y[k]; for (j in 1:9) {
      ngm1[j,k] = u[j]*cm1[j,k]*( y_k/( rIR*(1-h_k) + rHR*(h_k) ) + fu*(1-y_k)/rUR) }}

#calculate beta based on proposed R0 and max ev of NGM in week 1, i.e. how must scale NGM to achieve this R0 
beta = pars$R0/max(eigen(ngm1[,])[[1]])

return (beta)
}