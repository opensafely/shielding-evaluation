## R0 in week 1 (max contacts) - to calculate beta in likelihood of model
## Assumptions:
##   * R0 (whole pop) ~ R0_0
##   * beta_1 ~ beta_0
##   * Calculate beta_0 or R0_0 from the _0 population parameters: y_0, h_0, m_0, d_0, ad_0
## parameters updated by caller of this code:
##    * pars$beta = beta_0
##

BETA <- function(pars) {
cm1  = cm_0[,,1]   #main variable
ngm1 = cm1
u    = pars$u     #u_1 = u_0 = u => don't need specify the participant's contacts as _0 or _1
fu   = pars$fu    #est
fuO  = pars$fuO
rIR  = pars$rIR   #est
rIH  = pars$rIH
rIO  = pars$rIO   #est
rOD  = pars$rOD   #est
orOD = 1/rOD
orUR = 1/pars$rUR #est
ad   = pars$ad_0
for (k in 1:9){ 
  h_k = pars$h_0[k];    #est
  y_k = pars$y_0[k];    #est
  d_k = pars$d_0[k]*ad; #est
  for (j in 1:9) {
      ngm1[k,j] = u[j]*cm1[k,j]*( y_k*( (1+fuO*(rIO*d_k*orOD))/( rIR*(1-h_k-d_k) + rIH*(h_k) + rIO*(d_k) )) + fu*(1-y_k)*orUR) }}

#calculate beta based on proposed R0 and max ev of NGM in week 1, i.e. how must scale NGM to achieve this R0 
beta_0 = pars$R0/max(c(Re(eigen(ngm1[,])[[1]]),0.1))

return (beta_0)
}