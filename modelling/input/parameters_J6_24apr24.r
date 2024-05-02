pars <- within(pars, {
#run 24-04
age  = c(mean(0:4),mean(5:11),mean(12:17),mean(18:29),mean(30:39),mean(40:49),mean(50:59),mean(60:69),mean(70:90)) #85))
age9 = age[9]
age1 = age[1]
age3 = age[3]

hM_0=0.3796#0.3068
hM_1=0.9235#0.9096
hR_0=0.0416#0.0398
hR_1=0.0011#9e-04

dM_0=0.0253#0.0219
dM_1=0.0857#0.1036
dR_0=0.0909#0.0903
dR_1=0.3923#0.6061

yM1_0=0.4326#0.6498
yM1_1=0.1738#0.1609
yR1_0=0.0105#0.016
yR1_1=0.0151#0.0115

h_0 = hM_0*exp((age-age9)*hR_0)
h_1 = hM_1*exp((age-age9)*hR_1)
d_0 = dM_0*exp((age-age9)*dR_0) #0.2*pars$h_0*pars$m_0  *pars$ad (default=1)
d_1 = dM_1*exp((age-age9)*dR_0) #0.2*pars$h_1*pars$m_1  *pars$ad (default=1)

y_0  <- y
y_1  <- y
y_0[3:9] = yM1_0*exp((age[3:9]-age9)*yR1_0)
y_1[3:9] = yM1_1*exp((age[3:9]-age9)*yR1_1)


rIR    <- 1/2.707   #1/3 #J5h3 est    #1/5 #recovery, Davies Nat Med
rIH    <- 1/11.938  #1/8.5            #hospitalisation, Davies Lancet PH
rOD    <- 1/11.364  #rHD              #delay in death outside hospital in addition to usual time to hopitalisation
pE0    <- 1076/Npop #0.0030           #170k #Initial proportion of population in E state (sum of age groups)
Ea0    <- Na0*pE0         
Sa0    <- Na0-Ea0-Ia0-Ua0-Ha0-Oa0-Ra0-Da0

beta   <- 2.45      #2.37302 #1       #transmission rate between two given individuals

})
