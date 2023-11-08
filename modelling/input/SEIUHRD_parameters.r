pars <- list()
pars <- within(pars, {
    u <- c(0.40, 0.39, 0.38, 0.72, 0.86, 0.80, 0.82, 0.88, 0.74) #susceptibility - derived from Davies Nat Med
    y <- c(0.29, 0.27, 0.21, 0.26, 0.33, 0.40, 0.49, 0.63, 0.69) #critical fraction - derived from Davies Nat Med
    h <- c(0.0000, 0.0023, 0.0080, 0.0080, 0.0100, 0.0190, 0.0540, 0.1510, 0.4755) #derived from Davies 2020 Lancet
    m <- c(0.0000, 0.1125, 0.1125, 0.1229, 0.1200, 0.1211, 0.1259, 0.1238, 0.1243) #derived from Davies 2020 Lancet
	                            #m - mort prop - Davies assumes 1/m ~ 8.04 => variation seen within not very meaningful
    agedens<- c(0.0541, 0.0833, 0.0705, 0.1484, 0.1371, 0.1268, 0.1366, 0.1071, 0.1360);  agedens=agedens/sum(agedens) #sum 1 <= sum 0.9999
                              #population age distribution based on Census 2021, GOV.uk, CoMix age-bands 
    u_mean <- mean(u)
    phm    <- 1               #correction in proportion of H that become D
    dt     <- 0.1 #0.01#      #time step (days)
    ndays  <- 364             #no. days
    nt     <- ndays/dt        #no. time points
    na     <- 9               #age groups
    nw     <- 52              #week length of model run
    iw1    <- 1+(0:(nw-1))*7/dt  ##vector indices at start of each week (starts at 1, for R, apply "-1" for c++)
    rEI    <- 1/10#1/4 #1/3   #latency, Davies Nat Med
    rEU    <- rEI             #latency
    rIR    <- 1/7#1/5         #recovery, Davies Nat Med
    rUR    <- rIR             #recovery
    rIH    <- 1/8.5           #hospitalisation, Davied Lancet PH
    rHR    <- 1/8.6           #recovery at hospital	
    rHD    <- rHR             #death, Roz - varied during pandemic; 7d in Davies Lancet PH
    rRS    <- 0
    rC     <- 1/8.5           #rate of loss of positivity, Davies Lancet ID
    R0     <- 2.23 #3               #2020-wild-type basic reproduction rate, Knock et al 2021
    fu     <- 0.5             #relative transmission of U group
    beta   <- 1 #R0/(mean(u*y)/(rIR*(1-mean(h))+rHR*(mean(h)) + fu*mean(u*(1-y))/rUR ))# 1# 
    Npop   <- 24*1e6          #population size
    pE0    <- 0.015           #Initial proportion of population in E state (sum of age groups)
    pI0    <- 0.000           #Initial proportion of population in I state (sum of age groups)    
    Na0    <- agedens*Npop    #Initial population size by age group 
    Ea0    <- Na0*pE0         #ODE-accrued 29/01/20-24/02/20 based Davies Lancet PH, Nat Med
    Ia0    <- Na0*pI0
    Ua0    <- Na0*0.00
    Ha0    <- Na0*0.00
    Ra0    <- Na0*0.00
    Da0    <- Na0*0.00
    Sa0    <- Na0-Ea0-Ia0-Ua0-Ha0-Ra0-Da0
    logPI0 <- log10((Ea0[2]+Ia0[2]+Ua0[2])/Na0[2]) #proportion of infection in age-group 2
    pdm    <- 1               #peak hosp/Iw=(6000/Npop)/0.02 #proportion of data in relation to model
    k      <- 200             #dispersion/shape parameter of NB likelihood (and noise if simulating), Davies 2020 Nat Med
    cmdim1 <- na
    cmdim2 <- na
    cmdim3 <- 52
    cm     <- as.vector(array(0,dim=c(cmdim1,cmdim2,cmdim3)))
})
