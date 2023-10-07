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
    dt     <- 0.1 #0.01#      #time step (days)
    ndays  <- 364             #no. days
    nt     <- ndays/dt        #no. time points
    na     <- 9               #age groups
    rEI    <- 1/4 #1/3        #latency, Davies Nat Med
    rEU    <- rEI             #latency
    rIR    <- 1/5             #recovery, Davies Nat Med
    rUR    <- rIR             #recovery
    rIH    <- 1/8.5           #hospitalisation, Davied Lancet PH
    rHR    <- 1/8.6           #recovery at hospital	
    rHD    <- rHR             #death, Roz - varied during pandemic; 7d in Davies Lancet PH
    rRS    <- 0
    rC     <- 1/10   #cf ND   #rate of loss of positivity
    R0     <- 3               #2020-wild-type basic reproduction rate, Knock et al 2021
    fu     <- 0.5             #relative transmission of U group
    beta   <- R0/(mean(u*y)/(rIR*(1-mean(h))+rHR*(mean(h)) + fu*mean(u*(1-y))/rUR ))# 1# 
    Npop   <- 1e6             #population size
    Na0    <- Npop*agedens    #Initial population size by age group 
    Ea0    <- Na0*0.015       #ODE-accrued 29/01/20-24/02/20 based Davies Lancet PH, Nat Med
    Ia0    <- Na0*0.00
    Ua0    <- Na0*0.00
    Ha0    <- rep(0,na)
    Ra0    <- rep(0,na)
    Da0    <- rep(0,na)
    Sa0    <- Na0-Ea0-Ia0-Ua0-Ha0-Ra0-Da0
    cmdim1 <- na
    cmdim2 <- na
    cmdim3 <- 52
    cm     <- as.vector(array(0,dim=c(cmdim1,cmdim2,cmdim3)))
})
