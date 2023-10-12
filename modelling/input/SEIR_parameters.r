pars <- list()
pars <- within(pars, {
    u      <- c(0.40, 0.39, 0.38, 0.72, 0.86, 0.80, 0.82, 0.88, 0.74) #susceptibility - derived from Davies Nat Med
    u_mean <- mean(u)
    agedens<- c(0.0541, 0.0833, 0.0705, 0.1484, 0.1371, 0.1268, 0.1366, 0.1071, 0.1360);  agedens=agedens/sum(agedens) #sum 1 <= sum 0.9999
                              #population age distribution based on Census 2021, GOV.uk, CoMix age-bands 
    dt     <- 0.1 #0.01#      #time step (days)
    ndays  <- 364             #no. days
    nt     <- ndays/dt        #no. time points
    na     <- 9               #age groups
    nw     <- 52              #week length of model run
    iw1    <- 1+(0:(nw-1))*7/dt  ##vector indices at start of each week (starts at 1, for R, apply "-1" for c++)
    d      <- 0 #1/70         #death rate - for vital dynamics
    b      <- d               #birth rate
    rEI    <- 1/10#1/7#1/4#   #latency, Davies Nat Med
    rIR    <- 1/7#14#1/5#     #recovery, Davies Nat Med
    rRS    <- 0               #immunity waning
    R0     <- 3               #2020-wild-type basic reproduction rate, Knock et al 2021
    beta   <- R0*(rIR+d)/u_mean #transmission rate, for age-uniform, higher than NGM actual
    Npop   <- 24*1e6          #population size
    pE0    <- 0.015           #Initial proportion of population in E state (sum of age groups)
    pI0    <- 0.000           #Initial proportion of population in I state (sum of age groups)    
    Na0    <- Npop*agedens    #Initial population size by age group 
    Ea0    <- Na0*pE0         #ODE-accrued 29/01/20-24/02/20 based Davies Lancet PH, Nat Med
    Ia0    <- Na0*pI0
    Ra0    <- Na0*0.00
    Sa0    <- Na0-Ra0-Ea0-Ia0
    logPI0 <- log10((Ea0[2]+Ia0[2])/Na0[2]) #proportion of infection in age-group 2
    pdm    <- 1               #peak hosp/Iw=(6000/Npop)/0.02 #proportion of data in relation to model
    cmdim1 <- na
    cmdim2 <- na
    cmdim3 <- 52
    cm     <- as.vector(array(0,dim=c(cmdim1,cmdim2,cmdim3)))
})