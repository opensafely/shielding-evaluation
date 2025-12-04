pars <- list()
pars <- within(pars, {
    #Susceptibility
    u    <- c(0.40, 0.39, 0.38, 0.72, 0.86, 0.80, 0.82, 0.88, 0.74) #derived from lit
    #Critically infected fraction
    y    <- c(0.29, 0.27, 0.21, 0.26, 0.33, 0.40, 0.49, 0.63, 0.69) #derived from lit
    y0   <- y
    #Hospitalisation fraction
    h    <- c(0.0000, 0.0023, 0.0080, 0.0080, 0.0100, 0.0190, 0.0540, 0.1510, 0.4755) #derived from lit
    h[1] <- 0.0017   #log lin regression

    #Full prob of hosp
    yh   <- y*h

    ##Mort fraction outside hospital
    ad   <- 1        #proportionality factor

	#Shield (0, 1) and period (a, b) split
    #rm6
    m_0  <- c(0, 0.015, 0.009, 0.005, 0.016, 0.040, 0.084, 0.182, 0.409) #derived
    ma_0 <- c(0,     0, 0.026, 0.004, 0.025, 0.050, 0.122, 0.249, 0.498) #derived
    mb_0 <- c(0, 0.027,     0, 0.003, 0.010, 0.027, 0.039, 0.108, 0.292) #derived
    m_1  <- c(0,     0,     0, 0.016, 0.029, 0.096, 0.136, 0.221, 0.417) #derived
    ma_1 <- c(0,     0,     0, 0.030, 0.019, 0.104, 0.148, 0.257, 0.450) #derived
    mb_1 <- c(0,     0,     0, 0.034, 0.020, 0.074, 0.126, 0.181, 0.375) #derived

	#Shield (0, 1) split    
    h_0  <- h
    h_1  <- h
    
    ad_0 <- ad            #for R0, BETA, model
    ad_1 <- ad
    
    d_0  <- 0.2*h_0*m_0   #for R0, BETA, model
    d_1  <- 0.2*h_1*m_1

    y_0  <- y
    y_1  <- y
       
    yh_0  <- y_0*h_0
    yh_1  <- y_1*h_1

    ##Total population 
    Npopcoh    <- 23971680     #cohort size without care home residents 
    Npop       <- 56426280     #ONS-England population size mid 2020 without care home residents  
    ##England demography excluding care homes 
    ageons     <- c(0.04720406, 0.08843164, 0.07019831, 0.15191402, 0.13543311, 0.12743071, 0.13685126, 0.10701745, 0.13551944); ageons=ageons/sum(ageons)
    agecoh     <- c(0.04309295, 0.08317941, 0.06714483, 0.15433289, 0.14330911, 0.13128317, 0.13729614, 0.10707984, 0.13328165); agecoh=agecoh/sum(agecoh)
    ##Proportion shielding by age group 
    psagecomix = c(0,     0,     0,     0.0328, 0.0565, 0.0704, 0.1003, 0.1433, 0.2067)
    pscomix = sum(psagecomix*ageons)
    ##to Dec 2020
    psagecoh = c(0.007, 0.007, 0.008, 0.011, 0.016, 0.028, 0.048, 0.082, 0.137)
    pscoh    = sum(psagecoh*pars$ageons)
        
    dt     <- 0.1             #time step (days)
    ndays  <- 364             #no. days
    nt     <- ndays/dt        #no. time points
    na     <- 9               #no. age groups
    nw     <- 52              #week length of model run
    iw1    <- 1+(0:(nw-1))*7/dt  #vector indices at start of each week (from 1 in R, apply "-1" in c++)
    rseed  <- 0*ageons        #external infections, per age group
    rEI    <- 1/2             #latency
    rEU    <- rEI             #latency
    rIR    <- 1/3             #recovery
    rUR    <- rIR             #recovery
    IHoIR  <- 1
    rIH    <- rIR/(IHoIR)     #hospitalisation
    
    rHR    <- 1/12.00         #recovery rate in hospital
    rHRa   <- 1/10.07         #recovery rate in hospital
    rHRb   <- 1/14.20         #recovery rate in hospital
    rHD    <- 1/13.91         #death rate in hospital	
    rHDa   <- 1/12.50         #death rate in hospital	
    rHDb   <- 1/16.78         #death rate in hospital	

    rIO    <- rIH             #death rate outside hospital - assumed here
    rOD    <- rHD             #delay in death outside hospital in addition to usual time to hopitalisation
    rRS    <- 0
    rC     <- 1/8.0           #rate of loss of positivity
    R0     <- 2
    fu     <- 0.223           #relative infectiousness of U group
    fuO    <- 0               #relative infectiousness in O stage of transition I to D (DO)
    beta   <- 0.5             #probability of infection on contact

    pE0    <- 1000/Npop       #Initial proportion of population in E state (sum over age groups)
    agehosp<- c(1,1,0,0, 1,1,2,4,8); agehosp= agehosp/sum(agehosp) #proportion hospitalised in start week
    pI0    <- 100/Npop        #Initial proportion of population in I state (sum over age groups)    
    Na0    <- ageons*Npop     #Initial population size by age group
    Ea0    <- Na0*pE0         
    Ia0    <- (agehosp*Npop)*pI0
    Ua0    <- Na0*0.00
    Ha0    <- Na0*0.00
    Oa0    <- Na0*0.00
    Ra0    <- Na0*0.00
    Da0    <- Na0*0.00
    Sa0    <- Na0-Ea0-Ia0-Ua0-Ha0-Oa0-Ra0-Da0
    k      <- 1               #dispersion/shape parameter of NB likelihood (or noise if simulating) 
    kH     <- k               #dispersion/shape parameter - hospitalisations
    kDH    <- k               #dispersion/shape parameter - deaths in hospital 
    kDO    <- k               #dispersion/shape parameter - deaths outside hospital 
    ktot   <- 10              #magnification for totals
	
	#est
	parsKH <- 3 
	parsKD <- 1.05
	parsKDH<- 1.65
	parsKDO<- 0.5
	
    cmdim1 <- na
    cmdim2 <- na
    cmdim3 <- 52
    cm_0   <- as.vector(array(0,dim=c(cmdim1,cmdim2,cmdim3)))
    cm_1   <- cm_0
    cm     <- cm_0


})
