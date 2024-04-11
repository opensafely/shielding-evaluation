pars <- list()
pars <- within(pars, {
  #TODO: re-weigh u y h m with ons
    u    <- c(0.40, 0.39, 0.38, 0.72, 0.86, 0.80, 0.82, 0.88, 0.74) #susceptibility - derived from Davies Nat Med
    u_mean <- mean(u)
    #Critically infected fraction among exposed
    y    <- c(0.29, 0.27, 0.21, 0.26, 0.33, 0.40, 0.49, 0.63, 0.69) #critical fraction - derived from Davies Nat Med
    #Hospitalisation fraction among critically infected
    h    <- c(0.0000, 0.0023, 0.0080, 0.0080, 0.0100, 0.0190, 0.0540, 0.1510, 0.4755) #derived from Davies 2020 Lancet
    h[1] <- 0.0017 #age1 in log lin regression
    #Mortality fraction among hospitalised
    m    <- c(0.0000, 0.004, 0.005, 0.006, 0.018, 0.048, 0.094, 0.193, 0.411) #*cohort filtered events
    m[1] <- 0.0021 #age1 in log lin regression
    ##Mortality fraction among critically infected outside hospital
    ad   <- 0.1    #proportionality factor in d
    d    <- 2*h*m  #use da[a] = ad*d[a] #derived
    #Shield split
    m_0  <- c(0, 0.005, 0.006, 0.005, 0.016, 0.040, 0.084, 0.182, 0.409) #*
    m_1  <- c(0,     0,     0, 0.016, 0.033, 0.099, 0.138, 0.222, 0.418) #*
    #Shield split for testing
    ad_0 <- ad
    ad_1 <- ad
    y_0  <- y
    y_1  <- y
    h_0  <- h
    h_1  <- h
    d_0  <- d
    d_1  <- d
    m_0  <- m_0
    m_1  <- m_1
    ##England demography, Census 2021, CoMix age-bands,  ukpopestimatesmid2020on2021geography....xls

    ageons <- c(0.0466, 0.0873, 0.0693, 0.14997, 0.1337, 0.1258, 0.1351, 0.1058, 0.1358); ageons=ageons/sum(ageons)
    ##Cohort0 age distribution
    agecoh <- c(0.043 , 0.083,  0.067,  0.154,  0.143,  0.131,  0.137,  0.107,  0.135);   agecoh=agecoh/sum(agecoh)

    ##Proportion shielding by age group 
    ##Comix (tables_shielding_variable.pdf, 17dec)
    psagecomix = c(0,     0,     0,     0.0328, 0.0565, 0.0704, 0.1003, 0.1433, 0.2067)
    pscomix = sum(psagecomix*ageons) #0.078950 #7.9%, while 8.7% in CoMix
    ##cohort0 flags to Sep 2021
    #psagecoh = c(0.007, 0.007, 0.009, 0.021, 0.040,  0.0580, 0.0760, 0.1060, 0.2230) 
    #pscoh    = sum(psagecoh*agecoh) #0.069896 #7.0%
    ##cohort0 flags to Dec 2020, shieldA
    psagecoh = c(0.007, 0.007, 0.008, 0.011, 0.016, 0.028, 0.048, 0.082, 0.137)
    pscoh    = sum(psagecoh*pars$ageons)   #0.043025 #4.3% as JDat13.
        
    dt     <- 0.1             #time step (days)
    ndays  <- 364             #no. days
    nt     <- ndays/dt        #no. time points
    na     <- 9               #no. age groups
    nw     <- 52              #week length of model run
    iw1    <- 1+(0:(nw-1))*7/dt  ##vector indices at start of each week (starts at 1 for R, apply "-1" for c++)
    rseed  <- 0*ageons        #2*84*ageons #external infections, per age group, 2/county/day, 84 counties (regardless of internal contact matrix)
    rEI    <- 1/2             #1/3 #latency, Davies Nat Med
    rEU    <- rEI             #latency
    rIR    <- 1/3 #J5h3 est   #1/5 #recovery, Davies Nat Med
    rUR    <- rIR             #recovery
    rIH    <- 1/8.5           #hospitalisation, Davied Lancet PH
    rHR    <- 1/12.00         #1/8.6  #recovery rate in hospital, cohort
    rHD    <- 1/13.91         #rHR    #death rate in hospital, cohort	
    #rHD    <- rHR            #death rate in hospital, RE - varied during pandemic; 7d in Davies Lancet PH
    #rID    <- 1/(1/rIH+1/rHD)#death outside hospital - assumed here
    rIO    <- rIH             #death outside hospital - assumed here
    rOD    <- rHD             #delay in death outside hospital in addition to usual time to hopitalisation
    rRS    <- 0
    rC     <- 1/8.5           #rate of loss of positivity, Davies Lancet ID
    R0     <- 2.23            #3 #2020-wild-type basic reproduction rate, Knock et al 2021
    fu     <- 0.5             #relative transmission of U group, Davies Lancet ID
    beta   <- 1               #transmission rate between two given individuals
    Npopcoh<- 24.02*1e6       #cohort size
    Npop   <- 56.55*1e6       #ONS-England population size mid 2020, ukpopestimatesmid2020on2021geography....xls
    pE0    <- 0.0030          #170k #Initial proportion of population in E state (sum of age groups)
    pI0    <- 0.000           #Initial proportion of population in I state (sum of age groups)    
    Na0    <- ageons*Npop     #Initial population size by age group - updated 30jan from agecoh*Npop 
    Ea0    <- Na0*pE0         
    Ia0    <- Na0*pI0
    Ua0    <- Na0*0.00
    Ha0    <- Na0*0.00
    Oa0    <- Na0*0.00
    Ra0    <- Na0*0.00
    Da0    <- Na0*0.00
    Sa0    <- Na0-Ea0-Ia0-Ua0-Ha0-Oa0-Ra0-Da0
    logPI0 <- log10((Ea0[2]+Ia0[2]+Ua0[2])/Na0[2]) #proportion of infection in age-group 2
    k      <- 5#1               #dispersion/shape parameter of NB likelihood (and noise if simulating), Davies 2020 Nat Med
    kH     <- k               #dispersion/shape parameter - hospitalisation counts
    kDH    <- k               #dispersion/shape parameter - deaths in hospital counts
    kDO    <- k               #dispersion/shape parameter - eaths outside hospital counts
    sdH    <- 0.67 #jobs est  #scaling factor of sd for H across age groups
    cmdim1 <- na
    cmdim2 <- na
    cmdim3 <- 52
    cm_0   <- as.vector(array(0,dim=c(cmdim1,cmdim2,cmdim3)))
    cm_1   <- cm_0
    cm     <- cm_0
})
