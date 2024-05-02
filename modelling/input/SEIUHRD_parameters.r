pars <- list()
pars <- within(pars, {
  #TODO: Re-weight u y h again with latest ons
    #Susceptibility
    u    <- c(0.40, 0.39, 0.38, 0.72, 0.86, 0.80, 0.82, 0.88, 0.74) #derived from Davies Nat Med
    #Critically infected
    y    <- c(0.29, 0.27, 0.21, 0.26, 0.33, 0.40, 0.49, 0.63, 0.69) #derived from Davies Nat Med
    #Hospitalisation fraction 
    h    <- c(0.0000, 0.0023, 0.0080, 0.0080, 0.0100, 0.0190, 0.0540, 0.1510, 0.4755) #derived from Davies 2020 Lancet

    #Mort fraction
    m    <- c(0.0000, 0.004, 0.005, 0.006, 0.018, 0.048, 0.094, 0.193, 0.411) #derived

    ##Mort fraction outside hospital da[a] = ad*d[a]
    ad   <- 0.1   #propotionality fraction in d - assumed and estimated
    d    <- 2*h*m #model input: da[a] = ad*d[a]                       #derived
    
    ##England population age distribution, Census 2021, CoMix age-bands 
    ageons <- c(0.0466, 0.0873, 0.0693, 0.14997, 0.1337, 0.1258, 0.1351, 0.1058, 0.1358); ageons=ageons/sum(ageons)
    agecoh <- c(0.043 , 0.083,  0.067,  0.154,   0.143,  0.131,  0.137,  0.107,  0.135);  agecoh=agecoh/sum(agecoh)

    phm    <- 1               #correction in proportion of H that become D
    dt     <- 0.1 #0.01#      #time step (days)
    ndays  <- 364             #no. days
    nt     <- ndays/dt        #no. time points
    na     <- 9               #no. age groups
    nw     <- 52              #week length of model run
    iw1    <- 1+(0:(nw-1))*7/dt  ##vector indices at start of each week (from 1, for R, apply "-1" for c++)
    rseed  <- 0*ageons        #(0.45)*2*84*ageons #mean(l, l2, nHbD) #external infections, per age group, 2/county/day x 84 counties (regardless internal contact matrix)
    rEI    <- 1/2             #1/3 #latency, Davies Nat Med
    rEU    <- rEI             #latency
    rIR    <- 1/3 #J5h3 est   #1/5 #recovery, Davies Nat Med
    rUR    <- rIR             #recovery
    rIH    <- 1/8.5           #hospitalisation, Davied Lancet PH
    rHR    <- 1/12.00         #recovery rate in hospital 
    rHD    <- 1/13.91         #rHR #death rate in hospital 
    #rHD    <- rHR            #death rate in hospital, RE - varied during pandemic; 7d in Davies Lancet PH
    #rID    <- 1/(1/rIH+1/rHD)#death rate outside hospital - assumed here
    rIO    <- rIH             #death rate outside hospital - assumed here
    rOD    <- rHD             #delay in death outside hospital in addition to usual time to hopitalisation
    rRS    <- 0
    rC     <- 1/8.5           #rate of loss of positivity, Russell et al; Davies Lancet ID
    R0     <- 2.23            #2020-wild-type basic reproduction rate, Knock et al 2021
    fu     <- 0.5             #relative transmission of U group
    beta   <- 1               #transmission rate between two given individuals
    Npopcoh<- 24.02*1e6       #cohort size
    Npop   <- 56.55*1e6       #ONS-England population size mid 2020 
    pE0    <- 0.0030          #170k #Initial proportion of population in E state (sum of age groups)
    pI0    <- 0.000           #Initial proportion of population in I state (sum of age groups)    
    Na0    <- ageons*Npop     #Initial population size by age group 
    Ea0    <- Na0*pE0
    Ia0    <- Na0*pI0
    Ua0    <- Na0*0.00
    Ha0    <- Na0*0.00
    Oa0    <- Na0*0.00
    Ra0    <- Na0*0.00
    Da0    <- Na0*0.00
    Sa0    <- Na0-Ea0-Ia0-Ua0-Ha0-Ra0-Da0
    logPI0 <- log10((Ea0[2]+Ia0[2]+Ua0[2])/Na0[2]) #proportion of infection in age-group 2
    k      <- 1               #dispersion/shape parameter of NB likelihood (and noise if simulating)
    kH     <- k               #dispersion/shape parameter - hospitalisations
    kDH    <- k               #dispersion/shape parameter - deaths in hospital
    kDO    <- k               #dispersion/shape parameter - deaths outside hospital
    sdH    <- 0.67 #jobs est  #scaling factor of sd for H across age groups
    cmdim1 <- na
    cmdim2 <- na
    cmdim3 <- 52
    cm     <- as.vector(array(0,dim=c(cmdim1,cmdim2,cmdim3)))
})
