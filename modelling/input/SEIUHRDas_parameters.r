pars <- list()
pars <- within(pars, {
  #TODO: Re-weight u y h again with latest ons
    #Susceptibility
    u    <- c(0.40, 0.39, 0.38, 0.72, 0.86, 0.80, 0.82, 0.88, 0.74) #derived
    #Critically infected fraction
    y    <- c(0.29, 0.27, 0.21, 0.26, 0.33, 0.40, 0.49, 0.63, 0.69) #derived
    #Hospitalisation fraction
    h    <- c(0.0000, 0.0023, 0.0080, 0.0080, 0.0100, 0.0190, 0.0540, 0.1510, 0.4755) #derived
    h[1] <- 0.0017   #age1, log lin regression
    #Mort fraction
    m    <- c(0.0000, 0.004, 0.005, 0.006, 0.018, 0.048, 0.094, 0.193, 0.411) #derived
    m[1] <- 0.0021   #age1, log lin regression
    ##Mort fraction outside hospital
    ad   <- 1        #proportionality factor 0.1 redundant as using 0.2=0.1*2
    d    <- 0.2*h*m  #model input: da[a] = ad*d[a]                       #derived here
    #Shield split
    m_0  <- c(0, 0.005, 0.006, 0.005, 0.016, 0.040, 0.084, 0.182, 0.409) #derived
    ma_0 <- c(0,     0, 0.017, 0.007, 0.025, 0.050, 0.122, 0.249, 0.498) #derived
    mb_0 <- c(0, 0.009,     0, 0.004, 0.009, 0.029, 0.040, 0.108, 0.292) #derived
    m_1  <- c(0,     0,     0, 0.016, 0.033, 0.099, 0.138, 0.222, 0.418) #derived
    ma_1 <- c(0,     0,     0, 0.020, 0.025, 0.112, 0.150, 0.256, 0.451) #derived
    mb_1 <- c(0,     0,     0, 0.012, 0.041, 0.081, 0.123, 0.181, 0.376) #derived

    h_0  <- h
    h_1  <- h
    
    ad_0 <- ad            #need for R0, BETA
    ad_1 <- ad
    
    d_0  <- 0.2*h_0*m_0   #need for R0, BETA
    d_1  <- 0.2*h_1*m_1
    
    da_0 <- 0.2*h_0*ma_0
    db_0 <- 0.2*h_0*mb_0
    da_1 <- 0.2*h_1*ma_1
    db_1 <- 0.2*h_1*mb_1

    y_0  <- y
    y_1  <- y

    ##England demography, CoMix age-bands, 
    ageons <- c(0.0466, 0.0873, 0.0693, 0.14997, 0.1337, 0.1258, 0.1351, 0.1058, 0.1358); ageons=ageons/sum(ageons)
    agecoh <- c(0.043 , 0.083,  0.067,  0.154,   0.143,  0.131,  0.137,  0.107,  0.135);  agecoh=agecoh/sum(agecoh)

    ##Proportion shielding by age group 
    psagecomix = c(0,     0,     0,     0.0328, 0.0565, 0.0704, 0.1003, 0.1433, 0.2067)
    pscomix = sum(psagecomix*ageons)       #0.078950 #7.9% (Comix 8.7% prior truncation)
    ##shieldA to Dec 2020, 
    psagecoh = c(0.007, 0.007, 0.008, 0.011, 0.016, 0.028, 0.048, 0.082, 0.137)
    pscoh    = sum(psagecoh*pars$ageons)   #0.043025 #4.3% (JDat13 4.3%).
        
    dt     <- 0.1             #time step (days)
    ndays  <- 364             #no. days
    nt     <- ndays/dt        #no. time points
    na     <- 9               #no. age groups
    nw     <- 52              #week length of model run
    iw1    <- 1+(0:(nw-1))*7/dt  ##vector indices at start of each week (from 1 for R, apply "-1" for c++)
    rseed  <- 0*ageons        #external infections, per age group, 2/county/day x 84 counties (regardless internal contact matrix)
    rEI    <- 1/2             #1/3 #latency, Davies Nat Med
    rEU    <- rEI             #latency
    rIR    <- 1/3 #J5h3 est   #1/5 #recovery, Davies Nat Med
    rUR    <- rIR             #recovery
    rIH    <- 1/8.5           #hospitalisation, Davies Lancet PH
    
    rHR    <- 1/12.00         #recovery rate in hospital
    rHRa   <- 1/10.07 #1/12.00 #recovery rate in hospital
    rHRb   <- 1/14.20 #1/12.00 #recovery rate in hospital

    rHD    <- 1/13.91         #death rate in hospital	
    rHDa   <- 1/12.50 #1/13.91 #death rate in hospital	
    rHDb   <- 1/16.78 #1/13.91 #death rate in hospital	

    #rHD    <- rHR            #death rate in hospital, RE - varied during pandemic; 7d in Davies Lancet PH
    #rID    <- 1/(1/rIH+1/rHD)#death rate outside hospital - assumed here
    rIO    <- rIH             #death rate outside hospital - assumed here
    rOD    <- rHD              #delay in death outside hospital in addition to usual time to hopitalisation
    rODa   <- rHDa             #delay in death outside hospital in addition to usual time to hopitalisation
    rODb   <- rHDb             #delay in death outside hospital in addition to usual time to hopitalisation
    rRS    <- 0
    rC     <- 1/8.5           #rate of loss of positivity, Russell et al; Davies Lancet ID
    R0     <- 1.8 #2.23       #2020-wild-type basic reproduction rate, Knock et al 2021
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
    Sa0    <- Na0-Ea0-Ia0-Ua0-Ha0-Oa0-Ra0-Da0
    logPI0 <- log10((Ea0[2]+Ia0[2]+Ua0[2])/Na0[2]) #proportion of infection in age-group 2
    k      <- 1#10#1#5#1           #dispersion/shape parameter of NB likelihood (and noise if simulating) 
    kH     <- k               #dispersion/shape parameter - hospitalisations
    kDH    <- k               #dispersion/shape parameter - deaths in hospital 
    kDO    <- k               #dispersion/shape parameter - deaths outside hospital 
    sdH    <- 0.67 #jobs est  #scaling factor of sd for H across age groups
    cmdim1 <- na
    cmdim2 <- na
    cmdim3 <- 52
    cm_0   <- as.vector(array(0,dim=c(cmdim1,cmdim2,cmdim3)))
    cm_1   <- cm_0
    cm     <- cm_0
})

#pars <- within(pars, {
  #update
  #input_dir  <- getwd() #paste0(getwd(),"/modelling/input"))#
   source(paste0(input_dir,"/parameters_J6_10jun24.r"))
  #source(paste0(input_dir,"/parameters_J6_24apr24_rev.r"))
  #source(paste0(input_dir,"/parameters_J6_23may24.r"))
#})