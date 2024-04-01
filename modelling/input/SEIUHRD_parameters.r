pars <- list()
pars <- within(pars, {
  #TODO: re-weigh u y h m with ons
    u <- c(0.40, 0.39, 0.38, 0.72, 0.86, 0.80, 0.82, 0.88, 0.74) #susceptibility - derived from Davies Nat Med
    #Critically infected among exposed
    y <- c(0.29, 0.27, 0.21, 0.26, 0.33, 0.40, 0.49, 0.63, 0.69) #critical fraction - derived from Davies Nat Med
    #Hospitalisation fraction among critically infected
    h <- c(0.0000, 0.0023, 0.0080, 0.0080, 0.0100, 0.0190, 0.0540, 0.1510, 0.4755) #derived from Davies 2020 Lancet

    #cohort age-prob of hospitalised (not odds of hosp if given age) - disregards odds of infection (exposure & susc)
    hcoh <- c(0.0040, 0.0046, 0.0076, 0.0397, 0.0604, 0.0887, 0.1478, 0.1637, 0.4835) #sum(hcoh)=1
    #total contacts (adhoc normalisation) - JF25jan24 plot of EL data
    #c <- c(1.0, 1.05, 1.0, 0.46, 0.53, 0.55, 0.45, 0.40, 0.30) # round(c/min(c),4) - min <> 70+ (more confident of h agreement)
    c <- c(3.3333, 3.5000, 3.3333, 1.5333, 1.7667, 1.8333, 1.5000, 1.3333, 1.0000)
    h02 <- c(0.0012, 0.0013, 0.0023, 0.0259, 0.0342, 0.0484, 0.0985, 0.1228, 0.4835) #round(hcoh/(c/min(c)),4)
    #h03 <- round((hcoh/ageons)*(h[9]/max(hcoh/ageons))/c,4)
    h03 <- c(0.0034, 0.0020, 0.0044, 0.0231, 0.0342, 0.0514, 0.0974, 0.1550, 0.4755)
           #close to h (Davies), but less steep rise with age

    #Mortality fraction
    #m <- c(0.0000, 0.1125, 0.1125, 0.1229, 0.1200, 0.1211, 0.1259, 0.1238, 0.1243) #derived from Davies 2020 Lancet
	          #m - mort prop - Davies assumes 1/m ~ 8.04 => variation seen within not very meaningful
    m <- c(0.0000, 0.004, 0.005, 0.006, 0.018, 0.048, 0.094, 0.193, 0.411) #from full reporting in cohort
            #sum(m)=0.779 not meaningful, the fraction applies within ageg only
            #close to h: m/h = NaN 1.739 0.625 0.750 1.800 2.526 1.741 1.278 0.864
    ##England population age distribution based on Census 2021, GOV.uk, CoMix age-bands 
    ageons <- c(0.0541, 0.0833, 0.0705, 0.1484,  0.1371, 0.1268, 0.1366, 0.1071, 0.1360);  ageons=ageons/sum(ageons) #sum 1 <= sum 0.9999
    ##update 30jan - ukpopestimatesmid2020on2021geography_JF_ENGdemography.xls
    ageons <- c(0.0466, 0.0873, 0.0693, 0.14997, 0.1337, 0.1258, 0.1351, 0.1058, 0.1358); ageons=ageons/sum(ageons)
    ##TPP cohort age distribution
    #AH agecoh <- c(0.043 , 0.082,  0.066,  0.152,  0.141,  0.129,  0.136,  0.106,  0.134);   agecoh=agecoh/sum(agecoh  #cohort; missing data: 0.01
    #update 30jan based on cohort0
    agecoh <- c(0.043 , 0.083,  0.067,  0.154,  0.143,  0.131,  0.137,  0.107,  0.135);   agecoh=agecoh/sum(agecoh)  #cohort; missing data: 0

    u_mean <- mean(u)
    ##mortality fraction outside hospital da[a] = ad*d[a] (among I to H, I to R, and I to D)
    ad     <- 0.1 #0 #small, aim 0.5 #propotionality fraction in d - assumed and estimated
    d      <- 2*h*m #use=d*ad #fraction of direct deaths among critically infected - assumed
    phm    <- 1               #correction in proportion of H that become D
    dt     <- 0.1 #0.01#      #time step (days)
    ndays  <- 364             #no. days
    nt     <- ndays/dt        #no. time points
    na     <- 9               #age groups
    nw     <- 52              #week length of model run
    iw1    <- 1+(0:(nw-1))*7/dt  ##vector indices at start of each week (starts at 1, for R, apply "-1" for c++)
    rseed  <- 0*ageons #(0.45)*2*84*ageons #mean(l, l2, nHbD) # 5*ageons  #external infections, each age group, 2/county/day, 84 counties (regardless of internal contact matrix)
                              #[1]  7.912914 14.823979 11.767488 25.465660 22.702932 21.361472 22.940659 17.965372 23.059523
    rEI    <- 1/2             #1/3 #latency, Davies Nat Med
    rEU    <- rEI             #latency
    rIR    <- 1/3 #J5h3 est ##1/7#1/5         #recovery, Davies Nat Med
    rUR    <- rIR             #recovery
    rIH    <- 1/8.5           #hospitalisation, Davied Lancet PH
    rHR    <- 1/12.00 #1/8.6  #recovery rate in hospital - updated 30jan from full reporting in cohort
    rHD    <- 1/13.91 #rHR    #death rate in hospital    - updated 30jan from full reporting in cohort	
    #rHD    <- rHR            #death rate in hospital, Roz - varied during pandemic; 7d in Davies Lancet PH
    #rID    <- 1/(1/rIH+1/rHD)#death outside hospital - assumed here
    rIO    <- rIH             #death outside hospital - assumed here
    rOD    <- rHD             #delay in death outside hospital vs death since infection via hospital
    rRS    <- 0
    rC     <- 1/8.5           #rate of loss of positivity, Davies Lancet ID
    R0     <- 2.23 #3         #2020-wild-type basic reproduction rate, Knock et al 2021
    fu     <- 0.5             #relative transmission of U group
    beta   <- 1               #transmission rate between two given individuals
    Npopcoh<- 24.02*1e6       #cohort size - updated 30jan
    Npop   <- 56.55*1e6       #ONS-England population size mid 2020 - updated 30jan - ukpopestimatesmid2020on2021geography_JF_ENGdemography.xls
    pE0    <- 0.0030          #0.015 #Initial proportion of population in E state (sum of age groups)
    pI0    <- 0.000           #Initial proportion of population in I state (sum of age groups)    
    Na0    <- ageons*Npop     #Initial population size by age group - updated 30jan from agecoh*Npop 
    Ea0    <- Na0*pE0         #ODE-accrued 29/01/20-24/02/20 based Davies Lancet PH, Nat Med
    Ia0    <- Na0*pI0
    Ua0    <- Na0*0.00
    Ha0    <- Na0*0.00
    Oa0    <- Na0*0.00
    Ra0    <- Na0*0.00
    Da0    <- Na0*0.00
    Sa0    <- Na0-Ea0-Ia0-Ua0-Ha0-Ra0-Da0
    logPI0 <- log10((Ea0[2]+Ia0[2]+Ua0[2])/Na0[2]) #proportion of infection in age-group 2
    k      <- 1               #dispersion/shape parameter of NB likelihood (and noise if simulating), Davies 2020 Nat Med
    kH     <- k               #dispersion/shape parameter - hospitalisation counts
    kDH    <- k               #dispersion/shape parameter - deaths in hospital counts
    kDO    <- k               #dispersion/shape parameter - deaths outside hospital counts
    sdH    <- 0.67 #jobs est  #scaling factor of sd for H across age groups
    cmdim1 <- na
    cmdim2 <- na
    cmdim3 <- 52
    cm     <- as.vector(array(0,dim=c(cmdim1,cmdim2,cmdim3)))
})
