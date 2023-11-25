pset <- list()
pset <- within(pset, {
    TODAY <- format(Sys.Date(), "%d-%m-%Y")
    TIME  <- format(Sys.time(),'%H.%M.%S_%d-%m-%Y')
    ## Job no.
    Job="J4j2_" #J1=lm, J2=fit-simul, J3=SEIR_fit, J4...=SEIUHRD fits
    ## Model choice
    MODEL    <- c("SEIR", "SEIUHRD");
    imodel   <- 2#1; #2
    ### OpenSafely
    PLATFORM <- c("Simulate", "Fit dummy data", "OpenSafely"); #0, 1, 2
    iplatform<- 2#0#1#
    
    ### model fitting
    DOfit    <- 1; #(0=Dont fit, 1=simulated or OS data)
    
    ### model output length (weeks)
    nw       <- 52 #week length of run - resolution of contact matrix (really two weeks)
    
    ### Read contact data - options
    DOcmREAD <- 0; if (iplatform>0) {DOcmREAD=0} #(0=read normalised vector; 1=read original matrix files)
    DOcmONE  <- 0; if (iplatform>0) {DOcmONE=0}  #(0=use CoMix, 1=Use cm=1 in all its elements)
   
    ### input file names
    File_model_choice    <- paste0(MODEL[imodel],"aout_model.cpp")
    #File_model_choice    <- paste0(MODEL[imodel],"_model.cpp")
    File_parameters      <- paste0(MODEL[imodel],"_parameters.r")
    File_contact_data    <- "Contact_matrix_year-from-24Feb20_vector-lenght-9x9x52_norm=maxEV1.csv"

    ### output file names
    File_run             <- paste0(Job,"Simul_run.txt")
    File_contact_summary <- paste0(Job,"Contact_matrix_year-start-24Feb20_stats.txt")
    File_R0_week         <- paste0(Job,"Simul_R0_by_week.csv")
    File_data_plots      <- paste0(Job,"Data_Incidence.pdf")
    File_model_sim_plots <- paste0(Job,"Simul_Infected_and_R0.pdf")
    File_model_sim_data  <- paste0(Job,"Simul_Incidence.csv")
    File_fit_output      <- paste0(Job,"Fit_MCMC_BT.pdf")
    File_fit_variables   <- paste0(Job,"Fit_Variables_estimated.pdf")
    File_fit_data1       <- paste0(Job,"Fit_Variables_data1.pdf")
    File_fit_data2       <- paste0(Job,"Fit_Variables_data2.pdf")
    File_fit_summary     <- paste0(Job,"Fit_Bayesian_report.txt")
    File_data_hosp       <- paste0(Job,"covid_hosp_over_time2again.pdf")
    File_data_deaths     <- paste0(Job,"covid_deaths_over_time2again.pdf")
})