pset <- list()
pset <- within(pset, {
    TODAY <- format(Sys.Date(), "%d-%m-%Y")
    TIME  <- format(Sys.time(),'%H.%M.%S_%d-%m-%Y')
    ## Model choice
    MODEL    <- c("SEIR", "SEIUHRD");
    imodel   <- 1; #2
    ### OpenSafely
    PLATFORM <- c("Simulate", "Fit dummy data", "OpenSafely"); #0, 1, 2
    iplatform<- 2#1#2#
    
    ### model fitting
    DOfit    <- 1; #(0=Dont fit, 1=simulated or OS data)
    
    ### model output length (weeks)
    nw       <- 52 #week length of run - resolution of contact matrix (really two weeks)
    
    ### Read contact data - options
    DOcmREAD <- 0; if (iplatform>0) {DOcmREAD=0} #(0=read normalised vector; 1=read original matrix files)
    DOcmONE  <- 0; if (iplatform>0) {DOcmONE=0}  #(0=use CoMix, 1=Use cm=1 in all its elements)
   
    ### input file names
    File_model_choice    <- paste0(MODEL[imodel],"_model.cpp")
    File_parameters      <- paste0(MODEL[imodel],"_parameters.r")
    File_contact_data    <- "Contact_matrix_year-from-24Feb20_vector-lenght-9x9x52_norm=maxEV1.csv"

    ### output file names
    File_run             <- "SEIRmodel_run.txt"
    File_contact_summary <- "Contact_matrix_year-from-24Feb20_vector-lenght-9x9x52_norm=maxEV1_stats.txt"
    File_R0_week         <- "R0_by_week.csv"
    File_data_plots      <- "Infected_data_and_R0_by_week.pdf"
    File_model_plots     <- "Infected_and_R0_by_week.pdf"
    File_model_simulation<- "Infected_by_week.csv"
    File_fit_output      <- "MCMC_BT.pdf"
    File_fit_summary     <- "Bayesian_fit_report.txt"

})