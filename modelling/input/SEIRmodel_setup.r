pset <- list()
pset <- within(pset, {
    TODAY <- format(Sys.Date(), "%d-%m-%Y")
    TIME  <- format(Sys.time(),'%H.%M.%S_%d-%m-%Y')
    ## Model choice
    MODEL    <- c("SEIR", "SEIUHRD");
    imodel   <- 1; #2
    ### OpenSafely
    PLATFORM <- c("Remote", "OpenSafely");
    iplatform<- 2#1;
    ### model parameters for filenames
    source(file = paste0(input_dir,"/",MODEL[imodel],"_parameters.r"))
    ### model fitting
    DOfit    <- 1; #(0=Dont fit, 1=save simulated data and fit)
    ### model output length (weeks)
    nd       <- 52 #week id unit of change in contact matrix (really two weeks)
    ### Read contact data - options
    DOcmREAD <- 0; if (iplatform==2) {DOcmREAD=0} #(0=read normalised vector; 1=read original matrix files)
    DOcmONE  <- 0; if (iplatform==2) {DOcmONE=0}  #(0=use CoMix, 1=Use cm=1 in all its elements)
    ### input file names
    File_model_choice    <- paste0(MODEL[imodel],"_model.cpp")
    
    File_parameters      <- paste0(MODEL[imodel],"_parameters.r")
    
    File_contact_data    <- "Contact_matrix_year-from-24Feb20_vector-lenght-9x9x52_norm=maxEV1.csv"

    ### output file names
    File_run             <- if (iplatform==2)"SEIRmodel_run.txt" else paste0("SEIRmodel_run_",TIME,".txt")
    
    File_contact_summary <- "Contact_matrix_year-from-24Feb20_vector-lenght-9x9x52_norm=maxEV1_stats.txt"
    
    File_R0_week         <- if(iplatform==2) "R0_by_week.csv" else paste0("R0_by_week_R0unif=",round(pars$R0,3),".csv")
    
    File_model_plots     <- if(iplatform==2) "Infected_and_R0_by_week.pdf" else paste0("Infected_and_R0_by_week_beta=", round(pars$beta,3),",_R0unif=",round(pars$R0,3),"_",TODAY,".pdf")
    
    File_model_simulation<- if(iplatform==2) "Infected_by_week.csv" else paste0("Infected_by_week_beta=", round(pars$beta,1),
                                              ", R0unif=",round(pars$R0,1),", R0=",round(max(R0_week),1),
                                              ", IP=",round(pars$rIR,1),", pI2(log10)=",round(log10pI0,2),".csv")
    
    File_fit_output      <- if(iplatform==2) "MCMC_BT.pdf" else paste0("MCMC_BT_beta=",round(thetaTrue[1],3),
                                            "_R0=",round(thetaTrue[2],2),"_sd=",round(thetaTrue[3],1),"_iter=",niter,"_burn=",burnin,"_",pset$TODAY,".pdf")
    
    File_fit_summary     <- if(iplatform==2) "Bayesian_fit_report.txt" else paste0("Bayesian_fit_report_",TODAY,".txt")

})