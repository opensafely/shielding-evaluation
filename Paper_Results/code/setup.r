pset <- list()
pset <- within(pset, {
    TODAY <- format(Sys.Date(), "%d-%m-%Y")
    TIME  <- format(Sys.time(), '%H.%M.%S_%d-%m-%Y')
	
    ### Model choice
    MODEL    <- c("SEIR", "SEIUHRD");
    imodel   <- 2 #1;

    ### OpenSafely data
    PLATFORM <- c("Simulate", "Dummy data", "OpenSafely-rounded"); #0, 1, 2
    iplatform<- 2 #0#1#2#
    
    ### model fitting
    DOfit    <- 1; #(0=Dont fit, 1=simulated or OS data)
       
    ### model output length (weeks)
    nw       <- 52 #weeks of run - there is a contact matrix for each week from 27-01-2020 for over a year 
  
    ### input file names
    File_modelas_choice  <- paste0(MODEL[imodel],"as_model.cpp")  
    File_contact_datax3_spsynv_r10 <- "Contact_matrix_year-from-27Jan20_vector-lenght-9x9x52_spsynv_reduced10.csv" #contacts
    File_contact_datax3_spsynv_r20 <- "Contact_matrix_year-from-27Jan20_vector-lenght-9x9x52_spsynv_reduced20.csv"
    File_contact_datax3_spsynv_r30 <- "Contact_matrix_year-from-27Jan20_vector-lenght-9x9x52_spsynv_reduced30.csv"
    File_contact_datax3_spsynv_nb  <- "Contact_matrix_year-from-27Jan20_vector-lenght-9x9x52_spsynv_nb.csv"
    File_contact_datax3_spsynv_alt <- "Contact_matrix_year-from-27Jan20_vector-lenght-9x9x52_spsynv_alt.csv"
    File_contact_datax3_spsynv     <- "Contact_matrix_year-from-27Jan20_vector-lenght-9x9x52_spsynv.csv"

    ### output file names
    File_run             <- paste0("Fit_MCMC_parameters_input") #fit
    File_run_res         <- paste0("Parameters_input")          #res
    File_contact_summary <- paste0("Contact_matrix_year-start-27Jan20_stats") #contacts
    File_R0_week         <- paste0("R0_by_week.csv")            #R0_0
    File_model_sim_plots <- paste0("Simul_Infected_and_R0.pdf") #sim
    File_fit_output0     <- paste0("Fit_MCMC")                  #fit
    File_fit_summary     <- paste0("Fit_MCMC_summary.txt")      #sim
    File_fit_summary_1   <- paste0("Fit_MCMC_summary_1.txt")    #fit
    File_fit_summary_2   <- paste0("Fit_MCMC_summary_2.txt")    #fit

})



