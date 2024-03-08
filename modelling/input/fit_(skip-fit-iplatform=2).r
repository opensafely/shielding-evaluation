   
## Summary - output
sink(file = paste0(output_dir,"/",pset$File_run), append=TRUE, split=FALSE)
cat(" \n")
sink()

sink(file = paste0(output_dir,"/",pset$File_fit_summary_1),append=FALSE,split=FALSE)
cat(" \n")
sink()

sink(file = paste0(output_dir,"/",pset$File_fit_summary_2),append=FALSE,split=FALSE)
cat(" \n")
sink()

## svg Plots
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_marginalPlot")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())
##
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_plotout_lastPage")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())
##
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_correlationPlot")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())
##
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_Overall")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())
##
#if (!is.element(pset$iplatform,1)){
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_PosteriorSample")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())
##
#if (!is.element(pset$iplatform,1)){
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_AgeProfiles")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())
##
filenamepath = paste0(output_dir,"/",pset$File_fit_summary0,"_1")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())
filenamepath = paste0(output_dir,"/",pset$File_fit_summary0,"_2")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())
##
#filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_variables")
#svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())

## output from main.Rmh
## html01: output/modelling/J5_Main_Fit_Report.html
