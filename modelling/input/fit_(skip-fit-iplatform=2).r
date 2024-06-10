   
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

## svg Plots
filenamepath    = paste0(output_dir,"/",pset$File_fit_output0,"_marginalTrace")
svglite(paste0(filenamepath,"1",".svg")); print(plot(1:10,1:10)); invisible(dev.off())
svglite(paste0(filenamepath,"2",".svg")); print(plot(1:10,1:10)); invisible(dev.off())
svglite(paste0(filenamepath,"3",".svg")); print(plot(1:10,1:10)); invisible(dev.off())
svglite(paste0(filenamepath,"4",".svg")); print(plot(1:10,1:10)); invisible(dev.off())

## mcmc csv
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_mcmcChain")
for (i in 1:3) { #default: nchain=3
  write.csv(data.frame(1:10), file=paste0(filenamepath,i,".csv")) 
}

## svg Plots
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
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_PosteriorSample_rounded")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())
##
#if (!is.element(pset$iplatform,1)){
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_PosteriorSample_Prev_R0")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())
##
#if (!is.element(pset$iplatform,1)){
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_AgeProfiles")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())
##
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_AgeProfiles_rounded")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())
##
#if (!is.element(pset$iplatform,1)){
filenamepath = paste0(output_dir,"/",pset$File_fit_output0,"_plots_probs")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())

##
filenamepath = paste0(output_dir,"/",pset$File_fit_summary0,"_1")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())
filenamepath = paste0(output_dir,"/",pset$File_fit_summary0,"_2")
svglite(paste0(filenamepath,".svg")); print(plot(1:10,1:10)); invisible(dev.off())

