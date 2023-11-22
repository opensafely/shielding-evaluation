   
## Summary - output
sink(file = paste0(output_dir,"/",pset$File_fit_summary),append=FALSE,split=FALSE) #append=TRUE,split=FALSE)
cat("\n"); 
sink()
  

## pdf Plots
pdf(file = paste0(output_dir,"/",pset$File_fit_output))
print(plot(1:10,1:10))
dev.off()
##


## pdf: data frame
pdf(file = paste0(output_dir,"/",pset$File_fit_variables))
print(plot(1:10,1:10))
dev.off()

pdf(file = paste0(output_dir,"/",pset$File_fit_data1))
print(plot(1:10,1:10))
dev.off()

pdf(file = paste0(output_dir,"/",pset$File_fit_data2))
print(plot(1:10,1:10))
dev.off()
