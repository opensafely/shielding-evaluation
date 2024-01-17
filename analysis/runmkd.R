
library(rmarkdown)

#HD_data.log
#renv::record("renv@0.16.0")
#renv::restore(packages = "renv")
print("input Rmd \n")
print(paste0(getwd(),"/analysis/HDdata.Rmd"))

jobno = "JDat10_"

rmarkdown::render(input=paste0(getwd(),"/analysis/HDdata.Rmd"), clean = T, 
                  output_dir = paste0(getwd(),"/output/HDsynthesis"), 
                  output_file = paste0(jobno,"HDdata.html") )
