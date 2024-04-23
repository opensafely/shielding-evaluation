
library(rmarkdown)

print("input Rmd \n")
print(paste0(getwd(),"/analysis/HDdata.Rmd"))

jobno = "JDat15_"

rmarkdown::render(input       = paste0(getwd(),"/analysis/HDdata.Rmd"), clean = T, 
                  output_dir  = paste0(getwd(),"/output/HDsynthesis"), 
                  output_file = paste0(jobno,"Report_HDdata.html") )
