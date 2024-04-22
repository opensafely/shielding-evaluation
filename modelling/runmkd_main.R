
library(rmarkdown)

print("input Rmd \n")
print(paste0(getwd(),"/main.Rmd"))

jobno = "J6bHbDhdyTotPrev_" #as in: pset$Job <= main <= setup.r
   
rmarkdown::render(input=paste0(getwd(),"/main.Rmd"), clean = T,  
                  output_dir = paste0(getwd(),"/output/modelling"),  #same as in main 
                  output_file = paste0(jobno,"Main_Fit_Report.html") )
