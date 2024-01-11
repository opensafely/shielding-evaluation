#orderly::orderly_develop_start(parameters = list(cache_path = "../../cache/contact_inference/"))
#stop("Orderly reloaded")
#orderly::orderly_develop_status()
#orderly::orderly_develop_clean()

library(rmarkdown)

#HD_data.log
#renv::record("renv@0.16.0")
#renv::restore(packages = "renv")

#rmarkdown::render("HDdata.Rmd", clean = T, output_dir = paste0(here::here("output/HDsynthesis")))
#rmarkdown::render(input="HDdata.Rmd", clean = T, output_dir = "output/HDsynthesis")
#OS: 
print("input Rmd \n")
print(paste0(getwd(),"/analysis/HDdata.Rmd"))
rmarkdown::render(input=paste0(getwd(),"/analysis/HDdata.Rmd"), clean = T, output_dir = "output/HDsynthesis")

#unlink("rtm_matrices", recursive = T)
#unlink("report_cache", recursive = T)
#unlink("report_files", recursive = T)
#unlink("libs", recursive = T)
#file.remove("HDdata.log")
#file.remove("HDdata.tex")
#file.remove("HDdata.spl")
