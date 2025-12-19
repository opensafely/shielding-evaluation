# Shielding-evaluation project

Code and data supporting the article “Hospitalisation and mortality impact of shielding during 2020 in England: a transmission modelling evaluation using the OpenSAFELY platform”.
Available as a preprint at medRxiv 2025.12.12.25342168; doi: https://doi.org/10.64898/2025.12.12.25342168


# Project overview
This project evaluates the shielding policy of 2020 in England using electronic health records accessed on the OpenSAFELY platform and a transmission model with survey-based dynamic social contacts.
https://github.com/opensafely/shielding-evaluation

 
# Pipeline

| Folder    | Function  |
| :------------ | :----------------------------------------------------------------------------------------------------------------------------------------------- |
|  /            | Main R scripts for running the pipelines to reproduce the results and to fit the trasnmission model.
|  /code/       | Sripts sourced by the pipeline for setting the transmission model, sampling posterior distributions, and outputing results.
|  /data/       | Aggregated (and rounded) incidence of COVID-19 severe outcomes, social contacts, and prevalence of positivity. |
|  /output/     | Main result outputs. |
