# Online repository for Fraser Franco et al. (XXXX)

![](https://img.shields.io/badge/license-CC%20BY--NC%204.0-green?style=for-the-badge)

## Description

Online repository to reproduce the results presented in :

Fraser Franco, M., Santostefano, F., Martin, G. A., Julien, Kelly, Clint D., Montiglio, P.-O. (XXXX) Individual foraging specialization and success change across experience in a virtual predator-prey system.

## General coding workflow

The subfolders in the [code folder](./code) are enumerated in order to reflect the worflow. 

Here are the steps highlighting the process with a link to the code subfolders :

1. [Run models](./code/01_code_models)
2. [Model validation](./code/02_code_model-validation)
3. [Model processing](./code/03_model-processing)
4. [Produce figures](./code/04_code_figures)
5. [Produce tables](./code/05_code_tables)


You will find the specific outputs generated from the R scripts in the [outputs](./outputs) folder. Each output file has a number that correponds to the R script used to generate it.

## Workflow to compute the models

I ran all the models on [Cedar](https://docs.alliancecan.ca/wiki/Cedar), a computer cluster managed by the [Digital Research Alliance of Canada](https://www.alliancecan.ca/en). You can open the .sh files in the [jobs_slurm](./jobs_slurm) folder to see the computer specifications used to run the models. The computer cluster runs on CentOS Linux 7.

Here is a table showing the workflow employed to generate the model outputs using R. This workflow is exclusively for `.R` files used to compute the Bayesian mixed models with the package "brms" (see the [01_code_models](./code/01_code_models) folder).

| Generator              | Operating system | Programming language | Code               | Results                  |
| ---------------------- | ---------------- | -------------------- | ------------------ | ------------------------ |
| Digital Research Alliance of Canada / Cedar | CentOS Linux 7   | R                    | [code](./code) folder | [outputs](./outputs) folder |

## Reproducibility

### Model fitting

The model fitting can be reproduced on your personal computer but requires important computing resources so fitting may take some time.

Output files will be promptly uploaded on an OSF repository. Stay tuned...

### R packages versions

I used the [renv](https://rstudio.github.io/renv/index.html) package to contain each individual R package version within the project. If you wish to reproduce our results in your personal computer (excluding the model files that were run on Cedar), please refer to the official renv [collaborating vignette](https://rstudio.github.io/renv/articles/collaborating.html) to implemement the workflow on your personal computer.