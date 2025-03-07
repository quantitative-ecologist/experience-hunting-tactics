#!/bin/bash
#SBATCH --account=def-monti
#SBATCH --nodes=1
#SBATCH --cpus-per-task=48
#SBATCH --mem=32000M
#SBATCH --time=00-14:30
#SBATCH --mail-user=fraser_franco.maxime@courrier.uqam.ca
#SBATCH --mail-type=ALL

module load StdEnv/2020
module load gcc/9.3.0
module load r-bundle-bioconductor/3.14
module load r/4.1.2

#export R_LIBS=~/.local/R/$EBVERSIONR/
Rscript B1_DHMLM-no-outlier.R