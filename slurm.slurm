#!/bin/bash
#SBATCH -J simple_script
#SBATCH -A workshop
#SBATCH -o jobname_%j.txt
#SBATCH -e jobname_%j.err
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=50:00
#SBATCH --mail-user=ppetrova@iu.edu
#SBATCH --mail-type=BEGIN,FAIL,END
#SBATCH --partition=general
