#!/bin/bash
#SBATCH -J test_job
#SBATCH -A slurm_acct_name
#SBATCH -o jobname_%j.txt
#SBATCH -e jobname_%j.err
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=5:00
#SBATCH --mail-user=username@iu.edu
#SBATCH --mail-type=BEGIN,FAIL,END
#SBATCH --partition=partition_name
