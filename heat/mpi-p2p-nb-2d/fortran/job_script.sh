#!/bin/bash
#SBATCH -J my_job_name
#SBATCH -o %J.out
#SBATCH -e %J.err
#SBATCH -n 4
#SBATCH -p small
#SBATCH -t 5
#SBATCH --time=00:01:00
#SBATCH --reservation=Summerschool

aprun -n 4 heat_mpi bottle.dat 1000
