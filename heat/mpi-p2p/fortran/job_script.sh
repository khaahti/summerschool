#!/bin/bash
#SBATCH -J my_job_name
#SBATCH -o %J.out
#SBATCH -e %J.err
#SBATCH -n 4
#SBATCH -p small
#SBATCH -t 5
#SBATCH --reservation=Summerschool

aprun -e OMP_NUM_THREADS=4 -n 4 -d 4 heat_mpi bottle.dat 1000
