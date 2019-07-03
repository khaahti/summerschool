#!/bin/bash
#SBATCH -J my_job_name
#SBATCH -o %J.out
#SBATCH -e %J.err
#SBATCH -n 4
#SBATCH -p small
#SBATCH -t 5
#SBATCH --reservation=Summerschool

aprun -e MPICH_MAX_THREAD_SAFETY=multiple -e OMP_NUM_THREADS=6 -n 4 -d 6 heat_mpi
