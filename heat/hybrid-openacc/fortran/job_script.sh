#!/bin/bash
#SBATCH -J my_job_name
#SBATCH -o %J.out
#SBATCH -e %J.err
#SBATCH -n 2
#SBATCH -p gpu
#SBATCH -t 00:01:00
#SBATCH --reservation=Summerschool
#SBATCH --gres=gpu:p100:1

module load pgi/19.9 openmpi/3.1.4 libpng/1.6

srun ./heat_openacc bottle.dat 2000
