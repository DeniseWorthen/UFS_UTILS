#!/bin/bash

# Edit account (-A) setting as required !

#SBATCH -e err
#SBATCH -o out
#SBATCH --job-name="wgts4rhs"
#SBATCH --account=infra-cpu
#SBATCH --qos=normal
#SBATCH --clusters=c6
#SBATCH --partition=batch
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=48
#SBATCH --time=30

module list
./test.sh
