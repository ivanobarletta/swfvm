#!/bin/bash

#BSUB -q s_short
#BSUB -J runsw
#BSUB -n 1
#BSUB -o logout.%J.out
#BSUB -e logerr.%J.err
#BSUB -P R000

module purge
module purge
module load intel19.5/19.5.281
module load impi19.5/19.5.281

mpiexec.hydra -l ./swfvm
