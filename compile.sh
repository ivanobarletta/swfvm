#!/bin/bash

#BSUB -q s_short
#BSUB -J mksw
#BSUB -n 1
#BSUB -o logcompile.%J.out
#BSUB -e logcompile.%J.err
#BSUB -P R000

module purge
module purge
module load intel19.5/19.5.281
module load impi19.5/19.5.281

mpiifort main.f90 -o swfvm
