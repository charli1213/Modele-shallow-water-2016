!/bin/bash
#PBS -l walltime=01:30:00
#PBS -l nodes=1:ppn=1
#PBS -q default
#PBS -j oe
#PBS -r n
#PBS -N main
cd $PBS_O_WORKDIR
./launcher
