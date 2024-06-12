#!/bin/bash

module load R
# module load go

# mount
# rclone mount OneDrive:ARIC\ Digital\ Tech\ Grant/ /users/xzhou2/aric-data/ --daemon

Rscript src/xinkai/find-steps.R $SLURM_ARRAY_TASK_ID
# Rscript src/xinkai/find-steps-stepcount.R $SLURM_ARRAY_TASK_ID

# unmount 
# fusermount -u /users/xzhou2/aric-data/

exit 0 