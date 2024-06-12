#!/bin/bash

module load R

Rscript find-step-Xinkai-OAK_miss_0601.R $SLURM_ARRAY_TASK_ID

exit 0 