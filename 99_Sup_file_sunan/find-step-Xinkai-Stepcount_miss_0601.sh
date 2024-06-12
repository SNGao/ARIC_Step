#!/bin/bash

module load R

Rscript find-step-Xinkai-Stepcount_miss_0601.R $SLURM_ARRAY_TASK_ID

exit 0 