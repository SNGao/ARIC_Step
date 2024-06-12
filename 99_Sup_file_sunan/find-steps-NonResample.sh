#!/bin/bash

module load R

Rscript find-step-NoResample.R $SLURM_ARRAY_TASK_ID

exit 0 