
library(stepcount)
stepcount::unset_reticulate_python()
stepcount::use_stepcount_condaenv()
stepcount::stepcount_check()

library(tidyverse)
library(here)

source("/dcs05/legacy-dcl01-smart/data/aric-pa/src/sunan/utils.R")
options(digits.secs = 3)

library(walking)
if (!have_forest()){
  install_forest()
}

library(adept)

sample_rate_orgin <- 80

# filepath for storing step estimates
datapath.step.est <- "/dcs05/legacy-dcl01-smart/data/aric-pa/results/xinkai/step-estimates/"
if(!file.exists(datapath.step.est)) dir.create(datapath.step.est)

# filepath for storing aggregated step counts
datapath.step.aggre  <- "/dcs05/legacy-dcl01-smart/data/aric-pa/results/xinkai/step-aggregated/"
if(!file.exists(datapath.step.aggre)) dir.create(datapath.step.aggre)

# a CSV file that stores the filename of the processed csv.gz files
idx <- as.numeric(commandArgs(trailingOnly=TRUE))[1]
temp <- read.csv('/dcs05/legacy-dcl01-smart/data/aric-pa/data/miss_list_xinkai.csv'); temp$X = NULL
subid = temp[idx,1]
subid.files <- list.files(paste0("data/gt3x (V9)/", subid))
csvgzfile <- subid.files[grep("csv.gz", subid.files)]
ftemp <- paste0("data/gt3x (V9)/", subid, "/", csvgzfile)
df <- readr::read_csv(ftemp)


oak = fit_oak(df)
readr::write_csv(oak, paste0(datapath.step.est, subid, "-oak.csv"))
oak_aggregated <- oak %>%
  mutate(date = lubridate::date(time)) %>%
  group_by(date) %>%
  summarise(steps = sum(steps_oak)) %>%
  ungroup() %>%
  mutate(
    id = subid,
    method = "oak"
  )

sdt = fit_sdt(df, sample_rate = sample_rate_orgin)
readr::write_csv(sdt, paste0(datapath.step.est, subid, "-sdt.csv"))
sdt_aggregated <- sdt %>%
  mutate(date = lubridate::date(time)) %>%
  group_by(date) %>%
  summarise(steps = sum(steps_sdt)) %>%
  ungroup() %>%
  mutate(
    id = subid,
    method = "sdt"
  )

vs = fit_vs(df, method_type = "revised", sample_rate = sample_rate_orgin, resample = TRUE)
readr::write_csv(vs, paste0(datapath.step.est, subid, "-vs.csv"))
vs_aggregated <- vs %>%
  mutate(date = lubridate::date(time)) %>%
  group_by(date) %>%
  summarise(steps = sum(steps_vs)) %>%
  ungroup() %>%
  mutate(
    id = subid,
    method = "vs"
  )

steps_aggregated <- rbind(
  oak_aggregated,
  sdt_aggregated,
  vs_aggregated
)

readr::write_csv(steps_aggregated, paste0(datapath.step.aggre,
                                          subid, "-OAK.csv"))

