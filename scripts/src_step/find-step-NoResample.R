
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

# filepath for the processed gt3x files
# see "https://johnmuschelli.com/SummarizedActigraphy/index.html" for processing details
datapath <- "/dcs05/legacy-dcl01-smart/data/aric-pa/results/sunan/gt3x-processed/"

# filepath for storing step estimates
datapath.step.est <- "/dcs05/legacy-dcl01-smart/data/aric-pa/results/sunan/step-estimates/"
if(!file.exists(datapath.step.est)) dir.create(datapath.step.est)

# filepath for storing aggregated step counts
datapath.step.aggre  <- "/dcs05/legacy-dcl01-smart/data/aric-pa/results/sunan/step-aggregated/"
if(!file.exists(datapath.step.aggre)) dir.create(datapath.step.aggre)

# a CSV file that stores the filename of the processed csv.gz files
gt3x.processed.fs <- read.csv("/dcs05/legacy-dcl01-smart/data/aric-pa/data/sunan_processed-gt3x-files.csv", header = T)
gt3x.processed.fs = read.csv('/dcs05/legacy-dcl01-smart/data/aric-pa/data/miss_list_all.csv')
gt3x.processed.fs$X = NULL
gt3x.processed.fs$file_name = sub('_V9', '', gt3x.processed.fs$file_name)

idx <- as.numeric(commandArgs(trailingOnly=TRUE))[1]
# 
f <- paste0(file.path(datapath, gt3x.processed.fs[idx, 1]), '.rds')
subid = parse_number(gsub(datapath, "", f))
# 
# f.notresample <- paste0(
#   datapath, 'F', subid, ".rds"
# )

df <- read_rds(f)

# All the sample rate will be used as default.

stepcount = fit_stepcount(df, model_type = "ssl", sample_rate = sample_rate_orgin)
readr::write_csv(stepcount, paste0(datapath.step.est, subid, "-stepcount.csv"))
stepcount_aggregated <- stepcount %>%
  mutate(date = lubridate::date(time)) %>%
  group_by(date) %>%
  summarise(steps = sum(steps_stepcount, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    id = subid,
    method = "stepcount"
)


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


adept = fit_adept(df, sample_rate = sample_rate_orgin)
readr::write_csv(adept, paste0(datapath.step.est, subid, "-adept.csv"))
adept_aggregated <- adept %>%
  mutate(date = lubridate::date(time)) %>%
  group_by(date) %>%
  summarise(steps = sum(steps_adept)) %>%
  ungroup() %>%
  mutate(
    id = subid,
    method = "adept"
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
  adept_aggregated,
  sdt_aggregated,
  vs_aggregated,
  stepcount_aggregated
)

readr::write_csv(steps_aggregated, paste0(datapath.step.aggre, subid, ".csv"))

