
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
temp <- read.csv('/dcs05/legacy-dcl01-smart/data/aric-pa/data/miss_list_stepcount.csv'); temp$X = NULL
subid = temp[idx,1]
subid.files <- list.files(paste0("/dcs05/legacy-dcl01-smart/data/aric-pa/data/gt3x (V9)/", subid))
csvgzfile <- subid.files[grep("csv.gz", subid.files)]
ftemp <- paste0("/dcs05/legacy-dcl01-smart/data/aric-pa/data/gt3x (V9)/", subid, "/", csvgzfile)
df <- readr::read_csv(ftemp)


# All the sample rate will be used as default.
stepcount_ssl = fit_stepcount(df, model_type = "ssl", sample_rate = sample_rate_orgin)
readr::write_csv(stepcount_ssl, paste0(datapath.step.est, subid, "-stepcount-ssl.csv"))
stepcount_ssl_aggregated <- stepcount_ssl %>%
  mutate(date = lubridate::date(time)) %>%
  group_by(date) %>%
  summarise(steps = sum(steps_stepcount, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    id = subid,
    method = "stepcount_ssl"
)

stepcount_rf = fit_stepcount(df, model_type = "rf", sample_rate = sample_rate_orgin)
readr::write_csv(stepcount_rf, paste0(datapath.step.est, subid, "-stepcount-rf.csv"))
stepcount_rf_aggregated <- stepcount_rf %>%
  mutate(date = lubridate::date(time)) %>%
  group_by(date) %>%
  summarise(steps = sum(steps_stepcount, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    id = subid,
    method = "stepcount_rf"
  )

steps_aggregated <- rbind(
  stepcount_rf_aggregated,
  stepcount_ssl_aggregated
)

readr::write_csv(steps_aggregated, paste0(datapath.step.aggre,
                                          subid, "-stepcount.csv"))

