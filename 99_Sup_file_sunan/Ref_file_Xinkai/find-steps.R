
library(tidyverse)
library(walking)
source("src/xinkai/utils.R")
options(digits.secs = 3)
sample_rate_truth <- 80 # raw sample rate

idx <- as.numeric(commandArgs(trailingOnly=TRUE))[1]
subid <- read.csv("data/file_xinkai_xk.csv")[idx, 1]
subid.files <- list.files(paste0("data/gt3x (V9)/", subid))
csvgzfile <- subid.files[grep("csv.gz", subid.files)]
ftemp <- paste0("data/gt3x (V9)/", subid, "/", csvgzfile)

# filepath for storing step estimates
datapath.step.est  <- "results/xinkai/"
if(!file.exists(datapath.step.est)) dir.create(datapath.step.est)

# filepath for storing aggregated step counts
datapath.step.aggre  <- "results/xinkai/step-aggregated/"
if(!file.exists(datapath.step.aggre)) dir.create(datapath.step.aggre)

dat <- readr::read_csv(ftemp)

# tempfile = system.file("test_data_bout.csv", package = "walking")
# tempdat = readr::read_csv(tempfile) %>% rename(HEADER_TIMESTAMP = `UTC time`)
# estimate_steps_forest(tempdat, sample_rate = 10L)
# estimate_steps_sdt(tempdat, sample_rate = 10L)

# oak = estimate_steps_forest(dat)
# # oak = estimate_steps_forest(dat, sample_rate = sample_rate_truth)
# readr::write_rds(oak, paste0(datapath.step.est, subid, "-oak.rds"))
# oak_aggregated <- oak %>%
#   mutate(date = lubridate::date(time)) %>%
#   group_by(date) %>%
#   summarise(steps = sum(steps)) %>%
#   ungroup() %>%
#   mutate(
#     id = subid,
#     method = "oak"
#   )
# 
# 
# sdt = estimate_steps_sdt(dat, sample_rate = sample_rate_truth) 
# readr::write_rds(sdt, paste0(datapath.step.est, subid, "-sdt.rds"))
# sdt_aggregated <- sdt %>%
#   mutate(date = lubridate::date(time)) %>%
#   group_by(date) %>%
#   summarise(steps = sum(steps)) %>%
#   ungroup() %>%
#   mutate(
#     id = subid,
#     method = "sdt"
#   )
# 
# vs = estimate_steps_verisense(
#   dat,
#   method = "revised",
#   resample_to_15hz = T
# )
# message("verisense completed")
# readr::write_rds(vs, paste0(datapath.step.est, subid, "-vs.rds"))
# vs_aggregated <- vs %>%
#   mutate(date = lubridate::date(time)) %>%
#   group_by(date) %>%
#   summarise(steps = sum(steps)) %>%
#   ungroup() %>%
#   mutate(
#     id = subid,
#     method = "vs"
#   )


# steps_aggregated exists because i ran stepcount first
# if(file.exists(paste0(datapath.step.aggre, subid, ".csv"))){
#   steps_aggregated <- read.csv(paste0(datapath.step.aggre, subid, ".csv")) %>%
#     mutate(date = as.Date(date))
#   steps_aggregated <- rbind(
#     steps_aggregated,
#     oak_aggregated,
#     # adept_aggregated,
#     sdt_aggregated,
#     vs_aggregated
#   )
# } else {
#   steps_aggregated <- rbind(
#     oak_aggregated,
#     # adept_aggregated,
#     sdt_aggregated,
#     vs_aggregated
#   )
# }
# readr::write_csv(steps_aggregated, paste0(datapath.step.aggre, subid, ".csv"))


adept = fit_adept(dat, sample_rate = sample_rate_truth)
readr::write_rds(adept, paste0(datapath.step.est, subid, "-adept.rds"))
adept_aggregated <- adept %>%
  mutate(date = lubridate::date(time)) %>%
  group_by(date) %>%
  summarise(steps = sum(steps_adept)) %>%
  ungroup() %>%
  mutate(
    id = subid,
    method = "adept"
  )
steps_aggregated <- read.csv(paste0(datapath.step.aggre, subid, ".csv")) %>%
  mutate(date = as.Date(date))
steps_aggregated <- rbind(
  steps_aggregated,
  adept_aggregated
)
readr::write_csv(steps_aggregated, paste0(datapath.step.aggre, subid, ".csv"))








