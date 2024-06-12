
# Stepcount needs to be run in a different script because its python env conflicts
# with that of oak/forest.

library(tidyverse)
library(reticulate)
# devtools::install_github("jhuwit/stepcount")
library(stepcount)
stepcount::unset_reticulate_python()
stepcount::use_stepcount_condaenv()
stepcount::stepcount_check()
options(digits.secs = 3)
sample_rate_truth <- 80 # raw sample rate


idx <- as.numeric(commandArgs(trailingOnly=TRUE))[1]
# subid <- read.csv("data/file_xinkai_xk.csv")[idx, 1]
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

# dat <- readr::read_csv(ftemp)

# Download stepcount model files if they do not exist.
stepcount.modelpath <- "data/stepcount-model"
if(!file.exists(stepcount.modelpath)) dir.create(stepcount.modelpath)
stepcount.model.ssl <- file.path(stepcount.modelpath, sc_model_filename(model_type = "ssl"))
stepcount.model.rf  <- file.path(stepcount.modelpath, sc_model_filename(model_type = "rf"))
if(!file.exists(stepcount.model.ssl)){
  sc_download_model(model_path = stepcount.model.ssl, model_type = "ssl")
}
if(!file.exists(stepcount.model.rf)){
  sc_download_model(model_path = stepcount.model.rf, model_type = "rf")
}
# Test that stepcount works.
# temp = system.file("extdata/P30_wrist100.csv.gz", package = "stepcount")
# if (stepcount_check()) {
#   out = stepcount(file = temp, model_path = stepcount.model.ssl)
# }
# df1 = readr::read_csv(temp)
# out_df = stepcount(file = df1)



# a CSV file that stores the filename of the processed csv.gz files
dat <- readr::read_csv(ftemp)
dat <- rename(dat, x = X, y = Y, z = Z)

sc.rf <-  stepcount::stepcount(
  file = dat, 
  model_path = stepcount.model.rf,
  model_type = "rf", 
  sample_rate = sample_rate_truth
)
# sc.rf <-  stepcount::stepcount(
#   file = dat, 
#   model_type = "rf"
# )
write_rds(sc.rf, paste0(datapath.step.est, subid, "-stepcount-rf.rds"))
stepcount_rf_aggregated <- sc.rf$steps %>%
  mutate(date = lubridate::date(time)) %>%
  group_by(date) %>% 
  summarise(steps = sum(steps, na.rm = T)) %>%
  ungroup() %>%
  mutate(id = subid, method = "stepcount_rf")

sc.ssl <- stepcount::stepcount(
  file = dat, 
  model_path = stepcount.model.ssl,
  model_type = "ssl", 
  sample_rate = sample_rate_truth
)
write_rds(sc.ssl, paste0(datapath.step.est, subid, "-stepcount-ssl.rds"))
stepcount_ssl_aggregated <- sc.ssl$steps %>%
  mutate(date = lubridate::date(time)) %>%
  group_by(date) %>% 
  summarise(steps = sum(steps, na.rm = T)) %>%
  ungroup() %>%
  mutate(id = subid, method = "stepcount_ssl")



steps_aggregated <- rbind(
  stepcount_rf_aggregated,
  stepcount_ssl_aggregated
)
readr::write_csv(steps_aggregated, paste0(datapath.step.aggre, subid, ".csv"))



# steps_aggregated <- read.csv(paste0(datapath.step.aggre, subid, ".csv")) %>%
#   mutate(date = as.Date(date))
# steps_aggregated <- rbind(
#   steps_aggregated,
#   stepcount_rf_aggregated,
#   stepcount_ssl_aggregated
# )
# readr::write_csv(steps_aggregated, paste0(datapath.step.aggre, subid, ".csv"))

# # 
# subid <- read.csv("data/file_xinkai_xk_screrun.csv")[, 1]
# datapath.step.est  <- "results/xinkai/"
# subid.rerun <- NULL
# # for(i in 1:706){
# for(i in 1:21){
#   if(!(
#     file.exists(paste0(datapath.step.est, subid[i], "-stepcount-rf.rds")) &
#     file.exists(paste0(datapath.step.est, subid[i], "-stepcount-ssl.rds"))
#   )){
#     subid.rerun <- c(subid.rerun, subid[i])
#   }
# }
# write.csv(
#   data.frame(subid = subid.rerun),
#   file = "data/file_xinkai_xk_screrun.csv",
#   row.names = F
# )
