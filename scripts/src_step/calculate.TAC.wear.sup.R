## Load Packages
# install.packages('actilifecounts')
library(actilifecounts)
library(dplyr)
library(lubridate)
library(readr)

## create folders to save results
# filepath for storing step estimates
datapath.TAC.est.sec <- "/dcs05/legacy-dcl01-smart/data/aric-pa/results/sunan/TAC.est.sec"
if(!file.exists(datapath.TAC.est.sec)) dir.create(datapath.TAC.est.sec)

# filepath for storing aggregated step counts
datapath.TAC.est.min <- "/dcs05/legacy-dcl01-smart/data/aric-pa/results/sunan/TAC.est.min"
if(!file.exists(datapath.TAC.est.min)) dir.create(datapath.TAC.est.min)

## Load data and calculate
options(digits.secs = 3)
folder.path = '/dcs05/legacy-dcl01-smart/data/aric-pa/data/gt3x (V9)/'
folder.names = list.files(folder.path)

## load missing TAC suvject
# check.list = read.csv('/dcs05/legacy-dcl01-smart/data/aric-pa/src/sunan/TAC.error.file.csv')
# colnames(check.list)[1] = 'index'
# matching_index = sapply(check.list$subjectid, function(id) which(grepl(id, folder.names)))
# check.list$index = matching_index

# id <- as.numeric(commandArgs(trailingOnly=TRUE))[1]
# idx = check.list$index[id]
# idx <- as.numeric(commandArgs(trailingOnly=TRUE))[1]

list = read.csv('subject.step.NotTAC.csv')

for (subjectid in list$subjectid){
  folder.path = '/dcs05/legacy-dcl01-smart/data/aric-pa/data/gt3x (V9)/'
  folder.names = list.files(folder.path)
  idx = which(grepl(subjectid, folder.names))
  subid = folder.names[idx]; print(subid)
  
  subid.files <- list.files(paste0("/dcs05/legacy-dcl01-smart/data/aric-pa/data/gt3x (V9)/", subid))
  csvgzfile <- subid.files[grep("csv.gz", subid.files)]
  
  
  if (length(csvgzfile) !=0){
    ftemp <- paste0("/dcs05/legacy-dcl01-smart/data/aric-pa/data/gt3x (V9)/", 
                    subid, "/", csvgzfile)
    raw = readr::read_csv(ftemp,
                          col_types = cols(time = col_character()))
    ## Find complete day interval for subjects 
    tmp = as.POSIXct(strptime(raw$time, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"))
    noon.point = unique(tmp[format(tmp, "%H:%M:%S") == "12:00:00"])
    first_noon <- min(noon.point)
    last_noon <- max(noon.point)
    
    start_index <- which(tmp >= first_noon)[1]
    end_index <- findInterval(last_noon-1, tmp)
    raw.selected <- raw[start_index:end_index,]
    
    if (dim(raw.selected)[1] <= 1000){
      dat.cbd.sec = data.frame(subjectid = subjectid,
                               error = 'Not enough observed days (<1)')
      dat.cbd.min = data.frame(subjectid = subjectid,
                               error = 'Not enough observed days (<1)')
    } else {
      freq = round(1/as.numeric(diff(as.POSIXct(raw.selected$time[1:10], format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))[1]))
      if (freq == 30){freq = 30} else {freq = 80}
      
      counts = get_counts(raw.selected[,-1], sf = freq, epoch = 1, lfe_select = FALSE, verbose = TRUE)
      time.seconds = unique(tmp[start_index:end_index])
      dat.cbd.sec = cbind(time.seconds, data.frame(counts))
      dat.cbd.min = dat.cbd.sec |>
        mutate(minute = floor_date(time.seconds, unit = "minute")) %>%
        group_by(minute) %>%
        summarise(
          VM_mean = mean(VM, na.rm = TRUE))
      dat.cbd.min$freq = freq
    }
    
    write_rds(dat.cbd.sec,
              paste0(datapath.TAC.est.sec, '/', subid, '.rds'))
    write_rds(dat.cbd.min,
              paste0(datapath.TAC.est.min, '/', subid, '.rds'))
    
    
  } else {
    folder.path = '/dcs05/legacy-dcl01-smart/data/aric-pa/results/sunan/gt3x-processed/'
    folder.names = list.files(folder.path)
    idx = which(grepl(subjectid, folder.names))
    subid = folder.names[idx]; print(subid)
    
    f <- paste0(folder.path, subid)
    raw <- data.frame(read_rds(f))
    
    ## Find complete day interval for subjects 
    tmp = as.POSIXct(strptime(raw$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
    noon.point = unique(tmp[format(tmp, "%H:%M:%S") == "12:00:00"])
    noon.point = noon.point[complete.cases(noon.point)] 
    first_noon <- min(noon.point)
    last_noon <- max(noon.point)
    
    start_index <- which(tmp >= first_noon)[1]
    end_index <- tail(which(tmp == last_noon-1), n = 1)
    raw.selected <- raw[start_index:end_index,]
    
    if (dim(raw.selected)[1] <= 1000){
      dat.cbd.sec = data.frame(subjectid = subjectid,
                               error = 'Not enough observed days (<1)')
      dat.cbd.min = data.frame(subjectid = subjectid,
                               error = 'Not enough observed days (<1)')
    } else {
      freq = round(1/as.numeric(diff(as.POSIXct(raw.selected$time[1:10], format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))[1]))
      if (freq == 30){freq = 30} else {freq = 80}
      
      counts = get_counts(raw.selected[,-1], sf = freq, epoch = 1, lfe_select = FALSE, verbose = TRUE)
      time.seconds = unique(format(raw$time[start_index:end_index], "%Y-%m-%d %H:%M:%S"))
      
      
      dat.cbd.sec = cbind(time.seconds, data.frame(counts))
      dat.cbd.sec$time.seconds = as.POSIXct(dat.cbd.sec$time.seconds)
      dat.cbd.min = dat.cbd.sec |>
        mutate(minute = floor_date(time.seconds, unit = "minute")) %>%
        group_by(minute) %>%
        summarise(
          VM_mean = mean(VM, na.rm = TRUE))
      dat.cbd.min$freq = freq
    }
    
    write_rds(dat.cbd.sec,
              paste0(datapath.TAC.est.sec, '/', sub('.rds', '', subid), '.rds'))
    write_rds(dat.cbd.min,
              paste0(datapath.TAC.est.min, '/', sub('.rds', '', subid), '.rds'))
  }
}

# 164793: observations<=1 day
