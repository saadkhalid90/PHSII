library(rdrop2)
library(dplyr)

## function that downloads, combines (BOS and GAT) and uploads to dropbox
download_upload_PHSII_BoS <- function(){
  ## getting the required packages
  library(rdrop2)
  
  ## setting the paths of main directory and naming the sub directory
  main_dir = "/Users/saadkhalid/Desktop/PHS II dash"
  sub_dir = "downloaded_data"
  
  ## create a folder titled 'downloaded_data' in the WD if its doesn't exist already
  dir.create(file.path(main_dir, sub_dir), showWarnings = F)
  
  ## listing the download links
  hc_link <- 'http://gatconsultancy.com/clustersurvey/report_exportalltable.php?table_name=household_info_characteristics&submit=Download'
  hl_link <- 'http://gatconsultancy.com/clustersurvey/report_exportalltable.php?table_name=hl_eligible_children&submit=Download'
  mother_link <- 'http://gatconsultancy.com/clustersurvey/report_exportalltable.php?table_name=mother_interview&submit=Download'
  child_link <- 'http://gatconsultancy.com/clustersurvey/report_exportalltable.php?table_name=child_under_two&submit=Download'
  
  ## listing download links from BoS server
  BOS_hl_link <- 'http://113.197.55.194/clustersurvey/report_exportalltable.php?table_name=hl_eligible_children&submit=Download'
  BOS_hc_link <- 'http://113.197.55.194/clustersurvey/report_exportalltable.php?table_name=household_info_characteristics&submit=Download'
  BOS_mother_link <- 'http://113.197.55.194/clustersurvey/report_exportalltable.php?table_name=mother_interview&submit=Download'
  BOS_child_link <- 'http://113.197.55.194/clustersurvey/report_exportalltable.php?table_name=child_under_two&submit=Download'
  
  ## downlaoding files from their links
  download.file(url = hl_link, destfile = paste0(sub_dir, "/hl_", Sys.Date(), ".csv"))
  download.file(url = hc_link, destfile = paste0(sub_dir, "/hc_", Sys.Date(), ".csv"))
  download.file(url = mother_link, destfile = paste0(sub_dir, "/mother_", Sys.Date(), ".csv"))
  download.file(url = child_link, destfile = paste0(sub_dir, "/child_", Sys.Date(), ".csv"))
  
  ## downloading files from BoS server
  download.file(url = BOS_hl_link, destfile = paste0(sub_dir, "/BOS_hl_", Sys.Date(), ".csv"))
  download.file(url = BOS_hc_link, destfile = paste0(sub_dir, "/BOS_hc_", Sys.Date(), ".csv"))
  download.file(url = BOS_mother_link, destfile = paste0(sub_dir, "/BOS_mother_", Sys.Date(), ".csv"))
  download.file(url = BOS_child_link, destfile = paste0(sub_dir, "/BOS_child_", Sys.Date(), ".csv"))
  
  ## reding in files to combine
  hl_BOS <- read.csv(paste0('downloaded_data/BOS_hl_', Sys.Date(), '.csv'), stringsAsFactors = F)
  hc_BOS <- read.csv(paste0('downloaded_data/BOS_hc_', Sys.Date(), '.csv'), stringsAsFactors = F)
  mother_BOS <- read.csv(paste0('downloaded_data/BOS_mother_', Sys.Date(), '.csv'), stringsAsFactors = F)
  child_BOS <- read.csv(paste0('downloaded_data/BOS_child_', Sys.Date(), '.csv'), stringsAsFactors = F)
  hl <- read.csv(paste0('downloaded_data/hl_', Sys.Date(), '.csv'), stringsAsFactors = F)
  hc <- read.csv(paste0('downloaded_data/hc_', Sys.Date(), '.csv'), stringsAsFactors = F)
  mother <- read.csv(paste0('downloaded_data/mother_', Sys.Date(), '.csv'), stringsAsFactors = F)
  child <- read.csv(paste0('downloaded_data/child_', Sys.Date(), '.csv'), stringsAsFactors = F)
  
  ## combining and removing duplicates (BOS and GAT server)
  binded_hl <- rbind(hl_BOS, hl)
  hl_comb <- binded_hl[!duplicated(binded_hl[1:3]),]
  
  col_names <- intersect(names(hc), names(hc_BOS))
  binded_hc <- rbind(hc_BOS %>% select_(.dots = col_names), hc %>% select_(.dots = col_names))
  hc_comb <- binded_hc[!duplicated(binded_hc[1:2]),]
  
  binded_mother <- rbind(mother_BOS, mother)
  mother_comb <- binded_mother[!duplicated(binded_mother[1:4]),]
  
  binded_child <- rbind(child_BOS, child)
  child_comb <- binded_child[!duplicated(binded_child[1:4]),]
  
  hl_comb <- hl_comb %>% select(hh1:hl6)
  hc_comb <- hc_comb %>% select(hh1:DATA_OPERATOR)
  mother_comb <- mother_comb %>% select(hh1:interview_end)
  child_comb <- child_comb %>% select(hh1:interview_end)
  
  
  write.csv(hl_comb, file = paste0("downloaded_data/hl_comb_", Sys.Date(), ".csv"), row.names = F)
  write.csv(hc_comb, file = paste0("downloaded_data/hc_comb_", Sys.Date(), ".csv"), row.names = F)
  write.csv(mother_comb, file = paste0("downloaded_data/mother_comb_", Sys.Date(), ".csv"), row.names = F)
  write.csv(child_comb, file = paste0("downloaded_data/child_comb_", Sys.Date(), ".csv"), row.names = F)
  
  ## getting filenames that were not downloaded today
  filenames <- list.files(file.path(main_dir, sub_dir))
  
  old_filenames <- filenames[!(filenames %in% grep(Sys.Date(), filenames, value = T))]
  old_filenames_dir <- file.path(sub_dir, old_filenames)
  
  ## delete all files that are not downloaded today
  sapply(old_filenames_dir, file.remove)
  
  ## saved application token to access dropbox through R 
  token <- readRDS("droptoken.rds")
  ##  accessing dropbox acount
  drop_acc(dtoken = token)
  
  ## listing the valid files and their directories
  upload_filenames <- list.files(file.path(main_dir, sub_dir))
  upload_filenames_dir <- sapply(upload_filenames, function(x) paste0(file.path(main_dir, sub_dir), "/", x))
  
  ## upload all the files onto dropbox
  sapply(unname(upload_filenames_dir), function(x) drop_upload(x, dest = "PHS II Data"))
  
  ## deleting all files that are not uploaded today
  
  ## getting file path
  drop_files <- drop_dir("PHS II Data")$path
  
  ## getting index of files to be deleted and apply delete on all
  del_index <- !(grepl(pattern = as.character(Sys.Date()), x = drop_files))
  sapply(drop_files[del_index], function(x) drop_delete(x))
}

download_upload_PHSII_BoS()






