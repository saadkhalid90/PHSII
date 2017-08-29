download_upload_PHSII <- function(){
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
  
  ## downlaoding files from their links
  download.file(url = hl_link, destfile = paste0(sub_dir, "/hl_", Sys.Date(), ".csv"))
  download.file(url = hc_link, destfile = paste0(sub_dir, "/hc_", Sys.Date(), ".csv"))
  download.file(url = mother_link, destfile = paste0(sub_dir, "/mother_", Sys.Date(), ".csv"))
  download.file(url = child_link, destfile = paste0(sub_dir, "/child_", Sys.Date(), ".csv"))
  
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
}

## run the function 
download_upload_PHSII()

