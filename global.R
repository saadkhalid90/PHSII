### --- code for global functions in the PHS dashboard app

## loading important libraries used in all functions
library(dplyr)

## download data to get cluster numbers and their districts
cluster_districts <- readRDS(file = 'cluster_district.rds')

## function for separating others indicator variable and text
finding_X <- function(x) {
  if (sum(grepl(pattern = "X", x)) != 0){
    return(unlist(strsplit(sub(" ", ";", x[grepl(pattern = "X", x)]), ";")))
  }
  else {
    return(c(NA, NA))
  }
}

## function for creating dummies for multiple entry questions
create_dummy <- function(var, max_alphabet = "F", others = TRUE, DK = FALSE, No_one = FALSE, dataset, remove_orig = FALSE, others_text = TRUE, var_sep = ""){
  ## retrieve data with commas from Azfar's table and split it based on comma separators
  with_comma <- as.character(dataset[,var])
  split_list <- strsplit(with_comma, split = ",")
  
  ## get aphabets and capitalize them, find out the max index
  alphabet <- toupper(letters)
  idx <- which(alphabet == max_alphabet) 
  ## getting options for multiple choice question
  options_ <- alphabet[1:idx]
  
  ## adding a No-one option if applicable
  if (No_one == TRUE){
    options_ <- c(options_, "Y")
  }
  ## adding a DK ("Z") option if applicable
  if (DK == TRUE){
    options_ <- c(options_, "Z")
  }
  ## creating dummies for alphabet options 
  DUMMY <- lapply(split_list, function(x) options_ %in% x)
  DUMMY <- as.data.frame(do.call(rbind, DUMMY))
  names(DUMMY) <- paste(var, options_, sep = var_sep)
  
  DUMMY <- as.data.frame(ifelse(DUMMY == TRUE, yes = 1, no = 0))
  ## print(names(DUMMY))
  ## creating dummies and a text column for others 
  if (others == TRUE){
    others_df <- as.data.frame(do.call(rbind, lapply(split_list, finding_X)))
    
    others_df_names <- paste(var, c("X", "Xtext"), sep = var_sep)
    names(others_df) <- others_df_names
    ## recoding
    others_df[, others_df_names[1]] <-  ifelse(test = others_df[, others_df_names[1]] == "X", 1, 0)
    others_df[, others_df_names[1]] <-  ifelse(test = is.na(others_df[, others_df_names[1]]), 0, others_df[, others_df_names[1]])
    
    ## remove the text column if text is false
    if (others_text == FALSE){
      others_df <- others_df %>%
        select_(.dots = others_df_names[1])
    }
    ## combining DUMMY and unknown data frame
    DUMMY_X <- cbind(DUMMY, others_df)
  }
  else {
    DUMMY_X <- DUMMY
  }
  
  APPEND_Dummy <- cbind(dataset, DUMMY_X)
  
  ## rearrange the table according to format
  dataset_names <- names(dataset)
  dummy_x_names <- names(DUMMY_X)
  var_idx <- which(dataset_names == var)
  ## moving the dummy next to the variable
  if (remove_orig == TRUE){
    APPEND_Dummy <- APPEND_Dummy %>% 
      select_(.dots = c(dataset_names[1:(var_idx-1)], sort(dummy_x_names), dataset_names[(var_idx + 1):length(dataset_names)]))
    
  }
  else{
    APPEND_Dummy <- APPEND_Dummy %>% 
      select_(.dots = c(dataset_names[1:var_idx], sort(dummy_x_names), dataset_names[(var_idx + 1):length(dataset_names)]))
  }
  return(APPEND_Dummy)
}

## function applied on all multiple entry questions

apply_dummy_all <- function(mother_data, hc_data, children_data){
  ## list of variables in mother data
  mother_upd <- create_dummy(var = "mn2", max_alphabet = "F", others = T, DK = F, dataset = mother_data, remove_orig = T, var_sep = "_")
  mother_upd <- create_dummy(var = "mn17", max_alphabet = "F", others = T, DK = F, No_one = T, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "mn27", max_alphabet = "K", others = T, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "mn30", max_alphabet = "I", others = F, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "mn31", max_alphabet = "H", others = T, DK = F, No_one = F, dataset = mother_upd, others_text = F, remove_orig = T)
  mother_upd <- create_dummy(var = "pn13", max_alphabet = "F", others = T, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "pn22", max_alphabet = "F", others = T, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "lm2", max_alphabet = "D", others = T, DK = T, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "lm3", max_alphabet = "C", others = F, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "lm7", max_alphabet = "G", others = T, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "lm8", max_alphabet = "F", others = T, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "lm11", max_alphabet = "D", others = T, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "lm13", max_alphabet = "D", others = T, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "lm15", max_alphabet = "G", others = T, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "lm16", max_alphabet = "F", others = T, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "lm19", max_alphabet = "D", others = T, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "lm21", max_alphabet = "D", others = T, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "lm23", max_alphabet = "H", others = F, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)
  mother_upd <- create_dummy(var = "lm25", max_alphabet = "H", others = F, DK = F, No_one = F, dataset = mother_upd, remove_orig = T)

  ## applying dummies to hc
  hc_upd <- create_dummy(var = "ws7", max_alphabet = "F", others = T, DK = T, No_one = F, dataset = hc_data, others_text = T, remove_orig = T)
  hc_upd <- create_dummy(var = "hw3b", max_alphabet = "D", others = F, DK = F, No_one = F, dataset = hc_upd, others_text = F, remove_orig = T)
  ## and children data
  children_upd <- create_dummy(var = "im17", max_alphabet = "I", others = T, DK = F, No_one = F, dataset = children_data, others_text = F) 
  
  ## return a list of updated data
  return(list(hc = hc_upd, mother = mother_upd, child = children_upd))
}

## function for separating time category and units
time_value_split <- function(var, data_set, remove_orig = TRUE){
  ## getting the required stringr library
  library(stringr)
  suppressMessages(library(dplyr))
  
  ## getting the needed column as a vector
  to_split <- data_set[, var]
  ## split based on a space to separate category and amount of time
  time_split_values <- str_split(sub(" ", ";", as.character(to_split)), ";")
  
  ## removing spaces from time variable
  time_split_values <- lapply(time_split_values, function(x) gsub(pattern = " ", replacement = "", x))
  ## combining elements of a list to a dataframe
  time_split_values <- as.data.frame(do.call(rbind, time_split_values))
  names(time_split_values) <- paste(var, "_", c("R", "I"), sep =  "")
  
  ## setting time value to NA if the category is 998 or 000
  time_split_values[(time_split_values[, 1] %in% c("998", "000", "98")) , 2] <- NA
  
  ## adding the split variables in the original dataset and rearranging variables
  Append_DUMMY <- cbind(data_set, time_split_values)
  dataset_names <- names(data_set)
  
  var_idx <- which(names(data_set) == var)
  Append_DUMMY <- Append_DUMMY %>% select_(.dots = c(dataset_names[1:(var_idx-1)], names(time_split_values), dataset_names[(var_idx + 1):length(dataset_names)]))
  
  return(Append_DUMMY)
}

## apply the time split function to the appropriate datasets (mother)

apply_time_split <- function(mother_data) {
  mother_upd <- time_value_split("mn2a", mother_data)
  mother_upd <- time_value_split("mn25", mother_upd)
  mother_upd <- time_value_split("pn2", mother_upd)
  mother_upd <- time_value_split("pn12a", mother_upd)
  mother_upd <- time_value_split("pn12b", mother_upd)
  mother_upd <- time_value_split("pn21a", mother_upd)
  mother_upd <- time_value_split("pn21b", mother_upd)
  mother_upd <- time_value_split("lm22", mother_upd)
  mother_upd <- time_value_split("lm24", mother_upd)
  return(mother_upd)
}

## function for checking mismatches across survey data
identify_missing <- function(hl, hc, mother, child){
  mothers_missing_children <- mother %>% anti_join(child, by = c("hh1", "hh2", "wm2" = "uf6")) %>% select(hh1:wm4)
  HHs_missing_data <- subset(hc %>% anti_join(mother, by = c("hh1", "hh2")), 
                             hh11 == 1) %>% select(hh1:hh2)
  
  missing_hl_mother <- subset(hl %>% anti_join(mother, by = c("hh1", "hh2", "hl1" = "wm2")), 
                              hl4a == 0) %>% select(hh1:hl4a) 
  missing_hl_child <- subset(hl %>% anti_join(child, by = c("hh1", "hh2", "hl1" = "uf4")), 
                             hl4a != 0) %>% select(hh1:hl4a)
  
  ## add district names
  cluster_districts <- readRDS("cluster_district.rds")
  
  mothers_missing_children$District <- cluster_districts$NAME.OF.DISTRICT[as.numeric(sub("RL", "", as.character(mothers_missing_children$hh1)))] 
  HHs_missing_data$District <- cluster_districts$NAME.OF.DISTRICT[as.numeric(sub("RL", "",as.character(HHs_missing_data$hh1)))] 
  missing_hl_mother$District <- cluster_districts$NAME.OF.DISTRICT[as.numeric(sub("RL", "", as.character(missing_hl_mother$hh1)))] 
  missing_hl_child$District <- cluster_districts$NAME.OF.DISTRICT[as.numeric(sub("RL", "", as.character(missing_hl_child$hh1)))] 
  
  return(list(hl_moth = missing_hl_mother %>% select(District, hh1:hl4a), 
              hl_child = missing_hl_child %>% select(District, hh1:hl4a), 
              HH_moth = HHs_missing_data %>% select(District, hh1:hh2), 
              moth_child  = mothers_missing_children  %>% select(District, hh1:wm4)))
}

## function to get the completed set of interviews across the four forms
complete_interviews <- function(mother_data, children_data, hc_data){
  ## removing empty rows (judging by NA hh1)
  mother_data <- mother_data[!(is.na(mother_data$hh1)), ]
  children_data <- children_data[!(is.na(children_data$hh1)), ]
  hc_data <- hc_data[!(is.na(hc_data$hh1)), ]
  
  ## removing extraneous columns that have been read in
  children_data$X <- NULL
  mother_data$X <- NULL
  hc_data$X <- NULL
  
  ##  joins to get teh completed data
  mother_c <- mother_data %>% semi_join(children_data, by = c("hh1", "hh2", "wm2" = "uf6"))
  hc_mother <- hc_data %>% right_join(mother_c, by = c("hh1", "hh2"))

  return(list(hc_mother = hc_mother, child = children_data))
}

## create summary using the completed datasets function
create_summary <- function(hl_data, hc_data, mother_data, child_data){
  ## getting the completed set of interviews
  comp_int <- complete_interviews(mother_data = mother_data, 
                                  children_data = child_data, 
                                  hc_data = hc_data)
  ## get the four types of missing cases
  missing <- identify_missing(hl = hl_data, 
                              hc = hc_data, 
                              mother = mother_data, 
                              child = child_data)
  ## group_by and summarise to creare cluster level summaries
  ## info on completed interviews
  ## info on mismatches
  HH_moth_sum <- comp_int$hc_mother %>%  
    group_by(hh1) %>%
    summarise(HHs = n_distinct(hh2), 
              n_mothers = n())
  child_sum <- comp_int$child %>%  
    group_by(hh1) %>%
    summarise(n_child = n())
  hl_child_miss <-  missing$hl_child %>%  
    group_by(hh1) %>%
    summarise(hl_child = n())
  hl_moth_miss <-  missing$hl_moth %>%  
    group_by(hh1) %>%
    summarise(hl_moth = n())
  HH_moth_miss <-  missing$HH_moth %>%  
    group_by(hh1) %>%
    summarise(hc_moth = n())
  moth_child_miss <-  missing$moth_child %>%  
    group_by(hh1) %>%
    summarise(moth_child = n())
  
  ## summary for HH12, status of interview
  hh12_summary <- hc_data %>%
    group_by(hh1) %>%
    summarise(n_PC = sum(hh12 == 2, na.rm = T),
              n_NE = sum(hh12 == 3, na.rm = T),
              n_not_H = sum(hh12 == 4, na.rm = T),
              n_Lock = sum(hh12 == 5, na.rm = T),
              n_dem = sum(hh12 == 6, na.rm = T),
              n_ref = sum(hh12 == 7, na.rm = T))
  
  ## joining all kinds of summaries created above to get one big tabel to be shown on dash
  
  summary_PHS <- HH_moth_sum %>% 
    full_join(child_sum, by = "hh1") %>%
    full_join(hl_moth_miss, by = "hh1") %>%
    full_join(hl_child_miss, by = "hh1") %>%
    full_join(HH_moth_miss, by = "hh1") %>%
    full_join(moth_child_miss, by = "hh1") %>%
    left_join(hh12_summary, by = "hh1")
  
  ## get rid of any cases with NA cluster no
  summary_PHS <- summary_PHS[!is.na(summary_PHS$hh1), ]
  
  ## replacing all NAs with zeros
  summary_PHS[is.na(summary_PHS)] <- 0
  
  ## getting districts across cluster numbers
  cluster_districts <- readRDS("cluster_district.rds")
  
  ## adding the district across each cluster number, also include a provision for RL cluster numbers
  summary_PHS$District <- cluster_districts$NAME.OF.DISTRICT[as.numeric(sub("RL", "", as.character(summary_PHS$hh1)))] 

  ## rearranging columsn to have district on teh very left, teh rest on the right
  summary_PHS <- summary_PHS %>% select(District, hh1:n_ref)
    
  return(summary_PHS)
}

## functions for computing indicators
compute_BF_indic<- function(mother_data, child_data, hc_data, type = "global"){
  ever_breastfed <- sum(mother_data$mn24 == 1, na.rm = T)/ nrow(mother_data)
  ## mother_data_split <- time_value_split(var = "mn25", data_set = mother_data, remove_orig = T)
  early_breastfed <- sum(as.character(mother_data$mn25_R) == '000', na.rm = T)/ nrow(mother_data)
  bottle_feed <- sum(child_data$bd4 == 1, na.rm = T)/ nrow(child_data)
  excl_breastfed <- sum(mother_data$mn26 == 2, na.rm = T)/ nrow(mother_data)
  
  ## --- EXCLUSIVE BF from BD section --- 
  ## append age to child table
  child_date <- append_date_enum(child_data, hc_data)
  
  child_6 <- child_date %>% filter(child_age <= 6)
  ## child liquid food intake 
  ## selecting the relevant columns
  child_bd7 <- child_6 %>% select(contains("bd7"))
  ## creating a data frame of logicals, TRUE indicates a no (code "2")
  child_bd7 <- as.data.frame(sapply(child_bd7, as.character) == "2")
  
  ## child solid food intake
  ## selecting the relevant columns
  child_bd8 <- child_6 %>% select(contains("bd8"))
  ## creating a data frame of logicals, TRUE indicates a no (code "2")
  child_bd8 <- as.data.frame(sapply(child_bd8, as.character) == "2")
  
  ## if all true (bd7 and bd8 correspond to liquid and ssemi solid foods) 
  child_bd7_log <- rowSums(child_bd7, na.rm = T, dims = 1) == ncol(child_bd7)
  child_bd8_log <- rowSums(child_bd8, na.rm = T, dims = 1) == ncol(child_bd8)
  
  child_bd8_log
  
  ## questions about whether teh child is still being breastfed 
  BD2_log <- child_6$bd2 == 1
  BD3_log <- child_6$bd3 == 1
  
  ## combine the logics (no liquid semi solid consumption in last 24 hours and is being Breastfed)
  exc_log <- Reduce("&", list(BD2_log, BD3_log, child_bd7_log, child_bd8_log))
  
  ## computing indicator
  excl_breastfed_BF <- sum(exc_log, na.rm = T)/nrow(child_6)
  
  ## for district wise table
  if (type == "district"){
    ## removing nas from data (just in case - Rstudio version)
    mother_data <- mother_data[!(is.na(mother_data$hh1)), ]
    child_data <- child_data[!(is.na(child_data$hh1)), ]
    ## trimming spaces to avoid future problems with joins based on district name
    mother_data$district <- trimws(as.character(cluster_districts$NAME.OF.DISTRICT[as.integer(sub("RL", "", as.character(mother_data$hh1)))]), which = "both")
    child_data$district <- trimws(as.character(cluster_districts$NAME.OF.DISTRICT[as.integer(sub("RL", "", as.character(child_data$hh1)))]), which = "both")
    hc_data$district <- trimws(as.character(hc_data$district), which = "both")
    
    ## getting unique districts and applying the function to all districts
    district_inData <- unique(mother_data$district)
    district_level <- t(as.data.frame(sapply(district_inData, function(x) compute_BF_indic(subset(mother_data, district == x), 
                                                                                           subset(child_data, district == x),
                                                                                           subset(hc_data, district == x)))))
    return(district_level)
  }
  else {
    ## return rounded off indicator values
    return(c(ever_bf = round(ever_breastfed, 4), 
             early_bf = round(early_breastfed, 4), 
             bottle_feed = round(bottle_feed, 4), 
             excl_bf = round(excl_breastfed, 4),
             excl_bf_BD = round(excl_breastfed_BF, 4))) 
  }
}

compute_AN_indic <- function(mother_data, type = "global"){
  ## computing the coverage of AN care 
  AN_log <- (mother_data$mn1 == 1)
  AN_cov <- sum(AN_log, na.rm = T)/ nrow(mother_data)
  
  ## content of antenatal care
  bloodp_logic <- (mother_data$mn4_bloodpressure == 1) 
  blood_logic <- (mother_data$mn4_blood == 1)
  urine_logic <- (mother_data$mn4_urine == 1)
  weigh_logic <- (mother_data$mn4_weighed == 1)
  
  comb_logical <- Reduce("&", list(bloodp_logic, blood_logic, urine_logic))
  comb_logical_4 <- Reduce("&", list(bloodp_logic, blood_logic, urine_logic, weigh_logic))
  three <- sum(comb_logical, na.rm = T)/nrow(mother_data)
  all_four <- sum(comb_logical_4, na.rm = T)/nrow(mother_data)
  
  ## computing district level AN indic
  if (type == "district"){
    mother_data <- mother_data[!(is.na(mother_data$hh1)), ]
    mother_data$district <- as.character(cluster_districts$NAME.OF.DISTRICT[as.integer(sub("RL", "", as.character(mother_data$hh1)))])
    district_inData <- unique(mother_data$district)
    district_level <- t(as.data.frame(sapply(district_inData, function(x) compute_AN_indic(subset(mother_data, district == x)))))
    
    return(district_level)
  }
  else {
    ## returning rounded off indics
    return(c(AN_cov = round(AN_cov, 4),
             three = round(three, 4), 
             all_four = round(all_four, 4)))  
  }
}


compute_delivery_indic <- function(mother_data, type = "global"){
  ## skilled birth attendance
  skilled_logic <- lapply(X = strsplit(as.character(mother_data$mn17), ','), function(x) {x %in% toupper(letters[1:5])})
  skilled_logic <- lapply(skilled_logic, sum)
  skilled_logic <- as.logical(skilled_logic)
  
  skilled_att <- sum(skilled_logic, na.rm = T)/nrow(mother_data)
  
  ## institutional delivery (code b/w 21 and 26 and 31 and 36)
  inst_deliv_var <- regmatches(x = mother_data$mn18, m = regexpr("^[0-9][0-9]", text = mother_data$mn18))
  inst_num <- as.numeric(inst_deliv_var) %in% c(seq(21, 26), seq(31, 36))
  inst_deliv <- sum(inst_num, na.rm = T)/ nrow(mother_data)
  
  ## Caeserrean section
  caes_num <- sum(as.numeric(mother_data$mn19) == 1, na.rm = T)
  caeserian <- caes_num/ nrow(mother_data)
  
  ## computing district level indics
  if (type == "district"){
    mother_data <- mother_data[!(is.na(mother_data$hh1)), ]
    mother_data$district <- as.character(cluster_districts$NAME.OF.DISTRICT[as.integer(sub("RL", "", as.character(mother_data$hh1)))])
    district_inData <- unique(mother_data$district)
    district_level <- t(as.data.frame(sapply(district_inData, function(x) compute_delivery_indic(subset(mother_data, district == x)))))
    
    return(district_level)
  }
  
  else {
    return(c(skilled_birth = round(skilled_att, 4), 
             Inst_delivery = round(inst_deliv, 4), 
             c_section = round(caeserian, 4)))  
  }
}

## functions for calculating Post natal indicators (global and district level)
compute_PN_indic <- function(mother_data, type = "global"){
  
  ## stay in Health facility
  ## relevant variable PN2
  ## greater than 22 hours to be counted
  
  ## converting time cat. and units to numeric
  pn2_R <- as.numeric(as.character(mother_data$pn2_R))
  pn2_I <- as.numeric(as.character(mother_data$pn2_I))
  
  ## creating logical for greater than 12 hours and summing it up to get numerator
  PN_stay_num <- sum((pn2_R == 1 & pn2_I >=12) | (pn2_R >= 2 & pn2_R <= 3), na.rm = T)
  ## computing the post-partum stay indicator
  PN_stay <- PN_stay_num/ nrow(mother_data) 
  
  ## PN health check for newborn
  ## child was checked at health facility, home or was geven a 
  ## post natal health visit within 2 days
  ## Relevant variables - PN3, PN7, PN12A, PN12B
  
  ## getting logical for whether the child was checked in facility or at home
  pn3_log <- mother_data$pn3 == 1 
  pn7_log <- mother_data$pn7 == 1
  
  ## converting PN12A to numeric so that checks can be applied (within 2 days after birth)
  pn12a_R <- as.numeric(as.character(mother_data$pn12a_R))
  pn12a_I <- as.numeric(as.character(mother_data$pn12a_I))
  pn12a_logic <- (pn12a_R == 1) | (pn12a_R == 2 & pn12a_I <= 2)
  
  ## converting PN12A to numeric so that checks can be applied (within 2 days after birth)
  pn12b_R <- as.numeric(as.character(mother_data$pn12b_R))
  pn12b_I <- as.numeric(as.character(mother_data$pn12b_I))
  pn12b_logic <- (pn12b_R == 1) | (pn12b_R == 2 & pn12b_I <= 2)
  
  ## combining all logicals through an or operator
  hck_NB_log <- Reduce("|", list(pn3_log, pn7_log, pn12a_logic, pn12b_logic))
  
  hck_NB_num <-  sum(hck_NB_log, na.rm = T)
  hck_NB <- hck_NB_num/ nrow(mother_data)
  
  ## PN health check for the mother
  ## getting logical whether the mother received a health check in facility or at home
  pn4_log <- mother_data$pn4 == 1 
  pn8_log <- mother_data$pn8 == 1
  
  ## converting PN21A to numeric so that checks can be applied (within 2 days after birth of child)
  
  pn21a_R <- as.numeric(as.character(mother_data$pn21a_R))
  pn21a_I <- as.numeric(as.character(mother_data$pn21a_I))
  pn21a_logic <- (pn21a_R == 1) | (pn21a_R == 2 & pn21a_I <= 2)
  
  ## converting PN21B to numeric so that checks can be applied (within 2 days after birth of child)
  pn21b_R <- as.numeric(as.character(mother_data$pn21b_R))
  pn21b_I <- as.numeric(as.character(mother_data$pn21b_I))
  pn21b_logic <- (pn21b_R == 1) | (pn21b_R == 2 & pn21b_I <= 2)
  
  ## combining all logicals through an or operator
  hck_moth_log <- Reduce("|", list(pn4_log, pn8_log, pn21a_logic, pn21b_logic))
  
  hck_moth_num <-  sum(hck_moth_log, na.rm = T)
  hck_moth <- hck_moth_num/ nrow(mother_data)
  
  ## if type is "district", make all computations at the district level
  if (type == "district"){
    mother_data <- mother_data[!(is.na(mother_data$hh1)), ]
    mother_data$district <- as.character(cluster_districts$NAME.OF.DISTRICT[as.integer(sub("RL", "", as.character(mother_data$hh1)))])
    district_inData <- unique(mother_data$district)
    district_level <- t(as.data.frame(sapply(district_inData, function(x) compute_PN_indic(subset(mother_data, district == x), type = ""))))
    
    return(district_level)
  }
  
  else {
    return(c(PN_stay = round(PN_stay, 4), 
             hck_newbor = round(hck_NB, 4), 
             hck_moth = round(hck_moth, 4)))
  }
}

append_date_enum <- function(data, hc_data){
  ## getting date of enumeration (hh5) from the hc data
  hc_date <- hc_data %>% select(hh1, hh2, hh5)
  ## appending the date to the dataset in the argument
  data  <- data %>% left_join(hc_date, by = c("hh1", "hh2"))
  
  ## getting the date of enumeration and convertingit into date object
  enum_date <- as.Date(data[, "hh5"], format = "%d-%m-%Y")
  ## getting the birthdate and converting into date element
  birth_date <- as.Date(data[, "ag1"], format = "%d-%m-%Y")
  
  ## computing child's age at the date of the interview
  data$child_age <- interval(start = birth_date, end = enum_date) /
    duration(num = 1, units = "months")
  
  ## return the appended data
  return(data)
}

compute_immun_indic<- function(data, var1, var2, type = "global", age_group = 1, first_birthday = T){
  ## getting the birthdate and converting into date element
  birth_date <- as.Date(data[, "ag1"], format = "%d-%m-%Y")
  
  ## getting the dates of the relevant vaccines
  dates_text <- data[, var1]
  dates <- as.Date(data[, var1],format = "%d-%m-%Y")
  
  ## computing age at vaccination in months
  interv <- interval(start = birth_date, end = dates) /
    duration(num = 1, units = "months")
  
  ## checking if the child received the vaccine within his/ her first birthday
  if (first_birthday == T){
    card <- (interv < 12)
  }
  
  else if (first_birthday == F){
    card <- is.numeric(interv)
  } 
  card[is.na(card)] <- FALSE
  
  ## count mentions of vaccine without date on teh card
  card_no_date <- grepl(pattern = "44", x = dates_text)
  card_no_date[is.na(card_no_date)] <- FALSE
  
  ## also cheking if dates exist for vacine one and two if the variable is opv3 or penta3
  
  if (var1 == "im3_opv.3"){
    card_2 <- as.character(data[ ,"im3_opv.2"]) != ""
    card_2[is.na(card_2)] <- FALSE
    card_1 <- as.character(data[ ,"im3_opv.1"]) != ""
    card_1[is.na(card_1)] <- FALSE
    
    card <- card & card_1 & card_2
  }
  
  if (var1 == "im3_penta.3"){
    card_2 <- as.character(data[ ,"im3_penta.2"]) != ""
    card_2[is.na(card_2)] <- FALSE
    card_1 <- as.character(data[ ,"im3_penta1"]) != ""
    card_1[is.na(card_1)] <- FALSE
    
    card <- card & card_1 & card_2
  }
  
  ## checking if var2 is either of im10 or im12, in that case check if teh child has
  ## gotten at least three vaccines (polio and penta)
  
  ## if any other vaccines, just check if the mother recalls 
  if (var2 %in% c("im10", "im12")){
    recall <- (data[, var2] >= 3 & data[, var2] < 8)
    recall[is.na(recall)] <- FALSE
    
  }
  else {
    recall <- (data[, var2] == 1)
    recall[is.na(recall)] <- FALSE
  }
  
  ## or operation on the three logicals (card, card_nodate and recall
  logic <- Reduce("|", list(card, card_no_date, recall))
  ind <- sum(logic, na.rm = T)/ nrow(data)
  
  return(ind)
}

## indicator for NN tatanus (minimum 2)
compute_NT_indic <- function(data){
  NT_logic <- ((data[, "mn6"] == 1) & (data[, "mn7"] >= 2) & (data[, "mn7"] < 8 ))
  NT_indic <- sum(NT_logic, na.rm = T)/ nrow(data)
  return(NT_indic)
}

compute_immun_district <- function(mother_data, child_data) {
  mother_data <- remove_empty_rows(mother_data)
  child_data <- remove_empty_rows(child_data)
  
  mother_data$district <- as.character(cluster_districts$NAME.OF.DISTRICT[as.numeric(sub("RL", "", mother_data$hh1))])
  child_data$district <- as.character(cluster_districts$NAME.OF.DISTRICT[as.numeric(sub("RL", "", child_data$hh1))])
  
  districts <- unique(child_data$district)
  district_BCG <- sapply(X = districts, function(x) compute_immun_indic(subset(child_data, district == x), var1 = "im3_bcg", var2 = "im7"))
  district_OPV3 <- sapply(X = districts, function(x) compute_immun_indic(subset(child_data, district == x), var1 = "im3_opv.3", var2 = "im10"))
  district_PENTA3 <- sapply(X = districts, function(x) compute_immun_indic(subset(child_data, district == x), var1 = "im3_penta.3", var2 = "im12"))
  district_measles1 <- sapply(X = districts, function(x) compute_immun_indic(subset(child_data, district == x), var1 = "im3_measles.I", var2 = "X16a"))
  district_NT <- sapply(X = districts, function(x) compute_NT_indic(subset(mother_data, district == x)))
  
  district_level_df <- data.frame(BCG = percent(district_BCG, 2), 
                                  OPV3 = percent(district_OPV3, 2), 
                                  PENTA3 = percent(district_PENTA3, 2), 
                                  measles1 = percent(district_measles1, 2), 
                                  NN_Tatanus = percent(district_NT, 2))
  row.names(district_level_df) <- districts
  return(district_level_df)
}

compute_immun_indic_ind <- function(data, var1, var2, type = "global", first_birthday = T){
  library(lubridate)
  
  ## getting the birthdate and converting into date element
  birth_date <- as.Date(data[, "ag1"], format = "%d-%m-%Y")
  
  ## getting the dates of the relevant vaccines
  dates_text <- data[, var1]
  dates <- as.Date(data[, var1],format = "%d-%m-%Y")
  
  ## computing age at vaccination in months
  interv <- interval(start = birth_date, end = dates) /
    duration(num = 1, units = "months")
  
  ## checking if the child received the vaccine within his/ her first birthday
  if (first_birthday == T){
    card <- (interv < 12)
  }
  ## counting even if the vaccine was not received by the first birthday
  if (first_birthday == F){
    card <- is.finite(interv)
  }
  
  card[is.na(card)] <- FALSE
  
  ## count mentions of vaccine without date on teh card
  card_no_date <- grepl(pattern = "44", x = dates_text)
  card_no_date[is.na(card_no_date)] <- FALSE
  
  ## also cheking if dates exist for vacine one and two if the variable is opv3 or penta3
  
  ## checking if var2 is either of im10 or im12, in that case check if teh child has
  ## gotten at least three vaccines (polio and penta)
  
  ## if any other vaccines, just check if the mother recalls 
  if (var2 %in% c("im10", "im12", "im15b")){
    recall <- data[ ,var2] >= as.numeric(regmatches(x = var1, m = regexpr("([1-3])$", text = var1)))
    recall[is.na(recall)] <- FALSE
  }
  else {
    recall <- (data[, var2] == 1)
    recall[is.na(recall)] <- FALSE
  }
  
  ## or operation on the three logicals (card, card_nodate and recall
  logic <- Reduce("|", list(card, card_no_date, recall))
  ind <- sum(logic, na.rm = T)/ nrow(data)
  
  return(list(ind = ind, logic = logic))
}


compute_comp_immun_cov  <- function(child_data){
  ## compute the logicals and inds lists of all vaccines
  BCG <- compute_immun_indic_ind(data = child_data, var1 = "im3_bcg", var2 = "im7", first_birthday = F)
  OPV0 <- compute_immun_indic_ind(data = child_data, var1 = "im3_opv.0", var2 = "im9", first_birthday = F)
  OPV1 <- compute_immun_indic_ind(data = child_data, var1 = "im3_opv.1", var2 = "im10", first_birthday = F)
  OPV2 <- compute_immun_indic_ind(data = child_data, var1 = "im3_opv.2", var2 = "im10", first_birthday = F)
  OPV3 <- compute_immun_indic_ind(data = child_data, var1 = "im3_opv.3", var2 = "im10", first_birthday = F)
  PENTA1 <- compute_immun_indic_ind(data = child_data, var1 = "im3_penta1", var2 = "im12", first_birthday = F)
  PENTA2 <- compute_immun_indic_ind(data = child_data, var1 = "im3_penta.2", var2 = "im12", first_birthday = F)
  PENTA3 <- compute_immun_indic_ind(data = child_data, var1 = "im3_penta.3", var2 = "im12", first_birthday = F)
  PCV1 <- compute_immun_indic_ind(data = child_data, var1 = "im3_pcv10.1_pneumo.1", var2 = "im15b", first_birthday = F)
  PCV2 <- compute_immun_indic_ind(data = child_data, var1 = "im3_pcv10.2_pneumo.2", var2 = "im15b", first_birthday = F)
  PCV3 <- compute_immun_indic_ind(data = child_data, var1 = "im3_pcv10.3_pneumo.3", var2 = "im15b", first_birthday = F)
  MEASLES1 <- compute_immun_indic_ind(data = child_data, var1 = "im3_measles.I", var2 = "X16a", first_birthday = F)
  MEASLES2 <- compute_immun_indic_ind(data = child_data, var1 = "im3_measles.II", var2 = "X16b", first_birthday = F)
  
  ## applying and operation onthe logicals of all vaccines=
  FULL <- Reduce("&", list(BCG$logic, OPV0$logic, OPV1$logic, OPV2$logic, OPV3$logic, PENTA1$logic, PENTA2$logic, PENTA3$logic, 
                           PCV1$logic, PCV2$logic, PCV3$logic, MEASLES1$logic))
  ## calculating full coverage indicator
  FULL_cov <- sum(FULL, na.rm = T)/nrow(child_data)
  
  ## Return a DF of all vaccines
  return(data.frame(BCG = percent(BCG$ind, 2), 
                    OPV0 = percent(OPV0$ind, 2),
                    OPV1 = percent(OPV1$ind, 2),
                    OPV2 = percent(OPV2$ind, 2),
                    OPV3 = percent(OPV3$ind, 2),
                    PENTA1 = percent(PENTA1$ind, 2),
                    PENTA2 = percent(PENTA2$ind, 2),
                    PENTA3 = percent(PENTA3$ind, 2), 
                    PCV1 = percent(PCV1$ind, 2),
                    PCV2 = percent(PCV2$ind, 2),
                    PCV3 = percent(PCV3$ind, 2),
                    measles1 = percent(MEASLES1$ind, 2),
                    measles2 = percent(MEASLES2$ind, 2),
                    Full = percent(FULL_cov, 2), row.names = c("Punjab level")))
}


compute_immun_cov_district <- function(child_data) {
  ## removing records with no cluster number just in case 
  child_data <- remove_empty_rows(child_data)
  ## adding districts across each cluster number
  child_data$district <- as.character(cluster_districts$NAME.OF.DISTRICT[as.numeric(sub("RL", "", child_data$hh1))])
  
  ## getting unique districts
  districts <- unique(child_data$district)
  districts <- districts[!is.na(districts)]
  
  ## applying the coverage function across all districts
  district_BCG <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_bcg", var2 = "im7", first_birthday = F)$ind)
  district_OPV0 <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_opv.0", var2 = "im9", first_birthday = F)$ind)
  district_OPV1 <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_opv.1", var2 = "im10", first_birthday = F)$ind)
  district_OPV2 <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_opv.2", var2 = "im10", first_birthday = F)$ind)
  district_OPV3 <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_opv.3", var2 = "im10", first_birthday = F)$ind)
  district_PENTA1 <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_penta1", var2 = "im12", first_birthday = F)$ind)
  district_PENTA2 <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_penta.2", var2 = "im12", first_birthday = F)$ind)
  district_PENTA3 <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_penta.3", var2 = "im12", first_birthday = F)$ind)
  district_PCV1 <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_pcv10.1_pneumo.1", var2 = "im15b", first_birthday = F)$ind)
  district_PCV2 <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_pcv10.2_pneumo.2", var2 = "im15b", first_birthday = F)$ind)
  district_PCV3 <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_pcv10.3_pneumo.3", var2 = "im15b", first_birthday = F)$ind)
  district_MEASLES1 <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_measles.I", var2 = "X16a", first_birthday = F)$ind)
  district_MEASLES2 <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_measles.II", var2 = "X16b", first_birthday = F)$ind)
  district_MEASLES2 <- sapply(X = districts, function(x) compute_immun_indic_ind(subset(child_data, district == x), var1 = "im3_measles.II", var2 = "X16b", first_birthday = F)$ind)
  district_FULL <- sapply(X = districts, function(x) compute_comp_immun_cov(subset(child_data, district == x))$Full)
  
  ## creating data frame of all vaccines/ district level
  district_level_df <- data.frame(BCG = percent(district_BCG, 2), 
                                  OPV0 = percent(district_OPV0, 2),
                                  OPV1 = percent(district_OPV1, 2),
                                  OPV2 = percent(district_OPV2, 2),
                                  OPV3 = percent(district_OPV3, 2),
                                  PENTA1 = percent(district_PENTA1, 2),
                                  PENTA2 = percent(district_PENTA2, 2),
                                  PENTA3 = percent(district_PENTA3, 2), 
                                  PCV1 = percent(district_PCV1, 2),
                                  PCV2 = percent(district_PCV2, 2),
                                  PCV3 = percent(district_PCV3, 2),
                                  measles1 = percent(district_MEASLES1, 2),
                                  measles2 = percent(district_MEASLES2, 2),
                                  Full = percent(district_FULL, 2))
  ## setting the row names as names of districts
  row.names(district_level_df) <- districts
  ## computing all vaccine indics at punjab level
  punjab_level_df <- compute_comp_immun_cov(child_data = child_data)
  ## Combining Punjab and district level indicators
  district_level_df <- rbind(punjab_level_df, district_level_df)
  return(district_level_df)
}


global_ind <- function(mother_data, child_data, hc_data){
  AN <- compute_AN_indic(mother_data, type = "global")
  mother_time_split <- time_value_split(var = "pn2", data_set = mother_data, remove_orig = T)
  mother_time_split <- time_value_split(var = "pn12a", data_set = mother_time_split, remove_orig = T)
  mother_time_split <- time_value_split(var = "pn12b", data_set = mother_time_split, remove_orig = T)
  mother_time_split <- time_value_split(var = "pn21a", data_set = mother_time_split, remove_orig = T)
  mother_time_split <- time_value_split(var = "pn21b", data_set = mother_time_split, remove_orig = T)
  mother_time_split <- time_value_split(var = "mn25", data_set = mother_time_split, remove_orig = T)
  
  PN <- compute_PN_indic(mother_time_split, type = "global")
  BF <- compute_BF_indic(mother_time_split, child_data, hc_data, type = "global")
  deliv <- compute_delivery_indic(mother_data, type = "global")
  
  child <- append_date_enum(child_data, hc_data)
  child_under12 <- child %>% filter(child_age < 12)
  child_over12 <- child %>% filter(child_age >= 12 & child_age < 24)
  
  ## immunization indicators
  immun <- c("bcg_under12" = compute_immun_indic(child_under12, var1 = "im3_bcg", var2 = "im7"),
             "OPV3_under12" = compute_immun_indic(child_under12, var1 = "im3_opv.3", var2 = "im10"),
             "Penta3_under12" = compute_immun_indic(child_under12, var1 = "im3_penta.3", var2 = "im12"),
             "measlesI_under12" = compute_immun_indic(child_under12, var1 = "im3_measles.I", var2 = "X16a"),
             "bcg_over12" = compute_immun_indic(child_over12, var1 = "im3_bcg", var2 = "im7"),
             "OPV3_over12" = compute_immun_indic(child_over12, var1 = "im3_opv.3", var2 = "im10"),
             "Penta3_over12" = compute_immun_indic(child_over12, var1 = "im3_penta.3", var2 = "im12"),
             "measlesI_over12" = compute_immun_indic(child_over12, var1 = "im3_measles.I", var2 = "X16a"),
             "NT_tatanus" = compute_NT_indic(mother_data) 
  )
  return(as.data.frame(c(AN, PN, BF, deliv, immun)))
}

remove_empty_rows <- function(data){
  return(data[!is.na(data$hh1), ])
}


without_electric <- function(hc_data, var2 = 'hc8_opt3'){
  logic <- (hc_data[, "hc8_opt1"] == 2 & hc_data[, var2] == 1)
  logic[is.na(logic)] <- FALSE
  incon <- hc_data[logic, ] %>% select(hh1:hh2, hh3name, district, hh5)
  
  return(incon)
}

## hc_data <- hc

cattle_incon <- function(hc_data){
  ## check if HH has cattle
  cattle_logic <- hc_data$hc13 == 1
  ## select columns/ getting amount of cattle/ diff types
  hc_cattle <- hc_data %>% select(hc14_cat1:hc14_cat5)
  ## making sure all are numeric 
  
  hc_cattle <- data.frame(lapply(hc_cattle, function(x) as.numeric(as.character(x))))
  
  ## summing the cattle of all types
  cattle_num <- rowSums(x = hc_cattle, na.rm = T, dims = 1)
  
  ## checking for inconsistency
  cattle_incon_logic <- cattle_logic & (cattle_num == 0)
  
  ## replacing NAs with false
  cattle_incon_logic[is.na(cattle_incon_logic)] <- FALSE
  ## getting credentials of inconsistent entries
  return(hc_data[cattle_incon_logic, ] %>% 
           select(hh1, hh2, hh3name, district, hh5))
}


hosp_notest <- function(mother_data, hc_data){
  ## checking if the ANC took place in public or pvt hospital
  hosp_logical <- mother_data$mn2b == "21" | mother_data$mn2b == "31" 
  
  ## getting ANC content info from mother data
  moth_AN <- mother_data %>% 
    select(starts_with("mn4_")) %>%
    select(-(contains("family")))
  
  ## adding all entries of ANC content
  AN_sum <- rowSums(x = moth_AN, na.rm = T, dims = 1)
  
  ## checking for a condition when all tests are 'NO'
  AN_sum_logic <- AN_sum == 8
  
  ## logic for hospital and all tests 'NO'
  hosp_notest_logic <- hosp_logical & AN_sum_logic
  hosp_notest_logic[is.na(hosp_notest_logic)] <- FALSE
  
  ## listing ids of the inconsistencies
  list_HHs <- mother_data[hosp_notest_logic, ] %>% select(hh1, hh2)
  
  ## getting id credentials for HC data
  hc_cred <- hc_data  %>% select(hh1, hh2, hh3name, district, hh5)
  
  ## adding credentials to ids
  list_HHs <- list_HHs %>% left_join(hc_cred, by = c('hh1', 'hh2'))
  
  ## returning the dataframe with problems
  return(list_HHs)
}

AF_incon <- function(mother_data, hc_data){
  ## checking if observation and check are inconsistent
  glasses_logic <- (mother_data$af2 != mother_data$af5)
  glasses_logic[is.na(glasses_logic)] <- FALSE
  h_aid_logic <- (mother_data$af3 != mother_data$af7)
  h_aid_logic[is.na(h_aid_logic)] <- FALSE
  
  ## retrieving the ids of the inconsistencies
  glasses <- mother_data[glasses_logic, ] %>% select(hh1, hh2)
  h_aid <- mother_data[h_aid_logic, ] %>% select(hh1, hh2)
  
  ## get credentials from HH data
  hc_cred <- hc_data  %>% select(hh1, hh2, hh3name, district, hh5)
  
  ## joining credentials ro the list of ID's
  glasses <- glasses %>% left_join(hc_cred, by = c('hh1', 'hh2'))
  h_aid <- h_aid %>% left_join(hc_cred, by = c('hh1', 'hh2'))
  
  return(list(glasses = glasses, h_aid = h_aid))
}

join_cred <- function(data, hc_data){
  ## function to join district, enumerator and date to identified ICs
  hc_cred <- hc_data  %>% select(hh1, hh2, hh3name, district, hh5)
  data <- data %>% left_join(hc_cred, by = c('hh1', 'hh2'))
  return(data)
}


immun_incon <- function(child_data, hc_data){
  ## check if card seen
  card_seen <- child_data$im1 == 1
  ## subsetting the date columns for immunization (converting into char)
  child_immun_dates <- child_data %>% select(contains("im3"))
  child_immun_dates <- data.frame(lapply(child_immun_dates, as.character), 
                                  stringsAsFactors=FALSE)
  ## a logical dataframe if the date field is empty or not
  empty_date_logic <- data.frame(lapply(child_immun_dates, 
                                        function(x) x == ""))
  
  ## a logical vector for all empty 
  all_empty_logic <- (rowSums(empty_date_logic, na.rm = T) == ncol(empty_date_logic))
  ## returning ids and joining credentials
  card_seen_ND <- child_data[(card_seen & all_empty_logic), ] %>% select(hh1, hh2)
  card_seen_ND <- join_cred(card_seen_ND, hc_data)
  
  ## a logical vector for all complete, add all FALSE
  all_comp_logic <- (rowSums(empty_date_logic, na.rm = T) == 0)
  ## checking if enumerator entered 'all vaccinations are not listed'
  all_vacc_no <- (as.character(child_data$im4) == "No")  | (as.character(child_data$im4) == "2")
  
  ## logical for all dates but wrong follow up skip
  all_Dates_No_logic <- (all_comp_logic & all_vacc_no)
  ## returning IDs and joining credentials
  all_Dates_No <- child_data[all_Dates_No_logic, ] %>% select(hh1, hh2)
  all_Dates_No <- join_cred(all_Dates_No, hc_data)
  
  ## logical for BCG, OPV.0 empty and all the rest of the dates are full
  all_but_BCG_logic <- empty_date_logic$im3_bcg == TRUE & 
    empty_date_logic$im3_opv.0 == TRUE &
    rowSums(empty_date_logic, na.rm = T) == 2
  
  ## returning IDs and joining credentials
  all_but_BCG <- child_data[all_but_BCG_logic, ] %>% select(hh1, hh2)
  all_but_BCG <- join_cred(all_but_BCG, hc_data)
  
  ## any date empty but im4 == Yes
  
  any_empty_logic <- (rowSums(empty_date_logic, na.rm = T) > 0)
  (all_recorded_log <- as.character(child_data$im4) == "Yes") | (as.character(child_data$im4) == "1")
  
  ## getting the ids and joining creds
  any_empty_all_rec <- child_data[(any_empty_logic & all_recorded_log), ] %>% select(hh1, hh2)
  any_empty_all_rec <- join_cred(any_empty_all_rec, hc_data)
  
  ## returning a list of three inconsistency tables
  return(list(card_seen_ND = card_seen_ND,
              all_Dates_WS = all_Dates_No,
              all_but_BCG = all_but_BCG,
              any_empty_all_rec = any_empty_all_rec))
}

id_neg_age <- function(child_data, hc_data) {
  ## get birthday of the children
  birth_date <- as.Date(child_data$ag1, format = "%d-%m-%Y") 
  
  ## getting immunization dates and converting them into date vars
  immun_dates <- child_data %>% select(contains("im3_"))
  
  immun_dates <- data.frame(lapply(immun_dates, function(x) as.character(x)))
  immun_dates <- data.frame(lapply(immun_dates, function(x) as.Date(x, format = "%d-%m-%Y")))
  
  ## function for calculating age in months
  months_age <- function(birth_date, end_date){
    interv <- interval(start = birth_date, end = end_date) /
      duration(num = 1, units = "months")
    return(interv)
  } 
  
  ## calculating age at immunization
  age_immun <- data.frame(lapply(immun_dates, function(x) months_age(birth_date = birth_date, 
                                                                     end_date = x)))
  ## checking if age is negative, to identify errors
  NI_logic <- as.logical(apply(X = age_immun, MARGIN = 1, FUN = function(x) sum(x < 0)))
  NI_logic[is.na(NI_logic)] <- FALSE
  
  ## return ids and combining credentials
  id_cred <- join_cred(data = child_data[NI_logic, ] %>% select (hh1, hh2), 
                       hc_data = hc_data)
  return(id_cred)
}


non_elect_sum <- function(hc_data, mother_data, child_data){
  ## compute immunization incon
  immun_incon_ <- immun_incon(child_data = child_data, hc_data = hc_data)
  
  ## getting district level summaries
  summary_CSND <- immun_incon_$card_seen_ND %>%
    group_by(district) %>%
    summarize(card_seen_ND = n())
  
  summary_ADWS <- immun_incon_$all_Dates_WS %>%
    group_by(district) %>%
    summarize(all_Dates_WS = n())
  
  summary_ABCG <- immun_incon_$all_but_BCG %>%
    group_by(district) %>%
    summarize(all_but_BCG = n())
  
  summary_EWS <- immun_incon_$any_empty_all_rec %>%
    group_by(district) %>%
    summarize(empt_dat_im4 = n())
  
  ## getting adult functioning incon
  AF_incon_ <- AF_incon(mother_data = mother_data, hc_data = hc_data)
  
  ## getting district level summaries
  summary_glasses <- AF_incon_$glasses %>%
    group_by(district) %>%
    summarize(glasses = n())
  
  summary_h_aid <- AF_incon_$h_aid %>%
    group_by(district) %>%
    summarize(h_aid = n())
  
  ## AN care level incon
  hosp_notest_ <- hosp_notest(mother_data = mother_data, hc_data = hc_data)
  
  ## getting summaries
  summary_hosp_NT <- hosp_notest_ %>%
    group_by(district) %>%
    summarize(hosp_NT = n())
  
  ## Incon about cattle
  cattle_incon_ <- cattle_incon(hc_data = hc_data)
  
  ## getting summary
  summary_cattle <- cattle_incon_ %>%
    group_by(district) %>%
    summarize(cattle = n())
  
  ## negative age incon
  neg_age_incon_ <- id_neg_age(child_data = child_data, 
                              hc_data = hc_data)
  summary_neg_age <- neg_age_incon_ %>%
    group_by(district) %>%
    summarize(neg_age = n())
  
  ## combining all the different summaries
  non_elect_sum <- summary_ABCG %>%
    full_join(summary_ADWS, by = c('district')) %>%
    full_join(summary_CSND, by = c('district')) %>%
    full_join(summary_EWS, by = c('district')) %>%
    full_join(summary_glasses, by = c('district')) %>%
    full_join(summary_h_aid, by = c('district')) %>%
    full_join(summary_hosp_NT, by = c('district')) %>%
    full_join(summary_cattle, by = c('district')) %>%
    full_join(summary_neg_age, by = c('district'))
  
  ## converting district to character
  non_elect_sum$district <- as.character(non_elect_sum$district)
  
  ## flitering out emptya and NA

  non_elect_sum <- non_elect_sum %>% filter(!(district == "" | is.na(district)))
  non_elect_sum[is.na(non_elect_sum)] <- 0
  
  return(non_elect_sum)
}


elect_sum <- function(hc_data){
  ## listing the variables on which the without without_elect fun will be applied
  vars <- c("Television" = "hc8_opt3",
            "Fridge" = "hc8_opt5",
            "Computer" = "hc8_opt7",
            "AC" = "hc8_opt8",
            "WashingMachine" = "hc8_opt9",
            "Fan" = "hc8_opt10",
            "Iron" = "hc8_opt13",
            "DunkyPump" = "hc8_opt15",
            "ElectricityFuel" = "hc6")
  ## applying the without_electric function and doing joins in a loop
  for (i in vars){
    w_e <- without_electric(hc_data, var2 = i)
    w_e_sum <- w_e %>% group_by(district) %>%
      summarize(n = n())
    if (i == vars[1]){
      combined_sum <- w_e_sum
    }
    else{
      combined_sum <- combined_sum %>%
        full_join(w_e_sum, by = c("district"))
    }
    
  }
  names(combined_sum)<- c("district", names(vars))
  combined_sum[is.na(combined_sum)] <- 0
  return(combined_sum)
}

filter_incon <- function(mother_data, hc_data){
  mother_data <- mother_data[!(is.na(mother_data$hh1)), ]
  inst_deliv_var <- as.character(mother_data$mn18)
  inst_deliv_var <- regmatches(x = inst_deliv_var, m = regexpr("^[0-9][0-9]", text = inst_deliv_var))
  inst_deliv_var <- as.numeric(inst_deliv_var)
  inst_deliv_log <- inst_deliv_var %in% c(21:26, 31:36)
  non_inst_deliv_log <- inst_deliv_var %in% c(11, 12, 13, 96)
  
  pn_1_skip <- trimws(as.character(mother_data$pn1), which = "both")
  
  no_skip_log_pn1 <- pn_1_skip == "No the child was not delivered in a health facility"
  yes_skip_log_pn1 <- pn_1_skip == "Yes the child was delivered in a health facility"
  
  sum(inst_deliv_log & no_skip_log_pn1)
  
  inst_no_pn1 <- mother_data[(inst_deliv_log & no_skip_log_pn1), ] %>% select(hh1:hh2)
  inst_no_pn1$inst_no_pn1 <- 1
  
  non_inst_yes_pn1 <- mother_data[(non_inst_deliv_log & yes_skip_log_pn1), ] %>% select(hh1:hh2)
  non_inst_yes_pn1$non_inst_yes_pn1 <- 1
  
  pn_15_skip <- trimws(as.character(mother_data$pn15), which = "both")
  
  no_skip_log_pn15 <- pn_15_skip == "No the child was not delivered in a health facility"
  yes_skip_log_pn15 <- pn_15_skip == "Yes the child was delivered in a health facility"
  
  inst_no_pn15 <- mother_data[(inst_deliv_log & no_skip_log_pn15), ] %>% select(hh1, hh2)
  inst_no_pn15$inst_no_pn15 <- 1
  
  non_inst_yes_pn15 <- mother_data[(non_inst_deliv_log & yes_skip_log_pn15), ] %>% select(hh1, hh2)
  non_inst_yes_pn15$non_inst_yes_pn15 <- 1
  
  
  ## getting the skilled birth att variable and replacing others options
  skill_att_var <- gsub(pattern = " [A-Z]*" , replacement = "", 
                        x = as.character(mother_data$mn17))
  skill_att_var 
  
  skilled_options <- toupper(letters[1:5]) 
  
  skilled_options
  
  skilled_log <- as.logical(unlist(lapply(str_split(skill_att_var, ","), function(x) sum(skilled_options %in% x))))
  
  pn_17_skip <- trimws(as.character(mother_data$pn17), which = "both")
  
  no_skip_log_pn17 <- pn_17_skip == "No delivery not assisted by a health professional or traditional birth attendant health worker"
  yes_skip_log_pn17 <- pn_17_skip == "Yes delivery assisted by a health professional or traditional birth attendant"
  
  skilled_no <- mother_data[(skilled_log & no_skip_log_pn17), ] %>% select(hh1:hh2)
  skilled_no$skilled_no <- 1
  
  
  at_least_two <- mother_data$mn7 >= 2
  
  mn8_skip <- as.character(mother_data$mn8)
  
  at_least_two_log <- mn8_skip == "At least two tetanus injections during last pregnancy"
  only_one_log <- mn8_skip == "Only one tetanus injection during last pregnancy"
  
  two_only_one <- mother_data[(at_least_two & only_one_log), ] %>% select(hh1:hh2)
  two_only_one$two_only_one <- 1
  one_atl_two <- mother_data[((mother_data$mn7 == 1) & at_least_two_log), ] %>% select(hh1:hh2)
  one_atl_two$one_atl_two <- 1
  
  combined_filters <- inst_no_pn1 %>% full_join(non_inst_yes_pn1) %>%
    full_join(inst_no_pn15) %>%
    full_join(non_inst_yes_pn15) %>%
    full_join(skilled_no) %>%
    full_join(two_only_one) %>%
    full_join(one_atl_two)
  
  combined_filters <- combined_filters[!(is.na(combined_filters$hh1)), ]
  combined_filters[is.na(combined_filters)] <- 0
  combined_filters <- join_cred(data = combined_filters, hc_data = hc_data) %>% select(hh1,hh2, hh3name, district, hh5, everything())
  
  filter_summary <- combined_filters %>%
    group_by(district) %>%
    summarise(inst_no_pn1 = sum(inst_no_pn1),
              non_inst_yes_pn1 = sum(non_inst_yes_pn1),
              inst_no_pn15 = sum(inst_no_pn15),
              non_inst_yes_pn15 = sum(non_inst_yes_pn15),
              skilled_no_pn17 = sum(skilled_no),
              two_only_one_mn8 = sum(two_only_one),
              one_atl_two_mn8 = sum(one_atl_two))
  
  return(list(filter = combined_filters, summary = filter_summary))
}


plot_incon <- function(dataset){
  dataset$hh5 <- as.Date(dataset$hh5, format = "%d-%m-%Y")
  date_wise <- dataset %>% 
    group_by(hh5) %>%
    summarise(n = n())
  library(ggplot2)
  ggplot(data = date_wise, aes(hh5, n)) + geom_line() + 
    geom_point() 
}
  
## plot_incon(dataset = immun_incon(child, hc)$any_empty_all_rec)

## function to summarize enumerator level progress
enum_progress <- function(mother_data, child_data, hc_data){
  ## getting the seemingly completed interviews
  comp_int <- complete_interviews(mother_data = mother_data, 
                                  children_data = child_data, 
                                  hc_data = hc_data)
  ## converting date to a date object and joining enum level info in child interview 
  comp_int$hc_mother$hh5 <- as.Date(comp_int$hc_mother$hh5, format = '%d-%m-%Y')
  comp_int$child <- join_cred(data = comp_int$child, hc_data = comp_int$hc_mother)
  
  ## creating a summary(cluster level, datewise) - HHs and mother
  enum_date_wise_mo <- comp_int$hc_mother %>% 
    group_by(hh1, hh5, hh3name, hh3code, district) %>%
    summarise(HHs = n_distinct(hh2),
              n_mothers = n())
  
  ## creating a summary(cluster level, datewise) - HHs and mother
  enum_date_wise_ch <- comp_int$child %>% 
    group_by(hh1, hh5, hh3name) %>%
    summarise(n_child = n())
  
  ## joining mother and child interview numbers
  enum_date_wise <- enum_date_wise_mo %>% left_join(enum_date_wise_ch, 
                                                    by = c('hh1', 'hh5', 'hh3name')) 
  enum_date_wise$hh3code <- as.factor(enum_date_wise$hh3code)
  ## creating summary (consolidated)
  consol_enum <- enum_date_wise %>% 
    group_by(hh3name, hh3code, district) %>%
    summarise(HHs = sum(HHs), n_mothers = sum(n_mothers), n_child = sum(n_child)) %>%
    arrange(desc(HHs))
   
  ## returning a list of consolidated and datewise summary
  return(list(consol = consol_enum, date_wise = enum_date_wise))
}

