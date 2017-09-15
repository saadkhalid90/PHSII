## loading libraries
library(dplyr)

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
create_dummy <- function(var, max_alphabet = "F", others = TRUE, DK = FALSE, No_one = FALSE, dataset, remove_orig = FALSE, others_text = TRUE){
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
  names(DUMMY) <- paste(var, "_", options_, sep = "")
  
  DUMMY <- as.data.frame(ifelse(DUMMY == TRUE, yes = 1, no = 0))
  ## print(names(DUMMY))
  ## creating dummies and a text column for others 
  if (others == TRUE){
    others_df <- as.data.frame(do.call(rbind, lapply(split_list, finding_X)))
    
    others_df_names <- paste(var, "_", c("X", "Xtext"), sep ="")
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
  mother_upd <- create_dummy(var = "mn2", max_alphabet = "F", others = T, DK = F, dataset = mother_data, remove_orig = T)
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
  
  mothers_missing_children$District <- cluster_districts$NAME.OF.DISTRICT[as.numeric(as.character(mothers_missing_children$hh1))] 
  HHs_missing_data$District <- cluster_districts$NAME.OF.DISTRICT[as.numeric(as.character(HHs_missing_data$hh1))] 
  missing_hl_mother$District <- cluster_districts$NAME.OF.DISTRICT[as.numeric(as.character(missing_hl_mother$hh1))] 
  missing_hl_child$District <- cluster_districts$NAME.OF.DISTRICT[as.numeric(as.character(missing_hl_child$hh1))] 
  
  return(list(hl_moth = missing_hl_mother %>% select(District, hh1:hl4a), 
              hl_child = missing_hl_child %>% select(District, hh1:hl4a), 
              HH_moth = HHs_missing_data %>% select(District, hh1:hh2), 
              moth_child  = mothers_missing_children  %>% select(District, hh1:wm4)))
}

complete_interviews <- function(mother_data, children_data, hc_data){
  ## removing empty rows (judging by NA hh1)
  mother_data <- mother_data[!(is.na(mother_data$hh1)), ]
  children_data <- children_data[!(is.na(children_data$hh1)), ]
  hc_data <- hc_data[!(is.na(hc_data$hh1)), ]
  
  children_data$X <- NULL
  mother_data$X <- NULL
  hc_data$X <- NULL
  
  mother_c <- mother_data %>% semi_join(children_data, by = c("hh1", "hh2", "wm2" = "uf6"))
  hc_mother <- hc_data %>% right_join(mother_c, by = c("hh1", "hh2"))

  return(list(hc_mother = hc_mother, child = children_data))
}


create_summary <- function(hl_data, hc_data, mother_data, child_data){
  comp_int <- complete_interviews(mother_data = mother_data, 
                                  children_data = child_data, 
                                  hc_data = hc_data)
  missing <- identify_missing(hl = hl_data, 
                              hc = hc_data, 
                              mother = mother_data, 
                              child = child_data)
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
  
  ##hc_data
  
  summary_PHS <- HH_moth_sum %>% 
    full_join(child_sum, by = "hh1") %>%
    full_join(hl_moth_miss, by = "hh1") %>%
    full_join(hl_child_miss, by = "hh1") %>%
    full_join(HH_moth_miss, by = "hh1") %>%
    full_join(moth_child_miss, by = "hh1") %>%
    left_join(hh12_summary, by = "hh1")
  
  summary_PHS <- summary_PHS[!is.na(summary_PHS$hh1), ]
  
  ## replacing all NAs with zeros
  summary_PHS[is.na(summary_PHS)] <- 0
  
  cluster_districts <- readRDS("cluster_district.rds")
  
  summary_PHS$District <- cluster_districts$NAME.OF.DISTRICT[as.numeric(as.character(summary_PHS$hh1))] 

  summary_PHS <- summary_PHS %>% select(District, hh1:n_ref)
    
  return(summary_PHS)
}
# 

create_enum_summary <- function(hl_data, hc_data, mother_data, child_data){
  ## getting completed and missing cases
  comp_int <- complete_interviews(mother_data = mother_data,
                                  children_data = child_data,
                                  hc_data = hc_data)
  missing <- missing <- identify_missing(hl = hl_data,
                                         hc = hc_data,
                                         mother = mother_data,
                                         child = child_data)
  HH_moth_sum <- comp_int$hc_mother %>%
    group_by(hh3name) %>%
    summarise(HHs = n_distinct(hh2),
              n_mothers = n())
  
  ## extracting info on enumerators
  enum <- comp_int$hc_mother %>% select(hh1, hh2, hh3name)
  ## removing any duplicated entries (id is hh1 and hh2)
  enum <- enum[!duplicated(enum[1:2]),]
  
  ## getting children's data with enumerators
  child_with_enum <- comp_int$child %>% 
    left_join(enum, by = c("hh1", "hh2"))
  
  child_sum <- child_with_enum %>%
    group_by(hh3name) %>%
    summarise(n_child = n())
  
  hl_child_miss <-  missing$hl_child %>%
    group_by(hh1, hh2) %>%
    summarise(hl_child = n())
  hl_child_miss <- hl_child_miss %>% 
    left_join(enum, by = c("hh1", "hh2"))
  
  hl_moth_miss <-  missing$hl_moth %>%
    group_by(hh1, hh2) %>%
    summarise(hl_moth = n())
  
  hl_moth_miss <- hl_moth_miss %>% 
    left_join(enum, by = c("hh1", "hh2"))
  
  HH_moth_miss <-  missing$HH_moth %>%
    group_by(hh1, hh2) %>%
    summarise(hc_moth = n())
  HH_moth_miss <- HH_moth_miss %>% 
    left_join(enum, by = c("hh1", "hh2"))
  
  moth_child_miss <-  missing$moth_child %>%
    group_by(hh1, hh2) %>%
    summarise(moth_child = n())
  moth_child_miss <- moth_child_miss %>% 
    left_join(enum, by = c("hh1", "hh2"))
  
  summary_PHS_enum <- HH_moth_sum %>% 
    full_join(child_sum, by = "hh1") %>%
    full_join(hl_moth_miss, by = "hh3name") %>%
    full_join(hl_child_miss, by = "hh3name") %>%
    full_join(HH_moth_miss, by = "hh3name") %>%
    full_join(moth_child_miss, by = "hh3name")
  
  ## replacing all NAs with zeros
  ##summary_PHS[is.na(summary_PHS)] <- 0
  
  cluster_districts <- readRDS("cluster_district.rds")
  
  summary_PHS_enum$District <- cluster_districts$NAME.OF.DISTRICT[summary_PHS_enum$hh1] 
  
  summary_PHS_enum <- summary_PHS_enum %>% select(District, hh3name:moth_child)
  
  return(summary_PHS_enum)
}

# comp_int <- complete_interviews(mother_data = mother,
#                                 children_data = child,
#                                 hc_data = hc)
# missing <- missing <- identify_missing(hl = hl,
#                                        hc = hc,
#                                        mother = mother,
#                                        child = child)
# HH_moth_sum <- comp_int$hc_mother %>%
#   group_by(hh1,hh3name) %>%
#   summarise(HHs = n_distinct(hh2),
#             n_mothers = n())
# 
# enum <- comp_int$hc_mother %>% select(hh1, hh2, hh3name)
# 
# enum <- enum[!duplicated(enum[1:2]),]
# 
# child_with_enum <- comp_int$child %>% 
#   left_join(enum, by = c("hh1", "hh2"))
# 
# child_sum <- child_with_enum %>%
#   group_by(hh1, hh3name) %>%
#   summarise(n_child = n())
# 
# hl_child_miss <-  missing$hl_child %>%
#   group_by(hh1) %>%
#   summarise(hl_child = n())
# hl_moth_miss <-  missing$hl_moth %>%
#   group_by(hh1) %>%
#   summarise(hl_moth = n())
# HH_moth_miss <-  missing$HH_moth %>%
#   group_by(hh1) %>%
#   summarise(hc_moth = n())
# moth_child_miss <-  missing$moth_child %>%
#   group_by(hh1) %>%
#   summarise(moth_child = n())
# 
# summary_PHS_enum <- HH_moth_sum %>% 
#   full_join(child_sum, by = c("hh1", "hh3name")) %>%
#   full_join(hl_moth_miss, by = c("hh1", "hh3name")) %>%
#   full_join(hl_child_miss, by = c("hh1", "hh3name")) %>%
#   full_join(HH_moth_miss, by = c("hh1", "hh3name")) %>%
#   full_join(moth_child_miss, by = c("hh1", "hh3name"))

## functions for computing indicators

compute_BF_indic<- function(mother_data, child_data, type = "global"){
  ever_breastfed <- sum(mother_data$mn24 == 1, na.rm = T)/ nrow(mother_data)
  ## mother_data_split <- time_value_split(var = "mn25", data_set = mother_data, remove_orig = T)
  early_breastfed <- sum(as.character(mother_data$mn25_R) == '000', na.rm = T)/ nrow(mother_data)
  bottle_feed <- sum(child_data$bd4 == 1, na.rm = T)/ nrow(child_data)
  excl_breastfed <- sum(mother_data$mn26 == 2, na.rm = T)/ nrow(mother_data)
  
  if (type == "district"){
    mother_data <- mother_data[!(is.na(mother_data$hh1)), ]
    child_data <- child_data[!(is.na(child_data$hh1)), ]
    mother_data$district <- as.character(cluster_districts$NAME.OF.DISTRICT[as.integer(as.character(mother_data$hh1))])
    child_data$district <- as.character(cluster_districts$NAME.OF.DISTRICT[as.integer(as.character(child_data$hh1))])
    district_inData <- unique(mother_data$district)
    district_level <- t(as.data.frame(sapply(district_inData, function(x) compute_BF_indic(subset(mother_data, district == x), 
                                                                                           subset(child_data, district == x)))))
    
    return(district_level)
  }
  else {
    return(c(ever_bf = round(ever_breastfed, 4), 
             early_bf = round(early_breastfed, 4), 
             bottle_feed = round(bottle_feed, 4), 
             excl_bf = round(excl_breastfed, 4))) 
  }
}


compute_AN_indic <- function(mother_data, type = "global"){
  ## content of antenatal care
  bloodp_logic <- (mother_data$mn4_bloodpressure == 1) 
  blood_logic <- (mother_data$mn4_blood == 1)
  urine_logic <- (mother_data$mn4_urine == 1)
  weigh_logic <- (mother_data$mn4_weighed == 1)
  
  comb_logical <- Reduce("&", list(bloodp_logic, blood_logic, urine_logic))
  comb_logical_4 <- Reduce("&", list(bloodp_logic, blood_logic, urine_logic, weigh_logic))
  three <- sum(comb_logical, na.rm = T)/nrow(mother_data)
  all_four <- sum(comb_logical_4, na.rm = T)/nrow(mother_data)
  
  if (type == "district"){
    mother_data <- mother_data[!(is.na(mother_data$hh1)), ]
    mother_data$district <- as.character(cluster_districts$NAME.OF.DISTRICT[as.integer(as.character(mother_data$hh1))])
    district_inData <- unique(mother_data$district)
    district_level <- t(as.data.frame(sapply(district_inData, function(x) compute_AN_indic(subset(mother_data, district == x)))))
    
    return(district_level)
  }
  else {
    return(c(three = round(three, 4), 
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
  
  if (type == "district"){
    mother_data <- mother_data[!(is.na(mother_data$hh1)), ]
    mother_data$district <- as.character(cluster_districts$NAME.OF.DISTRICT[as.integer(as.character(mother_data$hh1))])
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


compute_PN_indic <- function(mother_data, type = "global"){
  
  ## stay in Health facility
  # if (type == "global"){
  #   mother_data_split <- time_value_split(var = "pn2", data_set = mother_data, remove_orig = T)
  # }
  
  pn2_R <- as.numeric(as.character(mother_data$pn2_R))
  pn2_I <- as.numeric(as.character(mother_data$pn2_I))
  PN_stay_num <- sum((pn2_R == 1 & pn2_I >=12) | (pn2_R >= 2 & pn2_R <= 3), na.rm = T)
  PN_stay <- PN_stay_num/ nrow(mother_data) 
  
  ## PN health check for newborn
  # if (type == "global"){
  #   mother_data_split <- time_value_split(var = "pn12a", data_set = mother_data, remove_orig = T)
  #   mother_data_split <- time_value_split(var = "pn12b", data_set = mother_data_split, remove_orig = T)
  # }
  
  pn12a_R <- as.numeric(as.character(mother_data$pn12a_R))
  pn12a_I <- as.numeric(as.character(mother_data$pn12a_I))
  pn12a_logic <- (pn12a_R == 1) | (pn12a_R == 2 & pn12a_I <= 2)
  
  pn12b_R <- as.numeric(as.character(mother_data$pn12b_R))
  pn12b_I <- as.numeric(as.character(mother_data$pn12b_I))
  pn12b_logic <- (pn12b_R == 1) | (pn12b_R == 2 & pn12b_I <= 2)
  
  hck_NB_num <-  sum(pn12a_logic, na.rm = T) + sum(pn12b_logic, na.rm = T)
  hck_NB <- hck_NB_num/ nrow(mother_data)
  
  ## PN health check for the mother
  # if (type == "global"){
  #   mother_data_split <- time_value_split(var = "pn21a", data_set = mother_data, remove_orig = T)
  #   mother_data_split <- time_value_split(var = "pn21b", data_set = mother_data_split, remove_orig = T)
  # }
  
  pn21a_R <- as.numeric(as.character(mother_data$pn21a_R))
  pn21a_I <- as.numeric(as.character(mother_data$pn21a_I))
  pn21a_logic <- (pn21a_R == 1) | (pn21a_R == 2 & pn21a_I <= 2)
  
  pn21b_R <- as.numeric(as.character(mother_data$pn21b_R))
  pn21b_I <- as.numeric(as.character(mother_data$pn21b_I))
  pn21b_logic <- (pn21b_R == 1) | (pn21b_R == 2 & pn21b_I <= 2)
  
  hck_moth_num <-  sum(pn21a_logic, na.rm = T) + sum(pn21b_logic, na.rm = T)
  hck_moth <- hck_moth_num/ nrow(mother_data)
  
  if (type == "district"){
    mother_data <- mother_data[!(is.na(mother_data$hh1)), ]
    mother_data$district <- as.character(cluster_districts$NAME.OF.DISTRICT[as.integer(as.character(mother_data$hh1))])
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


compute_immun_indic<- function(data, var1, var2, type = "global"){
  
  dates_text <- data[, var1]
  dates <- as.Date(data[, var1],format = "%d-%m-%Y")
  
  birth_date <- as.Date(data[, "ag1"], format = "%d-%m-%Y")  
  
  interv <- interval(start = birth_date, end = dates) /
    duration(num = 1, units = "months")
  
  card <- (interv < 12)
  card[is.na(card)] <- FALSE
  
  card_no_date <- grepl(pattern = "44", x = dates_text)
  card_no_date[is.na(card_no_date)] <- FALSE
  
  if (var2 %in% c("im10", "im12")){
    recall <- (data[, var2] >= 3 & data[, var2] < 8)
    recall[is.na(recall)] <- FALSE
    
  }
  else {
    recall <- (data[, var2] == 1)
    recall[is.na(recall)] <- FALSE
  }
  
  logic <- Reduce("|", list(card, card_no_date, recall))
  ind <- sum(logic)/ nrow(data)
  
  return(ind)
}

compute_NT_indic <- function(data){
  NT_logic <- ((data[, "mn6"] == 1) & (data[, "mn7"] >= 2) & (data[, "mn7"] < 8 ))
  NT_indic <- sum(NT_logic, na.rm = T)/ nrow(data)
  return(NT_indic)
}

compute_immun_district <- function(mother_data, child_data) {
  mother_data <- remove_empty_rows(mother_data)
  child_data <- remove_empty_rows(child_data)
  
  mother_data$district <- as.character(cluster_districts$NAME.OF.DISTRICT[as.numeric(mother_data$hh1)])
  child_data$district <- as.character(cluster_districts$NAME.OF.DISTRICT[as.numeric(child_data$hh1)])
  
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
  all_vacc_no <- as.character(child_data$im4) == "No"
  
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
  
  ## returning a list of three inconsistency tables
  return(list(card_seen_ND = card_seen_ND,
              all_Dates_WS = all_Dates_No,
              all_but_BCG = all_but_BCG))
}
# 

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





