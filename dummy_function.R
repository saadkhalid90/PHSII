## loading libraries
library(dplyr)

## function for separating others indicator variable and text
finding_X <- function(x) {
  if (sum(grepl(pattern = "X", x)) != 0){
    return(unlist(strsplit(sub(" ", ";", x[grepl(pattern = "X", x)]), ";")))
  }
  else {
    return(c(NA, NA))
  }
}
var = "mn30"
dataset <- mother
dataset <- chck
max_alphabet = "I"

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
chck <- create_dummy(var = "mn2", max_alphabet = "F", others = T, DK = F, dataset = mother_hc, remove_orig = T)
chck <- create_dummy(var = "mn17", max_alphabet = "F", others = T, DK = F, No_one = T, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "mn27", max_alphabet = "K", others = T, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "mn30", max_alphabet = "I", others = F, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "mn31", max_alphabet = "H", others = T, DK = F, No_one = F, dataset = chck, others_text = F, remove_orig = T)
chck <- create_dummy(var = "pn13", max_alphabet = "F", others = T, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "pn22", max_alphabet = "F", others = T, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "lm2", max_alphabet = "D", others = T, DK = T, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "lm3", max_alphabet = "C", others = F, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "lm7", max_alphabet = "G", others = T, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "lm8", max_alphabet = "F", others = T, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "lm11", max_alphabet = "D", others = T, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "lm13", max_alphabet = "D", others = T, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "lm15", max_alphabet = "G", others = T, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "lm16", max_alphabet = "F", others = T, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "lm19", max_alphabet = "D", others = T, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "lm21", max_alphabet = "D", others = T, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "lm23", max_alphabet = "H", others = F, DK = F, No_one = F, dataset = chck, remove_orig = T)
chck <- create_dummy(var = "lm25", max_alphabet = "H", others = F, DK = F, No_one = F, dataset = chck, remove_orig = T)

chck <- create_dummy(var = "ws7", max_alphabet = "F", others = T, DK = T, No_one = F, dataset = chck, others_text = T, remove_orig = T)

children <- create_dummy(var = "im17", max_alphabet = "I", others = T, DK = F, No_one = F, dataset = children, others_text = F) 

var = "mn2a"
data_set <- mother

time_value_split <- function(var, data_set, remove_orig = TRUE){
  ## getting the required stringr library
  library(stringr)
  
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

chck <- time_value_split("mn2a", chck)
chck <- time_value_split("mn25", chck)
chck <- time_value_split("pn2", chck)
chck <- time_value_split("pn12a", chck)
chck <- time_value_split("pn12b", chck)
chck <- time_value_split("pn21a", chck)
chck <- time_value_split("pn21b", chck)
chck <- time_value_split("lm24", chck)

write.csv(chck, "hc_mother.csv")
write.csv(child, "child.csv")
