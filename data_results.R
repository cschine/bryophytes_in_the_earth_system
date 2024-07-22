###### DE-DUPE DATA FOR POSTER #####

library(stringr) #to trim file name strings
library(tidyverse) #for everything
library(stringdist) #for fuzzy string matching
library(beepr) #for beeping to notify user of function requiring input



#establish file paths
initial_file_path <- "./lit_search_results/batch_initial_match_tbls/"
deduplicated_file_path <- "./lit_search_results/batch_deduplicated_result_tbls/"

#list names
initial_file_names <- list.files(initial_file_path)
deduplicated_file_names <-list.files(deduplicated_file_path)

# create tbl for data
data_tbl <- tribble(
  ~"batch", ~"initial # of entries", ~"de-duplicated # of entries"
)



for(i in 1:length(initial_file_names)) {
  #split name strings and select number 1
  batch_name <- str_split_i(initial_file_names[i], "_", 1)
  batch_name_check <- str_split_i(deduplicated_file_names[i], "_", 1)
  
 #check that names match
  print(paste(batch_name, batch_name_check))
  
  #establish path to target file (one batch)
  target_initial_file_path <- (paste(initial_file_path, initial_file_names[i], sep=""))
  target_deduplicated_file_path <- (paste(deduplicated_file_path, deduplicated_file_names[i], sep=""))
 
  #call .csv of target batch file
  initial_file <- read.csv(target_initial_file_path) 
  deduplicated_file <- read.csv(target_deduplicated_file_path)
  
  #read.csv to read in initial file
  initial_row_number <- nrow(initial_file[i]) 
  deduplicated_row_number <- nrow(deduplicated_file[i])
  #check row numbers
  print(paste(initial_row_number, deduplicated_row_number))
  
  add_row (data_tbl, 
          batch = batch_name, 
          `initial # of entries`= initial_row_number, 
          `de-duplicated # of entries`= deduplicated_row_number )
}



