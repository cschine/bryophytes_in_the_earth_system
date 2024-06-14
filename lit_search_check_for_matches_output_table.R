# script to read in literature search results and check for duplicates


library(stringr) #to trim file name strings
library(tidyverse)

# read in csv for literature results
# loop through file list in target directory

#need to read each table into a list because can't do responsive naming withing a loop or function

# create file list for target directory 
target_dir <- "./lit_search_results/test_batch/"

read_search_results_into_list_from_directory <- function(directory_string) {
  lit_results_list <- list()
  file_list <- list.files(directory_string)
  
  #create vector of names to apply to list of results
  # by removing .csv at end of file name
  result_name_vector <- str_sub(file_list, end=-5)
  
  for(i in 1:length(file_list)) {
    
    #select target file from file list
    base_file_name <- file_list[i]
    
    #paste target file name together with target directory string for full path
    full_file_path <- paste(directory_string, base_file_name, sep="")

    #read in .csv
    result_tbl <- tibble(read.csv(full_file_path))
    
    #add search result column
    new_result_col_name <- set_names(rep(1, length(result_name_vector[i])), result_name_vector[i])
    result_tbl_mod <- add_column(result_tbl, !!!new_result_col_name)
    
    #place result table into list
    lit_results_list[[i]] <- result_tbl_mod
  }
  
  #name each list element from the shortened file name
  names(lit_results_list) <- result_name_vector
  
  #return the populated list
  return(lit_results_list)
}

search_results_list <- read_search_results_into_list_from_directory(target_dir)


# amend each search result table with a column that has the name of the search file and 
#   a 1 in the column for all of the records that are contained in that tibble
#   this will allow us to keep track of which searches turned up which records
#   and to add up the nuber of searches that returend a given record


# look for matches within each set of search results

# Write function to check for matches
## take in tibble
## separate out all titles
## loop through titles comparing each title to all other titles in turn
## set match threshold
## have any match that exceeds threshold sent to a command line prompt where the user
##     indicates whether it should be treated as a match or ignored



# still need to figure out how to deal with creating the next tibble
#   where all matches have been removed and all searches that have identified
#   a given record are recorded.
