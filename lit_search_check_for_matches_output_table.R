# script to read in literature search results and check for duplicates


library(stringr) #to trim file name strings
library(tidyverse)
library(stringdist) #for fuzzy string matching

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
    
    #extract name of database from name of file (first 6 characters)
    result_source <- str_sub(base_file_name, start=0, end=6)

    #depending on database format results all to the same format
    #need to get the correct format from PoP for Scopus and Google Scholar
    if (result_source=="google") {
      print("Database = Google Scholar")
      result_tbl_mod <- result_tbl %>% 
        select(Authors, Title, Year, Source, ArticleURL, Type, DOI)
    } else if (result_source=="scopus") {
      print("Database = Scopus")
      
    } else if (result_source=="wofsci") {
      print("Database = Web of Science")
      # add WoS details if I get access
    } else { 
      print("database string is not valid (first 6 characters of file name)")
      print(paste("for file: ", base_file_name))
      stop("function has been aborted")
    }
    
    #select and rename columns to match other search results
    # Google_Scholar
    #   Authors, Title, Year, Source, ArticleURL, Type, DOI
    # Scopus
    #   Authors, Title, Year, Source.title, Link, DOI
  
    
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


#####
#combine all search results into a single table after dealing with
# column naming inconsistencies
#combine df by dropping columns you don't want and standardizing names of 
# columns you do want and then smash them all together
# add this to function that reads in the data

# assign record ID to each row


# Function to check for matches within each set of results
# takes in tibble
check_for_string_matches_within_search <- function(search_result_tbl) {
  for (i in 1:nrow(search_result_tbl)) {
    #define target record and select entire row
    target_record_tbl <-search_result_tbl %>% slice(i)
    
    #define 2nd tibble with all record except target record
    drop <- i
    non_target_record_tbl <- search_result_tbl %>% filter(!row_number() %in% drop)
    
    #separate out title fields only
  }
}

#output matches to new data frame
#probably assign record id to each record and keep track of matches that way
#add column with potential duplicate(s) for each record

test <- amatch(target_record_tbl$Title, search_result_tbl$Title, 
               maxDist=10)

agrep(target_record_tbl$Title, search_result_tbl$Title, 
      max.distance = 10, value = TRUE) #returns name

agrep(target_record_tbl$Title, search_result_tbl$Title, max.distance = 10) #returns index value

## loop through titles comparing each title to all other titles in turn
## set match threshold
## have any match that exceeds threshold sent to a command line prompt where the user
##     indicates whether it should be treated as a match or ignored



# still need to figure out how to deal with creating the next tibble
#   where all matches have been removed and all searches that have identified
#   a given record are recorded.

