# script to read in literature search results and check for duplicates
# this file contains all of the functions developed to read in the 
# literature search results and remove duplicate records
# the final function outputs a table with a single record containing
# the search information for all duplicates

library(stringr) #to trim file name strings
library(tidyverse) #for everything
library(stringdist) #for fuzzy string matching
library(beepr) #for beeping to notify user of function requiring input

####################### FUNCTIONS ##############################

### Function that takes in directory with .csv files from individual searches
#from PoP and outputs either a tibble or a list of all the results
#the recordID option is only available for a tibble
read_search_results_from_directory <- function(directory_string, 
                                               output=c("tibble", "list"),
                                               RecordID=TRUE,
                                               begin_record_num=1) {
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
    
    #split file name string into parts to use 
    file_name_string_parts <- str_split_1(result_name_vector[i], "_")

    #add column with database name
    result_source <- file_name_string_parts[1]
    if (result_source=="google"| result_source== "scopus"| result_source== "wofsci") {
      print(paste("Database = ", result_source))
      database_col <- rep(result_source, length=nrow(result_tbl))
    } else { 
      print("database string is not valid (first 6 characters of file name)")
      print(paste("for file: ", base_file_name))
      stop("function has been aborted")
    }
   
    #create vector for search result column
    search_terms_vector <- c(file_name_string_parts[2], file_name_string_parts[4])
    search_terms_string <- paste(search_terms_vector, collapse="--")
    search_terms_col <- rep(search_terms_string, length=nrow(result_tbl))
    
    # modify table with selected columns and added source and search information
    result_tbl_mod <- result_tbl %>% 
      select(Authors, Title, Year, Source, ArticleURL, 
             Type, DOI, Cites, GSRank, QueryDate) %>%
      add_column(Database=database_col, SearchTerms=search_terms_col)
    
    #place result table into list
    lit_results_list[[i]] <- result_tbl_mod
  }
  
  if (output=="tibble") {
    #combine lists into a tibble
    result_tbl_all <- bind_rows(lit_results_list)
    #add recordID column if variable set to TRUE
    if (RecordID==TRUE) {
      
      number_of_records <- nrow(result_tbl_all)
      end_record_num <- begin_record_num + number_of_records - 1
      
      result_tbl_all <- result_tbl_all %>% 
        add_column(RecordID=seq(from=begin_record_num, to=end_record_num))
    }
    #return tibble
    return(result_tbl_all)
  } else {
    #name individual list elements with file name for each set of results
    names(lit_results_list) <- result_name_vector
    #return the populated list
    return(lit_results_list)
  }
}

### Function to check for matches within each set of results, takes in tibble
create_string_match_tbl_for_results <- function(search_result_tbl,
                                                threshold=5,
                                                max_matches=10){
  
  #create empty tibble for output data for target record and potential matches
  num_rows <- nrow(search_result_tbl) #get number of rows for output tbl
  num_cols <- max_matches+1
  # Create a matrix filled with NA
  na_matrix <- matrix(NA, nrow = num_rows, ncol = num_cols)
  # Convert the matrix to a tibble
  match_output_tbl <- as_tibble(na_matrix, .name_repair="minimal")
  
  # create column names based on input value for number of matches
  column_names <- c("target_record",paste0("match_", 1:max_matches))
  names(match_output_tbl) <- column_names
  
  for (i in 1:nrow(search_result_tbl)) {
    #print(i)
    
    cat(paste(i, " in ", nrow(search_result_tbl), "\n", sep=""))
    #define target record and select entire row
    target_record_tbl <-search_result_tbl %>% slice(i)
    target_recordID <- target_record_tbl$RecordID
    
    #define 2nd tibble with target record fields set to NA 
    #   so we don't get a record matching to itself
    non_target_record_tbl <- search_result_tbl
    non_target_record_tbl[i,] <- rep(NA,ncol(non_target_record_tbl))
    
    # search for matches using adist() Fuzzy Matching, part of Base R package
    # uses generalized Lehvenshtein edit distance
    # identify results that are below the threshold value
    result_match_ind <- which(adist(target_record_tbl$Title, 
                                  non_target_record_tbl$Title)<=threshold)
    # use the index values to retrieve the record IDs
    result_matches <- as_vector(non_target_record_tbl$RecordID[result_match_ind])
    
    # error message if there isn't enough space in the output tbl to record all
    # of the matches. 
    if (length(result_matches)>max_matches){
      print(paste("The number of matches for Record #", i,
                  "is", length(result_matches), sep=" "))
      print(paste("exceeds the maximum number of matches of",
                  max_matches,sep=" "))
      beep(sound="wilhelm")
      stop("function has been aborted")
    }
    
    #See if a match has already been identified
    if (length(result_matches) > 0) {
      # identify lowest record ID in matches
      minimum_match_ID <- min(result_matches)
      if (minimum_match_ID < target_recordID) {
        # if the minimum record ID in matches is lower than the target record
        #then look to see if there are any differences between the match sets identified
        min_match_ind<- which(match_output_tbl$target_record==minimum_match_ID)
        min_match_matches_vec <- as_vector(match_output_tbl[min_match_ind,])
        target_and_matches_vec <- c(target_recordID, result_matches)
        new_matches <- setdiff(target_and_matches_vec, min_match_matches_vec)
        result_matches <- new_matches
      }
    } 
    
    # create output vector to put in output tbl
    # create vector of NAs of uniform length
    output_vec <- rep(NA,num_cols)
    # create vector of values (probably not same length as empty vector)
    output_vals <- c(target_recordID, result_matches)
    # add values to NA vector  
    output_vec[1:length(output_vals)] <- output_vals
      
    #add vector to correct spot in output tibble
    match_output_tbl[i,] <- t(output_vec) #for some reason this needs to be transposed
    
  }
  beep(sound="coin")
  return(match_output_tbl)
}



### sub functions to call in match checking function
# function for assembling lines of text from a record
assemble_record_output <- function(input_tbl_row) {
  record_text <- c(paste("Title: ",input_tbl_row$Title),
                   paste("Author(s): ",input_tbl_row$Authors),
                   paste("Year: ", input_tbl_row$Year),
                   paste("Source: ", input_tbl_row$Source),
                   paste("Databse: ", input_tbl_row$Database),
                   paste("Search Terms: ", input_tbl_row$SearchTerms))
  return(record_text)
}

# function to format text for console output
format_text_for_console <- function(text_vector_) {
  # Print each line of text with a prefix for clarity
  for (line in text_vector_) {
    cat("  > ", line, "\n")
    }
}

#function to take command line input on record matches
take_user_input <- function(prompt="Is this a match to the Target Record? (y/n)") {
  # Prompt the user and read input from command line
  input <- readline(prompt)
  if (input!="y" & input!="n") {
    cat(paste(input," is not a valid response\n"))
    cat("Enter either y or n \n")
    input <- readline(prompt)
  } else {
    cat(paste("You entered: ", input, "\n"))
    # Return the input
    return(input)
  }
}


#function to take user input on matches and output tibble of confirmed matches
#input tibbles must match the format output by the previous 2 functions
user_comfirmation_of_matches <- function(match_tbl, record_tbl) {
  
  #create new match tbl with only confirmed matches
  confirmed_match_tbl <- match_tbl
  
  #create error and abort function if number of matches and 
  # number of records are not the same
  if(nrow(match_tbl)!=nrow(record_tbl)){
    error_text <- c(paste("Number of matched records", nrow(match_tbl)),
                paste("is different than number of records", nrow(record_tbl)))
    format_text_for_console(error_text)
    stop("function has been aborted")
  }
  
  
  # loop though each target record
  for (i in 1:nrow(match_tbl)){
  #for (i in 11:15) {
    # get target record information from search result tibble
    target_record_id <- match_tbl$target_record[i]
    target_record_tbl <-record_tbl %>% filter(RecordID==target_record_id)
    #parse the title string and determine length to use later
    target_title_length <- length(str_split_1(target_record_tbl$Title," "))
    #get vector of match RecordIDs for target record
    match_record_id_vec <- as.vector(match_tbl[i,2:ncol(match_tbl)])
    #remove NAs and get total number of matches to use in match for loop
    match_record_num <- length(match_record_id_vec[!is.na(match_record_id_vec)])
    
    cat(paste("Record # ", i, " of ", nrow(match_tbl),"\n"))
    
    #if statement for number of matches==0 and number of matches>0
    if(match_record_num==0){
      #Sys.sleep(0.5) #add pause so it doesn't hurt my brain
      #Send text for no matching records to the console
      cat("......................\n")
      cat("No matching records found\n")
      cat("=======================\n")
      #Sys.sleep(0.5) #add pause so it doesn't hurt my brain
    } else {
      #loop through all matches
      for (j in 1:match_record_num) {
        #get match record information from search result tibble
        match_record_id <- match_record_id_vec[j]
        match_record_tbl <-record_tbl %>% filter(RecordID==match_record_id)
        
        diss_dist <- adist(target_record_tbl$Title, match_record_tbl$Title)

        # conditional to deal with a few potential problems
        if (match_record_id==target_record_id){
          # the match_record_id and target_record_id should never be the same 
          # sends error to console
          cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")
          error_text <- c(paste("Target Record ID ", target_record_id, 
                                " and Match Record ID ", match_record_id, " are the same."),
                          paste("This should not happen. Go find the problem"))
          format_text_for_console(error_text)
          stop("function has been aborted")
        } else if (diss_dist==0 & target_title_length>4) {
          #If the titles of the target and the match are identical, no evaluation needed
          #print to the console and move on without any changes to the match_tbl
          #Sys.sleep(0.5) #add pause so it doesn't hurt my brain
          cat("=======================\n")
          cat("EXACT MATCH. YAY!!!\n")
          cat("......................\n")
          cat("Target Record Title:\n")
          format_text_for_console(paste(target_record_tbl$Title,"\n"))
          cat(paste("Is an exact match for Match Record ",j, " Title:\n"))
          format_text_for_console(paste(match_record_tbl$Title,"\n"))
          cat("Match is retained. No user input needed.\n")
          cat("=======================\n")
          
          #Sys.sleep(0.5) #add pause so it doesn't hurt my brain
        } else {
          #send target record information to the console
          #Sys.sleep(0.5) #add pause so it doesn't hurt my brain
          #beep(sound=1)
          cat("=======================\n")
          cat("Target Record:\n")
          target_command_line_text <- assemble_record_output(target_record_tbl)
          format_text_for_console(target_command_line_text)
          #Sys.sleep(0.5) #add pause so it doesn't hurt my brain
          
          #send match record information to the console
          cat("......................\n")
          cat(paste("Match ", j, " Record:\n"))
          match_command_line_text <- assemble_record_output(match_record_tbl)
          format_text_for_console(match_command_line_text)
          cat("=======================\n")
          
          #Sys.sleep(0.5) #add pause so it doesn't hurt my brain
          input_value <- take_user_input()
          
          #if the user says that something isn't a match, set that to NA in 
          #match output table
          if (input_value=="n") {
            confirmed_match_tbl[i,j+1] <- NA
          }
        }
        #Sys.sleep(0.75) #add pause so it doesn't hurt my brain
      }  
    }
  }
  return(confirmed_match_tbl)
}


#function to take input record tibble and match tibble 
# output tibble with no duplicates and target record has been amended with 
# information about deleted records

#needed: need to deal with records that have already been evaluated as duplicates
# where record number is higher than match
remove_duplicate_records <- function(match_tbl, record_tbl){
  #create empty tibble with correct column names for output of records
  keep_tbl <- tibble()
  duplicate_vec <- vector()
    
  #create error and abort function if number of matches and 
  # number of records are not the same
  if(nrow(match_tbl)!=nrow(record_tbl)){
    error_text <- c(paste("Number of matched records", nrow(match_tbl)),
                    paste("is different than number of records", nrow(record_tbl)))
    format_text_for_console(error_text)
    stop("function has been aborted")
  }
  
  # loop though each target record
  #for (i in 1:nrow(match_tbl)){
  for (i in 1:10) {
    
    target_record_num <- record_tbl$RecordID[i]
    
    # check to see if target record has already been evaluated as a match
    already_matched <- target_record_num %in% duplicate_vec
    
    # if the target record has already been evaluated as a match
    # then print out to the console and move on to the next record
    if (already_matched) {
      
      cat(paste("Record # ", i, " has alread been evaluated as a match \n"))
      cat(paste(">>> Record has been recorded as duplicate and removed"))
      
    } else {
      # get target record information from search result tibble
      target_record_id <- match_tbl$target_record[i]
      target_record_tbl <-record_tbl %>% filter(RecordID==target_record_id)
      
      #get vector of match RecordIDs for target record
      match_record_id_vec <- as.vector(match_tbl[i,2:ncol(match_tbl)])
      
      #remove NAs and get total number of matches to use in match for loop
      match_record_num <- length(match_record_id_vec[!is.na(match_record_id_vec)])
      
      #if statement so that we handle records with no matches
      # and records with matches differently
      if(match_record_num==0){
        # if there are no matches output the original target record unchanged
        output_tbl_line <- target_record_tbl
      } else {
        output_tbl_line <- target_record_tbl
        
        # add matches to vector of duplicates that have already been evaluated and removed
        match_vec_narm <- as_vector(match_record_id_vec[!is.na(match_record_id_vec)])
        duplicate_vec <- c(duplicate_vec,match_vec_narm)
        
        # set new string variables equal to existing target strings
        # will add to these strings in the match for loop
        new_database_str <- target_record_tbl$Database
        new_search_terms_str <- target_record_tbl$SearchTerms
        
        for (j in 1:match_record_num) { #loop through matches
          #amend target record with additional information on search terms and databases
          # number of searches that turned it up
          # remove duplicate record in output_tbl
          
          #get match record information from search result tibble
          match_record_id <- match_record_id_vec[j]
          match_record_tbl <-record_tbl %>% filter(RecordID==match_record_id)
          
          new_database_str <- paste(new_database_str, match_record_tbl$Database, sep=",")
          new_search_terms_str <- paste(new_search_terms_str, match_record_tbl$SearchTerms, sep=",")
          
        }
        #set target record tbl fields with amended strings 
        output_tbl_line$Database <- new_database_str
        output_tbl_line$SearchTerms <- new_search_terms_str
      }
      keep_tbl <- rbind(keep_tbl,output_tbl_line)
    }
  }
  result_list <- list(keep_tibble, duplicate_vec)
  return(result_list)
}
#################################################################################
