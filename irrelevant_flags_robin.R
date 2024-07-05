#Robin tries it on her own!
library(stringr) #to trim file name strings
library(tidyverse)
library(stringdist) #for fuzzy string matching
#locate data
target_dir <- "./lit_search_results/test_more/"

### Function that takes in directory with .csv files from individual searches
#from PoP and outputs either a tibble or a list of all the results
#the recordID option is only available for a tibble
read_search_results_from_directory <- function(directory_string, 
                                               output=c("tibble", "list"),
                                               RecordID=TRUE) {
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
      result_tbl_all <- result_tbl_all %>% 
        add_column(RecordID=seq(from=1, to=nrow(result_tbl_all)))
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

# create list of all search results in target directory
search_results_list <- read_search_results_from_directory(target_dir, output="list")

# create tibble of all search results in target directory
search_results_all_tbl <- read_search_results_from_directory(
  target_dir, output="tibble", RecordID=TRUE)

#make a new tbl of just the first 10 rows of a search result tbl
tiny_tbl <- search_results_all_tbl |> 
  slice_head(n = 10) 

tiny_tbl_row <- tiny_tbl |> 
  slice(1)

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

#function to take command line input on record matches
take_user_input_yn <- function (prompt = "Is this title relevant? (y/n)") {
  # Prompt the user and read input from command line
  input_yn <- readline (prompt)
  if (input_yn!="y" & input_yn!="n") {
    cat(paste(input_yn," is not a valid response\n"))
    cat("Enter either y or n \n")
    input <- readline(prompt)
  } else {
    cat(paste("You entered: ", input_yn, "\n"))
    # Return the input
    return(input_yn)
  }
}


#function to take command line input on record matches
take_user_input_flag <- function (prompt = "Reason for removal (1/2/3)") {
  # Prompt the user and read input from command line
  input_flag <- readline (prompt)
  if (input_flag!="1" & input_flag!="2" & input_flag!="3") {
    cat(paste(input_flag," is not a valid response\n"))
    cat("Enter either 1, 2, or 3 \n")
    input <- readline(prompt)
  } else {
    cat(paste("You entered: ", input_flag, "\n"))
    # Return the input
    return(input_flag)
  }
}

# function to format text for console output
format_text_for_console <- function(text_vector_) {
  # Print each line of text with a prefix for clarity
  for (line in text_vector_) {
    cat("  > ", line, "\n")
  }
}


#function to take user input on relevance and output tibble of confirmed relevance
user_confirmation_of_relevance <- function(tiny_tbl) {
  
  #create empty list to fill with confirmed_relevant_tbl and confirmed_irrelevant_tbl
  confirmed_list <- list()
  
  #create new tbl for relevant searches
  confirmed_relevant_tbl <- tiny_tbl
  #create new match tbl for IRrelevant searches
  confirmed_irrelevant_tbl <- tiny_tbl |> 
    add_column(Removal_Flag = NA)
  
  # loop though each target record
  for (i in 1:nrow(tiny_tbl)){
    # get target record information from search result tibble
    target_record_tbl <-tiny_tbl %>% slice(i)
    
    #send target record information to the console
    cat("......................\n")
    cat(paste ("Record ", " Record:\n"))
    relevance_command_line_text <- assemble_record_output(target_record_tbl)
    format_text_for_console(relevance_command_line_text)
    cat("=======================\n")
    
    Sys.sleep(0.5) #add pause so it doesn't hurt my brain
    input_value <- take_user_input_yn()
    
    #if the user confirms that something isn't relevant, set that to NA in 
    #relevant-tbl HOW to specify input
    if (input_value=="n") {
      flag_value <- take_user_input_flag()
      confirmed_irrelevant_tbl$Removal_Flag[i] <- flag_value
      confirmed_relevant_tbl[i,] <- NA
    } else if (input_value=="y") {
      confirmed_irrelevant_tbl[i,] <- NA
    }
  }
   #create list
  confirmed_list <- list(na.omit(confirmed_relevant_tbl),
                         na.omit(confirmed_irrelevant_tbl))
  #return the list of both tbls
  return(confirmed_list)
}

#change this to confirmed_list? can I do a function on a list like that?
confirmed_list <- user_confirmation_of_relevance(tiny_tbl)
