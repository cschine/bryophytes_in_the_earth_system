#Robin tries it on her own!
library(stringr) #to trim file name strings
library(tidyverse)

########FUNCTIONS#########


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
take_user_input <- function (prompt = "Is this title relevant? (y/n)") {
  # Prompt the user and read input from command line
  input <- readline (prompt)
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

# function to format text for console output
format_text_for_console <- function(text_vector_) {
  # Print each line of text with a prefix for clarity
  for (line in text_vector_) {
    cat("  > ", line, "\n")
  }
}

#function to take command line input on record matches
take_user_input_flag <- function (prompt = "Reason for removal (1/2/3/4)") {
  # Prompt the user and read input from command line
  input_flag <- readline (prompt)
  if (input_flag!="1" & input_flag!="2" & input_flag!="3" & input_flag!="4") {
    cat(paste(input_flag," is not a valid response\n"))
    cat("Enter 1, 2, 3, or 4 \n")
    input <- readline(prompt)
  } else {
    cat(paste("You entered: ", input_flag, "\n"))
    # Return the input
    return(input_flag)
  }
}

#function to take user input on relevance and output tibble of confirmed relevance
#input tibbles must match the format output by the previous 2 functions
user_confirmation_of_relevance <- function(file_path) {
  
  #read in .csv with deduplicated search result records
  cull_tbl <- tibble(read.csv(file_path))
  
  #create empty list to fill with confirmed_relevant_tbl and confirmed_irrelevant_tbl
  output_list <- list()
  
  #create new match tbl with only confirmed matches
  confirmed_relevant_tbl <- cull_tbl
  #create new match tbl for Irrelevant searches
  confirmed_irrelevant_tbl <- cull_tbl |> 
    add_column(Removal_Flag = NA)
  
  # loop though each target record
  for (i in 1:nrow(cull_tbl)){
    # get target record information from search result tibble
    target_record_tbl <-cull_tbl %>% slice(i)
    
    #send target record information to the console
    cat("......................\n")
    cat(paste("Record # ", i, " of ", nrow(cull_tbl),":\n"))
    relevance_command_line_text <- assemble_record_output(target_record_tbl)
    format_text_for_console(relevance_command_line_text)
    cat("=======================\n")
    
    #Sys.sleep(0.5) #add pause so it doesn't hurt my brain
    input_value <- take_user_input()
    
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
  output_list <- list(na.omit(confirmed_relevant_tbl),
                         na.omit(confirmed_irrelevant_tbl))
  #return the list of both tbls
  return(output_list)
}

######CALL FUNCTIONS#######

#locate data
target_dir <- "./lit_search_results/moss_batch_confirmed_match_tbl/"

# create tibble of all search results in target directory
search_results_all_tbl <- read_search_results_from_directory(
  target_dir, output="tibble", RecordID=TRUE)

#make a new tbl of just the first 10 rows of a search result tbl
tiny_tbl <- search_results_all_tbl |> 
  slice_head(n = 50) 

tiny_tbl_row <- tiny_tbl |> 
  slice(1)

confirmed_relevant_tbl <- user_confirmation_of_relevance(tiny_tbl)



