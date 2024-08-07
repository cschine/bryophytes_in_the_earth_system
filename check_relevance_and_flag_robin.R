#Robin tries it on her own!
library(stringr) #to trim file name strings
library(tidyverse)
library(stringdist) #for fuzzy string matching

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

### data frame containing flag definitions
flags <- read.csv("flag_descriptions.csv")
flag_values<- c(flags$FlagValue)

#function to take command line input on record matches
take_user_input <- function (prompt = "How relevant is this title? (input flag #)") {
  # Prompt the user and read input from command line
  input <- readline (prompt)
  if (input!= 1.1 && input!= 1.2) {
    cat(paste(input," is not a valid response\n"))
    cat("Enter valid flag #")
    input <- readline(prompt)
  } else {
    cat(paste("You entered: ", input, "\n", 
              "Meaning:", flags$Description[input]))
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

#function to take user input on relevance and output tibble with flag values for relevance
user_flagging <- function(cull_tbl) {
  
  #create new tbl with flag column added
  flagged_tbl <- cull_tbl |> 
    add_column(Relevance_Flag = NA)
  
  # loop though each target record
  for (i in 1:nrow(cull_tbl)){
    # get target record information from search result tibble
    target_record_tbl <-cull_tbl %>% slice(i)
    
    #print total number of records so we know what we're in for
    cat(paste("Record # ", i, " of ", nrow(cull_tbl),"\n"))
  
    #send target record information to the console
    cat("......................\n")
    cat(paste ("Record ", i, " Record:\n"))
    relevance_command_line_text <- assemble_record_output(target_record_tbl)
    format_text_for_console(relevance_command_line_text)
    cat("=======================\n")
    
    #store flag value in relevance flag column
    flag_value <- take_user_input()
    flagged_tbl$Relevance_Flag[i] <- flag_value
  }
  #return the data with flags assigned
  return(flagged_tbl)
}


######CALL FUNCTIONS#######

#set path to desired file
directory_path <- "./lit_search_results/batch_deduplicated_result_tbls/"
file_name <- "biocrust_batch_deduplicated_result_tbl.csv"
target_batch <- read.csv(paste(directory_path, file_name, sep=""))

#smaller batch to test functionality
test_batch <- target_batch |> slice_head(n = 10)

#run the function
culled_batch <- user_flagging(target_batch)



