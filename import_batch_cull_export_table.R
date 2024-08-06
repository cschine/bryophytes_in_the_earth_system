#script to read in deduplicated results table
# send each reference to the console for user flagging based on title
# output final table with flag results

library(stringr) #to trim file name strings
library(tidyverse)

########FUNCTIONS#########


### sub functions to call in match checking function
# function for assembling lines of text from a record
assemble_record_output <- function(input_tbl_row) {
  database_string_split <- strsplit(input_tbl_row$Database,",")
  search_num <- length(database_string_split[[1]])
  record_text <- c(paste("Title: ",input_tbl_row$Title),
                   paste("Author(s): ",input_tbl_row$Authors),
                   paste("Year: ", input_tbl_row$Year),
                   paste("Source: ", input_tbl_row$Source),
                   paste("Searches: ", search_num))
  return(record_text)
}

# function to format text for console output
format_text_for_console <- function(text_vector_) {
  # Print each line of text with a prefix for clarity
  for (line in text_vector_) {
    cat("  > ", line, "\n")
  }
}

`%ni%` <- Negate(`%in%`) #opposite of in

#function to take command line input on record matches
take_user_input_flag <- function(prompt = "Flag value (1/2/3/4/5/6)") {
  valid_input_flags <-c(1,1.1,1.2,1.3,2,3,3.1,3.2,3.3,3.4,4,5,6)
  # Prompt the user and read input from command line
  input_flag <- readline(prompt)
  while (input_flag %ni% valid_input_flags) {
    cat(paste(input_flag," is not a valid response\n"))
    cat("Enter 1/2/3/4/5/6")
    input_flag <- readline(prompt)
  } 
    
  cat(paste("You entered: ", input_flag, "\n"))
  # Return the input
  return(input_flag)

}

#function to take user input on relevance and output tibble of confirmed relevance
#input tibbles must match the format output by the previous 2 functions
user_flag_input <- function(file_path, output_filename) {
  
  # read in .csv with deduplicated search result records
  # two columns with row numbers have been appended to the beginning of
  # the table, the select command removes those 
  dedup_tbl <- tibble(read.csv(file_path)) %>% select(3:15)
  
  # check to see if the output file already exists
  # if the file does not exist create a file with just the headers
  if (!file.exists(output_filename)) {
    dedup_colnames <- colnames(dedup_tbl)
    flag_colnames <- c(dedup_colnames, "rel_flag")
    
    flag_tbl <- as_tibble(matrix(nrow = 0, ncol = length(flag_colnames)),
                          .name_repair = ~ flag_colnames)
    write_csv(flag_tbl, output_filename)
  } 
  
  # read in the output file
  flag_output_tbl <- read.csv(output_filename, header=TRUE)
  # extract the starting point from the output file
  start_ind <- nrow(flag_output_tbl) + 1
  
  # loop though each target record
  for (i in start_ind:nrow(dedup_tbl)){
    # get target record information from search result tibble
    target_record_tbl <-dedup_tbl %>% slice(i)
    
    #send target record information to the console
    cat("......................\n")
    cat(paste("Record # ", i, " of ", nrow(dedup_tbl),":\n"))
    relevance_command_line_text <- assemble_record_output(target_record_tbl)
    format_text_for_console(relevance_command_line_text)
    cat("=======================\n")
    
    # take user input for flag value
    flag_value <- take_user_input_flag()
    
    # assemble the output line to add to the table
    output_line <- target_record_tbl %>% bind_cols(rel_flag=flag_value)
    
    # add current record information to existing output file
    write_csv(output_line, output_filename, append=TRUE)
  }

}

######CALL FUNCTIONS#######

#locate data
all_dedup_path <- "./lit_search_results/batch_deduplicated_result_tbls/all_deduplicated_result_tbl.csv"
all_dedup_tbl <- tibble(read.csv(all_dedup_path)) %>% select(3:15)

#
slice_1 <- all_dedup_tbl %>% slice(1:7425)
slice_2 <- all_dedup_tbl %>% slice(7426:14851)
slice_3 <- all_dedup_tbl %>% slice(14852:22276)
slice_4 <- all_dedup_tbl %>% slice(22277:29702)
slice_5 <- all_dedup_tbl %>% slice(29703:37126)

test_output_name <- "./lit_search_results/flag_tbls/test.csv"

user_flag_input(all_dedup_path,test_output_name)


