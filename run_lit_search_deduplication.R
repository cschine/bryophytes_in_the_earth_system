# This script runs the functions to read in literature search results
# remove duplicates and output a final version of literature search results 
# without duplicates

#### Read in the functions ####
source("lit_search_check_for_matches_output_table.R")

############# Run the Functions ##############
# create file list for target directory 
target_dir <- "./lit_search_results/test_more/"

# create list of all search results in target directory
#search_results_list <- read_search_results_from_directory(target_dir, output="list")

# create tibble of all search results in target directory
search_results_all_tbl <- read_search_results_from_directory(
  target_dir, output="tibble", RecordID=TRUE, begin_record_num=2000)

# create a tibble with target recordID and record IDs for potential matches
result_match_tbl <- create_string_match_tbl_for_results(search_results_all_tbl,
                                                        threshold=10,
                                                        max_matches=24)

write.csv(result_match_tbl, "./lit_search_results/moss_batch_initial_match_tbl.csv")

confirmed_match_tbl <- user_comfirmation_of_matches(result_match_tbl, search_results_all_tbl)

deduplicated_results_tbl <- remove_duplicate_records()
