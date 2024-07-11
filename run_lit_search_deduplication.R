# This script runs the functions to read in literature search results
# remove duplicates and output a final version of literature search results 
# without duplicates

####### MOSS ########

#### Read in the functions ####
source("lit_search_check_for_matches_output_table.R")

############# Run the Functions ##############
# create file list for target directory 
target_dir <- "./lit_search_results/moss_batch/"

# create list of all search results in target directory
#search_results_list <- read_search_results_from_directory(target_dir, output="list")

# create tibble of all search results in target directory
moss_search_results_all_tbl <- read_search_results_from_directory(
  target_dir, output="tibble", RecordID=TRUE, begin_record_num=1)

# create a tibble with target recordID and record IDs for potential matches
moss_result_match_tbl <- create_string_match_tbl_for_results(moss_search_results_all_tbl,
                                                        threshold=10,
                                                        max_matches=40)

write.csv(moss_result_match_tbl, "./lit_search_results/moss_batch_initial_match_tbl.csv")

moss_confirmed_match_tbl <- user_comfirmation_of_matches(moss_result_match_tbl, moss_search_results_all_tbl)

write.csv(moss_confirmed_match_tbl, "./lit_search_results/moss_batch_confirmed_match_tbl.csv")

moss_deduplicated_results_tbl <- remove_duplicate_records(moss_confirmed_match_tbl,moss_search_results_all_tbl)

write.csv(moss_deduplicated_results_tbl[[1]], "./lit_search_results/moss_batch_deduplicated_result_tbl.csv")

####### BRYOPHYTE ########

#### Read in the functions ####
source("lit_search_check_for_matches_output_table.R")

############# Run the Functions ##############
# create file list for target directory 
target_dir <- "./lit_search_results/bryophyte_batch/"

# create list of all search results in target directory
#search_results_list <- read_search_results_from_directory(target_dir, output="list")

# create tibble of all search results in target directory
bryophyte_search_results_all_tbl <- read_search_results_from_directory(
  target_dir, output="tibble", RecordID=TRUE, begin_record_num=1)

# create a tibble with target recordID and record IDs for potential matches
bryophyte_result_match_tbl <- create_string_match_tbl_for_results(bryophyte_search_results_all_tbl,
                                                             threshold=10,
                                                             max_matches=40)

write.csv(bryophyte_result_match_tbl, "./lit_search_results/bryophyte_batch_initial_match_tbl.csv")

bryophyte_confirmed_match_tbl <- user_comfirmation_of_matches(bryophyte_result_match_tbl, bryophyte_search_results_all_tbl)

write.csv(bryophyte_confirmed_match_tbl, "./lit_search_results/bryophyte_batch_confirmed_match_tbl.csv")

bryophyte_deduplicated_results_tbl <- remove_duplicate_records(bryophyte_confirmed_match_tbl,bryophyte_search_results_all_tbl)

write.csv(bryophyte_deduplicated_results_tbl[[1]], "./lit_search_results/bryophyte_batch_deduplicated_result_tbl.csv")

