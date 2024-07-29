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

 # moss_confirmed_match_tbl<- read_csv("./lit_search_results/batch_confirmed_match_tbls/moss_batch_confirmed_match_tbl.csv")

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
  target_dir, output="tibble", RecordID=TRUE, begin_record_num=20000)

# create a tibble with target recordID and record IDs for potential matches
bryophyte_result_match_tbl <- create_string_match_tbl_for_results(bryophyte_search_results_all_tbl,
                                                             threshold=10,
                                                             max_matches=40)

write.csv(bryophyte_result_match_tbl, "./lit_search_results/bryophyte_batch_initial_match_tbl.csv")

bryophyte_confirmed_match_tbl <- user_comfirmation_of_matches(bryophyte_result_match_tbl, bryophyte_search_results_all_tbl)

write.csv(bryophyte_confirmed_match_tbl, "./lit_search_results/bryophyte_batch_confirmed_match_tbl.csv")

  #bryophyte_confirmed_match_tbl<- read_csv("./lit_search_results/batch_confirmed_match_tbls/bryophyte_batch_confirmed_match_tbl.csv")

bryophyte_deduplicated_results_tbl <- remove_duplicate_records(bryophyte_confirmed_match_tbl,bryophyte_search_results_all_tbl)

write.csv(bryophyte_deduplicated_results_tbl[[1]], "./lit_search_results/bryophyte_batch_deduplicated_result_tbl.csv")

####### SPHAGNUM ########

#### Read in the functions ####
source("lit_search_check_for_matches_output_table.R")

############# Run the Functions ##############
# create file list for target directory 
target_dir <- "./lit_search_results/sphagnum_batch/"

# create list of all search results in target directory
#search_results_list <- read_search_results_from_directory(target_dir, output="list")

# create tibble of all search results in target directory
sphagnum_search_results_all_tbl <- read_search_results_from_directory(
  target_dir, output="tibble", RecordID=TRUE, begin_record_num=50000)

# create a tibble with target recordID and record IDs for potential matches
sphagnum_result_match_tbl <- create_string_match_tbl_for_results(sphagnum_search_results_all_tbl,
                                                                  threshold=10,
                                                                  max_matches=100)

write.csv(sphagnum_result_match_tbl, "./lit_search_results/sphagnum_batch_initial_match_tbl.csv")

sphagnum_confirmed_match_tbl <- user_comfirmation_of_matches(sphagnum_result_match_tbl, sphagnum_search_results_all_tbl)

write.csv(sphagnum_confirmed_match_tbl, "./lit_search_results/sphagnum_batch_confirmed_match_tbl.csv")

 # sphagnum_confirmed_match_tbl<- read_csv("./lit_search_results/batch_confirmed_match_tbls/sphagnum_batch_confirmed_match_tbl.csv")

sphagnum_deduplicated_results_tbl <- remove_duplicate_records(sphagnum_confirmed_match_tbl,sphagnum_search_results_all_tbl)

write.csv(sphagnum_deduplicated_results_tbl[[1]], "./lit_search_results/sphagnum_batch_deduplicated_result_tbl.csv")


####### PLEUROZIUM ########

#### Read in the functions ####
source("lit_search_check_for_matches_output_table.R")

############# Run the Functions ##############
# create file list for target directory 
target_dir <- "./lit_search_results/pleurozium_batch/"

# create list of all search results in target directory
#search_results_list <- read_search_results_from_directory(target_dir, output="list")

# create tibble of all search results in target directory
pleurozium_search_results_all_tbl <- read_search_results_from_directory(
  target_dir, output="tibble", RecordID=TRUE, begin_record_num=70000)

# create a tibble with target recordID and record IDs for potential matches
pleurozium_result_match_tbl <- create_string_match_tbl_for_results(pleurozium_search_results_all_tbl,
                                                                 threshold=10,
                                                                 max_matches=100)

write.csv(pleurozium_result_match_tbl, "./lit_search_results/pleurozium_batch_initial_match_tbl.csv")

pleurozium_confirmed_match_tbl <- user_comfirmation_of_matches(pleurozium_result_match_tbl, pleurozium_search_results_all_tbl)

write.csv(pleurozium_confirmed_match_tbl, "./lit_search_results/pleurozium_batch_confirmed_match_tbl.csv")

# pleurozium_confirmed_match_tbl<- read_csv("./lit_search_results/batch_confirmed_match_tbls/pleurozium_batch_confirmed_match_tbl.csv")

pleurozium_deduplicated_results_tbl <- remove_duplicate_records(pleurozium_confirmed_match_tbl,pleurozium_search_results_all_tbl)

write.csv(pleurozium_deduplicated_results_tbl[[1]], "./lit_search_results/pleurozium_batch_deduplicated_result_tbl.csv")

####### SYNTRICHIA ########

#### Read in the functions ####
source("lit_search_check_for_matches_output_table.R")

############# Run the Functions ##############
# create file list for target directory 
target_dir <- "./lit_search_results/syntrichia_batch/"

# create list of all search results in target directory
#search_results_list <- read_search_results_from_directory(target_dir, output="list")

# create tibble of all search results in target directory
syntrichia_search_results_all_tbl <- read_search_results_from_directory(
  target_dir, output="tibble", RecordID=TRUE, begin_record_num=100000)

# create a tibble with target recordID and record IDs for potential matches
syntrichia_result_match_tbl <- create_string_match_tbl_for_results(syntrichia_search_results_all_tbl,
                                                                   threshold=10,
                                                                   max_matches=100)

write.csv(syntrichia_result_match_tbl, "./lit_search_results/syntrichia_batch_initial_match_tbl.csv")

syntrichia_confirmed_match_tbl <- user_comfirmation_of_matches(syntrichia_result_match_tbl, syntrichia_search_results_all_tbl)

write.csv(syntrichia_confirmed_match_tbl, "./lit_search_results/syntrichia_batch_confirmed_match_tbl.csv")

 syntrichia_confirmed_match_tbl<- read_csv("./lit_search_results/batch_confirmed_match_tbls/syntrichia_batch_confirmed_match_tbl.csv")

syntrichia_deduplicated_results_tbl <- remove_duplicate_records(syntrichia_confirmed_match_tbl,syntrichia_search_results_all_tbl)

write.csv(syntrichia_deduplicated_results_tbl[[1]], "./lit_search_results/syntrichia_batch_deduplicated_result_tbl.csv")

####### LIVERWORT ########

#### Read in the functions ####
source("lit_search_check_for_matches_output_table.R")

############# Run the Functions ##############
# create file list for target directory 
target_dir <- "./lit_search_results/liverwort_batch/"

# create list of all search results in target directory
#search_results_list <- read_search_results_from_directory(target_dir, output="list")

# create tibble of all search results in target directory
liverwort_search_results_all_tbl <- read_search_results_from_directory(
  target_dir, output="tibble", RecordID=TRUE, begin_record_num=110000)

# create a tibble with target recordID and record IDs for potential matches
liverwort_result_match_tbl <- create_string_match_tbl_for_results(liverwort_search_results_all_tbl,
                                                                   threshold=10,
                                                                   max_matches=100)

write.csv(liverwort_result_match_tbl, "./lit_search_results/liverwort_batch_initial_match_tbl.csv")

liverwort_confirmed_match_tbl <- user_comfirmation_of_matches(liverwort_result_match_tbl, liverwort_search_results_all_tbl)

write.csv(liverwort_confirmed_match_tbl, "./lit_search_results/liverwort_batch_confirmed_match_tbl.csv")

# liverwort_confirmed_match_tbl<- read_csv("./lit_search_results/batch_confirmed_match_tbls/liverwort_batch_confirmed_match_tbl.csv")

liverwort_deduplicated_results_tbl <- remove_duplicate_records(liverwort_confirmed_match_tbl,liverwort_search_results_all_tbl)

write.csv(liverwort_deduplicated_results_tbl[[1]], "./lit_search_results/liverwort_batch_deduplicated_result_tbl.csv")


####### BIOCRUST ########

#### Read in the functions ####
source("lit_search_check_for_matches_output_table.R")

############# Run the Functions ##############
# create file list for target directory 
target_dir <- "./lit_search_results/biocrust_batch/"

# create list of all search results in target directory
#search_results_list <- read_search_results_from_directory(target_dir, output="list")

# create tibble of all search results in target directory
biocrust_search_results_all_tbl <- read_search_results_from_directory(
  target_dir, output="tibble", RecordID=TRUE, begin_record_num=130000)

# create a tibble with target recordID and record IDs for potential matches
biocrust_result_match_tbl <- create_string_match_tbl_for_results(biocrust_search_results_all_tbl,
                                                                  threshold=10,
                                                                  max_matches=100)

write.csv(biocrust_result_match_tbl, "./lit_search_results/batch_initial_match_tbls/biocrust_batch_initial_match_tbl.csv")

biocrust_confirmed_match_tbl <- user_comfirmation_of_matches(biocrust_result_match_tbl, biocrust_search_results_all_tbl)

write.csv(biocrust_confirmed_match_tbl, "./lit_search_results/biocrust_batch_confirmed_match_tbl.csv")

 #biocrust_confirmed_match_tbl<- read_csv("./lit_search_results/batch_confirmed_match_tbls/biocrust_batch_confirmed_match_tbl.csv")

biocrust_deduplicated_results_tbl <- remove_duplicate_records(biocrust_confirmed_match_tbl,biocrust_search_results_all_tbl)

write.csv(biocrust_deduplicated_results_tbl[[1]], "./lit_search_results/batch_deduplicated_result_tbls/biocrust_batch_deduplicated_result_tbl.csv")

####### PEAT ########

#### Read in the functions ####
source("lit_search_check_for_matches_output_table.R")

############# Run the Functions ##############
# create file list for target directory 
target_dir <- "./lit_search_results/peat_batch/"

# create list of all search results in target directory
#search_results_list <- read_search_results_from_directory(target_dir, output="list")

# create tibble of all search results in target directory
peat_search_results_all_tbl <- read_search_results_from_directory(
  target_dir, output="tibble", RecordID=TRUE, begin_record_num=150000)

# create a tibble with target recordID and record IDs for potential matches
peat_result_match_tbl <- create_string_match_tbl_for_results(peat_search_results_all_tbl,
                                                                 threshold=10,
                                                                 max_matches=100)

write.csv(peat_result_match_tbl, "./lit_search_results/batch_initial_match_tbls/peat_batch_initial_match_tbl.csv")

peat_confirmed_match_tbl <- user_comfirmation_of_matches(peat_result_match_tbl, peat_search_results_all_tbl)

write.csv(peat_confirmed_match_tbl, "./lit_search_results/peat_batch_confirmed_match_tbl.csv")

#peat_confirmed_match_tbl<- read_csv("./lit_search_results/batch_confirmed_match_tbls/peat_batch_confirmed_match_tbl.csv")

peat_deduplicated_results_tbl <- remove_duplicate_records(peat_confirmed_match_tbl,peat_search_results_all_tbl)

write.csv(peat_deduplicated_results_tbl[[1]], "./lit_search_results/batch_deduplicated_result_tbls/peat_batch_deduplicated_result_tbl.csv")

####### NON-VASCULAR ########

#### Read in the functions ####
source("lit_search_check_for_matches_output_table.R")

############# Run the Functions ##############
# create file list for target directory 
target_dir <- "./lit_search_results/nonvascular_batch/"

# create list of all search results in target directory
#search_results_list <- read_search_results_from_directory(target_dir, output="list")

# create tibble of all search results in target directory
nonvascular_search_results_all_tbl <- read_search_results_from_directory(
  target_dir, output="tibble", RecordID=TRUE, begin_record_num=170000)

# create a tibble with target recordID and record IDs for potential matches
nonvascular_result_match_tbl <- create_string_match_tbl_for_results(nonvascular_search_results_all_tbl,
                                                             threshold=10,
                                                             max_matches=100)

write.csv(nonvascular_result_match_tbl, "./lit_search_results/batch_initial_match_tbls/nonvascular_batch_initial_match_tbl.csv")

nonvascular_confirmed_match_tbl <- user_comfirmation_of_matches(nonvascular_result_match_tbl, nonvascular_search_results_all_tbl)

write.csv(nonvascular_confirmed_match_tbl, "./lit_search_results/nonvascular_batch_confirmed_match_tbl.csv")

#nonvascular_confirmed_match_tbl<- read_csv("./lit_search_results/batch_confirmed_match_tbls/nonvascular_batch_confirmed_match_tbl.csv")

nonvascular_deduplicated_results_tbl <- remove_duplicate_records(nonvascular_confirmed_match_tbl,nonvascular_search_results_all_tbl)

write.csv(nonvascular_deduplicated_results_tbl[[1]], "./lit_search_results/batch_deduplicated_result_tbls/nonvascular_batch_deduplicated_result_tbl.csv")

