#COMBINING ALL OF THE BATCHES

final_batches_path <- "./lit_search_results/batch_deduplicated_result_tbls"
final_file_names <- list.files(final_batches_path)

#choose a file to model empty tbl off of
first_file <- read.csv(paste(final_batches_path, final_file_names[1], sep="/"))
#create empty tibble for output data for target record and potential matches
num_rows <- nrow(first_file) #get number of rows for output tbl
num_cols <- ncol(first_file)
# Create a matrix filled with NA
na_matrix <- matrix(NA, nrow = num_rows, ncol = num_cols)
# Convert the matrix to a tibble
all_searches_output_tbl <- as_tibble(na_matrix, .name_repair="minimal")

for(i in 1:9){
  
  #call .csv of target batch file
  target_file <- read.csv(paste(final_batches_path, final_file_names[i], sep="/"))
  
  all_searches_tbl <- rbind(all_searches_output_tbl,target_file) 
  }
