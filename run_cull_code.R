source("import_batch_cull_export_table.R")

#locate data
all_dedup_path <- "./lit_search_results/batch_deduplicated_result_tbls/all_deduplicated_result_tbl.csv"
all_dedup_tbl <- tibble(read.csv(all_dedup_path)) %>% select(3:15)

#
slice_1 <- all_dedup_tbl %>% slice(1:7425)
slice_2 <- all_dedup_tbl %>% slice(7426:14851)
slice_3 <- all_dedup_tbl %>% slice(14852:22276)
slice_4 <- all_dedup_tbl %>% slice(22277:29702)
slice_5 <- all_dedup_tbl %>% slice(29703:37126)

write_csv(slice_1, "./lit_search_results/batch_deduplicated_result_tbls/all_deduplicated_result_tbl_1.csv")
write_csv(slice_2, "./lit_search_results/batch_deduplicated_result_tbls/all_deduplicated_result_tbl_2.csv")
write_csv(slice_3, "./lit_search_results/batch_deduplicated_result_tbls/all_deduplicated_result_tbl_3.csv")
write_csv(slice_4, "./lit_search_results/batch_deduplicated_result_tbls/all_deduplicated_result_tbl_4.csv")
write_csv(slice_5, "./lit_search_results/batch_deduplicated_result_tbls/all_deduplicated_result_tbl_5.csv")

test_output_name <- "./lit_search_results/flag_tbls/test.csv"

user_flag_input(all_dedup_path,test_output_name)
