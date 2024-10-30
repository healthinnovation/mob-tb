filepaths_fixed <- fs::dir_ls("data/interim/fixed/")[c(1, 5, 6)]
datasets_fixed <- purrr::map(filepaths_fixed, readr::read_csv)
colnames(datasets_fixed[[2]])
nodes <- purrr::reduce(datasets_fixed, dplyr::inner_join, by = "ubigeo")
readr::write_csv(nodes, "data/processed/network/nodes.csv", na = "")

filepaths_yearly <- fs::dir_ls("data/interim/yearly/")
datasets_yearly <- purrr::map(filepaths_yearly, readr::read_csv)
tb <- purrr::reduce(datasets_yearly, dplyr::left_join, by = c("ubigeo", "year"))
readr::write_csv(tb, "data/processed/tb.csv", na = "")







