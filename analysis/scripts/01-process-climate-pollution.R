library(fs)
library(purrr)
library(readr)
library(dplyr)

input_path <- "analysis/data/raw/climate-pollution/"
file_paths <- dir_ls(input_path)

datasets <- map(file_paths, read_csv, col_types = "cd")

names(datasets) <- datasets %>%
  names() %>%
  path_file() %>%
  path_ext_remove()

dataset <- reduce(datasets, inner_join, by = "ubigeo")

interim_path <- path("analysis/data/interim/")
output_filename <- "02-climate-pollution.csv"
output_filepath <- path(interim_path, output_filename)
write_csv(dataset, output_filepath, na = "")
