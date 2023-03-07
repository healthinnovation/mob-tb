library(fs)
library(purrr)
library(readr)
library(dplyr)

input_path <- "data/raw/climate/"
file_paths <- dir_ls(input_path)

datasets <- map(file_paths, read_csv, col_types = "cd")

names(datasets) <- datasets %>%
  names() %>%
  path_file() %>%
  path_ext_remove()

dataset <- datasets %>%
  reduce(inner_join, by = "ubigeo")
  # mutate(across(where(is.numeric), scales::rescale))

interim_path <- path("data/interim/to-merge/")
output_filename <- "02-climate.csv"
output_filepath <- path(interim_path, output_filename)
write_csv(dataset, output_filepath, na = "")
