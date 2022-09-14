library(fs)
library(readr)
library(purrr)
library(dplyr)

input_path <- "analysis/data/interim/to-join/"
file_paths <- dir_ls(input_path)

datasets <- map(file_paths, read_csv, col_types = "c")

names(datasets) <- datasets %>%
  names() %>%
  path_file() %>%
  path_ext_remove()

dataset <- reduce(datasets, left_join, by = "ubigeo")

nodes_full <- select(dataset, -c(sairtemp, sh, sradiation, swindspeed, urban_area))

output_path <- path("analysis/data/interim/")
output_filename <- "nodes-full.csv"
output_filepath <- path(output_path, output_filename)
write_csv(nodes_full, output_filepath, na = "")
