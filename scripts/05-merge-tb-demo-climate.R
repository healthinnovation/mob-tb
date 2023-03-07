library(fs)
library(readr)
library(purrr)
library(dplyr)

input_path <- "data/interim/to-merge/"
file_paths <- dir_ls(input_path)

datasets <- map(file_paths, read_csv, col_types = "c")

names(datasets) <- datasets %>%
  names() %>%
  path_file() %>%
  path_ext_remove()

dataset <- reduce(datasets, inner_join, by = "ubigeo")

lima <- dataset %>%
  select(-c(population, sairtemp, sh, sradiation, swindspeed, urban_area))

output_path <- path("data/interim/")
output_filename <- "lima.csv"
output_filepath <- path(output_path, output_filename)
write_csv(lima, output_filepath, na = "")
