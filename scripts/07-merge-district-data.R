input_path <- "data/interim/to-merge/"
file_paths <- fs::dir_ls(input_path)

datasets <- purrr::map(file_paths, readr::read_csv, col_types = "c")

names(datasets) <- datasets |>
  names() |>
  fs::path_file() |>
  fs::path_ext_remove()

districts <- purrr::reduce(datasets, dplyr::inner_join, by = "ubigeo")

readr::write_csv(districts, "data/interim/districts.csv", na = "")
