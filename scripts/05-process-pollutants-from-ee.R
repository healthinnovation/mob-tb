input_path <- "data/raw/pollutants-from-ee/"
out_path <- "data/interim/to-merge/"
if (!dir.exists(out_path)) dir.create(out_path, recursive = TRUE)

# CO ----------------------------------------------------------------------
co_filename <- "04-co.csv"
co_input_filepath <- fs::path(input_path, co_filename)
co_raw <- readr::read_csv(co_input_filepath, col_types = "cdddddddddddd")
co <- co_raw |>
  janitor::clean_names() |>
  dplyr::mutate(co = rowSums(dplyr::across(dplyr::starts_with("co")))) |>
  dplyr::select(ubigeo, co)
co_output_filepath <- fs::path(out_path, co_filename)
readr::write_csv(co, co_output_filepath)

# NO2 ---------------------------------------------------------------------
no2_filename <- "05-no2.csv"
no2_input_filepath <- fs::path(input_path, no2_filename)
no2_raw <- readr::read_csv(no2_input_filepath, col_types = "cdddddddddddd")
no2 <- no2_raw |>
  janitor::clean_names() |>
  dplyr::mutate(no2 = rowSums(dplyr::across(dplyr::starts_with("no2")))) |>
  dplyr::select(ubigeo, no2)
no2_output_filepath <- fs::path(out_path, no2_filename)
readr::write_csv(no2, no2_output_filepath)

# O3 ----------------------------------------------------------------------
o3_filename <- "06-o3.csv"
o3_input_filepath <- fs::path(input_path, o3_filename)
o3_raw <- readr::read_csv(o3_input_filepath, col_types = "cdddddddddddd")
o3 <- o3_raw |>
  janitor::clean_names() |>
  dplyr::mutate(o3 = rowSums(dplyr::across(dplyr::starts_with("o3")))) |>
  dplyr::select(ubigeo, o3)
o3_output_filepath <- fs::path(out_path, o3_filename)
readr::write_csv(o3, o3_output_filepath)
