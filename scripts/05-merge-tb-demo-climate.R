library(fs)
library(dplyr)

input_path = "data/interim/to-merge/"
file_paths = dir_ls(input_path)

datasets = purrr::map(file_paths, readr::read_csv, col_types = "c")

names(datasets) = datasets |>
  names() |>
  path_file() |>
  path_ext_remove()

dataset = purrr::reduce(datasets, inner_join, by = "ubigeo")

districts = dataset |>
  select(
    -c(
      department, province, population, sairtemp, sh, sradiation, swindspeed,
      urban_area, idh, life_expectancy:family_income, hh_inadequate_char:aai,
      ntl, so2, tmmn, tmmx
    )
  )

readr::write_csv(districts, "data/interim/districts.csv", na = "")
