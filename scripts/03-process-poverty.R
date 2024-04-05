# Monetary poverty --------------------------------------------------------
monetary_poverty_raw <- readxl::read_xlsx(
  "data/raw/poverty/monetary-poverty.xlsx", sheet = 2, range = "A9:F2159",
  col_names = c(
    "ubigeo", "sufix", "district", "population_2020", "lower_bound", "upper_bound"
  ),
  col_types = c("text", "text", "text", "numeric", "numeric", "numeric")
)
monetary_poverty <- monetary_poverty_raw |>
  tidyr::drop_na(sufix, population_2020) |>
  dplyr::mutate(monetary_poverty = (lower_bound + upper_bound) / 2) |>
  dplyr::select(ubigeo, monetary_poverty)
readr::write_csv(monetary_poverty, "data/interim/to-merge/02-monetary-poverty.csv")

# Non-monetary poverty ----------------------------------------------------
nonmonetary_poverty_raw <- readxl::read_xlsx(
  "data/raw/poverty/nonmonetary-poverty.xlsx", sheet = 1, range = "B11:G1884",
  col_names = c(
    "ubigeo", "department", "orovince", "district", "population",
    "at_least_one_ubn"
  ),
  col_types = c("text", "text", "text", "text", "numeric", "numeric")
)
nonmonetary_poverty <- nonmonetary_poverty_raw |>
  dplyr::mutate(nonmonetary_poverty = 100 * at_least_one_ubn / population) |>
  dplyr::select(ubigeo, nonmonetary_poverty)
readr::write_csv(nonmonetary_poverty, "data/interim/to-merge/03-nonmonetary-poverty.csv")


