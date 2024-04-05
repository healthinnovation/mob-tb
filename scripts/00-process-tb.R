tb_raw <- readr::read_csv(
  "data/raw/tb-lima.csv",
  col_types = "cdddddddddccccddddddddc",
  col_select = c(YEAR, ubigeo_eess, casos_nuevos, NAME_3, NAME_S)
)

north_lima <- c(
  "150102", "150106", "150110", "150112", "150117", "150125", "150135", "150139"
)

center_lima <- c(
  "150104", "150105", "150113", "150115", "150101", "150116", "150120", "150122",
  "150128", "150130", "150131", "150136", "150140", "150141"
)

south_lima <- c(
  "150108", "150119", "150123", "150124", "150126", "150127", "150129", "150133",
  "150138", "150142", "150143"
)

east_lima <- c(
  "150103", "150107", "150109", "150111", "150114", "150118", "150132", "150134",
  "150137"
)

tb <- tb_raw |>
  dplyr::filter(YEAR == 2017) |>
  dplyr::rename(
    ubigeo = ubigeo_eess, district = NAME_3, district_short = NAME_S,
    new_cases = casos_nuevos
  ) |>
  dplyr::select(-YEAR) |>
  dplyr::arrange(ubigeo) |>
  dplyr::mutate(
    subregion = dplyr::case_when(
      ubigeo %in% north_lima ~ "North Lima",
      ubigeo %in% center_lima ~ "Center Lima",
      ubigeo %in% south_lima ~ "South Lima",
      ubigeo %in% east_lima ~ "East Lima"
    )
  )

readr::write_csv(tb, "data/interim/to-merge/00-tb.csv", na = "")
