tb_raw <- readr::read_csv(
  "data/raw/tb.csv",
  col_types = "cciiiiiiiiiiii"
)

north_lima <- c(
  "150102", "150106", "150110", "150112", "150117", "150125", "150135", "150139"
)

center_lima <- c(
  "150104", "150105", "150113", "150115", "150101", "150116", "150120", "150122",
  "150128", "150130", "150131", "150136", "150140", "150141", "150121"
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
  tidyr::pivot_longer(
    cols = -c(ubigeo, district), names_to = c("diagnostic", "year"),
    names_sep = "_", values_to = "cases"
  ) |>
  dplyr::mutate(
    diagnostic = toupper(diagnostic),
    region = dplyr::case_when(
      ubigeo %in% north_lima ~ "NORTHERN LIMA",
      ubigeo %in% center_lima ~ "CENTRAL LIMA",
      ubigeo %in% south_lima ~ "SOUTHERN LIMA",
      ubigeo %in% east_lima ~ "EASTERN LIMA",
      TRUE ~ "CALLAO"
    )
  ) |>
  dplyr::arrange(year, diagnostic, ubigeo)

readr::write_csv(tb, "data/interim/to-merge/00-tb.csv", na = "")
