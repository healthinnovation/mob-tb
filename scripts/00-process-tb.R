tb_raw <- readr::read_csv(
  "data/raw/tb.csv",
  col_types = "cciiiiiiiiiiii",
  col_select = -district
)

tb <- tb_raw |>
  tidyr::pivot_longer(
    cols = -ubigeo, names_to = c(".value", "year"),
    names_sep = "_"
  ) |>
  dplyr::arrange(year, ubigeo)

readr::write_csv(tb, "data/interim/yearly/00-tb.csv", na = "")
