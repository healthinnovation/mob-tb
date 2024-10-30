# Monetary poverty --------------------------------------------------------
monetary_poverty_raw <- readxl::read_xlsx(
  "data/raw/socio/monetary-poverty.xlsx", sheet = 2, range = "A9:F2159",
  col_names = c(
    "ubigeo", "sufix", "district", "population_2020", "lower_bound", "upper_bound"
  ),
  col_types = c("text", "text", "text", "numeric", "numeric", "numeric")
)
monetary_poverty <- monetary_poverty_raw |>
  tidyr::drop_na(sufix, population_2020) |>
  dplyr::mutate(monetary_poverty = (lower_bound + upper_bound) / 2) |>
  dplyr::select(ubigeo, monetary_poverty) |>
  dplyr::filter(stringr::str_sub(ubigeo, 1, 4) %in% c("0701", "1501"))
readr::write_csv(monetary_poverty, "data/interim/fixed/02-monetary-poverty.csv")

# Non-monetary poverty ----------------------------------------------------
nonmonetary_poverty_raw <- readxl::read_xlsx(
  "data/raw/socio/nonmonetary-poverty.xlsx", sheet = 1, range = "B11:G1884",
  col_names = c(
    "ubigeo", "department", "province", "district", "population",
    "at_least_one_ubn"
  ),
  col_types = c("text", "text", "text", "text", "numeric", "numeric")
)
nonmonetary_poverty <- nonmonetary_poverty_raw |>
  dplyr::mutate(ubn = 100 * at_least_one_ubn / population) |>
  dplyr::select(ubigeo, ubn) |>
  dplyr::filter(stringr::str_sub(ubigeo, 1, 4) %in% c("0701", "1501"))
readr::write_csv(nonmonetary_poverty, "data/interim/fixed/03-nbi.csv")

# HDI ---------------------------------------------------------------------
hdi_raw <- readr::read_csv("data/raw/socio/idh18.csv")
hdi <- dplyr::select(hdi_raw, ubigeo, hdi = IDH)
readr::write_csv(hdi, "data/interim/fixed/01-hdi.csv")

# Population --------------------------------------------------------------
population_raw <- readxl::read_excel(
  "data/raw/socio/population.xlsx", col_names = FALSE, skip = 5
)

population_2023 <- population_raw |>
  tidyr::drop_na(`...1`, `...2`) |>
  dplyr::filter(!grepl("UBIGEO", `...1`)) |>
  dplyr::filter(stringr::str_sub(`...1`, 5, 6) != "00")

names(population_2023) <- c(
  "ubigeo", "district", "population_2018", "population_2019", "population_2020",
  "population_2021", "population_2022"
)

ubigeo_keys <- readr::read_csv(
  "data/raw/socio/keys_ubigeo.csv", col_types = "ccc",
  col_select = dplyr::starts_with("ubigeo")
)

population_2017_raw <- population_2023 |>
  dplyr::left_join(ubigeo_keys, by = c("ubigeo" = "ubigeo_2023")) |>
  dplyr::mutate(ubigeo_2017 = ifelse(is.na(ubigeo_2017), ubigeo, ubigeo_2017))

population_2017 <- population_2017_raw |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("population"), \(x) ifelse(grepl("[a-zA-Z]", x), "0", x))) |>
  dplyr::mutate(dplyr::across(dplyr::starts_with("population"), \(x) as.numeric(x))) |>
  dplyr::group_by(ubigeo_2017) |>
  dplyr::summarise(
    dplyr::across(dplyr::starts_with("population"), \(x) sum(x, na.rm = TRUE))
  ) |>
  dplyr::rename(ubigeo = ubigeo_2017)

population_2017_long <- population_2017 |>
  tidyr::pivot_longer(
    starts_with("population"),
    names_prefix = "population_",
    names_to = "year",
    values_to = "population"
  ) |>
  dplyr::group_by(ubigeo) |>
  tidyr::complete(year = as.character(2018:2023)) |>
  dplyr::ungroup()

population_2018_2023 <- population_2017_long |>
  dplyr::group_by(ubigeo) |>
  dplyr::mutate(population = zoo::na.spline(population)) |>
  dplyr::ungroup()

readr::write_csv(population_2018_2023, "data/interim/yearly/01-population.csv")
