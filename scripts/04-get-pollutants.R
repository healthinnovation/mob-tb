# Before calling the rgee package, please go to https://github.com/r-spatial/rgee and
# carefully check the installation requirements for your OS. Otherwise, it may not work.
library(rgee)
library(sf)

output_path <- "data/raw/pollutants-from-ee/"
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

ee_Initialize()
# Sometimes this function throws an error about a "Bad request" when initializing Google Earth Engine.
# Running rgee::ee_clean_credentials() worked for me. For other possible solutions see:
# https://github.com/r-spatial/rgee/issues/271

data("Peru", package = "innovar")

districts <- st_drop_geometry(Peru)
districts_ee <- innovar::pol_as_ee(Peru , id = "ubigeo", simplify = 1000)

# CO ----------------------------------------------------------------------
co_raw <- districts_ee |>
  innovar::get_co(
    from = "2019-01-01", to = "2019-12-31",
    band = "CO_column_number_density", fun = "mean"
  )
co_filename <- "co.csv"
co_filepath <- fs::path(output_path, co_filename)
readr::write_csv(co_raw, co_filepath, na = "")

# NO2 ---------------------------------------------------------------------
no2_raw <- districts_ee |>
  innovar::get_no2(
    from = "2019-01-01", to = "2019-12-31",
    band = "NO2_column_number_density", fun = "mean"
  )
no2_filename <- "no2.csv"
no2_filepath <- fs::path(output_path, no2_filename)
readr::write_csv(no2_raw, no2_filepath, na = "")

# O3 ----------------------------------------------------------------------
o3_raw <- districts_ee |>
  innovar::get_o3(
    from = "2019-01-01", to = "2019-12-31",
    band = "O3_column_number_density", fun = "mean"
  )
o3_filename <- "o3.csv"
o3_filepath <- fs::path(output_path, o3_filename)
readr::write_csv(o3_raw, o3_filepath, na = "")
