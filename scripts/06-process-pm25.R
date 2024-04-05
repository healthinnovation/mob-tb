library(sf)

# NetCDF file with the annual PM2.5 global estimates can be downloaded from
# https://wustl.app.box.com/v/ACAG-V5GL03-GWRPM25/file/1039404401959
pm25_global <- terra::rast("data/raw/pm25/V5GL03.HybridPM25.Global.201701-201712.nc")

data("Peru", package = "innovar")
peru_polygon <- dplyr::summarise(Peru)

pm25_peru <- pm25_global |>
  terra::crop(peru_polygon) |>
  terra::mask(peru_polygon)

pm25_districts_raw <- exactextractr::exact_extract(
  x = pm25_peru, y = Peru, fun = "sum", append_cols = TRUE, progress = FALSE
)

pm25_districts <- dplyr::select(pm25_districts_raw, ubigeo, pm25 = sum)
readr::write_csv(pm25_districts, "data/interim/to-merge/07-pm25.csv")
