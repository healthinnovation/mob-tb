library(sf)

# NetCDF file with the annual PM2.5 global estimates can be downloaded from
# https://wustl.app.box.com/v/ACAG-V5GL03-GWRPM25/file/1039404401959
pm25_global <- terra::rast("data/raw/pm25/V5GL03.HybridPM25.Global.201801-201812.nc")

districts <- sf::read_sf("data/raw/Distrito.gpkg")
peru <- dplyr::summarise(districts)

pm25_peru <- pm25_global |>
  terra::crop(peru) |>
  terra::mask(peru)

pm25_raw <- exactextractr::exact_extract(
  x = pm25_peru, y = districts, fun = "sum", append_cols = TRUE, progress = FALSE
)

pm25 <- dplyr::select(pm25_raw, ubigeo, pm25 = sum)
readr::write_csv(pm25, "data/interim/fixed/04-pm25.csv")
