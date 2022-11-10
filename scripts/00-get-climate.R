library(rgee)
library(innovar)
library(sf)
library(janitor)
library(dplyr)
library(purrr)
library(readr)
library(fs)

output_path <- "data/raw/climate/"

ee_Initialize()

data("Peru")

districts <- st_drop_geometry(Peru)
districts_ee <- pol_as_ee(Peru , id = "ubigeo", simplify = 1000)

# CO ----------------------------------------------------------------------

co_raw <- districts_ee %>%
  get_co(
    from = "2019-01-01", to = "2019-12-31",
    band = "CO_column_number_density", fun = "mean"
  )

co <- co_raw %>%
  clean_names() %>%
  mutate(co = rowMeans(across(starts_with("co")))) %>%
  select(ubigeo, co)

co_filename <- "co.csv"
co_filepath <- path(output_path, co_filename)
write_csv(co, co_filepath, na = "")

# NO2 ---------------------------------------------------------------------

no2_raw <- districts_ee %>%
  get_no2(
    from = "2019-01-01", to = "2019-12-31",
    band = "NO2_column_number_density", fun = "mean"
  )

no2 <- no2_raw %>%
  clean_names() %>%
  mutate(no2 = rowMeans(across(starts_with("no2")))) %>%
  select(ubigeo, no2)

no2_filename <- "no2.csv"
no2_filepath <- path(output_path, no2_filename)
write_csv(no2, no2_filepath, na = "")

# O3 ----------------------------------------------------------------------

o3_raw <- districts_ee %>%
  get_o3(
    from = "2019-01-01", to = "2019-12-31",
    band = "O3_column_number_density", fun = "mean"
  )

o3 <- o3_raw %>%
  clean_names() %>%
  mutate(o3 = rowMeans(across(starts_with("o3")))) %>%
  select(ubigeo, o3)

o3_filename <- "o3.csv"
o3_filepath <- path(output_path, o3_filename)
write_csv(o3, o3_filepath, na = "")

# SO2 ---------------------------------------------------------------------

so2_raw <- districts_ee %>%
  get_so2(
    from = "2019-01-01", to = "2019-12-31",
    band = "SO2_column_number_density", fun = "mean"
  )

so2 <- so2_raw %>%
  clean_names() %>%
  mutate(so2 = rowMeans(across(starts_with("so2")))) %>%
  select(ubigeo, so2)

so2_filename <- "so2.csv"
so2_filepath <- path(output_path, so2_filename)
write_csv(so2, so2_filepath, na = "")

# Aerosol index -----------------------------------------------------------

aai_raw <- districts_ee %>%
  get_aero(
    from = "2019-01-01", to = "2019-12-31",
    band = "absorbing_aerosol_index", fun = "mean"
  )

aai <- aai_raw %>%
  clean_names() %>%
  mutate(aai = rowMeans(across(starts_with("uavi")))) %>%
  select(ubigeo, aai)

aai_filename <- "aai.csv"
aai_filepath <- path(output_path, aai_filename)
write_csv(aai, aai_filepath, na = "")

# PM2.5 -------------------------------------------------------------------

pm25_raw <- read_csv(
  "data/raw/pm25.csv",
  col_select = c("UBIGEO", "pm2.5_mean"),
  show_col_types = FALSE
)

pm25 <- rename(pm25_raw, ubigeo = UBIGEO, pm25 = `pm2.5_mean`)

pm25_filename <- "pm25.csv"
pm25_filepath <- path(output_path, pm25_filename)
write_csv(pm25, pm25_filepath, na = "")

# Minimum temperature -----------------------------------------------------

tmmn_raw <- districts_ee %>%
  get_climate(
    from = "2019-01-01", to = "2019-12-31", by = "month",
    band = "tmmn", fun = "mean"
  )

tmmn <- tmmn_raw %>%
  clean_names() %>%
  mutate(tmmn = rowMeans(across(starts_with("tmmn")))) %>%
  select(ubigeo, tmmn)

tmmn_filename <- "tmmn.csv"
tmmn_filepath <- path(output_path, tmmn_filename)
write_csv(tmmn, tmmn_filepath, na = "")

# Maximum temperature -----------------------------------------------------

tmmx_raw <- districts_ee %>%
  get_climate(
    from = "2019-01-01", to = "2019-12-31", by = "month",
    band = "tmmx", fun = "mean"
  )

tmmx <- tmmx_raw %>%
  clean_names() %>%
  mutate(tmmx = rowMeans(across(starts_with("tmmx")))) %>%
  select(ubigeo, tmmx)

tmmx_filename <- "tmmx.csv"
tmmx_filepath <- path(output_path, tmmx_filename)
write_csv(tmmx, tmmx_filepath, na = "")

# Humidity ----------------------------------------------------------------

sh_raw <- districts_ee %>%
  get_fldas(
    from = "2019-01-01", to = "2019-12-31", by = "month",
    band = "Qair_f_tavg", fun = "mean"
  )

sh <- sh_raw %>%
  clean_names() %>%
  mutate(sh = rowMeans(across(starts_with("qair_f_tavg")))) %>%
  select(ubigeo, sh)

sh_filename <- "sh.csv"
sh_filepath <- path(output_path, sh_filename)
write_csv(sh, sh_filepath, na = "")

# Surface downward shortwave radiation ------------------------------------

sradiation_raw <- districts_ee %>%
  get_fldas(
    from = "2019-01-01", to = "2019-12-31", by = "month",
    band = "SWdown_f_tavg", fun = "mean"
  )

sradiation <- sradiation_raw %>%
  clean_names() %>%
  mutate(sradiation = rowMeans(across(starts_with("s_wdown_f_tavg")))) %>%
  select(ubigeo, sradiation)

sradiation_filename <- "sradiation.csv"
sradiation_filepath <- path(output_path, sradiation_filename)
write_csv(sradiation, sradiation_filepath, na = "")

# Near surface air temperature -------------------------------------------

sairtemp_raw <- districts_ee %>%
  get_fldas(
    from = "2019-01-01", to = "2019-12-31", by = "month",
    band = "Tair_f_tavg", fun = "mean"
  )

sairtemp <- sairtemp_raw %>%
  clean_names() %>%
  mutate(sairtemp = rowMeans(across(starts_with("tair_f_tavg")))) %>%
  select(ubigeo, sairtemp)

sairtemp_filename <- "sairtemp.csv"
sairtemp_filepath <- path(output_path, sairtemp_filename)
write_csv(sairtemp, sairtemp_filepath, na = "")

# Near surface wind speed -------------------------------------------------

swindspeed_raw <- districts_ee %>%
  get_fldas(
    from = "2019-01-01", to = "2019-12-31", by = "month",
    band = "Wind_f_tavg", fun = "mean"
  )

swindspeed <- swindspeed_raw %>%
  clean_names() %>%
  mutate(swindspeed = rowMeans(across(starts_with("wind_f_tavg")))) %>%
  select(ubigeo, swindspeed)

swindspeed_filename <- "swindspeed.csv"
swindspeed_filepath <- path(output_path, swindspeed_filename)
write_csv(swindspeed, swindspeed_filepath, na = "")

# Global human modification -----------------------------------------------

ghm_raw <- districts_ee %>%
  get_ghm(fun = "mean")

ghm <- clean_names(ghm_raw)

ghm_filename <- "ghm.csv"
ghm_filepath <- path(output_path, ghm_filename)
write_csv(ghm, ghm_filepath, na = "")

# Urban area --------------------------------------------------------------

urban_raw <- districts_ee %>%
  get_urban(from = "2019-01-01", to = "2019-12-31")

urban <- rename(urban_raw, urban_area = X0_urban)

urban_filename <- "urban.csv"
urban_filepath <- path(output_path, urban_filename)
write_csv(urban, urban_filepath, na = "")

# Night time light --------------------------------------------------------

night_raw <- districts_ee %>%
  get_nlv2(from = "2019-01-01", to = "2019-12-31", fun = "mean")

night <- rename(night_raw, ntl = ntl2019)

night_filename <- "night.csv"
night_filepath <- path(output_path, night_filename)
write_csv(night, night_filepath, na = "")

