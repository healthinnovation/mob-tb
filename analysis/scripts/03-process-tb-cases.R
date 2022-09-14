library(fs)
library(readr)
library(dplyr)

raw_path <- path("analysis/data/raw/")
tb_filename <- "tb-cases-lima.csv"
tb_filepath <- path(raw_path, tb_filename)

tb_raw <- read_csv(
  tb_filepath, col_types = "cdddddddddccccddddddddc",
  col_select = c(YEAR, ubigeo_eess, casos_nuevos),
)

tb <- tb_raw %>%
  filter(YEAR == 2017) %>%
  rename(ubigeo = ubigeo_eess, new_cases = casos_nuevos) %>%
  select(ubigeo, new_cases) %>%
  arrange(ubigeo)

interim_path <- "analysis/data/interim/"
output_filename <- "03-tb-cases.csv"
output_filepath <- path(interim_path, output_filename)
write_csv(tb, output_filepath, na = "")
