library(fs)
library(readxl)
library(innovar)
library(dplyr)
library(readr)

raw_path <- path("data", "raw")
infile <- "district-edu-institution-census-2017.xlsx"
infile_path <- path(raw_path, infile)
mob_raw <- read_excel(infile_path)

mob_od <- get_od_data(mob_raw)
mob_od_matrix <- get_od_data(mob_raw, wide = TRUE)

#TODO: Add argument in get_od_data to choose which column is origin and which
# is destination

mob_lima_metro_od <-
  mob_od %>%
  filter(prov_ori == "LIMA", prov_des == "LIMA")

names(mob_lima_metro_od) <- c(
  "ubigeo_des", "ubigeo_ori", "cases", "dept_des", "prov_des", "distr_des",
  "dept_ori", "prov_ori", "distr_ori"
)

mob_lima_metro_od <-
  mob_lima_metro_od %>%
  select(ubigeo_ori, ubigeo_des, cases) %>%
  # arrange(dept_ori, prov_ori, distr_ori, dept_des, prov_des, distr_des)
  arrange(ubigeo_ori, ubigeo_des)

output_path <- path("data", "interim")
output_file_name <- "mob-lima-metro-od.csv"
output_file_path <- path(output_path, output_file_name)
write_csv(mob_lima_metro_od, output_file_path, na = "")
