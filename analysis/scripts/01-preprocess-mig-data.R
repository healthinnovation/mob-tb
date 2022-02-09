library(fs)
library(readxl)
library(innovar)
library(dplyr)
library(readr)

raw_path <- path("data", "raw")
infile <- "district-migration-census-2017.xlsx"
infile_path <- path(raw_path, infile)
mig_raw <- read_excel(infile_path)

mig_od <- get_od_data(mig_raw)
mig_od_matrix <- get_od_data(mig_raw, wide = TRUE)

#TODO: Add argument in get_od_data to choose which column is origin and which
# is destination

lima_metro <- c("LIMA", "CALLAO")

mig_lima_metro_od <-
  mig_od %>%
  filter(
    dept_ori %in% lima_metro, dept_des %in% lima_metro
  )

names(mig_lima_metro_od) <- c(
  "ubigeo_des", "ubigeo_ori", "cases", "dept_des", "prov_des", "distr_des",
  "dept_ori", "prov_ori", "distr_ori"
)

mig_lima_metro_od <-
  mig_lima_metro_od %>%
  select(
    ubigeo_ori, ubigeo_des, cases, dept_ori, prov_ori, distr_ori, dept_des,
    prov_des, distr_des
  ) %>%
  # arrange(dept_ori, prov_ori, distr_ori, dept_des, prov_des, distr_des)
  arrange(ubigeo_ori, ubigeo_des)

output_path <- path("data", "interim")
output_file_name <- "mig-lima-metro-od.csv"
output_file_path <- path(output_path, output_file_name)
write_csv(mig_lima_metro_od, output_file_path, na = "")
