library(fs)
library(readxl)
library(innovar)
library(dplyr)

raw_path <- path("data", "raw")
infile <- "district-edu-institution-census-2017.xlsx"
infile_path <- path(raw_path, infile)
mob_raw <- read_excel(infile_path)

mob_od <- get_od_data(mob_raw)
mob_od_matrix <- get_od_data(mob_raw, wide = TRUE)

#TODO: Add argument in get_od_data to choose which column is origin and which
# is destination

lima_metro <- c("LIMA", "CALLAO")
mob_lima_metro_od <-
  mob_od %>%
  filter(
    dept_ori %in% lima_metro, dept_des %in% lima_metro
  )
names(mob_lima_metro_od) <- c(
  "ubigeo_des", "ubigeo_ori", "cases", "dept_des", "prov_des", "distr_des",
  "dept_ori", "prov_ori", "distr_ori"
)
mob_lima_metro_od <-
  mob_lima_metro_od %>%
  select(
    ubigeo_ori, ubigeo_des, cases, dept_ori, prov_ori, distr_ori, dept_des,
    prov_des, distr_des
  ) %>%
  # arrange(dept_ori, prov_ori, distr_ori, dept_des, prov_des, distr_des)
  arrange(ubigeo_ori, ubigeo_des)
