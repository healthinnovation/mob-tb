library(fs)
library(haven)
library(dplyr)
library(tidyr)

raw_path <- "data/raw/"
census_filename <- "2017-census-population.sav"
census_filepath <- path(raw_path, census_filename)
census <- read_sav(census_filepath)

interim_path <- "data/interim/"

od_study <- census %>%
  filter(c5_p5 == 1, c5_p14 == 1, c5_p15a_cod != 99, c5_p15d_cod == "") %>%
  select(origin = ubigeo, c5_p15b_cod:c5_p15a_cod) %>%
  drop_na() %>%
  mutate(destination = paste(c5_p15b_cod, c5_p15c_cod, c5_p15a_cod, sep = "")) %>%
  mutate(destination = ifelse(destination == "", origin, destination)) %>%
  group_by(origin, destination) %>%
  summarise(cases = n(), .groups = "drop") %>%
  arrange(origin, destination, cases)

od_study_filename <- "origin-destination-study.csv"
od_study_filepath <- path(interim_path, od_study_filename)
readr::write_csv(od_study, od_study_filepath)

od_work <- census %>%
  filter(
    c5_p5 == 1, c5_p4_1 >= 14, !is.na(c5_p16), !(c5_p17 %in% c(6, 7)),
    !is.na(c5_p23), c5_p23c_cod != 99, c5_p23a_cod != 99, c5_p23d_cod == ""
  ) %>%
  select(origin = ubigeo, c5_p23b_cod:c5_p23a_cod) %>%
  drop_na() %>%
  mutate(destination = paste(c5_p23b_cod, c5_p23c_cod, c5_p23a_cod, sep = "")) %>%
  mutate(destination = ifelse(destination == "", origin, destination)) %>%
  group_by(origin, destination) %>%
  summarise(cases = n(), .groups = "drop") %>%
  arrange(origin, destination, cases)

od_work_filename <- "origin-destination-work.csv"
od_work_filepath <- path(interim_path, od_work_filename)
readr::write_csv(od_work, od_work_filepath)

# PEAO: Población económicamente activa ocupada

student_peao <- census %>%
  filter(c5_p5 == 1) %>%
  select(ubigeo, c5_p4_1, c5_p14:c5_p17, c5_p23:c5_p23d_cod) %>%
  mutate(
    student = ifelse(c5_p14 == 1 & !is.na(c5_p15) & c5_p15a_cod != 99 & c5_p15d_cod == "", 1, 0),
    peao = ifelse(
      c5_p4_1 >= 14 & c5_p16 %in% c(1, 2) & !(c5_p17 %in% c(6, 7)) & !is.na(c5_p23) &
        c5_p23c_cod != 99 & c5_p23a_cod != 99 & c5_p23d_cod == "", 1, 0
    )
  ) %>%
  filter(student == 1 | peao == 1) %>%
  mutate(id = row_number())

od_raw <- student_peao %>%
  select(
    origin = ubigeo, c5_p15b_cod:c5_p15a_cod, c5_p23b_cod:c5_p23a_cod, student,
    peao, id
  ) %>%
  mutate(
    destination_study = paste(c5_p15b_cod, c5_p15c_cod, c5_p15a_cod, sep = ""),
    destination_work = paste(c5_p23b_cod, c5_p23c_cod, c5_p23a_cod, sep = "")
  ) %>%
  mutate(
    destination_study = ifelse(student == 0, NA, destination_study),
    destination_work = ifelse(peao == 0, NA, destination_work)
  ) %>%
  select(-c(c5_p15b_cod:c5_p23a_cod)) %>%
  pivot_longer(
    cols = destination_study:destination_work,
    names_to = "type",
    names_pattern = "destination_(.*)",
    values_to = "destination"
  ) %>%
  drop_na(destination) %>%
  mutate(destination = ifelse(destination == "", origin, destination))

od <- od_raw %>%
  group_by(origin, destination) %>%
  summarise(cases = n_distinct(id), .groups = "drop")

od_filename <- "origin-destination.csv"
od_filepath <- path(interim_path, od_filename)
readr::write_csv(od, od_filepath)

population <- census %>%
  group_by(
    department = departamento, province = provincia, district = distrito, ubigeo
  ) %>%
  summarise(
    population = n(),
    population_permanent = sum(ifelse(c5_p5 == 1, 1, 0)),
    .groups = "drop"
  ) %>%
  arrange(ubigeo)

population_filename <- "population.csv"
population_filepath <- path(raw_path, population_filename)
readr::write_csv(population, population_filepath)

