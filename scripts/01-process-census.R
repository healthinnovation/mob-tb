input_path <- "data/raw/2017-census/"
census_filename <- "2017-census-people.sav"
census_filepath <- fs::path(input_path, census_filename)
census <- haven::read_sav(census_filepath)

# Origin-destination data for students ------------------------------------
od_study <- census |>
  dplyr::filter(c5_p5 == 1, c5_p14 == 1, c5_p15a_cod != 99, c5_p15d_cod == "") |>
  dplyr::select(origin = ubigeo, c5_p15b_cod:c5_p15a_cod) |>
  tidyr::drop_na() |>
  dplyr::mutate(
    destination = paste(c5_p15b_cod, c5_p15c_cod, c5_p15a_cod, sep = "")
  ) |>
  dplyr::mutate(destination = ifelse(destination == "", origin, destination)) |>
  dplyr::group_by(origin, destination) |>
  dplyr::summarise(cases = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(origin, destination, cases)

interim_path <- "data/interim/"
od_study_filename <- "origin-destination-study.csv"
od_study_filepath <- fs::path(interim_path, od_study_filename)
readr::write_csv(od_study, od_study_filepath)
rm(od_study)
gc()

# Origin-destination data for workers -------------------------------------
od_work <- census |>
  dplyr::filter(
    c5_p5 == 1, c5_p4_1 >= 14, !is.na(c5_p16), !(c5_p17 %in% c(6, 7)),
    !is.na(c5_p23), c5_p23c_cod != 99, c5_p23a_cod != 99, c5_p23d_cod == ""
  ) |>
  dplyr::select(origin = ubigeo, c5_p23b_cod:c5_p23a_cod) |>
  tidyr::drop_na() |>
  dplyr::mutate(
    destination = paste(c5_p23b_cod, c5_p23c_cod, c5_p23a_cod, sep = "")
  ) |>
  dplyr::mutate(destination = ifelse(destination == "", origin, destination)) |>
  dplyr::group_by(origin, destination) |>
  dplyr::summarise(cases = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(origin, destination, cases)

od_work_filename <- "origin-destination-work.csv"
od_work_filepath <- fs::path(interim_path, od_work_filename)
readr::write_csv(od_work, od_work_filepath)
rm(od_work)
gc()

# Origin-destination data for students and workers ------------------------
## PEAO: Población económicamente activa ocupada
student_peao <- census |>
  dplyr::filter(c5_p5 == 1) |>
  dplyr::select(ubigeo, c5_p4_1, c5_p14:c5_p17, c5_p23:c5_p23d_cod) |>
  dplyr::mutate(
    student = ifelse(
      c5_p14 == 1 & !is.na(c5_p15) & c5_p15a_cod != 99 & c5_p15d_cod == "", 1, 0
    ),
    peao = ifelse(
      c5_p4_1 >= 14 & c5_p16 %in% c(1, 2) & !(c5_p17 %in% c(6, 7)) & !is.na(c5_p23) &
        c5_p23c_cod != 99 & c5_p23a_cod != 99 & c5_p23d_cod == "", 1, 0
    )
  ) |>
  dplyr::filter(student == 1 | peao == 1) |>
  dplyr::mutate(id = dplyr::row_number())

od_raw <- student_peao |>
  dplyr::select(
    origin = ubigeo, c5_p15b_cod:c5_p15a_cod, c5_p23b_cod:c5_p23a_cod, student,
    peao, id
  ) |>
  dplyr::mutate(
    destination_study = paste(c5_p15b_cod, c5_p15c_cod, c5_p15a_cod, sep = ""),
    destination_work = paste(c5_p23b_cod, c5_p23c_cod, c5_p23a_cod, sep = "")
  ) |>
  dplyr::mutate(
    destination_study = ifelse(student == 0, NA, destination_study),
    destination_work = ifelse(peao == 0, NA, destination_work)
  ) |>
  dplyr::select(-c(c5_p15b_cod:c5_p23a_cod)) |>
  tidyr::pivot_longer(
    cols = destination_study:destination_work,
    names_to = "type",
    names_pattern = "destination_(.*)",
    values_to = "destination"
  ) |>
  tidyr::drop_na(destination) |>
  dplyr::mutate(destination = ifelse(destination == "", origin, destination))

od <- od_raw |>
  dplyr::group_by(origin, destination) |>
  dplyr::summarise(cases = dplyr::n_distinct(id), .groups = "drop")

od_filename <- "origin-destination-total.csv"
od_filepath <- fs::path(interim_path, od_filename)
readr::write_csv(od, od_filepath)
rm(od_raw)
rm(od)
gc()

# Permanent population  ---------------------------------------------------
population <- census |>
  dplyr::group_by(
    department = departamento, province = provincia, district = distrito, ubigeo
  ) |>
  dplyr::summarise(
    # population = dplyr::n(),
    population_permanent = sum(ifelse(c5_p5 == 1, 1, 0)),
    .groups = "drop"
  ) |>
  dplyr::arrange(ubigeo)

tomerge_path <- "data/interim/to-merge/"
population_filename <- "01-population.csv"
population_filepath <- fs::path(tomerge_path, population_filename)
readr::write_csv(population, population_filepath)

