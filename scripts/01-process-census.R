census_filepath <- "data/raw/2017-census/2017-census-people.sav"
census_raw <- haven::read_sav(
  census_filepath,
  # n_max = 100,
  col_select = c(
    ubigeo, c5_p4_1, c5_p5, c5_p14, c5_p15, c5_p15b_cod:c5_p15d_cod, c5_p16:c5_p17,
    c5_p23, c5_p23b_cod:c5_p23d_cod
  )
)

tb_filepath <- "data/interim/to-merge/00-tb.csv"
tb <- readr::read_csv(tb_filepath, col_types = "ccciic")
ubigeos <- unique(tb$ubigeo)

census <- dplyr::filter(census_raw, ubigeo %in% ubigeos)
rm(census_raw)

# Origin-destination data for students ------------------------------------
## c5_p5 == 1:  Vive permanentemente en este distrito: 1 (Sí)
## c5_p14 == 1:  Actualmente, asiste a algún colegio, instituto o universidad: 1 (Sí)
## !is.na(c5_p15):  La institución educativa al que asiste está ubicada: Valor no nulo
## c5_p15a_cod != 99:  Distrito - institución al que asiste: No es 99 (valor perdido)
## c5_p15d_cod == "": País - institución al que asiste: No hay data de país
od_study <- census |>
  dplyr::filter(
    c5_p5 == 1, c5_p14 == 1, !is.na(c5_p15), c5_p15a_cod != 99, c5_p15d_cod == ""
  ) |>
  dplyr::select(origin = ubigeo, c5_p15b_cod:c5_p15a_cod) |>
  tidyr::drop_na() |>
  dplyr::mutate(
    destination = paste(c5_p15b_cod, c5_p15c_cod, c5_p15a_cod, sep = "")
  ) |>
  dplyr::mutate(destination = ifelse(destination == "", origin, destination)) |>
  dplyr::filter(destination %in% ubigeos) |>
  dplyr::group_by(origin, destination) |>
  dplyr::summarise(cases = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(origin, destination, cases)

od_study_filepath <- "data/interim/origin-destination-study.csv"
readr::write_csv(od_study, od_study_filepath)
rm(od_study)

# Origin-destination data for workers -------------------------------------
## c5_p5 == 1:  Vive permanentemente en este distrito: 1 (Sí)
## c5_p4_1 >= 14: Edad en años: Mayor o igual a 14
## c5_p16 == 1: La semana pasada, ¿trabajó por algún pago en dinero o especie?: 1 (Sí)
## !is.na(c5_p23):  Su centro de trabajo está ubicado: Valor no nulo
## c5_p23a_cod != 99: Distrito donde está ubicado su centro trabajo: No es 99 (valor perdido)
## c5_p23d_cod == "": País donde está ubicado su centro trabajo: No hay data de país
od_work <- census |>
  dplyr::filter(
    c5_p5 == 1, c5_p4_1 >= 14, c5_p16 == 1, !is.na(c5_p23), c5_p23a_cod != 99,
    c5_p23d_cod == ""
    # !(c5_p17 %in% c(6, 7)),
    #
  ) |>
  dplyr::select(origin = ubigeo, c5_p23b_cod:c5_p23a_cod) |>
  tidyr::drop_na() |>
  dplyr::mutate(
    destination = paste(c5_p23b_cod, c5_p23c_cod, c5_p23a_cod, sep = "")
  ) |>
  dplyr::mutate(destination = ifelse(destination == "", origin, destination)) |>
  dplyr::filter(destination %in% ubigeos) |>
  dplyr::group_by(origin, destination) |>
  dplyr::summarise(cases = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(origin, destination, cases)

od_work_filepath <- "data/interim/origin-destination-work.csv"
readr::write_csv(od_work, od_work_filepath)
rm(od_work)

# Origin-destination data for students and workers ------------------------
student_worker <- census |>
  dplyr::filter(c5_p5 == 1) |>
  dplyr::select(ubigeo, c5_p4_1, c5_p14:c5_p17, c5_p23:c5_p23d_cod) |>
  dplyr::mutate(
    student = ifelse(
      c5_p14 == 1 & !is.na(c5_p15) & c5_p15a_cod != 99 & c5_p15d_cod == "", 1, 0
    ),
    worker = ifelse(
      c5_p4_1 >= 14 & c5_p16 == 1 & !is.na(c5_p23) & c5_p23a_cod != 99 &
        c5_p23d_cod == "",
      1, 0
    )
  ) |>
  dplyr::filter(student == 1 | worker == 1) |>
  dplyr::mutate(id = dplyr::row_number())

od_raw <- student_worker |>
  dplyr::select(
    origin = ubigeo, c5_p15b_cod:c5_p15a_cod, c5_p23b_cod:c5_p23a_cod, student,
    worker, id
  ) |>
  dplyr::mutate(
    destination_study = paste(c5_p15b_cod, c5_p15c_cod, c5_p15a_cod, sep = ""),
    destination_work = paste(c5_p23b_cod, c5_p23c_cod, c5_p23a_cod, sep = "")
  ) |>
  dplyr::mutate(
    destination_study = ifelse(student == 0, NA, destination_study),
    destination_work = ifelse(worker == 0, NA, destination_work)
  ) |>
  dplyr::select(-c(c5_p15b_cod:c5_p23a_cod)) |>
  tidyr::pivot_longer(
    cols = destination_study:destination_work,
    names_to = "type",
    names_pattern = "destination_(.*)",
    values_to = "destination"
  ) |>
  tidyr::drop_na(destination) |>
  dplyr::mutate(destination = ifelse(destination == "", origin, destination)) |>
  dplyr::filter(destination %in% ubigeos)

od <- od_raw |>
  dplyr::group_by(origin, destination) |>
  dplyr::summarise(cases = dplyr::n_distinct(id), .groups = "drop")

od_filepath <- "data/interim/origin-destination-total.csv"
readr::write_csv(od, od_filepath)
rm(od_raw)
rm(od)
rm(student_worker)

# Permanent population  ---------------------------------------------------
population <- census |>
  dplyr::group_by(ubigeo) |>
  dplyr::summarise(
    # population = dplyr::n(),
    population_permanent = sum(ifelse(c5_p5 == 1, 1, 0)),
    .groups = "drop"
  ) |>
  dplyr::arrange(ubigeo)

population_filepath <- "data/interim/to-merge/01-population.csv"
readr::write_csv(population, population_filepath)

