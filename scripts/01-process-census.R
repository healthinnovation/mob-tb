census_filepath <- "data/raw/2017-census/2017-census-people.sav"
census_raw <- haven::read_sav(
  census_filepath,
  # n_max = 100,
  col_select = c(
    ubigeo:distrito, area, c5_p4_1, c5_p5, c5_p14, c5_p15,
    c5_p15b_cod:c5_p15d_cod, c5_p16:c5_p17, c5_p23, c5_p23b_cod:c5_p23d_cod
  )
)

census <- dplyr::filter(census_raw, ccdd %in% c("07", "15"), ccpp == "01")

# Origin-destination data for students ------------------------------------
## c5_p5 == 1:  Vive permanentemente en este distrito: 1 (Sí)
## c5_p14 == 1:  Actualmente, asiste a algún colegio, instituto o universidad: 1 (Sí)
## !is.na(c5_p15):  La institución educativa al que asiste está ubicada: Valor no nulo
## c5_p15a_cod != 99:  Distrito - institución al que asiste: No es 99 (valor perdido)
## c5_p15d_cod == "": País - institución al que asiste: No hay data de país
od_study <- census |>
  dplyr::filter(
    c5_p5 == 1, c5_p14 == 1, !is.na(c5_p15), c5_p15b_cod %in% c("07", "15", ""),
    c5_p15c_cod %in% c("01", ""), c5_p15a_cod != 99, c5_p15d_cod == ""
  ) |>
  dplyr::select(origin = ubigeo, c5_p15b_cod:c5_p15a_cod) |>
  tidyr::drop_na() |>
  dplyr::mutate(
    destination = paste(c5_p15b_cod, c5_p15c_cod, c5_p15a_cod, sep = "")
  ) |>
  dplyr::mutate(destination = ifelse(destination == "", origin, destination)) |>
  dplyr::group_by(origin, destination) |>
  dplyr::summarise(flow = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(origin, destination, flow)

# Origin-destination data for workers -------------------------------------
## c5_p5 == 1:  Vive permanentemente en este distrito: 1 (Sí)
## c5_p4_1 >= 14: Edad en años: Mayor o igual a 14
## c5_p16 == 1: La semana pasada, ¿trabajó por algún pago en dinero o especie?: 1 (Sí)
## !is.na(c5_p23):  Su centro de trabajo está ubicado: Valor no nulo
## c5_p23a_cod != 99: Distrito donde está ubicado su centro trabajo: No es 99 (valor perdido)
## c5_p23d_cod == "": País donde está ubicado su centro trabajo: No hay data de país
od_work <- census |>
  dplyr::filter(
    c5_p5 == 1, c5_p4_1 >= 14, c5_p16 == 1, !is.na(c5_p23),
    c5_p23b_cod %in% c("07", "15", ""), c5_p23c_cod %in% c("01", ""),
    c5_p23a_cod != 99, c5_p23d_cod == ""
  ) |>
  dplyr::select(origin = ubigeo, c5_p23b_cod:c5_p23a_cod) |>
  tidyr::drop_na() |>
  dplyr::mutate(
    destination = paste(c5_p23b_cod, c5_p23c_cod, c5_p23a_cod, sep = "")
  ) |>
  dplyr::mutate(destination = ifelse(destination == "", origin, destination)) |>
  # dplyr::filter(destination %in% ubigeos) |>
  dplyr::group_by(origin, destination) |>
  dplyr::summarise(flow = dplyr::n(), .groups = "drop") |>
  dplyr::arrange(origin, destination, flow)

# Origin-destination data for students and workers ------------------------
student_worker <- census |>
  dplyr::filter(c5_p5 == 1) |>
  dplyr::select(ubigeo, c5_p4_1, c5_p14:c5_p17, c5_p23:c5_p23d_cod) |>
  dplyr::mutate(
    student = ifelse(
      c5_p14 == 1 & !is.na(c5_p15) & c5_p15b_cod %in% c("07", "15", "") &
      c5_p15c_cod %in% c("01", "") & c5_p15a_cod != 99 & c5_p15d_cod == "", 1, 0
    ),
    worker = ifelse(
      c5_p4_1 >= 14 & c5_p16 == 1 & !is.na(c5_p23) &
        c5_p23b_cod %in% c("07", "15", "") & c5_p23c_cod %in% c("01", "") &
        c5_p23a_cod != 99 & c5_p23d_cod == "",
      1, 0
    )
  ) |>
  dplyr::filter(student == 1 | worker == 1) |>
  dplyr::mutate(id = dplyr::row_number())

od_total_raw <- student_worker |>
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
  dplyr::mutate(destination = ifelse(destination == "", origin, destination))

od_total <- od_total_raw |>
  dplyr::group_by(origin, destination) |>
  dplyr::summarise(flow = dplyr::n_distinct(id), .groups = "drop")

rm(od_total_raw)
rm(student_worker)

edges <- dplyr::bind_rows(
  list(total = od_total, study = od_study, work = od_work), .id = "type"
)
readr::write_csv(edges, "data/processed/network/edges.csv")
rm(od_total)
rm(od_study)
rm(od_work)


# Permanent population  ---------------------------------------------------
north_lima <- c(
  "150102", "150106", "150110", "150112", "150117", "150125", "150135", "150139"
)

center_lima <- c(
  "150104", "150105", "150113", "150115", "150101", "150116", "150120", "150122",
  "150128", "150130", "150131", "150136", "150140", "150141", "150121"
)

south_lima <- c(
  "150108", "150119", "150123", "150124", "150126", "150127", "150129", "150133",
  "150138", "150142", "150143"
)

east_lima <- c(
  "150103", "150107", "150109", "150111", "150114", "150118", "150132", "150134",
  "150137"
)

districts <- census |>
  dplyr::group_by(ubigeo, departamento, provincia, distrito) |>
  dplyr::summarise(
    pop_2017 = dplyr::n(),
    pop_permanent_2017 = sum(ifelse(c5_p5 == 1, 1, 0)),
    # urban = sum(ifelse(area == 1, 1, 0)),
    # rural = sum(ifelse(area == 2, 1, 0)),
    .groups = "drop"
  ) |>
  dplyr::arrange(ubigeo) |>
  dplyr::mutate(
    region = dplyr::case_when(
      ubigeo %in% north_lima ~ "NORTHERN LIMA",
      ubigeo %in% center_lima ~ "CENTRAL LIMA",
      ubigeo %in% south_lima ~ "SOUTHERN LIMA",
      ubigeo %in% east_lima ~ "EASTERN LIMA",
      TRUE ~ "CALLAO"
    )
  )

districts_filepath <- "data/interim/fixed/00-districts.csv"
readr::write_csv(districts, districts_filepath)
