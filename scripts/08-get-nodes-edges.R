od_total <- readr::read_csv(
  "data/interim/origin-destination-total.csv", col_types = "ccd"
)
od_study <- readr::read_csv(
  "data/interim/origin-destination-study.csv", col_types = "ccd"
)
od_work <- readr::read_csv(
  "data/interim/origin-destination-work.csv", col_types = "ccd"
)

districts <- readr::read_csv(
  "data/interim/districts.csv", col_types = "cicccccidddddd"
)

od <- dplyr::bind_rows(
  list(total = od_total, study = od_study, work = od_work), .id = "type"
)

od_lima <- od |>
  dplyr::filter(origin %in% districts$ubigeo, destination %in% districts$ubigeo)

flow_intra <- od_lima |>
  dplyr::group_by(type) |>
  dplyr::filter(origin == destination) |>
  dplyr::select(type, ubigeo = origin, centrality_strength_intra = cases) |>
  dplyr::ungroup()

flow_intra_wide <- flow_intra |>
  tidyr::pivot_wider(
    id_cols = ubigeo, names_from = type, names_glue = "{type}_{.value}",
    values_from = centrality_strength_intra
  )

nodes <- districts |>
  dplyr::inner_join(flow_intra_wide, by = "ubigeo")

edges <- dplyr::rename(od_lima, weight = cases)

readr::write_csv(edges, "data/processed/edges.csv", na = "")
readr::write_csv(nodes, "data/processed/nodes.csv", na = "")

