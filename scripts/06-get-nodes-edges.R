library(dplyr)

od_total = readr::read_csv(
  "data/interim/origin-destination.csv", col_types = "ccd"
)
od_study = readr::read_csv(
  "data/interim/origin-destination-study.csv", col_types = "ccd"
)
od_work = readr::read_csv(
  "data/interim/origin-destination-work.csv", col_types = "ccd"
)

districts = readr::read_csv(
  "data/interim/districts.csv", col_types = "ccdddddddddcc"
)

od = bind_rows(
  list(total = od_total, study = od_study, work = od_work), .id = "type"
)

od_lima = od |>
  filter(origin %in% districts$ubigeo, destination %in% districts$ubigeo)

flow_intra = od_lima |>
  group_by(type) |>
  filter(origin == destination) |>
  select(type, ubigeo = origin, centrality_strength_intra = cases) |>
  ungroup()

# od_inter = od_lima |>
  # group_by(type) |>
  # filter(origin != destination) |>
  # ungroup()

# flow_in = od_inter |>
#   group_by(type, ubigeo = destination) |>
#   summarise(flow_in = sum(cases), .groups = "drop")

# flow_out = od_inter |>
#   group_by(type, ubigeo = origin) |>
#   summarise(flow_out = sum(cases), .groups = "drop")
#
# commuting_flow = flow_intra |>
#   left_join(flow_out, by = c("type", "ubigeo")) |>
#   tidyr::replace_na(list(flow_out = 0))
#
# commuting_flow_rate = commuting_flow |>
#   mutate(rate_intra = flow_intra / (flow_intra + flow_out))

# flow_total = flow_intra + flow_out

# commuting_flows_rates_wide = commuting_flows_rates |>
#   tidyr::pivot_wider(
#     id_cols = ubigeo, names_from = type, names_glue = "{type}_{.value}",
#     values_from = flow_intra:rate_out
#   )

flow_intra_wide = flow_intra |>
  tidyr::pivot_wider(
    id_cols = ubigeo, names_from = type, names_glue = "{type}_{.value}",
    values_from = centrality_strength_intra
  )

nodes = districts |>
  inner_join(flow_intra_wide, by = "ubigeo")

# Weight is calculated using out flow to every other district in Lima

# edges = od_inter |>
#   group_by(type) |>
#   inner_join(flow_out, by = c("type", "origin" = "ubigeo")) |>
#   mutate(
#     weight = cases / flow_out
#   ) |>
#   select(c(type, origin, destination, weight)) |>
#   ungroup()

edges = rename(od_lima, weight = cases)

readr::write_csv(edges, "data/processed/edges.csv", na = "")
readr::write_csv(nodes, "data/processed/nodes.csv", na = "")


