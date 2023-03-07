library(fs)
library(dplyr)
library(readr)
library(scales)

interim_path <- path("data/interim/")

od_full_filename <- "origin-destination.csv"
od_full_filepath <- path(interim_path, od_full_filename)
od_full <- read_csv(od_full_filepath, col_types = "ccd")

od_study_filename <- "origin-destination-study.csv"
od_study_filepath <- path(interim_path, od_study_filename)
od_study <- read_csv(od_study_filepath, col_types = "ccd")

od_work_filename <- "origin-destination-work.csv"
od_work_filepath <- path(interim_path, od_work_filename)
od_work <- read_csv(od_work_filepath, col_types = "ccd")

lima_filename <- "lima.csv"
lima_filepath <- path(interim_path, lima_filename)
lima <- read_csv(lima_filepath, col_types = "ccccd")

od <- bind_rows(
  list(full = od_full, study = od_study, work = od_work), .id = "source"
)

od_lima <- od %>%
  filter(origin %in% lima$ubigeo, destination %in% lima$ubigeo)

flow_intra <- od_lima %>%
  group_by(source) %>%
  filter(origin == destination) %>%
  select(source, ubigeo = origin, flow_intra = cases) %>%
  ungroup()

od_inter <- od_lima %>%
  group_by(source) %>%
  filter(origin != destination) %>%
  ungroup()

# flow_in <- od_inter %>%
#   group_by(source, ubigeo = destination) %>%
#   summarise(flow_in = sum(cases), .groups = "drop")

# flow_out <- od_inter %>%
#   group_by(source, ubigeo = origin) %>%
#   summarise(flow_out = sum(cases), .groups = "drop")
#
# commuting_flow <- flow_intra %>%
#   left_join(flow_out, by = c("source", "ubigeo")) %>%
#   tidyr::replace_na(list(flow_out = 0))
#
# commuting_flow_rate <- commuting_flow %>%
#   mutate(rate_intra = flow_intra / (flow_intra + flow_out))

# flow_total = flow_intra + flow_out

# commuting_flows_rates_wide <- commuting_flows_rates %>%
#   tidyr::pivot_wider(
#     id_cols = ubigeo, names_from = source, names_glue = "{source}_{.value}",
#     values_from = flow_intra:rate_out
#   )

flow_intra_wide <- flow_intra %>%
  tidyr::pivot_wider(
    id_cols = ubigeo, names_from = source, names_glue = "{source}_{.value}",
    values_from = flow_intra
  )

nodes <- lima %>%
  inner_join(flow_intra_wide, by = "ubigeo")

# Weight is calculated using out flow to every other district in Lima

# edges <- od_inter %>%
#   group_by(source) %>%
#   inner_join(flow_out, by = c("source", "origin" = "ubigeo")) %>%
#   mutate(
#     weight = cases / flow_out
#   ) %>%
#   select(c(source, origin, destination, weight)) %>%
#   ungroup()

edges <- od_inter

processed_path <- path("data/processed/")

edges_filename <- "edges.csv"
edges_filepath <- path(processed_path, edges_filename)
write_csv(edges, edges_filepath, na = "")

nodes_filename <- "nodes.csv"
nodes_filepath <- path(processed_path, nodes_filename)
write_csv(nodes, nodes_filepath, na = "")


