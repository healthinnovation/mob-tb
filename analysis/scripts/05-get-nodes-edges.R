library(fs)
library(dplyr)
library(readr)
library(scales)

interim_path <- path("analysis/data/interim/")
od_filename <- "od-work-study.csv"
od_filepath <- path(interim_path, od_filename)
od_full <- read_csv(od_filepath, col_types = "ccd")

nodes_full_filename <- "nodes-full.csv"
nodes_full_filepath <- path(interim_path, nodes_full_filename)
nodes_full <- read_csv(
  nodes_full_filepath, col_types = "ccccddddddddddddddddddddddddd"
)

intra_mob <- od_full %>%
  filter(origin == destination) %>%
  select(ubigeo = origin, cases)

total_mob <- od_full %>%
  group_by(ubigeo = origin) %>%
  summarise(cases = sum(cases), .groups = "drop")

mob <- total_mob %>%
  inner_join(intra_mob, by = "ubigeo", suffix = c("_total", "_intra")) %>%
  mutate(intra_mob = cases_intra / cases_total) %>%
  select(ubigeo, intra_mob)

nodes <- nodes_full %>%
  filter(province == "LIMA") %>%
  inner_join(mob, by = "ubigeo") %>%
  select(-population) %>%
  mutate(
    across(aai:tmmx, ~ rescale(.x))
  )

inter_mob <- od_full %>%
  filter(origin != destination) %>%
  group_by(origin) %>%
  summarise(total_cases = sum(cases))

edges <- od_full %>%
  filter(origin %in% nodes$ubigeo, destination %in% nodes$ubigeo) %>%
  filter(origin != destination) %>%
  inner_join(inter_mob, by = "origin") %>%
  mutate(weight = 100 * (cases / total_cases)) %>%
  select(-c(cases, total_cases))

processed_path <- path("analysis/data/processed/")
edges_filename <- "edges.csv"
edges_filepath <- path(processed_path, edges_filename)
write_csv(edges, edges_filepath, na = "")

nodes_filename <- "nodes.csv"
nodes_filepath <- path(processed_path, nodes_filename)
write_csv(nodes, nodes_filepath, na = "")
