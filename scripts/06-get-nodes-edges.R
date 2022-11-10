library(fs)
library(dplyr)
library(readr)
library(scales)

interim_path <- path("data/interim/")
od_filename <- "origin-destination.csv"
od_filepath <- path(interim_path, od_filename)
od <- read_csv(od_filepath, col_types = "ccd")

lima_filename <- "lima.csv"
lima_filepath <- path(interim_path, lima_filename)
lima <- read_csv(
  lima_filepath, col_types = "ccccddddddddddddddddddddddddd"
)

intra_mob <- od %>%
  filter(origin == destination) %>%
  select(ubigeo = origin, cases)

total_mob <- od %>%
  group_by(ubigeo = origin) %>%
  summarise(cases = sum(cases), .groups = "drop")

mob <- total_mob %>%
  inner_join(intra_mob, by = "ubigeo", suffix = c("_total", "_intra")) %>%
  mutate(intra_mob = cases_intra / cases_total) %>%
  select(ubigeo, intra_mob)

nodes <- lima %>%
  inner_join(mob, by = "ubigeo") %>%
  select(-population) %>%
  mutate(across(aai:tmmx, ~ rescale(.x)))

inter_mob <- od %>%
  filter(origin != destination) %>%
  group_by(origin) %>%
  summarise(total_cases = sum(cases))

edges <- od %>%
  filter(origin != destination) %>%
  filter(origin %in% nodes$ubigeo, destination %in% nodes$ubigeo) %>%
  inner_join(inter_mob, by = "origin") %>%
  mutate(weight = cases / total_cases) %>%
  select(-c(cases, total_cases))

processed_path <- path("data/processed/")
edges_filename <- "edges.csv"
edges_filepath <- path(processed_path, edges_filename)
write_csv(edges, edges_filepath, na = "")

nodes_filename <- "nodes.csv"
nodes_filepath <- path(processed_path, nodes_filename)
write_csv(nodes, nodes_filepath, na = "")
