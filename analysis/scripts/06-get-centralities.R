library(fs)
library(dplyr)
library(readr)
library(tidygraph)

processed_path <- fs::path("analysis/data/processed/")
nodes_filename <- "nodes.csv"
nodes_filepath <- fs::path(processed_path, nodes_filename)
nodes <- read_csv(nodes_filepath, col_types = "cccc")

edges_filename <- "edges.csv"
edges_filepath <- fs::path(processed_path, edges_filename)
edges <- read_csv(edges_filepath, col_types = "ccd")

network <- tbl_graph(
  node_key = "ubigeo", nodes = nodes, edges = edges, directed = TRUE
)

network_order <- igraph::gorder(network)

centralities_raw <- network %>%
  activate(nodes) %>%
  mutate(
    in_degree = centrality_degree(mode = "in", normalized = TRUE),
    out_degree = centrality_degree(mode = "out"),
    degree = centrality_degree(mode = "all"),
    in_strength = centrality_degree(weights = weight, mode = "in"),
    out_strength = centrality_degree(weights = weight, mode = "out"),
    strength = centrality_degree(weights = weight, mode = "all"),
    in_closeness = centrality_closeness(weights = weight, mode = "in"),
    out_closeness = centrality_closeness(weights = weight, mode = "out"),
    closeness = centrality_closeness(weights = weight, mode = "all"),
    betweenness = centrality_betweenness(weights = weight),
    # eigenvector = centrality_eigen(weights = weight, directed = TRUE), #   PageRank may be better for directed graphs
    # https://lists.nongnu.org/archive/html/igraph-help/2015-11/msg00020.html
    page_rank = centrality_pagerank(weights = weight),
    alpha = centrality_alpha(weights = weight),
    authority = centrality_authority(weights = weight),
    hub = centrality_hub(weights = weight)
  ) %>%
  activate(nodes) %>%
  as_tibble()

centralities <- centralities_raw %>%
  mutate(across(in_degree:hub, ~ scales::rescale(.x)))

centralities_filename <- "mob-tb.csv"
centralities_filepath <- path(processed_path, centralities_filename)
write_csv(centralities, centralities_filepath, na = "")


