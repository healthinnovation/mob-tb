library(dplyr)
library(tidygraph)

processed_path <- fs::path("data/processed/")

nodes_filename <- "nodes.csv"
nodes_filepath <- fs::path(processed_path, nodes_filename)
nodes <- readr::read_csv(nodes_filepath, col_types = "ccccd")

edges_filename <- "edges.csv"
edges_filepath <- fs::path(processed_path, edges_filename)
edges <- readr::read_csv(edges_filepath, col_types = "cccd")

get_centralities <- function(graph, weight) {
  graph_centralities <- graph %>%
    mutate(
      centrality_degree_in = centrality_degree(
        mode = "in", normalized = TRUE
      ),
      centrality_degree_out = centrality_degree(
        mode = "out", normalized = TRUE
      ),
      centrality_degree_all = centrality_degree(
        mode = "all", normalized = TRUE
      ),
      centrality_strength_in = centrality_degree(
        weights = {{weight}}, mode = "in", normalized = TRUE
      ),
      centrality_strength_out = centrality_degree(
        weights = {{weight}}, mode = "out", normalized = TRUE
      ),
      centrality_strength_all = centrality_degree(
        weights = {{weight}}, mode = "all", normalized = TRUE
      ),
      centrality_closeness_in = centrality_closeness(
        weights = 1 / {{weight}}, mode = "in", normalized = TRUE
      ),
      centrality_closeness_out = centrality_closeness(
        weights = 1 / {{weight}}, mode = "out", normalized = TRUE
      ),
      centrality_closeness_all = centrality_closeness(
        weights = 1 / {{weight}}, mode = "all", normalized = TRUE
      ),
      centrality_betweenness_directed = centrality_betweenness(
        weights = 1 / {{weight}}, directed = TRUE, normalized = TRUE
      ),
      centrality_eigenvector_directed = centrality_eigen(
        weights = {{weight}}, directed = TRUE, scale = TRUE
      ),
      centrality_pagerank_directed = centrality_pagerank(
        weights = {{weight}}, directed = TRUE
      )
    ) %>%
    activate(nodes) %>%
    tibble::as_tibble()
}

centralities_nested <- edges %>%
  tidyr::nest(edges = -source) %>%
  mutate(
    graph = purrr::map(
      edges, ~ tbl_graph(
        nodes = nodes, edges = .x, directed = TRUE, node_key = "ubigeo"
      )
    ),
    centrality = purrr::map(graph, ~ get_centralities(.x, weight = cases))
  )

centralities_long <- centralities_nested %>%
  tidyr::unnest(centrality) %>%
  select(-c(edges, graph))

centralities <- centralities_long %>%
  tidyr::pivot_wider(
    id_cols = department:work_flow_intra, names_from = source,
    names_glue = "{source}_{.value}",
    values_from = centrality_degree_in:centrality_pagerank_directed
  )

centralities_filename <- "centralities.csv"
centralities_filepath <- fs::path(processed_path, centralities_filename)
readr::write_csv(centralities, centralities_filepath, na = "")


