library(tidygraph)
library(igraph)

nodes <- readr::read_csv(
  "data/processed/nodes.csv", col_types = "cciiii"
)
edges <- readr::read_csv("data/processed/edges.csv", col_types = "cccd")

edges <- edges |>
  dplyr::group_by(type) |>
  dplyr::filter(origin != destination) |>
  dplyr::ungroup()

get_node_properties <- function(graph, weight) {
  graph_centralities <- graph |>
    activate(nodes) |>
    mutate(
      centrality_degree_in = degree(
        graph, mode = "in", normalized = TRUE
      ),
      centrality_degree_out = degree(
        graph, mode = "out", normalized = TRUE
      ),
      centrality_degree_all = degree(
        graph, mode = "all", normalized = TRUE
      ),
      centrality_strength_in = strength(
        graph, mode = "in"
      ),
      centrality_strength_out = strength(
        graph, mode = "out"
      ),
      centrality_strength_all = strength(
        graph, mode = "all"
      ),
      centrality_closeness_in = closeness(
        graph, weights = 1 / E(graph)$weight, mode = "in", normalized = TRUE
      ),
      centrality_closeness_out = closeness(
        graph, weights = 1 / E(graph)$weight, mode = "out", normalized = TRUE
      ),
      centrality_closeness_all = closeness(
        graph, weights = 1 / E(graph)$weight, mode = "all", normalized = TRUE
      ),
      centrality_betweenness_weighted = betweenness(
        graph, weights = 1 / E(graph)$weight, directed = TRUE, normalized = TRUE
      ),
      centrality_eigenvector_weighted = eigen_centrality(
        graph, weights = 1 / E(graph)$weight, directed = TRUE, scale = TRUE
      )$vector,
      centrality_pagerank_weighted = page_rank(
        graph, weights = 1 / E(graph)$weight, directed = TRUE
      )$vector,
      structural_eccentricity_in = eccentricity(
        graph, mode = "in"
      ),
      structural_eccentricity_out = eccentricity(
        graph, mode = "out"
      ),
      structural_eccentricity_all = eccentricity(
        graph, mode = "all"
      ),
      structural_transitivity_local = transitivity(
        as.undirected(graph, mode = "collapse", edge.attr.comb = "sum"),
        type = "local"
      ),
      structural_transitivity_weighted = transitivity(
        as.undirected(graph, mode = "collapse", edge.attr.comb = "sum"),
        type = "barrat"
      ),
      structural_knn_outout = knn(
        graph, mode = "out", neighbor.degree.mode = "out"
      )$knn,
      structural_knn_outin = knn(
        graph, mode = "out", neighbor.degree.mode = "in"
      )$knn,
      structural_knn_inout = knn(
        graph, mode = "in", neighbor.degree.mode = "out"
      )$knn,
      structural_knn_inin = knn(
        graph, mode = "in", neighbor.degree.mode = "in"
      )$knn,
      structural_constraint_weighted = constraint(
        graph
      )
    ) |>
    tibble::as_tibble()
}

graphs <- edges |>
  tidyr::nest(edges = -type) |>
  dplyr::mutate(
    graph = purrr::map(
      edges, \(x) tbl_graph(
        nodes = nodes, edges = x, directed = TRUE, node_key = "ubigeo"
      )
    ),
    node_properties = purrr::map(graph, \(x) get_node_properties(x, weight = weight))
  )

districts_long <- graphs |>
  tidyr::unnest(node_properties) |>
  dplyr::select(-c(edges, graph))

districts <- districts_long |>
  tidyr::pivot_wider(
    id_cols = ubigeo:work_centrality_strength_intra, names_from = type,
    names_glue = "{type}_{.value}",
    values_from = centrality_degree_in:structural_constraint_weighted
  )

readr::write_csv(districts, "data/processed/districts.csv", na = "")

