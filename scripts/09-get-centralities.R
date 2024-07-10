library(tidygraph)
library(igraph)

nodes <- readr::read_csv(
  "data/processed/network/nodes.csv", col_types = "cccciic"
)
edges <- readr::read_csv("data/processed/network/edges.csv", col_types = "cccd")

districts_raw <- sf::read_sf("data/raw/Distrito.gpkg")
districts <- districts_raw |>
  filter(ubigeo %in% nodes$ubigeo) |>
  select(ubigeo, geom) |>
  sf::st_transform(crs = 32718)

coords <- sf::st_centroid(districts)
dist_matrix <- sf::st_distance(coords)
rownames(dist_matrix) <- districts$ubigeo
colnames(dist_matrix) <- districts$ubigeo
adj_matrix <- graph.adjacency(dist_matrix, mode = "directed", weighted = TRUE)
edge_list <- get.edgelist(adj_matrix)

edge_dist <- tibble::tibble(
  origin = edge_list[, 1], destination = edge_list[, 2],
  distance = E(adj_matrix)$weight / 1000
)

get_flow_totals <- function(graph, weight) {
  graph <- graph |>
    activate(edges) |>
    mutate(weight = {{weight}})

  graph_totals <- graph |>
    activate(nodes) |>
    mutate(
      mass = strength(
        graph, mode = "out", loops = TRUE
      ),
      flow_in = strength(
        graph, mode = "in", loops = FALSE
      ),
      flow_out = strength(
        graph, mode = "out", loops = FALSE
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
    flow_totals = purrr::map(graph, \(x) get_flow_totals(x, weight = flow)),

  )

masses <- graphs |>
  dplyr::select(type, flow_totals) |>
  tidyr::unnest(flow_totals) |>
  dplyr::select(type, ubigeo, mass)

edges_weights <- edges |>
  dplyr::left_join(edge_dist, by = c("origin", "destination")) |>
  tidyr::replace_na(list(distance = 0)) |>
  dplyr::left_join(masses, by = c("type", "origin" = "ubigeo")) |>
  dplyr::left_join(
    masses, by = c("type", "destination" = "ubigeo"),
    suffix = c("_origin", "_destination")
  ) |>
  dplyr::mutate(
    gravity_0 = flow / (mass_origin * mass_destination),
    gravity_1 = (flow * (distance^2)) / (mass_origin * mass_destination)
  )

edges_weights |>
  dplyr::filter(distance > 0, type == "study") |>
  View()

# edges <- edges |>
#   dplyr::group_by(type) |>
#   dplyr::filter(origin != destination) |>
#   dplyr::ungroup()

get_node_properties <- function(graph, weight) {
  graph_centralities <- graph |>
    activate(nodes) |>
    mutate(
      centrality_degree_in = degree(
        graph, mode = "in", loops = TRUE, normalized = TRUE
      ),
      centrality_degree_out = degree(
        graph, mode = "out", loops = TRUE, normalized = TRUE
      ),
      centrality_degree_all = degree(
        graph, mode = "all", loops = TRUE, normalized = TRUE
      ),
      centrality_strength_in = strength(
        graph, mode = "in", loops = TRUE
      ),
      centrality_strength_out = strength(
        graph, mode = "out", loops = TRUE
      ),
      centrality_strength_all = strength(
        graph, mode = "all", loops = TRUE
      ),
      centrality_closeness_in = closeness(
        graph, mode = "in", weights = 1 / E(graph)$weight, normalized = TRUE
      ),
      centrality_closeness_out = closeness(
        graph, mode = "out", weights = 1 / E(graph)$weight, normalized = TRUE
      ),
      centrality_closeness_all = closeness(
        graph, mode = "all", weights = 1 / E(graph)$weight,  normalized = TRUE
      ),
      centrality_betweenness_weighted = betweenness(
        graph, directed = TRUE, weights = 1 / E(graph)$weight, normalized = TRUE
      ),
      centrality_eigenvector_weighted = eigen_centrality(
        graph, directed = TRUE, scale = TRUE, weights = 1 / E(graph)$weight
      )$vector,
      centrality_pagerank_weighted = page_rank(
        graph, directed = TRUE, weights = 1 / E(graph)$weight
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
        simplify(
          graph, remove.multiple = TRUE, remove.loops = TRUE,
          edge.attr.comb = "sum"
        ),
        mode = "out", neighbor.degree.mode = "out"
      )$knn,
      structural_knn_outin = knn(
        simplify(
          graph, remove.multiple = TRUE, remove.loops = TRUE,
          edge.attr.comb = "sum"
        ),
        mode = "out", neighbor.degree.mode = "in"
      )$knn,
      structural_knn_inout = knn(
        simplify(
          graph, remove.multiple = TRUE, remove.loops = TRUE,
          edge.attr.comb = "sum"
        ),
        mode = "in", neighbor.degree.mode = "out"
      )$knn,
      structural_knn_inin = knn(
        simplify(
          graph, remove.multiple = TRUE, remove.loops = TRUE,
          edge.attr.comb = "sum"
        ),
        mode = "in", neighbor.degree.mode = "in"
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

centralities <- graphs |>
  tidyr::unnest(node_properties) |>
  dplyr::select(-c(edges, graph)) |>
  dplyr::relocate(type, .before = region)

# centralities <- centralities_long |>
#   tidyr::pivot_wider(
#     id_cols = ubigeo:region, names_from = type,
#     names_glue = "{type}_{.value}",
#     values_from = -c(type, ubigeo:region)
#   )

readr::write_csv(centralities, "data/processed/centralities.csv", na = "")

