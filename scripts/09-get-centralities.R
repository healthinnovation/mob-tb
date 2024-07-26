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
  distance = E(adj_matrix)$weight
)

get_flow_totals <- function(graph, weight) {
  graph <- graph |>
    activate(edges) |>
    rename(weight = {{weight}})

  graph_totals <- graph |>
    activate(nodes) |>
    mutate(
      mass = centrality_degree(
        weights = weight, mode = "out", loops = TRUE
      ),
      flow_in = centrality_degree(
        weights = weight, mode = "in", loops = FALSE
      ),
      flow_out = centrality_degree(
        weights = weight, mode = "out", loops = FALSE
      )
    ) |>
    tibble::as_tibble()
}

graphs_flow_totals <- edges |>
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

edges_gravity <- edges |>
  dplyr::left_join(edge_dist, by = c("origin", "destination")) |>
  tidyr::replace_na(list(distance = 0)) |>
  dplyr::left_join(masses, by = c("type", "origin" = "ubigeo")) |>
  dplyr::left_join(
    masses, by = c("type", "destination" = "ubigeo"),
    suffix = c("_origin", "_destination")
  ) |>
  dplyr::mutate(
    gravity = (flow * (distance^2)) / (mass_origin * mass_destination)
  ) |>
  dplyr::select(-c(flow:mass_destination))

# edges_weights <- edges_weights |>
#   dplyr::group_by(type) |>
#   dplyr::filter(origin != destination) |>
#   dplyr::ungroup()

get_centralities <- function(graph, weight) {
  graph <- graph |>
    activate(edges) |>
    rename(weight = {{weight}}) |>
    simplify(remove.multiple = FALSE, remove.loops = TRUE) |>
    as_tbl_graph()

  graph_properties <- graph |>
    activate(nodes) |>
    mutate(
      centrality_degree_in = centrality_degree(mode = "in", loops = FALSE),
      centrality_degree_out = centrality_degree(mode = "out", loops = FALSE),
      centrality_degree_all = centrality_degree(mode = "all", loops = FALSE),
      centrality_strength_in = centrality_degree(
        weights = weight, mode = "in", loops = TRUE
      ),
      centrality_strength_out = centrality_degree(
        weights = weight, mode = "out", loops = TRUE
      ),
      centrality_strength_all = centrality_degree(
        weights = weight, mode = "all", loops = TRUE
      ),
      centrality_closeness_in = centrality_closeness(
        weights = 1/weight, mode = "in", normalized = TRUE
      ),
      centrality_closeness_out = centrality_closeness(
        weights = 1/weight, mode = "out", normalized = TRUE
      ),
      centrality_closeness_all = centrality_closeness(
        weights = 1/weight, mode = "all", normalized = TRUE
      ),
      centrality_betweenness_weighted = centrality_betweenness(
        weights = 1/weight, directed = TRUE, normalized = TRUE
      ),
      centrality_authority_weighted = centrality_authority(
        weights = weight, scale = TRUE
      ),
      centrality_hub_weighted = centrality_hub(
        weights = weight, scale = TRUE
      ),
      centrality_pagerank_weighted = centrality_pagerank(
        weights = weight, directed = TRUE
      )
    ) |>
    tibble::as_tibble()
}

graphs_centralities <- edges_gravity |>
  tidyr::nest(edges = -type) |>
  dplyr::mutate(
    graph = purrr::map(
      edges, \(x) tbl_graph(
        nodes = nodes, edges = x, directed = TRUE, node_key = "ubigeo"
      )
    ),
    centralities = purrr::map(
      graph, \(x) get_centralities(x, weight = gravity)
    )
  )

centralities <- graphs_centralities |>
  tidyr::unnest(centralities) |>
  dplyr::select(-c(edges, graph)) |>
  dplyr::relocate(type, .before = region)

readr::write_csv(centralities, "data/processed/centralities.csv", na = "")

