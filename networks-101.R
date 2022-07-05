library(igraph)

# Undirected graph
g <- graph_from_literal(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)

# Order
V(g)

# Size
E(g)

# Graph identifier
graph_id(g)

print_all(g)

# Plot
plot(g)

# Directed graph
dg <- graph_from_literal(1-+2, 1-+3, 2++3)
plot(dg)

dg <- graph_from_literal(Sam-+Mary, Sam-+Tom, Mary++Tom)
print_all(dg)

dg <- graph_from_literal(1-+2, 1-+3, 2++3)
print_all(dg)

V(dg)$name <- c("Sam", "Mary", "Tom")
print_all(dg)

# Edge list
as_edgelist(dg)

# Adjacency matrix
as_adjacency_matrix(g)
