library(purrr)
library(tidyverse)
library(ggplot2)
library(readr)
library(survey)
library(cowplot)
library(patchwork)
library(readxl)
library(parameters)
library(ggbiplot)

nodes <- read_csv("data/processed/network/nodes.csv")

nodes_pca <- nodes %>%
  select(hdi, monetary_poverty, ubn)

principal_components(
  nodes_pca,
  n = "auto",
  sparse = FALSE,
  sort = FALSE,
  threshold = NULL,
  standardize = TRUE
)

principal_components(
  nodes_pca,
  n = "all",
  sparse = FALSE,
  sort = FALSE,
  threshold = NULL,
  standardize = TRUE
)

#R base:

my_pca <- prcomp(nodes_pca, scale = TRUE,
                 center = TRUE, retx = T)
names(my_pca)

# Summary
summary(my_pca)
my_pca

library(factoextra)

fviz_contrib(my_pca, choice = "var", axes = 1, top = 10)

my_pca$rotation

#Visualización

# ggbiplot(my_pca, groups = nodes$distrito)

my_pca$rotation

pca_values <- as.data.frame(my_pca$x)$PC1
pca <- tibble(ubigeo = nodes$ubigeo, pc1 = pca_values)
write_csv(pca, "data/interim/fixed/05-pca.csv")

