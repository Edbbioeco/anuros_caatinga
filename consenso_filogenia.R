# Pacotes ----

library(ape)

library(tidyverse)

library(phytools)

library(ggtree)

# Dados ----

## Árvore filogenética ----

### Importando ----

tree <- ape::read.nexus("output.nex")

### Calculando o consenso ----

tree_cons <- tree |>
  phytools::consensus.edges(if.absent = "ignore",
                            collapse = FALSE)

### Visualizando ----

tree_cons

tree_cons$tip.label

tree_cons |> ape::plot.phylo(type = "fan",
                             show.tip.label = TRUE,
                             edge.color = "blue",
                             edge.width = 1.5,
                             tip.color = "black",
                             cex = 0.45,
                             label.offset = 0.001)

# Histograma dos comprimentos dos ramos ----

tree_cons$edge.length |>
  data.frame() |>
  dplyr::rename("Comprimento dos ramos" = 1) |>
  ggplot(aes(`Comprimento dos ramos`)) +
  geom_histogram(color = 'black', binwidth = 5)

# Exportando ----

tree_cons |> ape::write.tree("tree_cons.phy")
