# Pacotes ----

library(ape)

library(sf)

library(tidyverse)

library(geiger)

library(picante)

library(phytools)

# Dados ----

## Árvore filogenética ----

### Importando ----

tree <- ape::read.tree("arvore.tre")

### Visualizando ----

tree$tip.label

tree %>% ape::plot.phylo(type = "fan",
                              show.tip.label = TRUE,
                              edge.color = "blue",
                              edge.width = 1.5,
                              tip.color = "black",
                              cex = 0.75,
                              label.offset = 0.001)


## Matriz de composição ----

### Importando ----

matriz <- readxl::read_xlsx("registros_finais.xlsx")

### Tratando ----

matriz_trat <- matriz %>%
  dplyr::select(6:177)

### Visualizando ----

matriz %>% dplyr::glimpse()

matriz

# Diversidade filogenética ----

## Alfa ----

picante::pd(matriz %>%
              dplyr::select())
