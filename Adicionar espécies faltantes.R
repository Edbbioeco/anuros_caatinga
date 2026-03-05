# Pacotes ----

library(ape)

library(tidyverse)

library(phytools)

library(ggtree)

# Dados ----

## Árvore filogenética ----

### Importando ----

tree <- ape::read.tree("tree_cons.phy")

### Visualizando ----

tree

tree$tip.label

tree |> ape::plot.phylo(type = "fan",
                        show.tip.label = TRUE,
                        edge.color = "blue",
                        edge.width = 1.5,
                        tip.color = "black",
                        cex = 0.45,
                        label.offset = 0.001)

## Espécies totais ----

### Importando ----

sinonimios <- readxl::read_xlsx("sinonimios.xlsx")

### Visualizando ----

sinonimios

sinonimios |> dplyr::glimpse()

# Tratando a filogenia ----

## Espécies que faltam ----

sps_filo <- tree_cons$tip.label

sps_filo

sps_faltam <- sinonimios |>
  dplyr::filter(`Está no VertLife` == "Não") |>
  dplyr:::pull(Sinonímio)

sps_faltam

novas_especies <- data.frame(especie = sps_faltam |> stringr::str_replace(" ", "_"),
                             genero = sps_faltam |> stringr::word(1))

novas_especies

## Adicionando ----

tree$tip.label

tree$tip.label |> length()

adicionar_sps <- function(id){

  tree <<- phytools::add.species.to.genus(tree |>
                                            phytools::force.ultrametric(),
                                          novas_especies$especie[id])

}

id <- 1:nrow(novas_especies)

id

purrr::map(id, adicionar_sps)

tree$tip.label

tree$tip.label |> length()

## Corrigindo o nome das espécies ----

sinonimios <- sinonimios |>
  dplyr::mutate(Espécie = Espécie |>  stringr::str_replace(" ", "_"),
                Sinonímio = Sinonímio |> stringr::str_replace(" ", "_"),
                Sinonímio = Sinonímio |> factor(levels = tree$tip.label)) |>
  dplyr::arrange(Sinonímio)

sinonimios$Sinonímio

sinonimios$Espécie

tree$tip.label

corrigir_tax <- function(id){

  if(tree$tip.label[id] != sinonimios$Espécie[id]){

    tree$tip.label[id] <<- sinonimios$Espécie[id]

  }

}

id <- 1:length(tree$tip.label)

purrr::map(id, corrigir_tax)

## Visualizando ----

ggtree::ggtree(tree, layout = "circular", size = 1) +
  ggtree::geom_tiplab(color = "black",
                      size = 3.25,
                      fontface = "bold.italic",
                      offset = 14.5)  +
  ggtree::xlim(0, 300) +
  ggtree::theme_tree()

# Exportando ----

tree |> ape::write.tree("tree_caatinga.phy")
