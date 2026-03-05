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

ggtree::ggtree(tree, layout = "circular", size = 1) +
  ggtree::geom_tiplab(color = "black",
                      size = 3.25,
                      fontface = "bold.italic",
                      offset = 14.5)  +
  ggtree::xlim(0, 300) +
  ggtree::theme_tree() +
  ggview::canvas(height = 10, width = 12)

## Espécies totais ----

### Importando ----

sinonimios <- readxl::read_xlsx("sinonimios.xlsx")

### Visualizando ----

sinonimios

sinonimios |> dplyr::glimpse()

# Tratando a filogenia ----

## Espécies que faltam ----

sps_filo <- tree$tip.label

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

tree$tip.label[tree$tip.label == "Phyllomedusa_nordestina"] <- "Pithecopus_nordestinus"

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

tree |> ape::plot.phylo(type = "fan",
                        show.tip.label = TRUE,
                        edge.color = "blue",
                        edge.width = 1.5,
                        tip.color = "black",
                        cex = 0.45,
                        label.offset = 0.001)

## Corrigindo o nome das espécies ----

sinonimios_trat <- sinonimios |>
  dplyr::mutate(Espécie = Espécie |>  stringr::str_replace(" ", "_"),
                Sinonímio = dplyr::if_else(Sinonímio == "Phyllomedusa nordestina",
                                           "Pithecopus nordestinus",
                                           Sinonímio),
                Sinonímio = Sinonímio |> stringr::str_replace(" ", "_"),
                Sinonímio = Sinonímio |> factor(levels = tree$tip.label)) |>
  as.data.frame() |>
  dplyr::arrange(Sinonímio)

sinonimios_trat

sps_dif_filo <- sinonimios_trat$Sinonímio |> setdiff(sinonimios_trat$Espécie)

sps_dif_filo

sps_corrigir <- sinonimios_trat$Espécie |> setdiff(tree$tip.label)

sps_corrigir

sps_filo <- tree$tip.label

sps_filo

corrigir_tax <- function(id){

  sps_filo <<- data.frame(sps_filo) |>
    dplyr::mutate(sps_filo = dplyr::case_when(
      sps_filo == sps_dif_filo[id] ~ sps_corrigir[id],
      .default = sps_filo)) |>
    dplyr::pull(sps_filo)

}

id <- 1:length(sps_corrigir)

id

purrr::map(id, corrigir_tax)

sps_filo

tree$tip.label <- sps_filo

tree$tip.label

## Visualizando ----

ggtree::ggtree(tree, layout = "circular", size = 1) +
  ggtree::geom_tiplab(color = "black",
                      size = 3.25,
                      fontface = "bold.italic",
                      offset = 14.5)  +
  ggtree::xlim(0, 300) +
  ggtree::theme_tree() +
  ggview::canvas(height = 10, width = 12)

# Exportando ----

tree |> ape::write.tree("tree_caatinga.phy")
