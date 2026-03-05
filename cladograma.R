
# Pacotes ----

library(ape)

library(readxl)

library(tidyverse)

library(tidytree)

library(ggtree)

library(ggtreeExtra)

library(ggview)

# Dados ----

## Árvore filogenética ----

### Importando ----

tree <- ape::read.tree("tree_caatinga.phy")

### Visualizando ----

tree

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

sinonimios |> as.data.frame()

sinonimios |> dplyr::glimpse()

# Tratando a filogenia ----

## Dataframe com a informações das famílias ----

dados_familias <- data.frame(Espécie = tree$tip.label) |>
  dplyr::left_join(sinonimios |>
                     dplyr::mutate(Espécie = Espécie |> stringr::str_replace(" ", "_")),
                   by = "Espécie") |>
  dplyr::select(1, 3) |>
  dplyr::rename("label" = Espécie) |>
  dplyr::distinct(label, Família) |>
  dplyr::mutate(Família = dplyr::if_else(label |>
                                           stringr::str_detect("Pithecopus|Hylomantis|Phyllomedusa"),
                                         "Phyllomedusidade",
                                         Família)) |>
  dplyr::mutate(Gênero = label |>
                  stringr::str_replace("_", " ") |>
                  stringr::word(1),
                Família  = dplyr::case_match(Gênero,
                                             "Pseudis" ~ "Hylidae",
                                             "Julianus" ~ "Hylidae",
                                             "Rhaebo" ~ "Bufonidae",
                                             .default = Família)) |>
  dplyr::group_by(Gênero) |>
  tidyr::fill(Família,
              .direction = "updown") |>
  dplyr::ungroup()

dados_familias |> as.data.frame()

## Adicionando a informação das famílias ----

tree_trat <- tidytree::as.treedata(tree) |>
  dplyr::left_join(dados_familias, by = "label") |>
  dplyr::mutate(label = label |> stringr::str_replace("_", " "))

tree_trat

# Cladograma -----

## Linear ----

ggtree::revts(ggtree::ggtree(tree_trat, size = 1)) +
  ggtree::geom_tiplab(color = "black", size = 3.25,
                      offset = 12.5,
                      fontface = "bold.italic") +
  scale_fill_viridis_d(option = "turbo") +
  ggtreeExtra::geom_fruit(geom = geom_tile,
                          mapping = aes(fill = Família),
                          color = "black",
                          width = 10,
                          offset = -0.035) +
  scale_x_continuous(breaks = seq(-200, 0, 20),
                     limits = c(-210, 60),
                     name = "Ma Years") +
  ggtree::theme_tree2() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(color = "black", size = 15),
        axis.title.x = element_text(color = "black", size = 15),
        axis.line.x = element_line(linewidth = 1)) +
  ggview::canvas(height = 14, width = 12)

ggsave(filename = "cladograma_linear.png", height = 14, width = 12)

## Circular ----

ggtree::ggtree(tree_trat,layout = "circular", size = 1) +
  ggtree::geom_tiplab(color = "black", size = 3.25,
                      offset = 14.5,
                      fontface = "bold.italic") +
  ggtreeExtra::geom_fruit(geom = geom_tile,
                          mapping = aes(fill = Família),
                          color = "black",
                          width = 10,
                          offset = 0.04) +
  scale_fill_viridis_d(option = "turbo") +
  xlim(-5, 350) +
  ggtree::theme_tree() +
  theme(legend.title = element_blank()) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "cladograma_circular.png", height = 10, width = 12)
