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

tree_cons <- tree %>%
  phytools::consensus.edges(collapse = FALSE) |>
  ape::multi2di()

### Visualizando ----

tree_cons

tree_cons$tip.label

tree_cons %>% ape::plot.phylo(type = "fan",
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

sinonimios %>% dplyr::glimpse()

sinonimios

# Corrigindo ----

## Espécies que faltam ----

sps_filo <- tree_cons$tip.label

sps_filo

sps_faltam <- sinonimios %>%
  dplyr::filter(`Está no VertLife` == "Não") %>%
  dplyr:::pull(Sinonímio)

sps_faltam

novas_especies <- data.frame(especie = sps_faltam %>% stringr::str_replace(" ", "_"),
                             genero = sps_faltam %>% stringr::word(1))

novas_especies

## Adicionando ----

### Criando a função ----

add_missing_species <- function(tree, missing_species) {

  new_tree <- tree

  for(sp in missing_species) {

    genus <- strsplit(sp, "_")[[1]][1]

    genus_tips <- grep(paste0("^", genus, "_"), tree$tip.label, value = TRUE)

    if(length(genus_tips) > 0) {
      genus_node <- phytools::findMRCA(tree, tips = genus_tips)

      new_tree <- phytools::add.species.to.genus(tree = new_tree,
                                                 species = sp,
                                                 where = "root")

    } else {

      warning(paste("Gênero não encontrado para a espécie:", sp))

    }

  }

  return(new_tree)

}

sps_faltam_trat <- sps_faltam %>% stringr::str_replace(" ", "_")

sps_faltam_trat

tree_cons2$tip.label

tree_cons$tip.label

tree_cons2 <- phytools::add.species.to.genus(force.ultrametric(tree_cons),
                                             sps_faltam_trat[1])

tree_cons3 <- tree_cons2 %>% phytools::add.species.to.genus(sps_faltam_trat[2])

tree_cons4 <- phytools::add.species.to.genus(tree_cons3 %>% force.ultrametric(),
                                             sps_faltam[3] %>% stringr::str_replace(" ", "_"))

tree_cons5 <- phytools::add.species.to.genus(tree_cons4,
                                             sps_faltam[4] %>% stringr::str_replace(" ", "_"))

tree_cons6 <- phytools::add.species.to.genus(tree_cons5,
                                             sps_faltam[5] %>% stringr::str_replace(" ", "_"))

tree_cons7 <- phytools::add.species.to.genus(tree_cons6,
                                             sps_faltam[6] %>% stringr::str_replace(" ", "_"))

tree_cons8 <- phytools::add.species.to.genus(tree_cons7,
                                             sps_faltam[7] %>% stringr::str_replace(" ", "_"))

tree_cons9 <- phytools::add.species.to.genus(tree_cons8,
                                             sps_faltam[8] %>% stringr::str_replace(" ", "_"))

tree_cons10 <- phytools::add.species.to.genus(tree_cons8,
                                             sps_faltam[9] %>% stringr::str_replace(" ", "_"))

tree_cons11 <- tree_cons8 %>% phytools::add.species.to.genus(sps_faltam[10] %>% stringr::str_replace(" ", "_"))

tree_cons_final <- tree_cons11

### Aplicando a função ----

tree_trat <-  add_missing_species(tree_cons, sps_faltam %>% stringr::str_replace(" ", "_"))

### Testando ----

df <- data.frame(sps = tree_trat$tip.label) %>%
  dplyr::left_join(data.frame(sps = sinonimios$Sinonímio %>% stringr::str_replace(" ", "_"),
                              sps2 = sinonimios$Espécie) %>%
                     dplyr::distinct(sps, .keep_all = TRUE),
                   by = "sps")

df

data.frame(s1 = df$sps,
           s2 = c(tree_trat$tip.label)) %>%
  dplyr::mutate(igual = dplyr::case_when(s1 == s2 ~ "sim",
                                         .default = "não"))

## Corrigindo ----

tree_trat$tip.label <- df$sps2
tree_trat$tip.label

## Exportando ----

tree_trat %>%
  ape::write.tree("arvore.tre")

nomes <- ecodados::filogenia_anuros$tip.label
nomes %>% sort()

setdiff(sps_faltam %>% stringr::str_replace(" ", "_"), sinonimios$Sinonímio)

tree_trat %>% ape::plot.phylo(type = "fan",
                              show.tip.label = TRUE,
                              edge.color = "blue",
                              edge.width = 1.5,
                              tip.color = "black",
                              cex = 0.45,
                              label.offset = 0.001)

tree_cons2 <- tree_cons

tree_cons2_ultra <- chronos(tree_cons2)

tree_cons3 <- tree_cons2 |>
  phytools::add.species.to.genus(sps_faltam[2] %>% stringr::str_replace(" ", "_")) |>
  force.ultrametric()

tree_cons2$tip.label
