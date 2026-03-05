# Pacotes ----

library(gert)

library(tidyverse)

# Arquivos ap´tos ----

gert::git_status() |>
  as.data.frame() |>
  dplyr::filter(file |> stringr::str_detect(".R$"))

# Adicionando os arquivos ----

gert::git_add(list.files(pattern = "Adicionar espécies faltantes.R"))

# Commitando ----

gert::git_commit("Script para adicionar as espécies faltantes à filogenia")

# Pushando ----

gert::git_push(remote = "")

# Pullando ----

# Resetando ----
