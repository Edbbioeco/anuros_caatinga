# Pacotes ----

library(gert)

library(tidyverse)

# Arquivos ap´tos ----

gert::git_status() |>
  as.data.frame() |>
  dplyr::filter(file |> stringr::str_detect(".R$"))

# Adicionando os arquivos ----

gert::git_add(list.files(pattern = "git_comandos.R"))

# Commitando ----

gert::git_commit("Script os comandos de Git")

# Pushando ----

gert::git_push(remote = "anuros_caatinga")

# Pullando ----

gert::git_pull(remote = "anuros_caatinga")

# Resetando ----

gert::git_reset_mixed()

gert::git_reset_soft(ref = "HEAD")
