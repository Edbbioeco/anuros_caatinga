# Pacotes ----

library(readxl)

library(tidyverse)

library(openxlsx)

# Dados ----

## GBIF ----

### Importando ----

comunidade_gbif <- readxl::read_xlsx("registros_gbif.xlsx")

### Visualizando ----

comunidade_gbif %>% dplyr::glimpse()

comunidade_gbif

## SpeciesLink ----

### Importando ----

comunidade_specieslink <- readxl::read_xlsx("registros_specieslink.xlsx")

### Visualizando ----

comunidade_specieslink %>% dplyr::glimpse()

comunidade_specieslink

## Sibbr ----

### Importando ----

comunidade_sibbr <- readxl::read_xlsx("registros_sibbr.xlsx")

### Visualizando ----

comunidade_sibbr %>% dplyr::glimpse()

comunidade_sibbr

## Bibliográficos ----

### Importando ----

comunidade_bib <- readxl::read_xlsx("registros_bib.xlsx")

### Visualizando ----

comunidade_bib %>% dplyr::glimpse()

comunidade_bib

# Dados unidos ----

## Unindo os dados de registro ----

comunidade <- dplyr::bind_rows(comunidade_gbif,
                              comunidade_specieslink,
                              comunidade_sibbr,
                              comunidade_bib) %>%
  dplyr::mutate(Species = dplyr::case_when(Species == "Boana albomarginatus" ~ "Boana albomarginata",
                                           Species == "Boana albopunctatus" ~ "Boana albopunctata",
                                           Species == "Thoropa megatympanum" ~ "Leptodactylus troglodytes",
                                           Species == "Scinax acuminatum" ~ "Scinax x-signatus",
                                           Species == "Physalaemus marmoratus" ~ "Physalaemus nattereri",
                                           Species == "Boana wavrini" ~ "Boana atlantica",
                                           .default = Species))

## Visualizando ----

comunidade

## Criando uma matriz de composição ----

matriz <- comunidade %>%
  dplyr::group_by(Assemblage, Species) %>%
  dplyr::summarise(Presence = max(Presence, na.rm = TRUE),
                   .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Species,
                     values_from = Presence,
                     values_fill = 0) %>%
  dplyr::left_join(comunidade %>%
                     dplyr::select(1:3, 6),
                   by = "Assemblage") %>%
  dplyr::relocate(Longitude:Source,
                  .before = `Boana crepitans`) %>%
  dplyr::distinct(Assemblage, .keep_all = TRUE)

matriz

ggplot() +
  geom_sf(data = caa, color = "black", fill = "gold") +
  geom_point(data = matriz, aes(Longitude, Latitude))

## Exportando ----

### comunidade ----

comunidade %>%
  openxlsx::write.xlsx("comunidade.xlsx")

comunidade %>%
  dplyr::filter(Species == "Thoropa megatympanum")

### Matriz ----

matriz %>%
  openxlsx::write.xlsx("matriz.xlsx")
