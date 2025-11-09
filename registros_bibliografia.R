# Pacotes ----

library(geobr)

library(readxl)

library(tidyverse)

library(parzer)

library(sf)

library(openxlsx)

# Dados ----

## Shapefile da caatinga ----

### Importando ----

caa <- geobr::read_biomes() %>%
  dplyr::filter(name_biome == "Caatinga")

### Visualizando ----

caa

caa %>%
  ggplot() +
  geom_sf(color = "black", fill = "gold")

## Grade ----

#### Importando ----

grade <- sf::st_read("grade.shp")

### Visualizando ----

grade

grade %>%
  ggplot() +
  geom_sf()

## Composição ----

### Importando ----

dados_bib_comp <- readxl::read_xlsx("inventario_anfibios_caatinga.xlsx")

### Visualizando ----

dados_bib_comp %>% dplyr::glimpse()

dados_bib_comp

## Coordenadas ----

### Importando ----

dados_bib_coords <- readxl::read_xlsx("inventario_anfibios_caatinga.xlsx",
                                      sheet = 2)

### Visualizando ----

dados_bib_coords %>% dplyr::glimpse()

dados_bib_coords

## Tratando ----

### Coordenadas ----

dados_bib_coords_trat <- dados_bib_coords %>%
  dplyr::select(Local, Longitude:Latitude) %>%
  dplyr::mutate(Longitude = Longitude %>% parzer::parse_lon(),
                Latitude = Latitude %>% parzer::parse_lat())

dados_bib_coords_trat %>% dplyr::glimpse()

dados_bib_coords_trat

### Composição ----

dados_bib_comp_trat <- dados_bib_comp %>%
  dplyr::left_join(dados_bib_coords_trat,
                   by = "Local")

dados_bib_comp_trat %>% dplyr::glimpse()

dados_bib_comp_trat

# Intersecção dos pontos ----

## Criando o shapefile de pontos ----

bib_shp <- dados_bib_comp_trat %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = grade %>% sf::st_crs())

oc_trat_shp

ggplot() +
  geom_sf(data = caa, color = "black", fill = "gold") +
  geom_sf(data = oc_trat_shp)

## Intersecção com a Caatinga ----

bib_inter <- bib_shp %>%
  sf::st_intersection(caa)

bib_inter

ggplot() +
  geom_sf(data = caa, color = "black", fill = "gold") +
  geom_sf(data = bib_inter)

## Intersecção com a grade ----

### Calculando ----

bib_grid <- sf::st_join(bib_inter, grade, join = st_intersects)

### Visualizando ----

bib_grid

bib_grid %>% dplyr::glimpse()

bib_grid %>%
  dplyr::filter(!is.na(FID)) %>%
  ggplot() +
  geom_sf(data = caa, color = "black", fill = "gold") +
  geom_sf()

## Matriz ---

### Extraindo as informações de grid ----

bib_grid_names <- bib_grid %>%
  tibble::as_tibble() %>%
  dplyr::select(FID) %>%
  dplyr::rename("Assemblage" = FID) %>%
  dplyr::mutate(Assemblage = paste0("Assemblage ", Assemblage))

bib_grid_names

### Extraindo as informações de coordenadas ----

bib_grid_coords <- bib_grid %>%
  sf::st_coordinates() %>%
  tibble::as_tibble() %>%
  dplyr::rename("Longitude" = X,
                "Latitude" = Y)

bib_grid_coords

### Criando um tibble ----

bib_tibble <- bib_inter %>%
  tibble::as_tibble() %>%
  dplyr::select(Espécie) %>%
  dplyr::rename("Species" = Espécie) %>%
  dplyr::mutate(Presence = 1) %>%
  dplyr::relocate(Presence, .before = Species)

bib_tibble

### Unindo ----

registros_bib <- dplyr::bind_cols(bib_grid_names,
                                  bib_grid_coords,
                                  bib_tibble) %>%
  dplyr::mutate(Source = "Bibliography") %>%
  dplyr::distinct(Assemblage, Longitude, Latitude, Species, .keep_all = TRUE)

registros_bib %>% dplyr::glimpse()

registros_bib

### Exportando ----

registros_bib %>%
  openxlsx::write.xlsx("registros_bib.xlsx")
