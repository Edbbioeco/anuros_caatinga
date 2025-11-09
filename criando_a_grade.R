# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

# Dados ----

## Importando ----

caa <- geobr::read_biomes() %>%
  dplyr::filter(name_biome == "Caatinga")

## Visualizando ----

caa

caa %>%
  ggplot() +
  geom_sf()

# Grade ----

## Criando a grade ----

grade <- sf::st_make_grid(caa, cellsize = 0.04488, square = TRUE) %>%
  sf::st_intersection(caa)


## Visualizando ----

grade

ggplot() +
  geom_sf(data = caa, color = "black") +
  geom_sf(data = grade, color = "red", fill = "transparent")

## Salvando a grade ----

grade %>% sf::st_write("grade.shp")
