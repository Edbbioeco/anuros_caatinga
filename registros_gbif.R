# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(ggsflabel)

library(openxlsx)

# Dados ----

## Shapefile dos estados do Nordeste ----

### Importando ----

ne <- geobr::read_state() %>%
  dplyr::filter(name_region == "Nordeste")

### Visualizando ----

ne %>%
  ggplot() +
  geom_sf()

## Shapefile da Caatinga ----

### Importando ----

caa <- geobr::read_biomes() %>%
  dplyr::filter(name_biome == "Caatinga")

### Visualizando ----

caa %>%
  ggplot() +
  geom_sf()

## Shapefile da grade ----

### Importando ----

grade <- sf::st_read("grade.shp")

### Visualizando ----

grade

grade %>%
  ggplot() +
  geom_sf()

## Ocorrências ----

### Importando ----

oc <- readr::read_csv2("occ.csv")

### Visualizando ----

oc %>% dplyr::glimpse()

oc

### Tratando ----

oc_trat <- oc %>%
  dplyr::select(family:stateProvince, decimalLatitude:decimalLongitude) %>%
  dplyr::rename("Latitude" = decimalLatitude,
                "Longitude" = decimalLongitude) %>%
  dplyr::mutate(Longitude = Longitude %>%
                  stringr::str_replace("^(-?\\d{2})(\\d+)$", "\\1.\\2") %>%
                  as.numeric(),
                Latitude = case_when(stringr::str_detect(as.character(Latitude), "^(-?[1-2])") ~ str_replace(as.character(Latitude), "^(-?\\d{2})(\\d+)$", "\\1.\\2"),
                                     stringr::str_detect(as.character(Latitude), "^(-?[3-9])") ~ stringr::str_replace(as.character(Latitude), "^(-?\\d{1})(\\d+)$", "\\1.\\2"),
                                     TRUE ~ as.character(Latitude)) %>%
                  as.numeric()) %>%
  dplyr::filter(!is.na(species) &
                  !is.na(Latitude) &
                  !is.na(Longitude))


oc_trat %>% dplyr::glimpse()

oc_trat

## Lista de Anfíbios da plataforma SALVE do ICMBio (https://salve.icmbio.gov.br/#/) ----

### Importanddo ----

lista <- readr::read_csv("lista_anfibios_salve.csv")

### Visualizando ----

lista

### Tratando ----

lista_trat <- lista %>%
  dplyr::filter(Bioma %>% stringr::str_detect("Caatinga")) %>%
  dplyr::select(Especie) %>%
  dplyr::mutate(species = Especie) %>%
  as.data.frame()

lista_trat

# Intersecção dos pontos ----

## Criando o shapefile de pontos ----

oc_trat_shp <- oc_trat %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = grade %>% sf::st_crs())

oc_trat_shp

ggplot() +
  geom_sf(data = caa) +
  geom_sf(data = oc_trat_shp)

## Intersecção com a Caatinga ----

gbif_inter <- oc_trat_shp %>%
  sf::st_intersection(caa)

gbif_inter

ggplot() +
  geom_sf(data = caa, color = "black", fill = "gold") +
  geom_sf(data = gbif_inter)

## Checando as espécies ----

### Lista de Espécies ----

lista_gbif <- gbif_inter$species %>% unique()

lista_gbif

### Alterando as espécies ----

# Rhinella marina, Rhinella jimi, Rhinella icterica = Rhinella diptycha
# Elachistocleis bicolor, Elachistocleis ovalis = Elachistocleis cesarii
# Pithecopus nordestinus (31299, 18649, 16159, 16003) = Pithecopus gonzagai
# Rhinella major = Rhinella granulosa
# Scinax acuminatus, Scinax nasicuus, Scinax tropicalia, Ololygon strigilata = Scinax x-signatus
# Dendropsophus leucophyllatus = Dendropsophus elegans
# Dendropsophus seniculus = Dendropsophus soaresi
# Boana punctata = Boana atlantica
# Rhinella margaritifera, Rhinella hoogmoedi = Rhinella dapsilis
# Leptodactylus bufonius, Leptodactylus pentadactylus = Leptodactylus vastus
# Scinax camposseabrai = Julianus camposseabrai
# Dendropsophus microcephalus = Dendropsophus branneri
# Boana marginata = Boana albomarginata
# Dendropsophus bipunctatus = Dendropsophus minutus
# Scinax alter = Scinax pachycrus
# Boana lundii = Boana faber
# Scinax squalirostris = Scinax pachycrus
# Phyllodytes edelmoi = Phyllodytes acuminatus
# Adenomera juikitam = Adenomera andreae
# Leptodactylus payaya, Leptodactylus latinasus = Leptodactylus latrans
# Physalaemus camacan = Physalaemus kroyeri
# Proceratophrys goyana = Proceratophrys cristiceps
# Pithecopus azureus = Pithecopus gonzagai

#### Espécies para excluir ----

# Pithecopus hypochondrialis
# Pristimantis relictus (24319)
# Brachycephalus pulex
# Eurycephalella alcinae
# Arariphrynus placidoi
# Cratia gracilis
# Haddadus binotatus

gbif_inter_trat <- gbif_inter %>%
  dplyr::filter(!species %in% c("Pithecopus hypochondrialis",
                                  "Brachycephalus pulex",
                                  "Eurycephalella alcinae",
                                  "Arariphrynus placidoi",
                                  "Cratia gracilis",
                                  "Haddadus binotatus",
                                  "Osteopilus ocellatus",
                                "Boana",
                                "scinax",
                                "Bokermannohyla",
                                "Eleutherodactylus",
                                "Scinax")) %>%
  dplyr::mutate(species = dplyr::case_when(species %in% c("Rhinella marina", "Rhinella jimi", "Rhinella icterica") ~ "Rhinella diptycha",
                                           species %in% c("Elachistocleis bicolor", "Elachistocleis ovalis") ~ "Elachistocleis cesarii",
                                           species == "Rhinella major" ~ "Rhinella granulosa",
                                           species %in% c("Scinax acuminatus", "Scinax nasicuus", "Ololygon strigilata", "Scinax nasicus", "Scinax tropicalia") ~ "Scinax x-signatus",
                                           species == "Dendropsophus leucophyllatus" ~ "Dendropsophus elegans",
                                           species == "Dendropsophus seniculus" ~ "Dendropsophus soaresi",
                                           species == "Boana punctata" ~ "Boana atlantica",
                                           species %in% c("Rhinella margaritifera", "Rhinella hoogmoedi") ~ "Rhinella dapsilis",
                                           species %in% c("Leptodactylus bufonius", "Leptodactylus pentadactylus") ~ "Leptodactylus vastus",
                                           species == "Scinax camposseabrai" ~ "Julianus camposseabrai",
                                           species == "Dendropsophus microcephalus" ~ "Dendropsophus branneri",
                                           species == "Boana marginata" ~ "Boana albomarginata",
                                           species == "Dendropsophus bipunctatus" ~ "Dendropsophus minutus",
                                           species == "Scinax alter" ~ "Scinax pachycrus",
                                           species == "Boana lundii" ~ "Boana faber",
                                           species == "Scinax squalirostris" ~ "Scinax pachycrus",
                                           species == "Phyllodytes edelmoi" ~ "Phyllodytes acuminatus",
                                           species == "Adenomera juikitam" ~ "Adenomera andreae",
                                           species == "Leptodactylus payaya" ~ "Leptodactylus latrans",
                                           species %in% c("Physalaemus camacan", "Leptodactylus latinasus") ~ "Physalaemus kroyeri",
                                           species == "Proceratophrys goyana" ~ "Proceratophrys cristiceps",
                                           .default = species))

gbif_inter_trat %>% dplyr::glimpse()

gbif_inter_trat

## Intersecção com a grade ----

### Calculando ----

gbif_grid <- sf::st_join(gbif_inter_trat, grade, join = st_intersects)

### Visualizando ----

gbif_grid

gbif_grid %>% dplyr::glimpse()

gbif_grid %>%
  dplyr::filter(!is.na(FID)) %>%
  ggplot() +
  geom_sf(data = caa, color = "black", fill = "gold") +
  geom_sf()

## Matriz ---

### Extraindo as informações de grid ----

gbif_grid_names <- gbif_grid %>%
  tibble::as_tibble() %>%
  dplyr::select(FID) %>%
  dplyr::rename("Assemblage" = FID) %>%
  dplyr::mutate(Assemblage = paste0("Assemblage ", Assemblage))

gbif_grid_names

### Extraindo as informações de coordenadas ----

gbif_grid_coords <- gbif_grid %>%
  sf::st_coordinates() %>%
  tibble::as_tibble() %>%
  dplyr::rename("Longitude" = X,
                "Latitude" = Y)

gbif_grid_coords

### Criando um tibble ----

gbif_tibble <- gbif_inter_trat %>%
  tibble::as_tibble() %>%
  dplyr::select(species) %>%
  dplyr::rename("Species" = species) %>%
  dplyr::mutate(Presence = 1) %>%
  dplyr::relocate(Presence, .before = Species)

gbif_tibble

### Unindo ----

registros_gbif <- dplyr::bind_cols(gbif_grid_names,
                                   gbif_grid_coords,
                                   gbif_tibble) %>%
  dplyr::mutate(Source = "GBIF") %>%
  dplyr::distinct(Assemblage, Longitude, Latitude, Species, .keep_all = TRUE)

registros_gbif %>% dplyr::glimpse()

registros_gbif

### Exportando ----

registros_gbif %>%
  openxlsx::write.xlsx("registros_gbif.xlsx")
