# Pacotes ----

library(tidyverse)

library(geobr)

library(sf)

library(openxlsx)

# Dados ----

## Registros ----

### Importando ----

sibbr <- readr::read_csv("data_sibbr.csv")

### Visualizando ----

sibbr %>% dplyr::glimpse()

sibbr

### Tratando ----

sibbr_trat <- sibbr %>%
  dplyr::select(scientificName, decimalLatitude:decimalLongitude) %>%
  dplyr::rename("Longitude" = decimalLongitude,
                "Latitude" = decimalLatitude,
                "Species" = scientificName) %>%
  dplyr::filter(!Longitude %>% is.na() &
                  !Latitude %>% is.na() &
                  !Species %>% is.na())

sibbr_trat %>% dplyr::glimpse()

sibbr_trat

## Shapefile da Caatinga ----

### Importnado ----

caa <- geobr::read_biomes() %>%
  dplyr::filter(name_biome == "Caatinga")

### Visualizando ----

caa

caa %>%
  ggplot() +
  geom_sf(color = "black", fill = "gold")

## Shapefile da grade ----

## Importando ----

grade <- sf::st_read("grade.shp")

### Visualizando ----

grade

grade %>%
  ggplot() +
  geom_sf()

# Intersecção dos pontos ----

## Criando o shapefile dos pontos ----

sibbr_trat_shp <- sibbr_trat %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = caa %>% sf::st_crs())

sibbr_trat_shp

sibbr_trat_shp %>%
  ggplot() +
  geom_sf()

## Intersecção com a Caatinga ----

sibbr_inter <- sibbr_trat_shp %>%
  sf::st_intersection(caa)

sibbr_inter

ggplot() +
  geom_sf(data = caa, color = "black", fill = "gold") +
  geom_sf(data = sibbr_inter)

## Checando as espécies ----

### Lista de Espécies ----

lista_sibbr <- sibbr_inter$Species %>% unique()

lista_sibbr

### Alterando as espécies ----

ggplot() +
  geom_sf(data = caa, color = "black", fill = "gold") +
  geom_sf_label(data = sibbr_inter %>%
                  dplyr::filter(Species == "Phyllodytes wuchereri"),
                aes(label = Species))

# !Species %>% stringr::str_detect("sp.")
# Species %in% c("Hyla", "Hypsiboas") ~ "Boana"
# Species %in% c("Rhinella marina", "Rhinella jimi", "Rhinella schneideri", "Bufo paracnemis", "Bufo marinus", "Rhinella icterica") ~ "Rhinella diptycha"
# !Species  %in% c("Physalaemus", "Rhinella", "Pseudopaludicola", "Dendropsophus", "Pristimantis", "Leptodactylus", "Hyla", "Leptodactylus", "Pleurodema", "Hylidae", "Adelophryne", "Anura", "Bufonidae", "Proceratophrys laticeps", "Phyllomedusa", ""Ischnocnema", "Leptodactylidae", "Elachistocleis", "Proceratophrys", "Frostius", "Crossodactylus trachystomus", "Corythomantis", "Hypsiboas", "Allobates olfersioides", "Lithobates", "Pseudis", "Odontophrynus", "Cycloramphidae", "Trachycephalus", "Stereocyclops incrassatus", "Hypsiboas multifasciatus", "Strabomantis", "Lepidobatrachus", "Hyalinobatrachium", "Frostius pernambucensis", "Rupirana", "Leptodactylus cunicularius", "Sphaenorhynchus lacteus", "Phyllodytes punctatus", "Dendropsophus minimus", "Adenomera", "Ceratophrys", "Bokermannohyla capra", "Phyllodytes edelmoi", "Oreobates", "Brachycephalus pulex", "Scinax perereca", "Microhylidae", "Phyllomedusa vaillantii", "Sphaenorhynchus bromelicola", "Scinax duartei", "Leptodactylus jolyi", "Pipa", "Aplastodiscus sibilatus")
# Latitude <= -10.518333 & Species == "Phyllomedusa nordestina" ~ "Pithecopus gonzagai"
# Latitude > -10.518333 & Species == "Phyllomedusa nordestina" ~ "Pithecopus nordestinus"
# Species == "Pipa pipa" ~ "Pipa carvalhoi"
# Species == "Scinax v-signatus" ~ "Scinax x-signatus"
# Species %in% c("Rhinella hoogmoedi", "Rhinella margaritifera") ~ "Rhinella dapsilis"
# Species == "Hypsiboas albopunctatus" ~ "Boana albopunctata"
# Species == "Hypsiboas punctatus" ~ "Boana punctata"
# Species == "Agalychnis granulosa" ~ "Hylomantis granulosa"
# Species == "Leptodactylus didymus" ~ "Leptodactylus mystaceus"
# Species == "Elachistocleis bicolor" ~ "Elachistocleis cesarii"

sibbr_inter_trat <- sibbr_inter %>%
  tibble::as_tibble() %>%
  tidyr::separate(col = geometry,
                  into = c("Longitude", "Latitude"),
                  sep = " ") %>%
  dplyr::mutate(dplyr::across(.cols = Longitude:Latitude,
                              readr::parse_number)) %>%
  dplyr::filter(!Species %in% c("Physalaemus",
                                 "Rhinella",
                                 "Pseudopaludicola",
                                 "Dendropsophus",
                                 "Pristimantis",
                                 "Leptodactylus",
                                 "Hyla",
                                 "Leptodactylus",
                                 "Pleurodema",
                                 "Hylidae",
                                 "Adelophryne",
                                "Bokermannohyla",
                                "Eleutherodactylus",
                                "Boana",
                                "Scinax",
                                 "Anura",
                                 "Bufonidae",
                                 "Proceratophrys laticeps",
                                 "Phyllomedusa",
                                 "Ischnocnema",
                                 "Leptodactylidae",
                                 "Elachistocleis",
                                 "Proceratophrys",
                                 "Frostius",
                                 "Crossodactylus trachystomus",
                                 "Corythomantis",
                                 "Hypsiboas",
                                 "Allobates olfersioides",
                                 "Lithobates",
                                 "Pseudis",
                                 "Odontophrynus",
                                 "Cycloramphidae",
                                 "Trachycephalus",
                                 "Stereocyclops incrassatus",
                                 "Hypsiboas multifasciatus",
                                 "Strabomantis",
                                 "Lepidobatrachus",
                                 "Hyalinobatrachium",
                                 "Frostius pernambucensis",
                                 "Rupirana",
                                 "Leptodactylus cunicularius",
                                 "Sphaenorhynchus lacteus",
                                 "Phyllodytes punctatus",
                                 "Dendropsophus minimus",
                                 "Adenomera",
                                 "Ceratophrys",
                                 "Bokermannohyla capra",
                                 "Phyllodytes edelmoi",
                                 "Oreobates",
                                 "Brachycephalus pulex",
                                 "Scinax perereca",
                                 "Microhylidae",
                                 "Phyllomedusa vaillantii",
                                 "Sphaenorhynchus bromelicola",
                                 "Scinax duartei",
                                 "Leptodactylus jolyi",
                                 "Pipa",
                                 "Aplastodiscus sibilatus") &
                  !Species %>% stringr::str_detect("sp")) %>%
  dplyr::mutate(Species = dplyr::case_when(Species %in% c("Rhinella marina", "Rhinella jimi", "Rhinella schneideri", "Bufo paracnemis", "Bufo marinus", "Rhinella icterica") ~ "Rhinella diptycha",
                                           Latitude >= -10.518333 &
                                             Species == "Phyllomedusa nordestina" ~ "Pithecopus gonzagai",
                                           Latitude < -10.518333 & Species == "Phyllomedusa nordestina" ~ "Pithecopus nordestinus",
                                           Species == "Pipa pipa" ~ "Pipa carvalhoi",
                                           Species == "Scinax v-signatus" ~ "Scinax x-signatus",
                                           Species %in% c("Rhinella hoogmoedi", "Rhinella margaritifera") ~ "Rhinella dapsilis",
                                           Species == "Hypsiboas albopunctatus" ~ "Boana albopunctata",
                                           Species %in% c("Hypsiboas punctatus", "Boana punctata") ~ "Boana atlantica",
                                           Species == "Agalychnis granulosa" ~ "Hylomantis granulosa",
                                           Species == "Leptodactylus didymus" ~ "Leptodactylus mystaceus",
                                           Species == "Elachistocleis bicolor" ~ "Elachistocleis cesarii",
                                           Species == "Phyllomedusa azurea" ~ "Pithecopus azureus",
                                           Species == "Boana albomarginatus" ~ "Boana albomarginata",
                                           Species == "Elachistocleis ovalis" ~ "Elachistocleis cesarii",
                                           Species == "Ceratophrys aurita" ~ "Ceratophrys joazeirensis",
                                           Species == "Leptodactylus bufonius" ~ "Leptodactylus troglodytes",
                                           Species == "Scinax strigilatus" ~ "Ololygon strigilata",
                                           Species == "Boana wavrini" ~ "Boana creptans",
                                           Species == "Leptodactylus pentadactylus" ~ "Leptodactylus vastus",
                                           Species == "Scinax curicica" ~ "Scinax x-signatus",
                                           Species == "Leptodactylus furnarius" ~ "Leptodactylus fuscus",
                                           Species == "Dendropsophus marmoratus" ~ "Dendropsophus soaresi",
                                           .default = Species),
                Species = Species %>% stringr::str_replace_all("Hyla|Hypsiboas", "Boana")) %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = caa %>% sf::st_crs())

sibbr_inter_trat

## Intersecção com a grade ----

### Calculando ----

sibbr_grid <- sf::st_join(sibbr_inter_trat, grade, join = st_intersects)

### Visualizando ----

sibbr_grid

sibbr_grid %>% dplyr::glimpse()

sibbr_grid %>%
  dplyr::filter(!is.na(FID)) %>%
  ggplot() +
  geom_sf(data = caa, color = "black", fill = "gold") +
  geom_sf()

## Matriz ----

### Extraindo as informações de grid ----

sibbr_grid_names <- sibbr_grid %>%
  tibble::as_tibble() %>%
  dplyr::select(FID) %>%
  dplyr::rename("Assemblage" = FID) %>%
  dplyr::mutate(Assemblage = paste0("Assemblage ", Assemblage))

sibbr_grid_names

### Extraindo as informações de coordenadas ----

sibbr_grid_coords <- sibbr_grid %>%
  sf::st_coordinates() %>%
  tibble::as_tibble() %>%
  dplyr::rename("Longitude" = X,
                "Latitude" = Y)

sibbr_grid_coords

### Criando um tibble ----

sibbr_tibble <- sibbr_grid %>%
  tibble::as_tibble() %>%
  dplyr::select(Species) %>%
  dplyr::mutate(Presence = 1) %>%
  dplyr::relocate(Presence, .before = Species)

sibbr_tibble

### Unindo ----

registros_sibbr <- dplyr::bind_cols(sibbr_grid_names,
                                    sibbr_grid_coords,
                                    sibbr_tibble) %>%
  dplyr::mutate(Source = "SIBBR") %>%
  dplyr::distinct(Assemblage, Longitude, Latitude, Species, .keep_all = TRUE)

registros_sibbr %>% dplyr::glimpse()

registros_sibbr

### Exportando ----

registros_sibbr %>%
  openxlsx::write.xlsx("registros_sibbr.xlsx")

