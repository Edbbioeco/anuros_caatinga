# Pacotes ----

library(readxl)

library(tidyverse)

library(geobr)

library(sf)

library(openxlsx)

# Dados ----

## Registros ----

### Importando ----

specieslink <- readxl::read_xlsx("dados_specieslink.xlsx")

### Visualizando ----

specieslink

specieslink %>% dplyr::glimpse()

### Checando se as coordenadas compreendem apenas ao Sul e ao Oeste ----

specieslink %>%
  dplyr::filter(longitude > 0 & latitude > 2)

### Tratando ----

specieslink_trat <- specieslink %>%
  dplyr::select(1:2, 5:7) %>%
  dplyr::filter(!longitude %>% is.na() &
                  !latitude %>% is.na() &
                  !scientificname %>% is.na()) %>%
  dplyr::mutate(longitude = longitude %>% as.numeric(),
                longitude = dplyr::if_else(longitude > 0,
                                           longitude * -1,
                                           longitude),
                latitude = latitude %>% as.numeric(),
                latitude = dplyr::if_else(latitude > 2,
                                          latitude * -1,
                                          latitude)) %>%
  dplyr::rename("Longitude" = longitude,
                "Latitude" = latitude,
                "Species" = scientificname)

specieslink_trat %>%
  dplyr::filter(Longitude > 0 | Latitude > 2)

specieslink_trat

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

specieslink_trat_shp <- specieslink_trat %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = caa %>% sf::st_crs())

specieslink_trat_shp

specieslink_trat_shp %>%
  ggplot() +
  geom_sf(color = "black", fill = "gold")

## Intersecção com a Caatinga ----

specieslink_inter <- specieslink_trat_shp %>%
  sf::st_intersection(caa)

specieslink_inter

ggplot() +
  geom_sf(data = caa, color = "black", fill = "gold") +
  geom_sf_label(data = specieslink_inter %>%
                  dplyr::filter(Species == "Hypsiboas punctatus"),
                aes(label = Species))

## Checando as espécies ----

### Lista de Espécies ----

lista <- specieslink_inter$Species %>% unique()

lista

### Alterando as espécies ----

# Retiar tudo o que tiver sp.
#  %in% c("Hyla", "Hypsiboas") ~ "Boana"
#  %in% c("Rhinella marina", "Rhinella jimi", "Rhinella schneideri", "Bufo paracnemis", "Bufo marinus", "Rhinella icterica") ~ "Rhinella diptycha"
# Retirar tudo o que for  %in% c("Pithecopus hypochondrialis", "Ischnocnema juipoca", "Scytopis", "Phrynohyas venulosa", "Leptodactylus", "Proceratophrys renalis", "Leptodactylus latinasus", "Leptodactylus cf. furnarius", "Pleurodema", "Pseudopaludicola cf. mystacalis", "Leptodactylus gr. marmoratus", "Sem Dados sem dado", "Physalaemus", "Scinax cf. fuscovarius cf. fuscovarius", "Leptodactylus/Pseudopaludicola", "Hypsiboas cf. lundii/crepitans cf. lundii/crepitans", "Phyllomedusa", "Odontophrynus", "Rhinella", "Pipa", "Scinax", "Não_Identificado", "Corythomantis", "Bokermannohyla", "Bokermannohyla gr. pseudopseudis gr. pseudopseudis", "Corythomantis cf. greeningi cf. greening", "Scinax cf. fuscomarginatus cf. fuscomarginatus", "Scinax gr. catharinae gr. catharinae", "Scinax gr. ruber gr. ruber", "Bokermannohyla gr. circumdata gr. circumdata")
# "Leptodactylus labyrinthicus" ~ "Leptodactylus vastus"
# "Ischnocnema ramagii" ~ "Pristimantis ramagii"
# "Phyllomedusa megacephala" ~ "Pithecopus megacephalus"
# "Scinax camposseabrai" ~ "Julianus camposseabra"
#  %in% c("Scinax catharinae (espinhaço)", "Scinax catharinae (sp. espinhaço)") ~ "Ololygon catharinae"
#  %in% c("Scinax ruber (amarelinho)", "Scinax ruber (amarelinho 2)") ~ "Scinax ruber"
# "Strabomantis aramunha" ~ "Haddadus aramunha"
# "Hypsiboas punctatus" ~ "Boana atlantica"
# "Rhinella hoogmoedi" ~ "Rhinella dapsilis"
# "Leptodactylus bufonius" ~ "Leptodactylus troglodytes"
#  %in% c("Bufo granulosus lutzi", "Bufo granulosus goeldi") ~ "Rhinella granulosa"
#  %in% c("Bufo crucifer", "Bufo ocellatus") ~ "Rhinella crucifer"
# "Scinax strigilatus" ~ "Ololygon strigilata"
# "Hypsiboas albomarginatus" ~ "Boana albomarginata"
# "Elachistocleis ovalis" ~ "Elachistocleis cesarii"
# "Phyllomedusa hypochondrialis" ~ "Pithecopos nordestinus"
# "Scinax acuminata" ~ "Scinax acuminatus"
#  %in% c("Scinax acuminata", "Scinax nasicus") ~ "Scinax x-signatus"
# "Hyla decipiens decipiens" ~ "Dendropsophus decipiens"
# "Hyla decipiens branneri" ~ "Dendropsophus branneri"
# "Hyla leucophyllata" ~ "Dendropsophus elegans"
# "Hyla senicula senicula" ~ "Dendropsophus soaresi"
# "Rana palmipes" ~ "Lithobates palmipes"
# "Pseudopaludicola cf. pocoto" ~ "Pseudopaludicola pocoto"
# "Dendropsophus cf. decipiens" ~ "Dendropsophus  decipiens"
# "Scinax cf. fuscovarius" ~ "Scinax fuscovarius"
# "Boana cf. raniceps" ~ "Boana raniceps"
# "Scinax cf. pachycrus" ~ "Scinax pachycrus"
# "Physalaemus cf. kroyeri" ~ "Physalaemus kroyeri"
#

specieslink_inter_trat <- specieslink_inter %>%
  dplyr::filter(!Species %in% c("Sem Dados sem dados",
                                "Pithecopus hypochondrialis",
                                "Ischnocnema juipoca",
                                "Vitreorana uranoscopa",
                                "Scytopis",
                                "Phrynohyas venulosa",
                                "Leptodactylus",
                                "Proceratophrys renalis",
                                "Leptodactylus latinasus",
                                "Leptodactylus cf. furnarius",
                                "Pleurodema",
                                "Pseudopaludicola cf. mystacalis",
                                "Leptodactylus gr. marmoratus",
                                "Sem Dados sem dado",
                                "Physalaemus",
                                "Scinax cf. fuscovarius cf. fuscovarius",
                                "Leptodactylus/Pseudopaludicola",
                                "Hypsiboas cf. lundii/crepitans cf. lundii/crepitans",
                                "Phyllomedusa",
                                "Odontophrynus",
                                "Rhinella",
                                "Pipa",
                                "Scinax",
                                "Boana",
                                "Hyla",
                                "Não_Identificado",
                                "Corythomantis",
                                "Bokermannohyla",
                                "Bokermannohyla gr. pseudopseudis gr. pseudopseudis",
                                "Corythomantis cf. greeningi cf. greeningi",
                                "Scinax cf. fuscomarginatus cf. fuscomarginatus",
                                "Scinax gr. catharinae gr. catharinae",
                                "Scinax gr. ruber gr. ruber",
                                "Bokermannohyla gr. circumdata gr. circumdata") &
                  !Species %>% stringr::str_detect("sp")) %>%
  dplyr::mutate(Species = dplyr::case_when(Species %in% c("Rhinella marina", "Rhinella jimi", "Rhinella schneideri", "Bufo paracnemis", "Bufo marinus", "Rhinella icterica") ~ "Rhinella diptycha",
                                           Species == "Leptodactylus labyrinthicus" ~ "Leptodactylus vastus",
                                           Species == "Ischnocnema ramagii" ~ "Pristimantis ramagii",
                                           Species == "Phyllomedusa megacephala" ~ "Pithecopus megacephalus",
                                           Species == "Scinax camposseabrai" ~ "Julianus camposseabra",
                                           Species %in% c("Scinax catharinae (espinhaço)", "Scinax catharinae (sp. espinhaço)") ~ "Ololygon catharinae",
                                           Species %in% c("Scinax ruber (amarelinho)", "Scinax ruber (amarelinho 2)") ~ "Scinax ruber",
                                           Species == "Strabomantis aramunha" ~ "Haddadus aramunha",
                                           Species %in% c("Hypsiboas punctatus", "Boana punctata") ~ "Boana atlantica",
                                           Species == "Rhinella hoogmoedi" ~ "Rhinella dapsilis",
                                           Species == "Leptodactylus bufonius" ~ "Leptodactylus troglodytes",
                                           Species %in% c("Bufo granulosus lutzi", "Bufo granulosus goeldi") ~ "Rhinella granulosa",
                                           Species %in% c("Bufo crucifer", "Bufo ocellatus") ~ "Rhinella crucifer",
                                           Species == "Scinax strigilatus" ~ "Ololygon strigilata",
                                           Species == "Hypsiboas albomarginatus" ~ "Boana albomarginata",
                                           Species == "Elachistocleis ovalis" ~ "Elachistocleis cesarii",
                                           Species %in% c("Phyllomedusa hypochondrialis", "Phyllomedusa nordestina") ~ "Pithecopos nordestinus",
                                           Species == "Scinax acuminata" ~ "Scinax acuminatus",
                                           Species %in% c("Scinax acuminata", "Scinax nasicus") ~ "Scinax x-signatus",
                                           Species == "Hyla decipiens decipiens" ~ "Dendropsophus decipiens",
                                           Species == "Hyla decipiens branneri" ~ "Dendropsophus branneri",
                                           Species == "Hyla leucophyllata" ~ "Dendropsophus elegans",
                                           Species == "Hyla senicula senicula" ~ "Dendropsophus soaresi",
                                           Species == "Rana palmipes" ~ "Lithobates palmipes",
                                           Species == "Pseudopaludicola cf. pocoto" ~ "Pseudopaludicola pocoto",
                                           Species == "Dendropsophus cf. decipiens" ~ "Dendropsophus decipiens",
                                           Species == "Scinax cf. fuscovarius" ~ "Scinax fuscovarius",
                                           Species == "Boana cf. raniceps" ~ "Boana raniceps",
                                           Species == "Scinax cf. pachycrus" ~ "Scinax pachycrus",
                                           Species == "Physalaemus cf. kroyeri" ~ "Physalaemus kroyeri",
                                           Species == "Phyllomedusa azurea" ~ "Pithecopus azureus",
                                           Species == "Leptodactylus ocellatus" ~ "Leptodactylus macrosternum",
                                           Species == "Boana albopunctatus" ~ "Boana albopunctata",
                                           Species == "Proceratophrys caramaschii" ~ "Proceratophrys cristiceps",
                                           Species == "Pseudopaludicola mineira" ~ "Pseudopaludicola pocoto",
                                           Species == "Scinax curicica" ~ "Scinax x-signatus",
                                           .default = Species),
                Species = Species %>% stringr::str_replace_all("Hyla|Hypsiboas", "Boana"))

specieslink_inter_trat$Species %>% unique()

## Intersecção com a grade ----

### Calculando ----

specieslink_grid <- sf::st_join(specieslink_inter_trat, grade, join = st_intersects)

### Visualizando ----

specieslink_grid %>% dplyr::glimpse()

specieslink_grid

specieslink_grid %>%
  dplyr::filter(!is.na(FID)) %>%
  ggplot() +
  geom_sf(data = caa) +
  geom_sf()

## Matriz ----

### Extraindo as informações de grid ----

specieslink_grid_names <- specieslink_grid %>%
  tibble::as_tibble() %>%
  dplyr::select(FID) %>%
  dplyr::rename("Assemblage" = FID) %>%
  dplyr::mutate(Assemblage = paste0("Assemblage ", Assemblage))

specieslink_grid_names

### Extraindo as informações de coordenadas ----

specieslink_grid_coords <- specieslink_grid %>%
  sf::st_coordinates() %>%
  tibble::as_tibble() %>%
  dplyr::rename("Longitude" = X,
                "Latitude" = Y)

gbif_grid_coords

### Criando um tibble ----

specieslink_tibble <- specieslink_inter_trat %>%
  tibble::as_tibble() %>%
  dplyr::select(Species) %>%
  dplyr::mutate(Presence = 1) %>%
  dplyr::relocate(Presence, .before = Species)

specieslink_tibble

### Unindo ----

registros_specieslink <- dplyr::bind_cols(specieslink_grid_names,
                                   specieslink_grid_coords,
                                   specieslink_tibble) %>%
  dplyr::mutate(Source = "SpeciesLink") %>%
  dplyr::distinct(Assemblage, Longitude, Latitude, Species, .keep_all = TRUE)

registros_specieslink %>% dplyr::glimpse()

registros_specieslink

### Exportando ----

registros_specieslink %>%
  openxlsx::write.xlsx("registros_specieslink.xlsx")
