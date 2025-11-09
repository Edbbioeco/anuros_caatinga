# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(readxl)

library(geodata)

library(here)

library(tidyterra)

library(elevatr)

library(terra)

library(openxlsx)

library(usdm)

library(reshape2)

library(viridis)

library(vegan)

library(fields)

# Dados ----

## Caatinga ----

### Importando ----

caa <- geobr::read_biomes() %>%
  dplyr::filter(name_biome == "Caatinga")

### Visualizando ----

caa

ggplot() +
  geom_sf(data = caa,color = "black", fill = "gold")

## Registros ----

### Importando ----

matriz <- readxl::read_xlsx("registros_finais.xlsx")

### Transformando em shapefile ----

matriz_shp <- matriz %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = caa %>% sf::st_crs())

### Visualizando ----

matriz

matriz_shp

ggplot() +
  geom_sf(data = caa, color = "black", fill = "gold") +
  geom_sf(data = matriz_shp)

## Variáveiis bioclimáticas ----

### Importando ----

bio <- geodata::worldclim_country(country = "BRA",
                                  var = "bio",
                                  path = here::here(),
                                  res = 0.5)

### Visualizando ----

bio

ggplot() +
  tidyterra::geom_spatraster(data = bio[[1]]) +
  scale_fill_viridis_c(na.value = "transparent")

## Altitude ----

### Importando ----

alt <- elevatr::get_elev_raster(locations = caa,
                                prj = sf::st_crs(caa),
                                z = 10) %>%
  terra::rast()

### Visualizando ----

alt

ggplot() +
  tidyterra::geom_spatraster(data = alt) +
  scale_fill_viridis_c(na.value = "transparent")

# Dados ambientais preditores ----

## Corrigindo o CRS ----

### Caatinga ----

caa_trat <- caa %>%
  sf::st_transform(crs = bio %>% terra::crs())

caa_trat

### Pontos ----

registros <- matriz_shp %>%
  sf::st_transform(crs = bio %>% terra::crs())

registros

### Altitude ----

terra::crs(alt) <- bio %>% terra::crs()

terra::crs(alt) == terra::crs(bio)

alt

## Recortando para a caatinga ----

### Variáveis Bioclimáticas ----

vars <- bio %>%
  terra::crop(caa_trat) %>%
  terra::mask(caa_trat)

vars

ggplot() +
  tidyterra::geom_spatraster(data = vars[[1]]) +
  geom_sf(data = caa, color = "red", fill = "transparent") +
  scale_fill_viridis_c(na.value = "transparent")

### Altitude ----

alt_trat <- alt %>%
  terra::crop(caa_trat) %>%
  terra::mask(caa_trat) %>%
  terra::resample(vars)

alt_trat

ggplot() +
  tidyterra::geom_spatraster(data = alt_trat) +
  geom_sf(data = caa, color = "red", fill = "transparent") +
  scale_fill_viridis_c(na.value = "transparent")

## Criando um rasters unificado ----

### Unificando ----

vars_trat <- c(vars, alt_trat)

vars_trat

### Alterando os nomes ----

names(vars_trat) <- c(paste0("Bio ", 01:19), "Altitude")

### Visualizando ----

vars_trat[[1:16]] %>% plot()

vars_trat[[17:20]] %>% plot()

## Extraindo os valores das variáveis ambientais ----

### Extraindo ----

dados_amb <- vars_trat %>%
  terra::extract(matriz_shp)

### Tratando ----

dados_amb <- dados_amb %>%
  dplyr::rename("Assemblage" = ID)

### Visualizando ----

dados_amb %>% dplyr::glimpse()

dados_amb

### Exportando ----

dados_amb %>%
  openxlsx::write.xlsx("dados_ambientais.xlsx")

## Multicolinearidade ----

### VIF ----

options(scipen = 999)

dados_amb %>%
  dplyr::select_if(is.numeric) %>%
  usdm::vif() %>%
  dplyr::arrange(VIF %>% desc())

options(scipen = 0)

### Índice de Correlação de Spearman ----

#### Calculando ----

correlacao <- dados_amb %>%
  dplyr::select_if(is.numeric) %>%
  cor(method = "spearman")

correlacao

#### Visualizando ----

correlacao_matriz <- correlacao %>% as.matrix()

correlacao_matriz[upper.tri(correlacao_matriz)] <- NA

correlacao_matriz

correlacao_matriz %>%
  reshape2::melt() %>%
  na.omit() %>%
  dplyr::mutate(igual = dplyr::case_when(Var1 == Var2 ~ "sim",
                                         .default = "não"),
                value = value %>% round(2)) %>%
  dplyr::filter(igual == "não") %>%
  dplyr::select(-igual) %>%
  dplyr::rename("Spearman Correlation Index" = value) %>%
  ggplot(aes(Var1, Var2, fill = `Spearman Correlation Index`, label = `Spearman Correlation Index`)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(color = "black") +
  coord_equal() +
  scale_fill_gradientn(breaks = seq(-1, 1, 0.2),
                       limits = c(-1, 1),
                       colours = c(viridis::viridis(n = 10) %>% rev(), viridis::viridis(n = 10))) +
  labs(x = NULL,
       y = NULL) +
  guides(fill = guide_colourbar(title.position = "right",
                                title.hjust = 0.5,
                                barwidth = 1.5,
                                barheight = 20,
                                frame.colour = "black",
                                ticks.colour = "black",
                                ticks.linewidth = 0.5)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12, angle = -90),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(angle = -90, color = "black", size = 15))

ggsave(filename = "correlacao.png", height = 10, width = 12)

# Teste de Mantel ----

## Criadno uma matriz de composição ----

matriz_comp <- matriz %>%
  dplyr::select(6:177)

matriz_comp

## Criadno uma matriz de coordenadas ----

matriz_dist <- matriz %>%
  dplyr::select(2:3)

matriz_dist

## Calculando as distâncias ----

### Distância na composição ----

comp_dist <- matriz_comp %>%
  vegan::vegdist(method = "jaccard")

comp_dist

### Distância geográfica ----

geo_dist <- matriz_dist %>%
  as.data.frame() %>%
  fields::rdist.earth(miles = F) %>%
  as.dist()

geo_dist

## Teste de Mantel ----

mantel <- vegan::mantel(geo_dist, comp_dist, permutations = 1000,
                        method = "spearman")

mantel

## Gráfico ----

distancias <- tibble::tibble(`Geographic Distance` = geo_dist %>%
                               as.matrix() %>%
                               reshape2::melt() %>%
                               dplyr::pull(3),
                             `Compositional Distance` = comp_dist %>%
                               as.matrix() %>%
                               reshape2::melt() %>%
                               dplyr::pull(3))

distancias

estatística <- mantel$statistic %>% round(3)

estatística

p <- if(mantel$signif < 0.01){
                    "< 0.01"} else{
                      paste0("p = ", mantel$signi %>% round(3))
                    }

p


distancias %>%
  ggplot(aes(`Geographic Distance`, `Compositional Distance`)) +
  geom_point(shape = 21, color = "black", fill = "gold") +
  geom_smooth(method = "lm", color = "blue", fill = "blue", alpha = 0.3) +
  labs(title = stringr::str_glue("r = {estatística}, p {p}")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title = element_text(color = "black", size = 15))

ggsave(filename = "teste_mantel.png", height = 10, width = 12)
