# Instalar e carregar pacotes necessários
install.packages("sf")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("ggmap")
library(sf)
library(ggplot2)
library(dplyr)
library(ggmap)

# 1. Carregar o shapefile do mapa de São Paulo
shapefile_sp <- st_read("C:/Users/Neto/Documents/Sistema-de-Bibliotecas-de-Sao-Paulo/shapefile/DEINFO_DISTRITO.shp")

# Verificar o CRS do shapefile e transformar para WGS84 se necessário
st_crs(shapefile_sp)  # Verificar CRS atual
shapefile_sp_wgs84 <- st_transform(shapefile_sp, crs = 4326)

# 2. Carregar e limpar os dados das bibliotecas
bib_sp <- readxl::read_excel("C:/Users/Neto/Documents/Sistema-de-Bibliotecas-de-Sao-Paulo/dados-jan-jun.xls")

# Filtrar dados
remover <- "Ponto fechado"
bib_sp_filtrado <- bib_sp %>%
  filter(!grepl(remover, Endereços, ignore.case = TRUE))

# Adicionar coordenadas

register_google(key = "insert api key")

bib_sp_filtrado$geocode <- geocode(bib_sp_filtrado$Endereços)
bib_sp_filtrado$latitude <- bib_sp_filtrado$geocode$lat
bib_sp_filtrado$longitude <- bib_sp_filtrado$geocode$lon

# Converter dados das bibliotecas em um objeto sf
bib_sf <- st_as_sf(bib_sp_filtrado, coords = c("longitude", "latitude"), crs = 4326)

# 3. Definir cores para os tipos de serviço
bib_sf <- bib_sf %>%
  mutate(Servico = case_when(
    Tipos_de_serviços == "Bibliotecas Públicas Municipais" ~ "blue",
    Tipos_de_serviços == "Pontos de Leitura" ~ "red",
    Tipos_de_serviços == "Bosques de Leitura" ~ "green",
    Tipos_de_serviços == "Ônibus da Cultura" ~ "purple",
    TRUE ~ "gray"
  ))

# 4. Plotar o mapa
ggplot() +
  geom_sf(data = shapefile_sp_wgs84, fill = "lightblue", color = "gray", alpha = 0.5) +
  geom_sf(data = bib_sf, aes(color = Servico), size = 3, shape = 16) +
  scale_color_identity(guide = "legend", 
                       labels = c("Bibliotecas Públicas Municipais",
                                  "Pontos de Leitura",
                                  "Bosques de Leitura",
                                  "Ônibus da Cultura")) + 
  theme_minimal() +
  labs(title = "Mapa de Bibliotecas em São Paulo",
       subtitle = "Distribuição das bibliotecas na cidade") +
  theme(legend.position = "bottom")

  
