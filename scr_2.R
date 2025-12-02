library(dplyr);library(sf);library(raster)


##Loading data

#Species data
data_gbif <- read.csv("data/2. Data_GBIF_clean.csv")
unique (data_gbif$species)

#Study area
iberoatlantic <- read_sf("data/IberoAtlantic_workshop.shp")
plot(iberoatlantic$geometry)

#Transforming species data into spatial object
data_gbif_sf <- st_as_sf(
  data_gbif,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs =      #which one?
)
plot(data_gbif_sf$geometry)

##Curating data

#cropping species data with study area
data_gbif_crop <- st_intersection(data_gbif_sf, iberoatlantic)
#Any problem? maybe we should check the projection

#Information on data projection
st_crs(iberoatlantic)         
st_crs(iberoatlantic)$epsg 

st_crs(data_gbif_sf)$epsg 

#Projecting to a common reference system
iberoatlantic <- st_transform(iberoatlantic, 4326)
data_gbif_crop <- st_intersection(data_gbif_sf, iberoatlantic)


##Function for distance filtering

library(units)

df_sf <- data_gbif_crop
distancia_minima =2000
data=df_sf
filtrar_puntos_por_distancia_por_nombre <- function(data, distancia_minima) {
  
  distancia_minima_units <- set_units(distancia_minima, "meters")
  
  # AGRUPACIÓN POR species (cambia aquí si quieres otro campo)
  subsets <- split(data, data$species)
  
  resultados <- lapply(subsets, function(subdata) {
    n <- nrow(subdata)
    eliminar <- logical(n)
    
    for (i in 1:(n - 1)) {
      if (!eliminar[i]) {
        distancias <- st_distance(subdata[i, ], subdata[(i + 1):n, ])
        cercanos <- which(distancias < distancia_minima_units)
        eliminar[cercanos + i] <- TRUE
      }
    }
    
    return(subdata[!eliminar, ])
  })
  
  resultado_combinado <- do.call(rbind, resultados)
  return(resultado_combinado)
}

#Runnign the function
df_filtrado <- filtrar_puntos_por_distancia_por_nombre(df_sf, distancia_minima = 2000)

#opcional si quieres simplificar el dataset
#df_filtrado <- df_filtrado %>% dplyr::select(species)


##Preparing the data to export
coords <- st_coordinates(df_filtrado)
df_filtrado_df <- as.data.frame(df_filtrado)

df_filtrado_df$lon <- coords[, "X"]
df_filtrado_df$lat <- coords[, "Y"]

df_filtrado_df$geometry <- NULL

#Export in csv
write.csv(df_filtrado_df, "C:/Users/adria/OneDrive/Desktop/Proyectos - Asturias/Simposio Sebot 2025/puntos_gbif_clean.csv")

#Export in .shp
st_write(
  df_filtrado,
  "data/puntos_gbif_clean.shp",
  delete_layer = TRUE   # por si ya existe
)


##Quick mapping in R

library(leaflet)

pal <- colorFactor(
  palette = terrain.colors(length(unique(data_gbif_crop$species))),
  domain  = data_gbif_crop$species
)
pal <- colorFactor(
  palette = rainbow(length(unique(data_gbif_crop$species))),
  domain  = data_gbif_crop$species
)
# otras opciones base:
# palette = heat.colors(...)
# palette = topo.colors(...)
# palette = cm.colors(...)
# palette = terrain.colors

mapa<-leaflet(data_gbif_crop) %>%
  addProviderTiles("OpenStreetMap") %>%   # capa base simple
  addPolygons(data = iberoatlantic,
              color = "blue",
              weight = 2,
              fill = FALSE) %>%
  addCircleMarkers(
    radius = 3,
    color = ~pal(species),   # <- color según species
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~species         # opcional: ver el nombre al pinchar
  ) %>%
  addLegend(
    "bottomright",
    pal = pal,
    values = ~species,
    title = "Species",
    opacity = 1
  )

mapa

##Export in HTML
library(htmlwidgets)

saveWidget(
  widget = mapa,
  file = "mapa_interactivo.html",
  selfcontained = TRUE   # incluye dependencias dentro del HTML
)

