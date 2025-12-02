
rm(list=ls()) #Clean the workspace


#####load required packages####
library(raster)
#library(rgdal)
library(sf)
library(dplyr)

############################################################################################################################################

##### 1. Loading predictor data#####

############################################################################################################################################
# dir("data/data_workshop_sebot/")
CHELSA_bio1 <- raster("data/data_workshop_sebot/CHELSA_bio1_1981-2010_V.2.1.tif")
# Temperatura media anual
CHELSA_bio4 <- raster("data/data_workshop_sebot/CHELSA_bio4_1981-2010_V.2.1.tif")
# Estacionalidad de la temperatura
CHELSA_bio12 <- raster("data/data_workshop_sebot/CHELSA_bio12_1981-2010_V.2.1.tif")
# Precipitación anual

#Stacking rasters

predictors <- raster::stack(CHELSA_bio1,CHELSA_bio4,CHELSA_bio12)
predictors
#plot(predictors)
proj4string(predictors) 

#If I wanted all files in a folder:
##files <- list.files(path = "C:/Users/adria/OneDrive/Desktop/GIS variables/Climate/Chelsa (1981-2010)", pattern = ".tif$", full.names=T)
##files
##predictors <- raster::stack(files)
##predictors

############################################################################################################################################

##### 2. Load presence points (2.1. CSV format AND 2.2. shapefile format) #####

############################################################################################################################################

##### 2.1. Load presence points (CSV format) #####

############################################################################################################################################

occurrence_data_csv <- read.csv("data/data_workshop_sebot/2. Data_GBIF_clean.csv", header = TRUE) #read in the data and assign it a name in English csv  AND put dec="." if the deimals are ","
str(occurrence_data_csv)

#subset the table to have only x and y point coordinates (for "extract" function below)
# p_xy_csv <- occurrence_data_csv[ , c("lon","lat")]
p_xy_csv <- occurrence_data_csv[ , c("decimalLongitude","decimalLatitude")]

head(p_xy_csv)

############################################################################################################################################

##### 2.2. Load presence points (shapefile format) #####

############################################################################################################################################

# occurrence_data_shapefile <- read_sf("data/puntos_gbif_clean.shp") #do this only if you have a shapefile
# 
# #Make sure the projections are the same
# st_crs(occurrence_data_shapefile)
# st_crs(CHELSA_bio1)
# 
# #Extracting coordinates
# p_xy_shapefile <- as.data.frame(st_coordinates(occurrence_data_shapefile))
# plot(p_xy_shapefile)

############################################################################################################################################

##### 3. Extract raster predictor values and make a data table #####

############################################################################################################################################

#Extract the predictor values for presence points
# IMPORTANTE. LE PONEMOS DATOS DE TEMPERATURA, PRECIPITACIONES, ETC A CADA UNA DE LAS OBSERVACIONES DE PLANTAS. 

CHELSA_bio1 <- extract(CHELSA_bio1, p_xy_csv)
head(CHELSA_bio1)#Check table
CHELSA_bio4 <- extract(CHELSA_bio4, p_xy_csv)
CHELSA_bio12 <- extract(CHELSA_bio12, p_xy_csv)

#Create a data.frame with gbifID

predictor_values <- data.frame(occurrence_data_csv$gbifID)

#Append the the species name and coordinates for the presence points
predictor_values$species <-  
  occurrence_data_csv$species
predictor_values$lon <-
  occurrence_data_csv$decimalLongitude
predictor_values$lat <-
  occurrence_data_csv$decimalLatitude

#Append predictor values in the same data.frame

predictor_values$CHELSA_bio1 <- CHELSA_bio1
predictor_values$CHELSA_bio4 <- CHELSA_bio4
predictor_values$CHELSA_bio12 <- CHELSA_bio12

head(predictor_values)

#Changing raster values to get correct values (from CHELSA technical specifications; https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf)

# predictor_values <- predictor_values %>%
#   # mutate(CHELSA_bio1 = CHELSA_bio1*0.1-273.15) %>%
#   mutate(CHELSA_bio4 = CHELSA_bio4*0.1) %>%
#   mutate(CHELSA_bio12 = CHELSA_bio12*0.1)
  
head(predictor_values)

#Save data.frame

write.csv(predictor_values,"4. Climatic data.csv")

############################################################################################################################################

##### 4. Plotting predictor values #####

############################################################################################################################################

library(ggplot2)
library(RColorBrewer)

################################################################################## 

#Mean annual temperature - Violin plots

################################################################################## 

brewer.pal(n = 3, name = "RdBu") #To know the colors

plot_mean_temp <- ggplot(predictor_values, aes(x=species, y=CHELSA_bio1, group = species, fill = species)) + 
  geom_violin() + geom_boxplot(width=0.1) + theme_bw() +
  scale_fill_manual(values = c("#67A9CF", "#F7F7F7", "#EF8A62")) + 
  xlab("Species") +
  labs(title = "B. Mean temperature (ºC)") +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size=18))

plot_mean_temp

################################################################################## 

#Temperature seasonality - Violin plots

################################################################################## 

plot_temp_season <- ggplot(predictor_values, aes(x=species, y=CHELSA_bio4, group = species, fill = species)) + 
  geom_violin() + geom_boxplot(width=0.1) + theme_bw() +
  scale_fill_manual(values = c("#67A9CF", "#F7F7F7", "#EF8A62")) + 
  xlab("Species") +
  labs(title = "C. Temperature seasonality (ºC/100)") +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size=18))

plot_temp_season
# Es una idea de la estacionalidad

############################################################################################################################################

#Annual precipitation amount - Violin plots

################################################################################## 

plot_annual_precip <- ggplot(predictor_values, aes(x=species, y=CHELSA_bio12, group = species, fill = species)) + 
  geom_violin() + geom_boxplot(width=0.1) + theme_bw() +
  scale_fill_manual(values = c("#67A9CF", "#F7F7F7", "#EF8A62")) + 
  xlab("Species") +
  labs(title = "A. Annual precipitation (mm)") +
  theme(legend.position = "none",
        axis.title.x = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 14),
        plot.title = element_text(size=18))

plot_annual_precip


##Filtering out outliers

luzu <- predictor_values %>% 
  dplyr::filter(species == "Luzula caespitosa")

out_vals <- boxplot.stats(luzu$CHELSA_bio1)$out
out_vals
out_vals2 <- boxplot.stats(luzu$CHELSA_bio4)$out
out_vals2


outliers_bio1 <- luzu %>% 
  dplyr::filter(CHELSA_bio1 %in% out_vals)

outliers_bio1_sf <- st_as_sf(
  outliers_bio1,
  coords = c("lon", "lat"),
  crs = 4326     #which one?
)


outliers_bio4 <- luzu %>% 
  dplyr::filter(CHELSA_bio4 %in% out_vals2)

outliers_bio4_sf <- st_as_sf(
  outliers_bio4,
  coords = c("lon", "lat"),
  crs = 4326     #which one?
)


library(htmlwidgets)
library(leaflet)
library(leaflet)


mapa<-leaflet(outliers_bio4_sf) %>%
  addProviderTiles("OpenStreetMap") %>%   # capa base simple
  # addPolygons(data = iberoatlantic,
  #             color = "blue",
  #             weight = 2,
  #             fill = FALSE) %>%
  addCircleMarkers(
    radius = 3,   # <- color según species
    stroke = FALSE,
    fillOpacity = 0.7      # opcional: ver el nombre al pinchar
  )

mapa

# PARA SACAR LOS OUTLAYERS EN DOS O MÁS VARIABLES A LA VEZ. 

ids_comunes <- intersect(
  outliers_bio1$`occurrence_data_csv.gbifID`,
  outliers_bio4$`occurrence_data_csv.gbifID`
)


ids_comunes_df <- intersect(
  outliers_bio1,
  outliers_bio4
)

predictor_values <- predictor_values %>% filter (!occurrence_data_csv.gbifID==ids_comunes)



