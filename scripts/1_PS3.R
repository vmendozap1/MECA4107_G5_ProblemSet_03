##########################################################
# BDML - MAY 24
# Problem Set # 3 
##########################################################

#rm(list = ls())
if(!require(pacman)) install.packages("pacman") ; require(pacman)
p_load( tidyverse, # tidy-data
        glmnet, # To implement regularization algorithms. 
        caret, # creating predictive models
        skimr,
        knitr,
        kableExtra,
        leaflet, # Mapas interactivos
        visdat, #Visualizaci´pon de missings
        osmdata,
        tmaptools,
        sf,
        GADMTools,
        stringr
)

#Seleccionamos el directorio y Cargamos las bases de datos
setwd("C:/Users/USER/OneDrive - Universidad de los andes/Semestre VIII/Big Data/MECA4107_G5_ProblemSet_03")

#Cargamos las bases de Datos
mzbarrio <- st_read("Data/manzanaestratificacion/ManzanaEstratificacion.shp")
test<-read.csv("Data/test.csv")
train<-read.csv("Data/train.csv")


      ################Visualizaciones################

#Visualizamos las manzanas con Estrato Social asignado
leaflet() %>% 
  addTiles() %>%  #capa base
  addPolygons(data = mzbarrio$geometry, fillColor = "red",color="black", weight = 1)

#Visualizamos los apartamentos por ubicación
leaflet() %>% 
  addTiles() %>%  #capa base
  addCircles(data=coordinates$geometry, fillColor = "blue",color = "blue",weight = 1)

#Plot de estratos por manzana e inmuebles
colores_estrato <- colorNumeric(palette = "RdYlBu", domain = mzbarrio$ESTRATO)
mzbarrio <- st_transform(mzbarrio, crs = 4326)  # Cambia la proyección a WGS84

# Crear el mapa Leaflet
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

leaflet() %>% 
  addTiles() %>%  # Capa base
  addPolygons(data = mzbarrio, fillColor = "#00BFFF", color = "#00BFFF", weight = 1) %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 9) %>% 
  addCircles(data = coordinates$geometry, fillColor = "#104E8B", color = "#104E8B", weight = 1) 
  

#Unificamos lat y lon en una variable
coordinates <- st_as_sf(train, coords = c("lon", "lat"), crs = st_crs(mzbarrio))

#Unificar Estrato por cercanía del inmueble al polígono
mzbarrio <- st_make_valid(mzbarrio)
sf_use_s2(FALSE)
train <- st_join(coordinates, mzbarrio, join=st_nearest_feature)


##########################################################
################ Limpieza de texto #######################

#Normalización
# Todo en minuscula
train <- train %>%
  mutate(description = str_to_lower(description))
# Eliminamos tildes
train <- train %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Eliminamos caracteres especiales
train <- train %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))
# Eliminamos espacios extras
train <- train %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))


############################################################
################ Extracción del Área #######################
#Extraemos Superficie total de los inmuebles 
train <- train %>%
  mutate(area= str_extract(description, "(\\d+) mts|(\\d+) mt2|(\\d+) metros|(\\d+) m2|(\\d+) m^2"))

#Extraemos la variable numérica y seleccionamos el area por percentiles
train <- train %>%
  mutate(area = as.integer(str_extract(area, "\\d+")))  %>%
  mutate(area = ifelse(is.na(area)==F,area, pmin(surface_total, surface_covered, na.rm = TRUE)))

low <- quantile(train$area, 0.05,na.rm=T)
up <- quantile(train$area, 0.90,na.rm=T)
train<- train %>% mutate(area=  ifelse(test=( area>= up), 
                                         yes= NA,
                                         no= area ))%>% 
  mutate(area=  ifelse(test=(area<= low), 
                                         yes= NA,
                                         no= area ))

############################################################
################ Precio por mts^2 #######################

train <- train %>%
  mutate(precio_mt2 = round(price / area, 0))
low <- quantile(train$precio_mt2, 0.05,na.rm=T)
up <- quantile(train$precio_mt2, 0.97,na.rm=T)

#Visualización del precio por m^2 
wdb <- st_drop_geometry(train)
ggplot(wdb, aes(x = precio_mt2)) +
  geom_histogram()+
  theme_classic()

# Estadísticas descriptivas
skim <- skim(train)
skim <- data.frame(skim)
skim <- skim %>%
  kbl() %>%
  kable_classic_2(full_width = F)

#Visulaización de Missings
vis_dat(as.data.frame(train$rooms))
summary(train$surface_total)
summary(train$surface_covered)

summary(train$)

vis_dat(as.data.frame(mzbarrio))

#summary(train)

#matriz de corrrelación
wdb<- wdb %>%
  select(`price`,`month`,`year`,`rooms`,`bedrooms`,bathrooms, ESTRATO,area,precio_mt2)
wdb <- na.omit(wdb)
matriz_correlacion <- cor(wdb)
matriz_correlacion <- matriz_correlacion %>%
  kbl() %>%
  kable_classic_2(full_width = F)





#Graficar en un mapa Por coordenadas
leaflet() %>%
  addTiles() %>%
  addCircles(lng = train$lon, 
             lat = train$lat)

###Mapa Interactivo###
#reescalar VAriables
train$precio_por_mt2_sc =1

#train <- train %>%
#  mutate(precio_por_mt2_sc =( (precio_mt2 - min(precio_mt2)) / (max(precio_mt2) - min(precio_mt2))))

# creamos una variable de color que depende del tipo de inmueble.

train <- train %>%
  mutate(color = case_when(property_type == "Apartamento" ~ "#2A9D8F",
                           property_type == "Casa" ~ "#F4A261"))

# Vamos a crear un mensaje en popup con html
html <- paste0("<b>Precio:</b> ",
               scales::dollar(train$price),
               "<br> <b>Area:</b> ",
               as.integer(train$surface_total), " mt2",
               "<br> <b>Tipo de immueble:</b> ",
               train$property_type,
               "<br> <b>Numero de alcobas:</b> ",
               as.integer(train$rooms),
               "<br> <b>Numero de baños:</b> ",
               as.integer(train$bathrooms),
               "<br> <b>Sector:</b> ",
               train$l4,
               "<br> <b>Barrio:</b> ",
               train$l5)

# Eliminamos los immuebles con área menor a 20
#db <- db %>% filter( surface_covered > 20)


# Encontramos el queremos que sea el centro del mapa 
latitud_central <- mean(train$lat)
longitud_central <- mean(train$lon)

# Creamos el plot
leaflet() %>%
  addTiles() %>%
  setView(lng = longitud_central, lat = latitud_central, zoom = 12) %>%
  addCircles(lng = train$lon, 
             lat = train$lat, 
             col = train$color,
             fillOpacity = 1,
             opacity = 1,
             radius = train$precio_por_mt2_sc*10,
             popup = html)


