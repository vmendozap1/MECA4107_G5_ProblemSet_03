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
        stringr,
        ggplot2,
        plotly,
        wordcloud,
        tidytext,
        SentimentAnalysis,
        tm,
        udpipe,
        syuzhet
        
)

#Seleccionamos el directorio y Cargamos las bases de datos
setwd("C:/Users/USER/OneDrive - Universidad de los andes/Semestre VIII/Big Data/MECA4107_G5_ProblemSet_03")
#setwd("C:/Users/madag/OneDrive - Universidad de los andes/Semestre VIII/Big Data/MECA4107_G5_ProblemSet_03")


#Cargamos las bases de Datos
mzbarrio <- st_read("Data/manzanaestratificacion/ManzanaEstratificacion.shp")
barrio <- st_read("Data/manzanaestratificacion/barrios-bogota.geojson")
test<-read.csv("Data/test.csv")
train<-read.csv("Data/train.csv")


#Unificamos lat y lon en una variable
train$longitud <- train$lon
train$latitud <- train$lat
test$longitud <- test$lon
test$latitud <- test$lat

mzbarrio <- st_transform(mzbarrio, crs = 4326)
barrio <- st_transform(barrio, crs = 4326)
coordinates_tr <- st_as_sf(train, coords = c("lon", "lat"), crs = st_crs(mzbarrio))
coordinates_te <- st_as_sf(test, coords = c("lon", "lat"), crs = st_crs(mzbarrio))

##########################################################
###########Primeras Visualizaciones #####################

#Visualizamos las manzanas con Estrato Social asignado
leaflet() %>% 
  addTiles() %>%  #capa base
  addPolygons(data = mzbarrio$geometry, fillColor = "red",color="black", weight = 1)

#Visualizamos los barrios de BTAo
leaflet() %>% 
  addTiles() %>%  #capa base
  addPolygons(data = barrio$geometry, fillColor = "red",color="black", weight = 1)

#Visualizamos los apartamentos por ubicación
leaflet() %>% 
  addTiles() %>%  #capa base
  addCircles(data=coordinates_tr$geometry, fillColor = "blue",color = "blue",weight = 1)

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
  addCircles(data = coordinates_tr$geometry, fillColor = "#104E8B", color = "#104E8B", weight = 1) 
  

##########################################################
### Agregamos la variable estrato ########################
#Unificar Estrato por cercanía del inmueble al polígono

mzbarrio <- st_make_valid(mzbarrio)
sf_use_s2(FALSE)
train<- st_join(coordinates_tr, mzbarrio, join=st_nearest_feature)
test <- st_join(coordinates_te, mzbarrio, join=st_nearest_feature)

##########################################################
### Agregamos la variable Barrio ########################
#Unificar Bariro por cercanía del inmueble al polígono
names(barrio)[names(barrio) == "nombre"] <- "barrio"
barrio <- st_make_valid(barrio)
sf_use_s2(FALSE)
train<- st_join(train, barrio, join=st_nearest_feature)
test <- st_join(test, barrio, join=st_nearest_feature)


##########################################################
### Agregamos Distamcia al CC más cercano ################


# Extraemos la info de todos los CCs
cc <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "shop" , value = "mall") 

# Cambiamos el formato para que sea un objeto sf (simple features)
cc_sf <- osmdata_sf(cc)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
cc_geometria <- cc_sf$osm_polygons %>% 
  dplyr::select(osm_id, name) 

# Guardemos los poligonos de los CC 
cc_geometria <- st_as_sf(cc_sf$osm_polygons)

# Calculamos el centroide de cada CC para aproximar su ubciacion como un solo punto 
cc_centroides <- st_centroid(cc_geometria, byid = T)
cc_centroides <- cc_centroides %>%
  mutate(x=st_coordinates(cc_centroides)[, "X"]) %>%
  mutate(y=st_coordinates(cc_centroides)[, "Y"])

#Visualización de los CC
leaflet() %>%
  addTiles() %>%
  addPolygons(data = cc_geometria, col = "red",weight = 10,
              opacity = 0.8,popup = cc_geometria$name) %>%
  addCircles(lng = cc_centroides$x, 
             lat = cc_centroides$y, 
             col = "darkblue", opacity = 0.5,radius =1)
#Rectificación de cooredenadas
cc_centroides_sf <- st_as_sf(cc_centroides, coords = c("x", "y"), crs=4326)
cc_sf_tr<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
cc_sf_te<- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)

# Calculo de la distancia tr
dist_matrix_tr <- st_distance(x = cc_sf_tr, y = cc_centroides_sf)
dist_min_tr <- apply(dist_matrix_tr, 1, min)  
# Calculo de la distancia te
dist_matrix_te <- st_distance(x = cc_sf_te, y = cc_centroides_sf)
dist_min_te <- apply(dist_matrix_te, 1, min)  

# Agregamos a Train y Test
train <- train %>% mutate(distancia_cc = dist_min_tr)
test <- test %>% mutate(distancia_cc = dist_min_te)



plot <- ggplot(train, aes(x = distancia_cc)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un Centro Comercial en metros", y = "Cantidad",
       title = "Distribución de la distancia a los Centros Comerciales") +
  theme_bw()

ggplotly(plot)


##########################################################
### Agregamos Distamcia al Supermercado más cercano ######

# Extraemos la info de todos los SMs
sm <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key = "shop" , value = "supermarket") 

# Cambiamos el formato para que sea un objeto sf (simple features)
sm_sf <- osmdata_sf(sm)

# De las features del parque nos interesa su geomoetría y donde estan ubicados 
sm_geometria <- sm_sf$osm_polygons %>% 
  dplyr::select(osm_id, name) 

# Guardemos los poligonos de los parques 
sm_geometria <- st_as_sf(sm_sf$osm_polygons)

# Calculamos el centroide de cada parque para aproximar su ubciacion como un solo punto 
sm_centroides <- st_centroid(sm_geometria, byid = T)
sm_centroides <- sm_centroides %>%
  mutate(x=st_coordinates(sm_centroides)[, "X"]) %>%
  mutate(y=st_coordinates(sm_centroides)[, "Y"])

#Visualización de los sm
leaflet() %>%
  addTiles() %>%
  addPolygons(data = sm_geometria, col = "red",weight = 10,
              opacity = 0.8,popup = sm_geometria$name) %>%
  addCircles(lng = sm_centroides$x, 
             lat = sm_centroides$y, 
             col = "darkblue", opacity = 0.5,radius =1)
#Rectificación de cooredenadas
sm_centroides_sf <- st_as_sf(sm_centroides, coords = c("x", "y"), crs=4326)
sm_sf_tr<- st_as_sf(train, coords = c("lon", "lat"),  crs = 4326)
sm_sf_te<- st_as_sf(test, coords = c("lon", "lat"),  crs = 4326)

# Calculo de la distancia tr
dist_matrix_tr <- st_distance(x = sm_sf_tr, y = sm_centroides_sf)
dist_min_tr <- apply(dist_matrix_tr, 1, min)  
# Calculo de la distancia te
dist_matrix_te <- st_distance(x = sm_sf_te, y = sm_centroides_sf)
dist_min_te <- apply(dist_matrix_te, 1, min)  

# Agregamos a Train y Test
train <- train %>% mutate(distancia_sm = dist_min_tr)
test <- test %>% mutate(distancia_sm = dist_min_te)


plot <- ggplot(train, aes(x = distancia_sm)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Distancia mínima a un Centro Comercial en metros", y = "Cantidad",
       title = "Distribución de la distancia a los Centros Comerciales") +
  theme_bw()
ggplotly(plot)


##########################################################
################ Outliers         #######################
#Baños
train<- train %>% mutate(bathrooms=  ifelse(test=( bathrooms>= 5), 
                                     yes= 5,
                                     no= bathrooms ))%>% 
  mutate(bathrooms=  ifelse(test=(bathrooms<= 1), 
                       yes= 1,
                       no= bathrooms ))

test<- test %>% mutate(bathrooms=  ifelse(test=( bathrooms>= 5), 
                                          yes= 5,
                                          no= bathrooms ))%>% 
  mutate(bathrooms=  ifelse(test=(bathrooms==0), 
                            yes= 1,
                            no= bathrooms ))

impute_banio <- function(df) {
  df %>%
    group_by(ESTRATO, bedrooms,rooms) %>%
    mutate(bathrooms = ifelse(is.na(bathrooms), floor(mean(bathrooms, na.rm = TRUE)), bathrooms)) %>%
    ungroup()
}

impute_banio_2 <- function(df) {
  df %>%
    group_by(ESTRATO) %>%
    mutate(bathrooms = ifelse(is.na(bathrooms), floor(mean(bathrooms, na.rm = TRUE)), bathrooms)) %>%
    ungroup()
}

train <- impute_banio(train)
test <- impute_banio(test)
train <- impute_banio_2(train)
test <- impute_banio_2(test)

summary(test)

#Habitaciones
train<- train %>% mutate(rooms=  ifelse(test=( rooms>= 6), 
                                            yes= 6,
                                            no= rooms ))%>% 
  mutate(rooms=  ifelse(test=(rooms<= 1), 
                            yes= 1,
                            no= rooms ))

test<- test %>% mutate(rooms=  ifelse(test=( rooms>= 6), 
                                          yes= 6,
                                          no= rooms ))%>% 
  mutate(rooms=  ifelse(test=(rooms==0), 
                            yes= 1,
                            no= rooms ))

impute_rooms <- function(df) {
  df %>%
    group_by(ESTRATO, bedrooms,bathrooms) %>%
    mutate(rooms = ifelse(is.na(rooms), floor(mean(rooms, na.rm = TRUE)), rooms)) %>%
    ungroup()
}

impute_rooms_2 <- function(df) {
  df %>%
    group_by(ESTRATO) %>%
    mutate(rooms = ifelse(is.na(rooms), floor(mean(rooms, na.rm = TRUE)), rooms)) %>%
    ungroup()
}

train <- impute_rooms(train)
test <- impute_rooms(test)
train <- impute_rooms_2(train)
test <- impute_rooms_2(test)

#Alcobas
train<- train %>% mutate(bedrooms=  ifelse(test=( bedrooms>= 6), 
                                        yes= 6,
                                        no= bedrooms ))%>% 
  mutate(bedrooms=  ifelse(test=(bedrooms<= 1), 
                        yes= 1,
                        no= bedrooms ))

test<- test %>% mutate(bedrooms=  ifelse(test=( bedrooms>= 6), 
                                      yes= 6,
                                      no= bedrooms ))%>% 
  mutate(bedrooms=  ifelse(test=(bedrooms== 0), 
                        yes= 1,
                        no= bedrooms ))

impute_bedrooms <- function(df) {
  df %>%
    group_by(ESTRATO, bedrooms,bathrooms) %>%
    mutate(bedrooms = ifelse(is.na(bedrooms), floor(mean(bedrooms, na.rm = TRUE)), bedrooms)) %>%
    ungroup()
}

impute_bedrooms_2 <- function(df) {
  df %>%
    group_by(ESTRATO) %>%
    mutate(bedrooms = ifelse(is.na(bedrooms), floor(mean(bedrooms, na.rm = TRUE)), bedrooms)) %>%
    ungroup()
}

train <- impute_bedrooms(train)
test <- impute_bedrooms(test)
train <- impute_bedrooms_2(train)
test <- impute_bedrooms_2(test)



##########################################################
################ Limpieza de texto #######################
##Normalización
#Train
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

#Test
test <- test %>%
  mutate(description = str_to_lower(description))
# Eliminamos tildes
test <- test %>%
  mutate(description = iconv(description, from = "UTF-8", to = "ASCII//TRANSLIT"))
# Eliminamos caracteres especiales
test <- test %>%
  mutate(description = str_replace_all(description, "[^[:alnum:]]", " "))
# Eliminamos espacios extras
test <- test %>%
  mutate(description = str_trim(gsub("\\s+", " ", description)))

############################################################
################ Extracción del Área #######################
#Extraemos Superficie total de los inmuebles 
#Train
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
#Test
test <- test %>%
  mutate(area= str_extract(description, "(\\d+) mts|(\\d+) mt2|(\\d+) metros|(\\d+) m2|(\\d+) m^2"))

#Extraemos la variable numérica y seleccionamos el area por percentiles
test <- test %>%
  mutate(area = as.integer(str_extract(area, "\\d+")))  %>%
  mutate(area = ifelse(is.na(area)==F,area, pmin(surface_total, surface_covered, na.rm = TRUE)))

low <- quantile(test$area, 0.05,na.rm=T)
up <- quantile(test$area, 0.90,na.rm=T)
test<- test %>% mutate(area=  ifelse(test=( area>= up), 
                                       yes= NA,
                                       no= area ))%>% 
  mutate(area=  ifelse(test=(area<= low), 
                       yes= NA,
                       no= area ))
############################################################
################ Imputamos area #########################
#Imputamos dadas caracaterísticas de otros aptos similares

impute_area <- function(df) {
  df %>%
    group_by(bathrooms, ESTRATO, bedrooms,rooms) %>%
    mutate(area = ifelse(is.na(area), mean(area, na.rm = TRUE), area)) %>%
    ungroup()}

impute_area_2 <- function(df) {
  df %>%
    group_by(ESTRATO) %>%
    mutate(area = ifelse(is.na(area), mean(area, na.rm = TRUE), area)) %>%
    ungroup()
}


train <- impute_area(train)
test <- impute_area(test)
train <- impute_area_2(train)
test <- impute_area_2(test)

summary(train)
summary(test)

############################################################
################ Codigo UPZ #########################

train$codigo_upz[is.na(train$codigo_upz)] <- 0
test$codigo_upz[is.na(test$codigo_upz)] <- 0

############################################################
########## Filtrado  de texto: Descripción  ################
#Creamos el Corpus
descripcion <- train$description

#preprocesamiento
descripcion <- removeNumbers(descripcion)
descripcion <- removePunctuation(descripcion)
descripcion <- tolower(descripcion)
descripcion <- stripWhitespace(descripcion)

#Tokenizamos
descripcion_tidy <- as.data.frame(descripcion) %>% unnest_tokens( "word", descripcion)

#Palabras Freceuntes
descripcion_tidy  %>% 
  count(word, sort = TRUE)   %>% 
  head()

#Eliminamos Stopwords
descripcion_tidy <- descripcion_tidy  %>% 
  anti_join(tibble(word =stopwords("spanish")))

#Analisis Exploratorio
wcloud <- wordcloud(descripcion_tidy$word, min.freq = 100, 
          colors= c(rgb(72/255, 191/255, 169/255),rgb(249/255, 220/255, 92/255), rgb(229/255, 249/255, 147/255))) 

# Definimos stop words adicionales 
custom_stopwords <- c("comida", "restaurante", "lugar", "día")

# Eliminamos los nuevos stop words
descripcion_tidy <- anti_join(descripcion_tidy, data.frame(word = custom_stopwords))

#Raiz de la Palabra

descripcion_tidy$radical <- stemDocument( descripcion_tidy$word, language="spanish")
descripcion_tidy %>% head()

#N-gramas
# Generar bigramas a partir del texto de las críticas
bigrams <- as.data.frame(descripcion) %>%
  unnest_tokens(bigram, descripcion, token = "ngrams", n = 2)


stop_words <- data.frame(word1 = stopwords("es"), 
                         word2 = stopwords("es"))


# Eliminar los bigramas que contengan palabras de parada
bigrams <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  anti_join(stop_words, by = "word1") %>%
  anti_join(stop_words, by = "word2") %>%
  unite(bigram, word1, word2, sep = " ")

# Calcular la frecuencia de los bigramas
bigram_freq <- bigrams %>%
  count(bigram, sort = TRUE)

# Visualizar los bigramas más frecuentes
ggplot(bigram_freq[1:10, ], aes(y = reorder(bigram, -n), x = n)) +
  geom_bar(stat = "identity", fill = "orange") +
  ggtitle("Bigramas más frecuentes") +
  ylab("Bigramas") +
  xlab("Frecuencia")

model <- udpipe_download_model(language = "spanish")

model <- udpipe_load_model(model$file_model)

# Separar bigramas 
bigrams_sep <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") 

# Anotar las partes del discurso en las reseñas
reviews_annotated1 <- udpipe_annotate(model, bigrams_sep$word1)


# Convertir los resultados a formato tibble
reviews_tibble1 <- as.data.frame(reviews_annotated1) %>%
  select(token, upos)

# Filtrar solo los adjetivos
adjetivos1 <- reviews_tibble1 %>%
  filter(upos == "ADJ") %>%
  select(token)

# Asociar los sentiemintos (esto puede ser demorado)
sentimientos_df <- get_nrc_sentiment(adjetivos1$token, lang="spanish")
sentimientos_df$adjetivos  = adjetivos1$token

head(sentimientos_df, 10)

############################################################
################ Precio por mts^2 #########################
#Train
#train <- train %>%
#  mutate(precio_mt2 = round(price / area, 0))
#low <- quantile(train$precio_mt2, 0.05,na.rm=T)
#up <- quantile(train$precio_mt2, 0.97,na.rm=T)

#Test
#test <- test %>%
#  mutate(precio_mt2 = round(price / area, 0))
#low <- quantile(test$precio_mt2, 0.05,na.rm=T)
#up <- quantile(test$precio_mt2, 0.97,na.rm=T)

#Visualización del precio por m^2 
#wdb <- st_drop_geometry(train)
#ggplot(wdb, aes(x = precio_mt2)) +
#  geom_histogram()+
#  theme_classic()



############################################################
#Algunas Distribuciones 
plot_ban <- ggplot(test, aes(x = bathrooms)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Baños", y = "Cantidad",
       title = "Distribución de la Cantidad de Baños") +
  theme_bw()
ggplotly(plot_ban)

plot_hab<- ggplot(test, aes(x = rooms)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Habitaciones", y = "Cantidad",
       title = "Distribución de la Cantidad de habitaciones") +
  theme_bw()
ggplotly(plot_hab)

plot_alc <- ggplot(train, aes(x = bedrooms)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Alcobas", y = "Cantidad",
       title = "Distribución de la cantidad de alcobas") +
  theme_bw()
ggplotly(plot_alc)

plot_are <- ggplot(train, aes(x = area)) +
  geom_histogram(bins = 50, fill = "darkblue", alpha = 0.4) +
  labs(x = "Area", y = "Cantidad",
       title = "Distribución del Areas") +
  theme_bw()
ggplotly(plot_are)

# Selección de Variables de interés y Est. descriptivas
train<- train %>%
  select(property_id,`price`,`month`,`year`,`rooms`,`bedrooms`,bathrooms, ESTRATO,area,description,latitud,longitud,distancia_sm,distancia_cc,barrio,codigo_upz,codigo_localidad )

test<- test %>%
  select(property_id,`price`,`month`,`year`,`rooms`,`bedrooms`,bathrooms, ESTRATO,area,description,latitud,longitud,distancia_sm,distancia_cc,barrio,codigo_upz,codigo_localidad )

skim <- skim(train)
skim <- data.frame(skim)
skim <- skim %>%
  kbl() %>%
  kable_classic_2(full_width = F)

# Visulaización de Missings
nas_train<- vis_dat(as.data.frame(train))
nas_test <- vis_dat(as.data.frame(test))

#matriz de corrrelación
wdb <- as.data.frame(train)
wdb<- wdb %>%
  select(`price`,`month`,`year`,`rooms`,`bedrooms`,bathrooms, ESTRATO,area)
wdb <- na.omit(wdb)
matriz_correlacion <- cor(wdb)
matriz_correlacion <- matriz_correlacion %>%
  kbl() %>%
  kable_classic_2(full_width = F)


#Exportar Base 

train<- as.data.frame(st_drop_geometry(train))
test<- as.data.frame(st_drop_geometry(test))
write.csv(train,"Data/trainfiltrado.csv", row.names = FALSE)
write.csv(test,"Data/testfiltrado.csv", row.names = FALSE)



