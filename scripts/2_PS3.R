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
        stargazer,
        ranger,
        tidymodels
)

#Seleccionamos el directorio y Cargamos las bases de datos
setwd("C:/Users/USER/OneDrive - Universidad de los andes/Semestre VIII/Big Data/MECA4107_G5_ProblemSet_03_v2")
# Cargar el espacio de trabajo
#load("BaseFinal.RData")

#Cargamos las bases de Datos
trainf <- train
testf <- test
#trainf<-read.csv("Data/trainfiltrado.csv")
#testf<-read.csv("Data/testfiltrado.csv")
template <- read.csv("Data/submission_template.csv")
summary(trainf)
summary(testf)

#Convertir a Var categóricas
trainf <- mutate_if(trainf, is.character, as.factor)
testf <- mutate_if(trainf, is.character, as.factor)
#normalizamos
trainf <- trainf %>%
  mutate_if(is.numeric, scale)
testf <- testf %>%
  mutate_if(is.numeric, scale)

#####################################################################
#Modelo y Predicción
#####################################################################

### definir el block boostrap 
p_load(sf)
train_sf <- st_as_sf(
  train_full,
  coords = c("lon", "lat"),
  crs = 4326)

p_load(spatialsample)
set.seed(4926)
block_folds <- spatial_block_cv(train_sf, v = 5)
autoplot(block_folds)


#Definimos la grilla de rpofundidad
mtry_grid<-expand.grid(mtry =c(30,40,50), # c(6,9!,11)
                       min.node.size= c(30,40,50), #c(5!,10,20,25) controla la complejidad del arbol
                       splitrule= 'variance') #splitrule 


cv_RForest <- train(price~month+year+rooms+bedrooms+bathrooms+ESTRATO+area+latitud+longitud+distancia_sm+distancia_cc+barrio+codigo_localidad+codigo_upz+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22+PC23+PC24+PC25+PC26+PC27+PC28+PC29+PC30+PC31+PC32+PC33+PC34+PC35+PC36+PC37+PC38+PC39+PC40+PC41+PC42+PC43+PC44+PC45+PC46+PC47+PC48+PC49+PC50+PC51+PC52+PC53+PC54+PC55+PC56+PC57+PC58+PC59+PC60+PC61+PC62+PC63+PC64+PC65+PC66+PC67+PC68+PC69+PC70+PC71+PC72+PC73+PC74+PC75+PC76+PC77+PC78+PC79+PC80+PC81+PC82+PC83+abiert+acab+acces+alcob+ampli+are+ascensor+balcon+ban+bao+baos+barri+bbq+bogot+buen+centr+cerc+cerr+chimene+closet+cocin+comedor+comercial+comunal+conjunt+const+constru+cuart+cuatr+cubiert+cuent+deposit+dos+edifici+espaci+estudi+excelent+exterior+garaj+gas+gimnasi+habit+habitacion+hermos+ilumin+iluminacion+independient+infantil+integral+interior+lamin+lavanderi+lind+mader+mts+natural+nivel+parqu+parqueader+pis+principal+priv+remodel+residencial+rop+sal+salon+sector+segund+segur+servici+social+terraz+transport+tres+ubic+ubicacion+uno+vias+vigil+visit+vist+zon,
                    data = trainf, 
                    method = "ranger",
                    trControl = block_folds,
                    metric="MAE",
                    tuneGrid = mtry_grid,
                    ntree=500,
                    importance="impurity")
cv_RForest


varImp(cv_RForest)


#Envío para Kagglee

predictSample <- testf   %>% 
  mutate(price = predict(cv_RForest, newdata = testf)    ## predicted class labels
  )  %>% select(property_id,price)

predictSample<- predictSample %>% 
  left_join(template) %>% 
  select(property_id,price)


write.csv(predictSample,"predictions/RF_6.csv", row.names = FALSE)