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
        ranger
)

#Seleccionamos el directorio y Cargamos las bases de datos
setwd("C:/Users/USER/OneDrive - Universidad de los andes/Semestre VIII/Big Data/MECA4107_G5_ProblemSet_03")

#Cargamos las bases de Datos
trainf<-read.csv("Data/trainfiltrado.csv")
testf<-read.csv("Data/testfiltrado.csv")
template <- read.csv("Data/submission_template.csv")
summary(trainf)
summary(testf)

#Random Forest Intento

set.seed(4926)
ctrl<- trainControl(method = "cv",
                    number = 5)

mtry_grid<-expand.grid(mtry =c(6,9,11), # c(70,80,90!)
                       min.node.size= c(25,35,45,55,65,75), #c(35,45,55,65,75!) controla la complejidad del arbol
                       splitrule= 'variance') #splitrule 

cv_RForest <- train(price~month+year+rooms+bedrooms+bathrooms+ESTRATO+area+latitud+longitud+distancia_sm+distancia_cc,
                    data = trainf, 
                    method = "ranger",
                    trControl = ctrl,
                    metric="MAE",
                    tuneGrid = mtry_grid,
                    ntree=500,
                    importance="impurity")
cv_RForest

#Ver las VAriables más Importantes
varImp(cv_RForest)


#Envío para Kagglee

predictSample <- test   %>% 
  mutate(price = predict(cv_RForest, newdata = test)    ## predicted class labels
  )  %>% select(property_id,price)

predictSample<- predictSample %>% 
  left_join(template) %>% 
  select(property_id,price)


write.csv(predictSample,"RF_1.csv", row.names = FALSE)

