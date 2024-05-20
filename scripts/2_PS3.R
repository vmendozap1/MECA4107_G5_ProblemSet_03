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
        stargazer
)

#Seleccionamos el directorio y Cargamos las bases de datos
setwd("C:/Users/USER/OneDrive - Universidad de los andes/Semestre VIII/Big Data/MECA4107_G5_ProblemSet_03")

#Cargamos las bases de Datos
train<-read.csv("Data/trainfiltrado.csv")
test<-read.csv("Data/testfiltrado.csv")
template <- read.csv("Data/submission_template.csv")

train <- na.omit(train)


#Random Forest Intento

set.seed(4926)
ctrl<- trainControl(method = "cv",
                    number = 5)

mtry_grid<-expand.grid(mtry =c(15,18,20), # c(8,11,15)
                       min.node.size= c(15,20,25,30,35), #controla la complejidad del arbol
                       splitrule= 'variance') #splitrule 

cv_RForest <- train(price~., 
                    data = train, 
                    method = "ranger",
                    trControl = ctrl,
                    metric="MAE",
                    tuneGrid = mtry_grid,
                    ntree=500,
                    importance="impurity")
cv_RForest

#Predicción rápida


modelo <- lm(price ~., data = train)
stargazer(modelo,type="text")

predictSample <- test   %>% 
  mutate(price = predict(modelo, newdata = test))%>% 
  select(property_id,price)

predictSample<- predictSample %>% 
  left_join(template) %>% 
  select(property_id,price)


#Envío para Kagglee

predictSample <- test   %>% 
  mutate(price = predict(cv_RForest, newdata = test)    ## predicted class labels
  )  %>% select(property_id,price)

predictSample<- predictSample %>% 
  left_join(test_hogares) %>% 
  select(property_id,Ingpcug,Lp)


write.csv(predictSample,"regression_RandomForest_1.csv", row.names = FALSE)