if(!require(pacman)) install.packages("pacman") ; require(pacman)
p_load( tidyverse, # tidy-data
        glmnet, # To implement regularization algorithms. 
        caret, # creating predictive models
        skimr,
        knitr,
        kableExtra,
        leaflet, 
        visdat, 
        osmdata,
        tmaptools,
        sf,
        GADMTools,
        stringr,
        stargazer,
        ranger
)

#Directorio 
setwd("/Users/valentinamendoza/Desktop/PS3")

#Cargar bases
trainf<-read.csv("trainfiltrado.csv")
testf<-read.csv("testfiltrado.csv")
template <- read.csv("submission_template.csv")
summary(trainf)
summary(testf)

#Variables categoricas
trainf$bedrooms <- as.factor(trainf$bedrooms)
trainf$ESTRATO<- as.factor(trainf$ESTRATO)
trainf$bathrooms<- as.factor(trainf$bathrooms)
trainf$rooms<- as.factor(trainf$rooms)
trainf$barrio<- as.factor(trainf$barrio)
trainf$month<- as.factor(trainf$month)
trainf$codigo_localidad<- as.factor(trainf$codigo_localidad)

testf$bedrooms <- as.factor(testf$bedrooms)
testf$ESTRATO<- as.factor(testf$ESTRATO)
testf$bathrooms<- as.factor(testf$bathrooms)
testf$rooms<- as.factor(testf$rooms)
testf$barrio<- as.factor(testf$barrio)
testf$month<- as.factor(testf$month)
testf$codigo_localidad<- as.factor(testf$codigo_localidad)


# Elastic Net 

ctrl<- trainControl(method = "cv",
                    number = 5,)
set.seed(4345)

model1 <- train(
  price~month+year+rooms+bedrooms+bathrooms+ESTRATO+area+latitud+longitud+distancia_sm+distancia_cc+codigo_localidad+codigo_upz,             
  data = trainf,         
  metric = "RMSE",         
  method = "glmnet",      
  trControl = ctrl,       
  tuneGrid = expand.grid(
    alpha = seq(0,1,by=.1),
    lambda =10^seq(10, -2, length = 50)    
    )
)
model1

predictSample <- testf   %>% 
  mutate(price = predict(model1, newdata = testf)    
  )  %>% select(property_id,price)

write.csv(predictSample,"EN.csv", row.names = FALSE)


