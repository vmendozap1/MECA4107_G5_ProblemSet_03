rm(list = ls())
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
        stringr,
        stargazer, #Tablas para text
        nnet, # redes neuronales de una sola capa
        keras,
        spatialsample #validación cruzada espacial
)

#Seleccionamos el directorio y Cargamos las bases de datos
setwd("C://Users//AlfredoRP//OneDrive - INALDE Business School - Universidad de La Sabana//Attachments//Economia//ML//Problem set 3//PS3//MECA4107_G5_ProblemSet_03//Data")

train <- read.csv("trainfiltrado.csv")
test <- read.csv("testfiltrado.csv")
template <- read.csv("submission_template.csv")

#train <- na.omit(train)#

#Red Neuronal con Keras ##
y <- log(train$price)
x <- as.matrix(train %>%  month, year, rooms, bedrooms, bathrooms, ESTRATO, area, precio_mt2)
x <- scale(x)

##

nn_1 <- keras_model_sequential() %>%
  layer_dense(units = 10, avtivation = "relu",
              input_shape = ncol(x)) %>%
  layer_dense(units = 1)

nn_1 %>% compile(loss = "mse",
                 optimizer = 'adam',
                 metrics = list("mean_absolute_error")
)

set.seed(889845)
history <- nn_1 %>% fit(
  x, y,
  epochs = 30,
  batch_size = 256,
  validation_split = 0.2
)

plot(history)

nn_1 %>% evaluate(x,y)

x_test <- test %>% select(month, year, rooms, bedrooms, bathrooms, ESTRATO, area, precio_mt2)

y_pred <- nn_1 %>% predict(x_test)



#Envío para Kagglee

predictSample <- test   %>% 
  mutate(price = predict(cv_RForest, newdata = test)    ## predicted class labels
  )  %>% select(property_id,price)

predictSample<- predictSample %>% 
  left_join(test_hogares) %>% 
  select(property_id,Ingpcug,Lp)


write.csv(predictSample,"neunoral_network_1.csv", row.names = FALSE)


##Red Neuronal con NNET ##

y <- log(train$price)
x <- as.matrix(train %>%  select(month, year, rooms, bedrooms, bathrooms, ESTRATO, area, precio_mt2))
x <- scale(x)

nn_2 <- nnet(y ~ x,
             data = train,
             size = 60,
             lineout = TRUE)

y_pred_2 <- predict(nn_2, newdata = test)


