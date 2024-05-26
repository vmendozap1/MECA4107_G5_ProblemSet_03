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

train <- train %>% mutate(barrio1 = factor(train$barrio))
test <- test %>% mutate(barrio1 = factor(test$barrio))



#Red Neuronal con Keras ##
y <- log(train$price)
x <- as.matrix(train %>%   select(month, year, rooms, bedrooms, bathrooms, ESTRATO, area, latitud, longitud, 
                                  distancia_sm, distancia_cc, codigo_upz, codigo_localidad))
x <- scale(x)

##Entrenar el modelo##

nn_1 <- keras_model_sequential() %>%
  layer_dense(units = 28, activation = "relu",
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

# Ensure the input data dictionary has the correct key
x_test <-  x_test %>% rename(columns={"month": "dense_1_input"})

# Predict using the updated input data
y_pred <- nn_1 %>% predict(x_test)

#Envío para Kagglee

predictSample <- test   %>% 
  mutate(price = predict(nn_1, newdata = test)    ## predicted class labels
  )  %>% select(property_id,price)

write.csv(predictSample,"neunoral_network_1.csv", row.names = FALSE)


##Red Neuronal con NNET ##

y <- log(train$price)
x <- as.matrix(train %>%  select(month, year, rooms, bedrooms, bathrooms, ESTRATO, area, latitud, longitud, 
                                 distancia_sm, distancia_cc, codigo_upz, codigo_localidad))
x <- scale(x)

nn_2 <- nnet(y ~ x,
             data = train,
             size = 60,
             lineout = TRUE)

summary(nn_2)

#Envío para Kagglee

predictSample <- test   %>% 
  mutate(price = predict(nn_2, newdata = test)    ## predicted class labels
  )  %>% select(property_id,price)

write.csv(predictSample,"neunoral_network_1.csv", row.names = FALSE)

