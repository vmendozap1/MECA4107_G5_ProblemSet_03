# Instalar los paquetes
if(!require(pacman)) install.packages("pacman") ; require(pacman)
p_load( tidyverse, # tidy-data
        glmnet, # To implement regularization algorithms. 
        caret, # creating predictive models
        rpart, #Árbol
        rpart.plot, #Graficos de Árbol
        ranger #Random Forest
        xtable
)



# Cargar la base de datos
setwd("/Users/dngonzalez/Documents/1. Universidad/Maestría/BDML_2024/repos/MECA4107_G5_ProblemSet_03/")
train <- read.csv("stores/train.csv")
test <- read.csv("stores/test.csv")