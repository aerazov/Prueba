library(caret)
library(tidyverse)
library(mlbench)

data("BreastCancer")

transparentTheme(trans = 0.4)
featurePlot(x = BreastCancer[, 2:11], y = BreastCancer$Class, 
            plot = "pairs", auto.key = list(columns = 2))


set.seed(707)
Index <- createDataPartition(BreastCancer$Class, list = FALSE, 
                             p = 0.8)
Train <- BreastCancer[Index, -1]
Test <- BreastCancer[-Index, -1]
summary(Train)

fitControl <- trainControl(method = "repeatedcv", 
                           number = 10, 
                           repeats = 10)

#Entrenamos el modelo
gbmFit1 <- train(Class ~ ., data = Train, method = "gbm", trControl = fitControl, 
                 verbose = FALSE)

#El error se debe a la presencia de NAs
#Se eliminan los NAs si son pocos

BC <- BreastCancer[complete.cases(BreastCancer), ]

set.seed(707)
Index <- createDataPartition(BC$Class, list = FALSE, p = 0.8)
Train1 <- BC[Index, -1]
Test1 <- BC[-Index, -1]
gbmFit1 <- train(Class ~ ., data = Train1, method = "gbm", trControl = fitControl, 
                 verbose = FALSE)
plot(gbmFit1)

confusionMatrix(data = predict(gbmFit1, Test1), reference = Test1$Class)

A <- predict(gbmFit1, Test1, type = "raw") #Prediccion clase
B <- predict(gbmFit1, Test1, type = "prob") #Prediccion probabilidad

Resultados <- bind_cols(A,B)
colnames(Resultados)[1] <- "Prediccion"

Resultados <- Resultados %>% 
  arrange(Prediccion,malignant)

view(Resultados)

#Series de tiempo
Contaminacion <- read_csv("https://raw.githubusercontent.com/derek-corcoran-barrios/derek-corcoran-barrios.github.io/master/CursoMultiPres/Capitulo_7/Contaminacion.csv")


#Intentamos simular el predecir el futuro, por eso el test será 2016 y 2017
Train <- Contaminacion %>% dplyr::filter(!(lubridate::year(Fecha) %in% 
                                             c(2016, 2017)))
Test <- Contaminacion %>% dplyr::filter(lubridate::year(Fecha) %in% 
                                          c(2016, 2017))

#Separar en bases de datos
#Horizon = Test
#InitialWindow = Train
#Skip = Intervalos

Graph_Slice <- function(Slices = Slices) {
  Slice <- list()
  for (i in 1:length(Slices$test)) {
    Window <- Train[Slices$train[[i]], ] %>% mutate(rep = as.character(i), 
                                                    class = "Window (Train)")
    Horizon <- Train[Slices$test[[i]], ] %>% mutate(rep = as.character(i), 
                                                    class = "Horizon (Test)")
    Slice[[i]] <- bind_rows(Window, Horizon)
  }
  Slices <- Slice %>% purrr::reduce(bind_rows)
  ggplot(Slices, aes(x = Fecha, y = MP25)) + geom_path(aes(color = class)) + 
    facet_wrap(~rep) + theme_bw()
}

#Equivalente al KfoldcrossValidation, toma 3 años y predice 1
Slices <- createTimeSlices(Train$MP25, horizon = 365, initialWindow = 365 * 3, fixedWindow = T, skip = 365) #Test de un año y un train de 3 años, con saltos de un año.
Graph_Slice(Slices)


#Probando un modelo
fitControl <- trainControl(method = "timeslice", horizon = 365, 
                           initialWindow = 365 * 3, fixedWindow = T, skip = 30)

gbmFitTime <- train(MP25 ~ ., data = Train, method = "gbm", trControl = fitControl, 
                    verbose = FALSE)


postResample(pred = predict(gbmFitTime, Test), obs = Test$MP25)

plot(gbmFitTime)
gbmFitTime$results
gbmFitTime$bestTune





























