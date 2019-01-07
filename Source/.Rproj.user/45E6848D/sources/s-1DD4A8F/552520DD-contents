########################################################################################
####################################Limpieza de datos###################################
########################################################################################
library(dplyr)
library(readr)

#Descargar documentoc con los datos
Web <- "http://archive.ics.uci.edu/ml/machine-learning-databases/internet_ads/ad.data"

#Agregar el documento descargado a una tabla
ad.data <- read.csv(Web, header=FALSE, sep=",", dec = ".")  

#Muestra la dimensón de la tabla 
dim(ad.data)

#Se visualizan todos los datos
#View(ad.data)

#copia de los datos en una nueva tabla para no volver a cargar los datos
ad.data1<-ad.data

#Reemplazar los "?" en NA
ad.data1[ad.data1=="   ?"] <- NA
ad.data1[ad.data1=="     ?"] <- NA
ad.data1[ad.data1=="?"] <- NA

#Muestra que tipo de vectores tiene la tabla
str(ad.data1)

#Muestra si existen NA
#is.na(ad.data1$V1)

#Verifica si existe algun NA
#any(is.na(ad.data1$V1))

#Se visualizan todos los datos
#View(ad.data1)

#Se visualizan en la consola todos los datos de un vector
#ad.data1$V1

#Muestra la dimensón de la tabla 
dim(ad.data1)

#omite las filas que contengan NA
ad.data2<-na.omit(ad.data1)

#Muestra que tipo de vectores tiene la tabla
str(ad.data2)

#Muestra la dimensón de la tabla 
dim(ad.data2)

#Se visualizan todos los datos
#View(ad.data2)

########################################################################################

############################# Fin Limpieza de datos ####################################
########################################################################################

########################################################################################
######################Seleccion de datos para entrenamiento#############################

#install.packages("magrittr")
library(magrittr) #es ára ocupar la funcionalidad %>%

# Selección muestra entrenamiento
datos <- ad.data2 %>%select_(~V1, ~V2, ~V3, ~V4, ~V1559)

datos$V1<-as.numeric(as.character(datos$V1))
datos$V2<-as.numeric(as.character(datos$V2))
datos$V3<-as.numeric(as.character(datos$V3))
datos$V4<-as.numeric(as.character(datos$V4))
datos<-as.data.frame(datos)
str(datos)

datos1<- datos %>%select_(~V1, ~V2, ~V3, ~V4)

# división de la muestra en entrenamiento y validación
train=sample(seq(length(datos$V1559)),length(datos$V1559)*0.70,replace=FALSE)
training=datos[train,]
test=datos[-train,]
########################################################################################

################### Fin Seleccion de datos para entrenamiento ##########################
########################################################################################

########################################################################################
#################################Metodos a ocupar#######################################
########################################################################################

########################################################################################
#################Algorítmo K-vecinos más cercanos(knn) #################################

########################################################################################
#######################Clasificacion con class - knn####################################
########################################################################################
library(class)
# Selección de variables
trainingKnn = training[,-ncol(datos)]
testKnn = test[,-ncol(datos)]

#x=data.frame(datos[,-ncol(datos)])
str(trainingKnn)
str(testKnn)

# K-Nearest Neighbors
knn.prd=knn(trainingKnn,testKnn,training$V1559,k=10,prob=TRUE)
table(knn.prd,test$V1559) #matriz de confusion
mean(knn.prd==test$V1559)*100 #porcentaje de bien clasificados
#Suma de los datos de la matriz de confuasion: 708

# Validación cruzada con la muestra de entrenamiento
#knn.cross=knn.cv(trainingKnn,training$V1559,k=10,prob=TRUE)
#table(knn.cross,training$V1559)
#mean(knn.cross==training$V1559)*100 #porcentaje de bien clasificados

########################################################################################

########################################################################################
#######################Clasificacion con kknn - knn#####################################
########################################################################################
#install.packages("kknn")
library("kknn")

suppressWarnings(suppressMessages(library(kknn)))
modelo <- train.kknn(V1559 ~ ., data = training, kmax = 10)
modelo

pred <- predict(modelo, test[, -5])
#pred

#CM <- table(testing[, 5], pred)
CM <- table(test$V1559, pred)
CM #matriz de confusion
#Suma de los datos de la matriz de confuasion: 708

precisión <- ((sum(diag(CM)))/sum(CM))*100
precisión #porcentaje de bien clasificados

plot(modelo)

########################################################################################

########################################################################################
#######################Clasificacion con caret - knn####################################
########################################################################################
#install.packages("MLmetrics")
library(caret)
library("MLmetrics")

TrainData <- training[,1:4]
TrainClasses <- training[,5]

knnFit1 <- train(TrainData, TrainClasses,
                 method = "knn",
                 preProcess = c("center", "scale"),
                 tuneLength = 10,
                 trControl = trainControl(method = "cv"))
knnFit1
#confusionMatrix(knnFit1) # Matriz de confusión de resultados en entrenamiento

pred <- predict(knnFit1, test)  # Anñalisis de resutados sobre datos de test
#confusionMatrix(pred, test$V1559)
table(pred,test$V1559) #matriz de confusion
mean(pred==datos$V1559[-train])*100 #porcentaje de bien clasificados
#Suma de los datos de la matriz de confuasion: 708

#De aqui en adelante es para poder realizar el grafico 
pred <- predict(knnFit1, test, type = "prob") # Predicción de probabilidades por clase
pred$obs <- test$V1559
pred$pred <- predict(knnFit1, test)

multiClassSummary(pred, lev = levels(pred$obs))

# Visualizar los fallos en la predicción
ggplot(pred, aes(x = pred, y = obs)) + geom_jitter(position = position_jitter(width = 0.25, height = 0.25))

########################################################################################

##################################### FIN KNN ##########################################
########################################################################################

########################################################################################
#####################################  SVM   ###########################################

########################################################################################
###########################Clasificacion con caret - SVM################################
########################################################################################
library(caret)
library(kernlab)
library("MLmetrics")


# Exploramos la distribución de las muestras según las distintas variables
featurePlot(training[,-5], y = training$V1559,  plot='ellipse')
featurePlot(training[,-5], y = training$V1559,  plot='box')

# Configuramos la selección del modelo durante entrenamiento
train10CV <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# Entrenamos una SVM con kernel RBF
svmRBF <- train(V1559 ~ ., data = training,
                method = "svmRadial", trControl = train10CV,
                preProc = c("center", "scale"))
svmRBF # Examinamos el resultado de entrenamiento

#confusionMatrix(svmRBF) # Matriz de confusión de resultados en entrenamiento

pred <- predict(svmRBF, test)  # Anñalisis de resutados sobre datos de test
#confusionMatrix(pred, test$V1559)

table(pred,test$V1559) #matriz de confusion
mean(pred==test$V1559)*100 #porcentaje de bien clasificados
#Suma de los datos de la matriz de confuasion: 708

#De aqui en adelante es para poder realizar el grafico 
pred <- predict(svmRBF, test, type = "prob") # Predicción de probabilidades por clase
pred$obs <- test$V1559
pred$pred <- predict(svmRBF, test)

multiClassSummary(pred, lev = levels(pred$obs))

# Visualizar los fallos en la predicción
ggplot(pred, aes(x = pred, y = obs)) + geom_jitter(position = position_jitter(width = 0.25, height = 0.25))

########################################################################################

########################################################################################
#######################Clasificacion con e1071 - SVM####################################
########################################################################################
#install.packages("e1071")  
library("e1071")

# se estima un modelo svm lineal para la muestra de entrenamiento
#svmfit=svm(datos$V1559~.,data=datos,kernel="linear",scale=FALSE,subset=train)
#print(svmfit)

#table(datos$V1559[train],svmfit$fitted)

# Predicción para la muestra test
#svm.pred=predict(svmfit,datos[-train,])
#summary(svm.pred)

#with(datos[-train,],table(svm.pred,V1559))

#table(svm.pred,datos[-train,5])
#mean(svm.pred==datos[-train,5])*100 #porcentaje de bien clasificados

# se estima un modelo svm radial para la muestra de entrenamiento y se predice la muestra de test
svmfit2=svm(datos$V1559~.,data=datos,kernel="radial",scale=FALSE,subset=train,probability=TRUE)
print(svmfit2)

svm.pred=predict(svmfit2,test,probability=TRUE)
#summary(svm.pred)

table(svm.pred,test$V1559)#matriz de confusion
mean(svm.pred==test$V1559)*100 #porcentaje de bien clasificados
#Suma de los datos de la matriz de confuasion: 708

########################################################################################

########################################################################################
#######################Clasificacion con kernlab - svm################################
########################################################################################
#install.packages("kernlab")
library("kernlab")
## Create a kernel function using the build in rbfdot function
rbf <- rbfdot(sigma=0.1)
rbf
## train a bound constraint support vector machine
datosmodel <- ksvm(V1559~.,data=training,type="C-bsvc",kernel=rbf,C=10,prob.model=TRUE)

datosmodel

## get fitted values
fitted(datosmodel)

## Test on the training set with probabilities as output
#datosmodel.svm.pred<-predict(datosmodel, datos[,-5])
datosmodel.svm.pred<-predict(datosmodel, test[,-5])


#table(datosmodel.svm.pred,datos[,5])
table(datosmodel.svm.pred,test$V1559)#matriz de confusion
mean(datosmodel.svm.pred==test$V1559)*100 #porcentaje de bien clasificados
#Suma de los datos de la matriz de confuasion: 708
########################################################################################

##################################### FIN SVM ##########################################
########################################################################################

########################################################################################
##########################  Arboles de clasificación   #################################

########################################################################################
###########################Arboles de clasificación(C50)################################
########################################################################################
# -----------------------------------------------------------------
# Carga el paquete específico del Árbol de clasificación C5.0
#install.packages("C50", dependencies = TRUE)
library(C50)

# Ejecución del modelo de clasificación C5.0
modelo <- C5.0(V1559~., data = training)
summary(modelo) # Información sobre el modelo

plot(modelo) # Gráfico

# Para detallar un nodo en particular se usaria la siguiente función
plot(modelo, subtree=2)  #Muestra un nodo en particular

# predicción
prediccion <- predict(modelo,newdata=test)

# Matriz de confusión
tabla <- table(prediccion, test$V1559) #matriz de confusion
tabla

100 * sum(diag(tabla)) / sum(tabla) #porcentaje de bien clasificados
#Suma de los datos de la matriz de confuasion: 708
########################################################################################

########################################################################################
###########################Arboles de clasificación(rpart)##############################
########################################################################################
#install.packages("rpart.plot")
library(rpart)
library("rpart.plot")


# Step1: Begin with a small cp. 
tree <- rpart(V1559 ~ ., data = training, control = rpart.control(cp = 0.0001), model = TRUE)
plot(tree);text(tree,pretty=0)


prediccion <- predict(tree,newdata=test, type="class")
tabla <- table(prediccion, test$V1559) #matriz de confusion
tabla

100 * sum(diag(tabla)) / sum(tabla) #porcentaje de bien clasificados

#Graficando los datos
plot(tree);text(tree, cex = 0.8, use.n = TRUE, xpd = TRUE)

prp(tree, faclen = 0, cex = 0.8, extra = 1)

tot_count <- function(x, labs, digits, varlen)
{
  paste(labs, "\n\nn =", x$frame$n)
}

prp(tree, faclen = 0, cex = 0.8, node.fun=tot_count)

only_count <- function(x, labs, digits, varlen)
{
  paste(x$frame$n)
}

boxcols <- c("pink", "palegreen3")[tree$frame$yval]

par(xpd=TRUE)
prp(tree, faclen = 0, cex = 0.8, node.fun=only_count, box.col = boxcols)
legend("bottomleft", legend = c("ad","nonad"), fill = c("pink", "palegreen3"),
       title = "Group")

########################################################################################

########################################################################################
###########################Arboles de clasificación(tree)##############################
########################################################################################
library(tree)
# Selección muestra entrenamiento
datos.tree = tree(datos$V1559~.,datos,subset=train)
summary(datos.tree)

plot(datos.tree);text(datos.tree,pretty=0)

datos.tree

datos.pred=predict(datos.tree, test,type="class")
summary(datos.pred)

tabla <- table(datos.pred, test$V1559) #matriz de confusion
tabla

100 * sum(diag(tabla)) / sum(tabla)  #porcentaje de bien clasificados

# Mediante validación cruzada se busca el mejor arbol de decision
#cv.datos=cv.tree(datos.tree,FUN=prune.misclass)
#cv.datos

## [1] "prune"         "tree.sequence"

#plot(cv.datos)

########################################################################################

############################ FIN Arboles de clasificacion ##############################
########################################################################################

########################################################################################
###############################Fin Metodos a ocupar#####################################
########################################################################################



#################################################
#########Resumen metodos utilizados##############
#KNN<- CARET, CLASS, KKNN
#svm <- e1071, kernlab, CARET
#tree <- c50,rpart, tree
#################################################


