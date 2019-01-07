library(googlesheets)
suppressMessages(library(dplyr))
library(gsheet)

# 4.1 Lectura de los datos
web <-"https://docs.google.com/spreadsheets/d/1cJZIUTPoYliSej8ZQumPdrvWjcZeRyuEqyC4JyXpx-8/edit#gid=1570725307"

#4.2 Agregar el documento descargado a una tabla
datos<-read.csv(text=gsheet2text(web, format='csv'), stringsAsFactors=FALSE, header = TRUE, dec = ".")

#4.3 Muestra la dimensón de la tabla 
dim(datos)

#4.4 Verifica para NAs
is.na(datos)

#4.5 Existe algun(os) NAs?
any(is.na(datos))

#4.6 Cuenta los NAs
sum(is.na(datos))

#4.7 omite las filas que contengan NA
datos2<-na.omit(datos)

#4.8 Muestra la dimensón de la tabla 
dim(datos2)

#4.9 Muestra que tipo de vectores tiene la tabla
str(datos2)

#4.10 muestra un resumen
summary(datos2)

par(mfrow=c(1,1))
#4.11.1 Obtener los minimos de las columnas
minimos<-apply(datos[,-1], 2, min)
plot(minimos,type="b",main="Posicion de X v/s Minimos de Columnas X", xlab="Posicion X", ylab="Min. Colunas X.", col="2", pch=18)

#4.11.2 Obtener los promedios de las columnas
promedios<-apply(datos[,-1], 2, mean)
plot(promedios,type="b",main="Posicion de X v/s Promedios de Columnas X", xlab="Posicion X", ylab="Prom. Colunas X.", col="2", pch=18)

#4.11.3 Obtener los maximos de las columnas
maximos<-apply(datos[,-1], 2, max)
plot(maximos,type="b",main="Posicion de X v/s Maximos de Columnas X", xlab="Posicion X", ylab="Max. Colunas X.", col="2", pch=18)

#5.1Normalizando los datos
normalize <- function(x) { return ((x - min(x)) / (max(x) - min(x)))}
datos_norm <- as.data.frame(lapply(datos, normalize))
sum(is.na(datos_norm))
dim(datos_norm)
View(datos_norm)

#5.2Verificando que las columnas que tienen NaN, ya que puede ser que toda la columna que se este analizando sea NaN
sum(is.na(datos_norm$X122))
sum(is.na(datos_norm$X127))
sum(is.na(datos_norm$X202))
sum(is.na(datos_norm$X205))
sum(is.na(datos_norm$X206))
sum(is.na(datos_norm$X207))
sum(is.na(datos_norm$X208))
sum(is.na(datos_norm$X209))
sum(is.na(datos_norm$X210))

#5.3 Eliminando las columnas que al ser normalizadas eran NaN que corresponden a los x(122,127,202,205,206,207,208,209,210)
datos_normal1<-datos_norm[,-c(123,128,203,206,207,208,209,210,211)]
View(datos_normal1)
#Obtengo los promedios solo de las columnas X
datos_norm_prom <-apply(datos_norm[,-1], 2, mean)
par(mfrow=c(1,2))
plot(datos_norm_prom,type="b",main="Posicion de X v/s Promedios de Columnas X (Normalizadas)", xlab="Posicion X", ylab="Prom. Colunas X.", col="2", pch=18)
boxplot(datos_norm_prom,
        main = "Promedio de datos Normalizados de X",
        ylab = "Promedio de datos Normalizados",
        xlab = "Variable X",
        col = rainbow(6, alpha=0.2),
        border = rainbow(6, v=0.6)
)

#6.1 volviendo a normalizar los datos despues del preprocesamiento de los datos
datos_norm2 <- as.data.frame(lapply(datos_normal1, normalize))

#6.2 Verificando si existen datos nulos despues de normalizar nuevamente
sum(is.na(datos_norm2))
View(datos_norm2)

#Obtengo los promedios solo de las columnas X para ver como vuelven a quedar los datos al normalizar nuevamente
datos_norm_prom2 <-apply(datos_norm2[,-1], 2, mean)
par(mfrow=c(1,2))
plot(datos_norm_prom2,type="b",main="Posicion de X v/s Promedios de Columnas X (Normalizadas)", xlab="Posicion X", ylab="Prom. Colunas X.", col="2", pch=18)
boxplot(datos_norm_prom2,
        main = "Promedio de datos Normalizados de X",
        ylab = "Promedio de datos Normalizados",
        xlab = "Variable X",
        col = rainbow(6, alpha=0.2),
        border = rainbow(6, v=0.6)
)

#7.1 Visualizacion
#install.packages('factoextra')
#install.packages('pca3d')
library(factoextra)
library(pca3d)
par(mfrow=c(1,1))
res.pca <- prcomp(datos_norm2, scale = TRUE)

#get_eig(res.pca)
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 35))
fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
pca2d(res.pca,group=c(1:42))
pca3d(res.pca, group = c(1:42))


#########################Proceso de clasificacion y comparacion#########################

#########################Seleccion de datos de prueba y entrenamiento para datos en bruto#########################
#Particion de datos(train y test)
library(randomForest)
#str(datos)
ind<- sample(2,nrow(datos), replace = T, prob = c(0.6,0.4))
train<-datos[ind==1,]
test<-datos[ind==2,]

#########################Clasificando con dato en bruto#########################

#Modelo RandomFrorest (Regresion)
rf60<-randomForest(Y ~ ., data=train)
rf60

#Prediccion y matiz de confusion - test
library(caret)
p<- predict(rf60,test)

# Matriz de confusión
tabla <- table(p, test$Y) #matriz de confusion
tabla


#########################Clasificacion con datos en bruto y reduccion de dimensiones#########################

#8.1 Reducción de la Dimensionalidad
#install.packages('Boruta')
library(Boruta)
boruta<-Boruta(Y~.,data=datos,doTrace=2, maxRuns=500)
print(boruta)
plot(boruta)
plotImpHistory(boruta)
getNonRejectedFormula(boruta)
#attStats(boruta)

#Evaluando que la dimensiones tentativas puedan ser confirmadas
boruta2<-TentativeRoughFix(boruta)
print(boruta2)
plotImpHistory(boruta2)
plot(boruta2,sort=FALSE)
getNonRejectedFormula(boruta2)
getConfirmedFormula(boruta2)


#Modelo RandomFrorest (Regresion)
rf60<-randomForest(Y ~ X8 + X10 + X62 + X68 + X70 + X92 + X181 + X190 + X301 + X302 + 
                     X306 + X308 + X310 + X370 + X452 + X460 + X480, data=train)
rf60

#Prediccion y matiz de confusion - test
library(caret)
p<- predict(rf60,test)

# Matriz de confusión
tabla <- table(p, test$Y) #matriz de confusion
tabla



#########################Seleccion de datos de prueba y entrenamiento para datos normalizados#########################
#Particion de datos(train y test)
library(randomForest)
#str(datos)
ind<- sample(2,nrow(datos_norm2), replace = T, prob = c(0.6,0.4))
train<-datos[ind==1,]
test<-datos[ind==2,]

#########################Clasificacion con datos normalizados#########################

#Modelo RandomFrorest (Regresion)
rf60<-randomForest(Y ~ ., data=train)
rf60

#Prediccion y matiz de confusion - test
library(caret)
p<- predict(rf60,test)

# Matriz de confusión
tabla <- table(p, test$Y) #matriz de confusion
tabla


#########################Clasifcacion con datos normalizados y reduccion de dimensiones#########################
#8.1 Reducción de la Dimensionalidad
#install.packages('Boruta')
library(Boruta)
boruta<-Boruta(Y~.,data=datos_norm2,doTrace=2, maxRuns=500)
print(boruta)
plot(boruta)
plotImpHistory(boruta)
getNonRejectedFormula(boruta)
#attStats(boruta)

#Evaluando que la dimensiones tentativas puedan ser confirmadas
boruta2<-TentativeRoughFix(boruta)
print(boruta2)
plotImpHistory(boruta2)
plot(boruta2,sort=FALSE)
getNonRejectedFormula(boruta2)
getConfirmedFormula(boruta2)

#Modelo RandomFrorest (Regresion)
rf15<-randomForest(Y ~ X10 + X68 + X70 + X92 + X181 + X302 + X306 + X308 + X310 + 
                     X370 + X452 + X460, data=train)
rf15


#Prediccion y matiz de confusion - test
library(caret)
p<- predict(rf15,test)
p

# Matriz de confusión
tabla <- table(p, test$Y) #matriz de confusion
tabla



#9 Regresion multiple, ya que Y depende de muchas varables X

#Seleccionando columnas de mayor relevancia
datosReducidos<-datos_norm2[,c( "Y","X10", "X68" ,  "X70" , "X92",  "X181" , "X302" , "X306" , "X308" , "X310" , "X370" , "X452" , "X460")]

#install.packages('PerformanceAnalytics')
library(PerformanceAnalytics)
chart.Correlation(datosReducidos)

#install.packages('GGally')
library(GGally)
ggpairs(datosReducidos, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

modelo<-lm(Y ~ X10 + X68 + X70 + X92 + X181 + X302 + X306 + X308 + X310 + 
             X370 + X452 + X460, data=datosReducidos)
summary(modelo)
step(object = modelo,direction = "both",trace = 1)

modelo<-(lm(formula = Y ~ X68 + X70 + X302, data = datosReducidos))
summary(modelo)

confint(lm(formula = Y ~ X68 + X70 + X302, data = datosReducidos))
par(mfrow=c(2,2))
plot(modelo)


#Validacion

library(ggplot2)
library(gridExtra)
plot1 <- ggplot(data = datosReducidos, aes(X68, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datosReducidos, aes(X70, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datosReducidos, aes(X302, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3)

qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

par(mfrow=c(1,1))

ggplot(data = datosReducidos, aes(modelo$fitted.values, modelo$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()

library(lmtest)
bptest(modelo)

library(corrplot)
corrplot(cor(dplyr::select(datosReducidos, X68, X70,X302)),
         method = "number", tl.col = "black")




#Clasificacion con knn
library(class)
# Selección muestra entrenamiento
datosknn <- datos_norm2 %>%select_(~Y,~X68,~X70,~X302)


# división de la muestra en entrenamiento y validación
train=sample(seq(length(datosknn$Y)),length(datosknn$Y)*0.70,replace=FALSE)
training=datosknn[train,]
test=datosknn[-train,]

trainingKnn = training[,-1]
testKnn = test[,-1]

#x=data.frame(datos[,-ncol(datos)])
str(trainingKnn)
str(testKnn)

# K-Nearest Neighbors
knn.prd=knn(trainingKnn,testKnn,training$Y,k=10,prob=TRUE)
table(knn.prd,test$Y) #matriz de confusion

# Validación cruzada con la muestra de entrenamiento
knn.cross=knn.cv(trainingKnn,training$Y,k=10,prob=TRUE)
table(knn.cross,training$Y)

#################################       Extra      ####################################
###############################analizando el modelo####################################
resumen<-summary(modelo)
attributes(resumen)

#La matriz varianza covarianza de los datos
resumen$cov
#r^2 del modelo
resumen$r.squared
#R ajustado
resumen$adj.r.squared
#Estadistica F de la prueba global
resumen$fstatistic
#para obtener los valores predecidos con el modelo
modelo$fitted.values
#los residuales
modelo$residuals
#graficar los residuales
plot(modelo$residuals)
boxplot(modelo$residuals)
#probar normalidad de os residuales
shapiro.test(modelo$residuals)

















