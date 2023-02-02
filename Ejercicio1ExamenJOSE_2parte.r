#-- REGRESION - Selección de variables 
install.packages("corrplot")
library(corrplot)
install.packages("pls")
library(MASS)
library(glmnet)
library(gridExtra)
library(pls)
library(pacman)
pacman::p_load(pacman,dplyr, ggplot2, rio, gridExtra, scales, ggcorrplot, e1071)
data(swiss)
dim(swiss)
head(swiss)
names(swiss)
attach(swiss) #para usar las variables sin $

swiss$Fertility
plot(swiss)
#Para generar las variables predictoras
x=model.matrix(Fertility~.,swiss)
print(x)
#Para quitar el intercepto
x=model.matrix(Fertility~.,swiss)[,-1]
print(x)

#Para generar la variable dependiente
y= swiss$Fertility
print(y)

# Apliacamos Lasso para minimizar la función reduciendo las variables predictoras
lasso.model=glmnet(x,y,alpha =1) #alpha=1 porque es Lasso
dim(coef(lasso.model))
Coeflasso=coef(lasso.model)  #vemos los coeficientes generados para cada función
print(Coeflasso)
lasso.model$lambda #vemos los lambdas generados para cada función 
plot(lasso.model,"lambda",label=TRUE) #imprimimos

#Para saber el lambda modelo 5 de ejemplo
lasso.model$lambda[5]
log(lasso.model$lambda[5])
coef60=coef(lasso.model)[,5]
print(coef60)
plot(lasso.model,"lambda",label=TRUE)
abline(v=log(lasso.model$lambda[5]), col="blue",lwd= 4,lty =3)

#Mejor lambda con validación cruzada 
sal.cv=cv.glmnet(x,y,alpha=1)
plot(sal.cv)
mejor.lambda =sal.cv$lambda.min
mejor.lambda
log(mejor.lambda)

#Buscamos los coeficientes con el mejor lambda de CV
pred_coef = predict(lasso.model, type = "coefficients", s = 1)
pred_coef
##hacer predicción con mejor lambda para obtener los valores de Fertility
coef(lasso.model)[,which(lasso.model$lambda==mejor.lambda)]
pred=predict(lasso.model, s=mejor.lambda,newx = x)
pred

#En este caso, Lasso con el mejor lambda no ha descartado variables predictoras
#Entonces se considera usar análisis de componentes principales para decidir por las variables
# predictoras significativas para Fertility y reducir la dimensionalidad

# Modelo PCR
modelo.pcr <- pcr(Fertility ~ ., data = swiss, scale = TRUE, 
                        validation = "CV")
# Resultado del ajuste del modelo
summary(modelo.pcr)

#Graficamos el error por número de componentes
#En este caso el error tiene una reducción significativa hasta el componente 2
#y casi estable en el componente 3
validationplot(modelo.pcr, val.type = "MSEP")

#Probamos el modelo con sólo dos componentes principales
# Encontramos que con los 2 primeros componentes principales se se alcanza 
# el 74% de varianza explicada de las variables explicativas y en un 56% la dependiente
modelo.pcr <- pcr(Fertility ~ ., data = swiss, scale = TRUE, ncomp = 2)
summary(modelo.pcr)

#Calculo del error para 2 componentes principales
pred.modelo.pcr <- predict(modelo.pcr, swiss, ncomp = 2)
test.MSE.pcr <- mean((pred.modelo.pcr - swiss$Fertility)^2)
test.MSE.pcr

#Probamos el modelo con sólo 3 componentes principales
# Encontramos que con los 3 primeros componentes principales se alcanza 
# el 90% de varianza explicada de las variables explicativas y en un 59% la dependiente
modelo.pcr_3 <- pcr(Fertility ~ ., data = swiss, scale = TRUE, ncomp = 3)
summary(modelo.pcr_3)

#Calculo del error para 3 componentes principales, este es menor
pred.modelo.pcr_3 <- predict(modelo.pcr_3, swiss, ncomp = 3)
test.MSE.pcr <- mean((pred.modelo.pcr_3 - swiss$Fertility)^2)
test.MSE.pcr

#Con los datos obtenidos a partir de Análisis de componentes principales
# se seleccionan las dos primeras variables explicativas  porque con sólo
# esas 2 se logra exliplicar el 56% de la variabilidad de la variable dependiente
# o Fertility del conjunto de datos. Podemos observar la multico 

#Graficamos la correlación entre las variables que explican la selección
M <- cor(swiss)
corrplot(M, method = "ellipse")              
corrplot(M, method="number", order="hclust", type="lower")
