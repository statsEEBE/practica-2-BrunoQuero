#codigo para problema dos
iris
x<-mis_dades$Petal.Length
 x
plot(x,y)
 xbar<- mean(x)
ybar<-mean(y)
xbar
ybar
m <- sum((x-xbar)*(y-ybar))/sum((x-xbar)^2) #calculamos la m de la recta de minimos cuadrados
m
#0.4089223
b<-ybar-m*xbar
b
#4.306603
m*1.5+b
#4.919987
mod<-lm(y~x)
ypredicted<-predict(mod, data.frame(x=x)) #significa que estoy creando una base de datos on una sola columna que sus datos son x
#con esos datos le decimos que utilice las x para crear una prediccion del modelo linear
ypredicted
plot(x,y, pch=16, col="blue")
lines(x,ypredicted)

rsq<-sum((ypredicted-ybar)^2)/sum((y-ybar)^2) #calculamos r^2 que tan precisa es la aproximacion de la linea
rsq
#0.7599546 es el multiple r-squared
summary(mod)
sqrt(rsq)
#0.8717538 el ceeficiente de correlaciones sqrt(r)
cor.test(x,y)

