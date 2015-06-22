

#Creamos la función que recibe los paquetes
install = function(pkg){
  #Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

#Instalamos primero "foreach"
install("foreach")

#Seleccionamos los archivos que queremos instalar
archive = c("rJava", "shiny", "rmarkdown", "foreach", "caret", "e1071", "rpart", "tree", "RWeka", "C50")
foreach(i = archive) %do% install(i)

library(devtools)
library (caret)

fpe <- read.table("C:\\Users\\Rodolfo\\Downloads\\lenses.data.txt",sep= "", header=F)
fpe[1]<-NULL

names(fpe) <- c("Age","S.prescription","astigmatic","tear rate","classes")
fpeo<-fpe
fpe$Age <- factor(fpe$Age,labels=c("young",  "pre-presbyopic", "presbyopic"))
fpe$S.prescription<- factor(fpe$S.prescription,labels=c("myope","hypermetrope"))
fpe$astigmatic<- factor(fpe$astigmatic,labels=c("no","yes"))
fpe$`tear rate`<- factor(fpe$`tear rate`,labels=c("reduced","normal"))
fpe$classes<- factor (fpe$classes,labels=c("hard","soft","no contact"))


fpeT<-fpe
fpeT<- fpeT[-c(2,4,10,16),] #sacados al azar por un programa en python
# import random
# t= 4
# while(t>0):
#   t=t-1
#   print(random.randrange(1,24))

fpeP<-fpe

fpeP<- fpeP[-c(1,3,5,6,7,8,9,11,12,13,14,15,17,18,19,20,21,22,23,24),]


finalT <- J48(formula=fpeT$classes~. , data=fpeT)
finalP <- J48(formula=fpeP$classes~., data=fpeP)

plot(finalT)
plot(finalP)
library(rpart)
rT <- rpart(formula=fpeT$classes~.  , data=fpeT, method="class")
rP <- rpart(formula=fpeP$classes~., data=fpeP, method= "class")
summary(rT)
summary(rP) #aqui podemos observar a mayor detalle el arbol aunque no graficamente

plotcp(rT) #aqui no encontre como hacer que quitara la megapoda 
plotcp(rP)

print(summary(finalT,fpeT)) #con este print podemos ver la matriz de confusion etc, datos mas bonitos
print(summary(finalP,fpeP))


xT<- predict(finalT,fpeT) #predictibilidad con decission tree
xP<- predict(finalP,fpeP)

summary(xT)
summary(xP)


confusionMatrix(data=xP,reference=fpeP$classes) #esta matriz de confusion es la de caret


yT<- predict(rT,fpeT,type="vector")
yP<- predict(rP,fpeP,type="vector")

print(yT)
print(yP)

confusionMatrix(data=yP,reference=fpeP$classes) #esta matriz de confusion es la de caret

