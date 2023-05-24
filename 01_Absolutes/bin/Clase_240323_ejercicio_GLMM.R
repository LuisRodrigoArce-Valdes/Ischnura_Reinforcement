### 23 de marzo del 2023
##Estadistica computacional en R
###24 de marzo del 2023
##Modelos binomial con n > 1 (dos variables forman 1 variable de respuesta)
#Modelo de efecto mixto generalizado

source("Funciones.r")
library(lme4)

data <- read.table("insects.txt", header = T)
head(data)

attach(data)

tapply(rowSums((data[,4:5])), block, sum)


sobrevivencia <- cbind(dead, alive)

M <- glmer(sobrevivencia ~ treatment + (1|block/treatment), family = "binomial"(link = "logit")) 

###dado que hay un tamaño de muestra diferente en cada replica no se puede utilizar unicamente la variable "dead" or "alive"
## (1|block/treatment) se coloca el factor fijo "treatment" dentro del factor aleatorio para indicar la identidad del tratamiento y separar las distintas replicas

car::Anova(M)
summary(M)

plot(M)


##predicion

sprayed <- predict(M, data.frame(treatment= "srayed", block=LETTERS[1:6]))
Cont <- predict(M, data.frame(treatment= "control", block=LETTERS[1:6]))

Medias <- c(mean(Cont), mean(sprayed)) 
Error <- c(se(Cont), se(sprayed))

lineplot.error(Medias, Error, backtrans = function(x){boot::inv.logit(x)}, col = "black", ylim = c(0,1), names=c("Control", "Sprayed"), xlab = "", ylab = "Probabilidad de muerte") 
abline(h=0)

##extraccion de datos -- coeficientes, errores, valores de p, etc etc

cont <- rbind(CTR=c(1,0), spr=c(1,1))

SUMMARY <- summary(multcomp::glht(M, cont))  


detach(data)

