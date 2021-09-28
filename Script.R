##CAPÍTULO 2: EXPERIMENTOS SIMPLES COMPARATIVOS###
####Editando la segunda línea
# cargando paquetes y datos:
# Ejemplo:
library(Sleuth2) #Contiene datos y ejemplos
library(mosaic) #Contiene datos y conjunto de utilidades elaborados para la enseñanza
datos<-case0101 
summary(datos)
histogram(~Score | Treatment, data = datos)
plot(Score ~ Treatment, data = datos)
favstats(Score ~ Treatment, data = datos)
t.test(Score ~ Treatment, alternative = "two.sided", var.equal = TRUE, data = datos)
summary(lm(Score ~ Treatment, data = datos))
anova(lm(Score ~ Treatment, data = datos))
residuos<- residuals.lm(lm(Score ~ Treatment, data = datos))
datos<-c(datos,as.data.frame(residuos))
shapiro.test(residuos)
bartlett.test(residuos~Treatment,datos)

##ACTIVIDAD 2:
library(Sleuth2)
datos<-case0301
summary(datos)
favstats(Rainfall ~ Treatment, data = datos)
plot(Rainfall ~ Treatment, data = datos)
favstats(log(Rainfall) ~ Treatment, data = case0301)
plot(log(Rainfall) ~ Treatment, data = datos)
anova(lm(log(Rainfall) ~ Treatment, data = datos))
summary(lm(log(Rainfall) ~ Treatment, data = datos))
anova(lm(Rainfall ~ Treatment, data = datos))


##CAPÍTULO 3: ###DISEÑO COMPLETO AL AZAR

library(agricolae) #cargando la librería agricolae
tratam <- c("A", "B", "C") #creando un vector con las etiquetas de los tres tratamientos
repeticion <- c(4, 3, 4) #indicando el número de repeticiones por tratamiento 4 para A, 3 para B y 4 para C
outdesign <- design.crd(tratam,r=repeticion,seed=777,serie=0)
book1 <- outdesign$book
print(book1)
write.csv(book1,"plan1.csv",row.names=FALSE) #el croquis del diseño es exportado con el nombre plan1.csv

# Cargando datos:
# Ejemplo:
library(mosaic)
datos <- read.table("kuehl2.3.txt",header=T)
summary(datos)
favstats(serum.T3 ~ trat, data = datos)
datos$trat<-as.factor(datos$trat)
anova(lm(serum.T3~trat,datos))
summary(lm(serum.T3~trat,datos))
par(mfrow=c(2,2))
plot(lm(serum.T3~trat,datos))
residuales<-residuals(lm(serum.T3~trat,datos))
datos<-cbind(datos,as.data.frame(residuales))
library(nortest)
ad.test(residuales)
#library(car)
#leveneTest(serum.T3~trat,datos)

pairwise.t.test(datos$serum.T3,datos$trat,p.adjust.method="none") #prueba de comparaciones por pares
pairwise.t.test(datos$serum.T3,datos$trat, p.adjust.method = "bonf") #diferencia mínima significativa
TukeyHSD(aov(serum.T3~trat,datos)) #prueba de tukey
library(multcomp)
model<-aov(formula = serum.T3 ~ trat, data = datos)
compTukey<- confint(glht(model, linfct = mcp(trat = "Tukey"))) #prueba de Tukey
summary(compTukey)

library(multcomp)
ni<-tapply(datos$serum.T3,datos$trat,length)
contr<-contrMat(ni,type="Dunnet")
model<-aov(formula = serum.T3 ~ trat, data = datos)
compDunnet<- confint (glht(model, linfct = mcp(trat = contr))) #prueba de Dunnet
compDunnet
summary(compDunnet)

library(agricolae)
model<-aov(formula = serum.T3 ~ trat, data = datos) #prueba de Duncan
duncan.test(model,"trat",console=TRUE)

library(multcomp)
model<-aov(formula = serum.T3 ~ trat, data = datos) #prueba de contrastes
K <- rbind("1 vs 3" = c(1,0,-1,0,0),
           "1,3 vs 4,5" = c(-1,0,-1,1,1),
           "4 vs 5" = c(0,0,0,1,-1),
           "2 vs 1,3,4,5" = c(-1,4,-1,-1,-1))
comp <- glht(model,linfct = mcp(trat=K),alternative = "two.sided")
summary(comp)

##ACTIVIDAD 3:
library(Sleuth2)
library(mosaic)
favstats(Lifetime ~ Diet, data = case0501)
anova(lm(Lifetime ~ Diet, data = case0501))
summary(lm(Lifetime ~ Diet, data = case0501))
bwplot(Lifetime ~ Diet, data = case0501) 
library(multcomp)
model<-aov(Lifetime ~ Diet, data = case0501)
K <- rbind("N/N85 vs NR/50" = c(0,1,0,-1,0,0))
comp <- glht(model,linfct = mcp(Diet=K),alternative = "two.sided")
summary(comp)


##CAPÍTULO 4: ###DISEÑO EN BLOQUE COMPLETO AL AZAR

# creando un croquis del diseño experiemntal
library(agricolae)
trt <- c("A", "B", "C","D","E")
repeticion <- 4
outdesign <- design.rcbd(trt,r=repeticion, seed=-513, serie=1)
book2 <- outdesign$book
print(book2)

# Ejemplo:
library(multcomp)
library(mosaic)
datos<-read.table("diseño1.txt",header=T)
datos$dias<-as.factor(datos$dias)
datos$solucion<-as.factor(datos$solucion)
favstats(efectividad ~ solucion, data = datos)
boxplot(datos$efectividad~datos$solucion,xlab="solucion",ylab="efectividad")
modelo1<-lm(efectividad~dias+solucion,datos)
anova(modelo1)
citukey <- confint(glht(modelo1, linfct = mcp(solucion = "Tukey")))
#En téminos de efectos:
op <- options (contrasts=c("contr.sum","contr.poly"))
modelo2<-lm(efectividad~dias+solucion,datos)
summary(modelo2)
par(mfrow=c(2,2))
plot(modelo1)
summary(citukey)

##ACTIVIDAD 4:
ejercitarse <- read.delim("C:/Users/ELBA VEGA/Desktop/ProjectsR-GitHub/GuiaDisenoExperimR/ejercitarse.txt")
str(ejercitarse)
library(reshape)
ejercitarse2 <- melt(ejercitarse, id="Sujeto")
str(ejercitarse2)


##CAPÍTULO 5: ###DISEÑO CUADRADO LATINO
library(agricolae)
trt <- c("A", "B", "C", "D")
outdesign <- design.lsd(trt, seed=543, serie=1)
print(outdesign$sketch)
book3 <- outdesign$book
print(book3)

# Ejemplo:
datos<-read.table("diseño2.txt",header=T, dec=",")
str(datos)
datos$crucero<-as.factor(datos$crucero)
datos$periodo<-as.factor(datos$periodo)
summary(datos$tiempo)
library(mosaic)
favstats(tiempo~secuencia , data = datos)
matrix(datos$secuencia,5,5,byrow=T)
library(ggplot2)
ggplot(datos,aes(x=crucero,y=tiempo,shape=periodo,group=secuencia))+geom_point()+geom_line(aes(linetype=secuencia))
ggplot(datos,aes(x=periodo,y=tiempo,shape=crucero,group=secuencia))+geom_point()+geom_line(aes(linetype=secuencia))
modelo3<-lm(tiempo~crucero+periodo+secuencia,datos)
anova(modelo3)
library(multcomp)
citukey <- confint(glht(modelo3, linfct = mcp(secuencia = "Tukey")))
summary(citukey)
op <- options (contrasts=c("contr.sum","contr.poly"))
modelo3b<-lm(tiempo~crucero+periodo+secuencia,datos)
summary(modelo3b)

##ACTIVIDAD 5:


##CAPÍTULO 5: ##INTRODUCCIÓN A LOS EXPERIMENTOS FACTORIALES
library(faraway)
datos<-data(abrasion,package="faraway")

# Ejemplo:

datos<-read.table("factorial1.txt",header=T)
str(datos)
datos$Bloque<-as.factor(datos$Bloque)
datos$A<-as.factor(datos$A)
datos$B<-as.factor(datos$B)
library(mosaic)
favstats(y~A+B , data = datos)
modelo<-lm(y~Bloque+A+B+A*B,datos)
anova(modelo)
par(mfcol=c(1,2))
plot(modelo, 1:2)
library(phia)
modelo.means <- interactionMeans(modelo,factors=c("A","B"))
modelo.means
plot(modelo.means)

#Análisis de efectos simples:
testInteractions(modelo, fixed="A", across="B")
testInteractions(modelo, fixed="B", across="A")

#Análisis de residuales:
obs.table <- xtabs(modelo.means$"adjusted mean" ~ A + B, modelo.means)
obs.table<- addmargins(obs.table,FUN=mean,quiet=TRUE)
print(obs.table,digits=4)

res.table <- obs.table - obs.table[3,4] # restando la media
res.table <- sweep(res.table, 1, res.table[,4]) # restando media fila
res.table <- sweep(res.table, 2, res.table[3,]) # restando media comlumna
print(res.table, digits=4)
testInteractions(modelo,residual=c("A","B"))
matplot(t(res.table[-3,-4]), type="b", xaxt="n", ylab="residuales interacción")
axis(1, at=1:3, labels=levels(datos$B))

#Análisis de diferencias en efectos principales
model2<-lm(y~Bloque+A+B,datos)
library(multcomp)
citukey <- confint(glht(model2, linfct = mcp(A = "Tukey")))
summary(citukey)

citukey <- confint(glht(model2, linfct = mcp(B = "Tukey")))
summary(citukey)

##ACTIVIDAD 6

library(faraway)
str(warpbreaks)
