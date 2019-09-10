########################
########################
####                ####
####  Hamlet Muñoz  ####
####  Data Science  ####
#### Tree and Logit ####
####    Neo 2.0     ####
####   2018-08-20   ####
####                ####
########################
########################

## Notas, médicos Javier Cifuentes (JCIFUENTES@neored.net) y Sergio Ambiado (). 
## Lo que interesa saber en un análisis logístico:
## 1.- Variables significativamente asociadas a mortalidad en todo el grupo; en los subgrupos 24-26 semanas, 24-28 semanas y 24-29 semanas.
## 2.- Variables significativamente asociadas a tiempo de estadía, peso al alta y EG al alta en la población general de sobrevivientes.

rm(list = ls()) ## Borra memoria

# 1 Funciones de interes
fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    
    #theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 2 Leemos las librerias que necesitaremos
#####
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('purrr')) install.packages('purrr'); library('purrr')
if (!require('corrplot')) install.packages('corrplot'); library('corrplot')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('gridExtra')) install.packages('gridExtra'); library('gridExtra')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('RColorBrewer')) install.packages('RColorBrewer'); library('RColorBrewer')
if (!require('showtext')) install.packages('showtext'); library('showtext')
if (!require('jsonlite')) install.packages('jsonlite'); library('jsonlite')
if (!require('curl')) install.packages('curl'); library('curl')
if (!require('knitr')) install.packages('knitr'); library('knitr')
if (!require('kableExtra')) install.packages('kableExtra'); library('kableExtra')
if (!require('magick')) install.packages('magick'); library('magick')
if (!require('cluster')) install.packages('cluster'); library('cluster')
if (!require('FactoMineR')) install.packages('FactoMineR'); library('FactoMineR')
if (!require('factoextra')) install.packages('factoextra'); library('factoextra')
if (!require('magrittr')) install.packages('magrittr'); library('magrittr')
if (!require('party')) install.packages("party"); library(party) # arboles de decision.
if (!require('rpart')) install.packages("rpart"); library(rpart) # arboles de decision.
if (!require('rpart.plot')) install.packages("rpart.plot"); library(rpart.plot) # Graficos de arboles de decision.
if (!require('C50')) install.packages("C50"); library(C50) # Algoritmos y datasets para arboles de decision.
if (!require('Amelia')) install.packages("Amelia"); library(Amelia) # Algoritmos y datasets para arboles de decision.

# 3 Definimos la carpeta de trabajo:
#####
setwd("C:/Hamlet/Indisa things/7.- Neo Cifuentes")

# 4 Cargamos los datos
#####
require('readxl')
Data = read_excel("2018-08-14 - resultados por EG.xlsx", sheet = "FR")
Data = as.data.frame(Data)
# Damos una primera mirada a los datos
head(Data)
colnames(Data)
# Una mirada desde arriba a la data
sapply(Data, function(x) sum(is.na(x))) # datos perdidos
sapply(Data, function(x) length(unique(x))) # Cantidad de valores unicos

# 5 Tratamiento de datos
#######
# Vemos datos faltantes
require('Amelia'); # Algoritmos y datasets para arboles de decision.
jpeg('1.- Missing values of Data.jpg')
missmap(Data, main = "Missing values vs observed")
dev.off()
length(Data[,1])
# Como hay mucho miss, modelemos con variables relevantes.
#SubData = Data[c(87,17,27,35, 23)] # para data en hoja "Base de Datos Corregida"
#SubData = Data[c(58,16,23,37, 20,15)] # primer intento: Aquí las columnas que impacten a la variable que queremos modelar

# 6 Quitaremos 2 variables que tienen muchos vacios, para aprovechar mejor la data.
#cols.dont.want <- "genome"
cols.dont.want <- c("ALTA CON OXIGENO", "Oxíg a los 28días") # if you want to remove multiple columns
SubData <- Data[, ! names(Data) %in% cols.dont.want, drop = F]

length(Data[1,]) # Columnas
length(Data[,1]) # Filas
length(SubData[1,]) # Columnas
length(SubData[,1]) # Filas

# 7 Vemos datos faltantes
jpeg('2.- Missing values vs observed of Subset.jpg')
missmap(SubData, main = "Missing values in SubData")
dev.off()
# col 257 y 495 de Edad Materna están vacías. Las quitamos, junto con todas las demás filas con miss.
SubData = SubData[complete.cases(SubData), ] # El mejor compactador.
length(SubData[,1]) # La data se redujo en 2 filas, que son donde X1 (Edad Materna) tiene 2 NA.
missmap(SubData, main = "Missing values in SubData") # el df está llenito
# 511 filas se redujo a 492. 19 filas menos que tenían missdata.

length(SubData[1,])
length(SubData[,1])
sapply(SubData, function(x) sum(is.na(x))) # No hay datos perdidos
sapply(SubData, function(x) length(unique(x))) # Cantidad de valores unicos
names(SubData) # tenemos 52 variables, 51 si descontamos a Y (la respuesta)

####################################################
###           Estadísticas Descriptivas          ###
####################################################

# Variable respuesta Y = Fallecimiento
# El resto se puede ver en el PowerBI de Neo publicado.
sapply(SubData, function(x) sum(is.na(x))) # No hay datos perdidos
sapply(SubData, function(x) length(unique(x))) # Cantidad de valores unicos
summary(SubData) # Estadísticas de resumen
# hacer un boxplot(SubData)

######################################################
### Correlación de todas las variables importantes ###
######################################################
Y = SubData$"FALLECIMIENTO"; Y  # FALLECIMIENTO, del bebé
X1= SubData$"TRASLADO"; X1 # TRASLADO
X2= SubData$"Múltiple"; X2 # embarazo Múltiple
X3= SubData$"periodo"; X3 # periodo
X4= SubData$"Cidoten Prenatal"; X4 # correlacionada con Antibióticos Prenatales
X5= SubData$"Peso Nacimiento"; X5 # Peso Nacimiento, significativa (negativa)
X6= SubData$"PEG"; X6 # Si No
X7= SubData$"Rango Peso Num"; X7 # ya transformadas a categoría num en la base
X8= SubData$"Categ Peso Num"; X8 # ya transformadas a categoría num en la base
X9= SubData$"Apgar 1'"; X9 
X10= SubData$"Apgar 5'"; X10 
X11= SubData$"masaje cardiaco"; X11 
X12= SubData$"adrenalina"; X12 
X13= SubData$"Perforaciòn Intestinal"; X13 
X14= SubData$"Sepsis precoz"; X14 
X15= SubData$"Sepsis tardia"; X15
X16= SubData$"EG al nacer"; X16

df = data.frame(Y,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16); df # Agregamos las variables al dataframe

# transformar a numérico: No=1, Si=2. Falta transformar X7 y X8 que son las categorías de peso.
df[, c(1:17)] = sapply(df[, c(1:17)], as.numeric); df 

names(df) = c("FALLECIMIENTO","TRASLADO","Múltiple","periodo","Cidoten Prenatal","Peso Nacimiento","PEG",
              "Rango Peso Num","Categ Peso Num",
              "Apgar 1'","Apgar 5'","masaje cardiaco","adrenalina",
              "Perforaciòn Intestinal" ,"Sepsis precoz","Sepsis tardia","EG al nacer")

dim(df) # filas columnas
sapply(df, function(x) sum(is.na(x))) # No hay datos perdidos
sapply(df, function(x) length(unique(x))) # Cantidad de valores unicos

is.data.frame(df)
# lapply(df, as.integer) # categorías como valores enteros.

## Correlaciones ##
# cor(df, use="pairwise.complete.obs")
cor(df,method="pearson") # Correlación lineal
cor(df,method="spearman") # Correlación monotónica
cor(df,method="kendall") #

# Ver correlaciones
require('ggplot2')
require('corrplot')

#png("3.- Correlation Subset.jpeg", height = 500,width =600)
#col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
#                           "cyan", "#007FFF", "blue", "#00007F"))
#corrplot(cor(df,method="pearson"),col=col4(100),tl.srt = 70,tl.col="black",tl.cex=1,method="circle",cl.ratio=.4,cl.align="l") # method="number"
corrplot(cor(df, method="pearson"), type = "upper", order = "original", method="circle",
         tl.col = "black", tl.srt = 45, tl.cex=0.7, cl.ratio=.4,cl.align="l"
         #,title="Matriz de Correlación"
)
cor(df,method="pearson") # Correlación lineal
#dev.off()

## Test Chi-cuadrado, dependencia entre las variables categóricas ##
#install.packages('MASS'); library(MASS) # ?
cuadro1 = table(Y,X5); cuadro1; chisq.test(cuadro1) # H0: Independencia. p-valor<0.05, Rech H0, existe evidencia significativa para aseverar que existe relación entre las variables.
cuadro2 = table(Y,X4); cuadro2; chisq.test(cuadro2) # p-valor<0.05, Rech H0.
cuadro3 = table(Y,X10); cuadro3; chisq.test(cuadro3) # Rech H0. son dep
cuadro4 = table(Y,X2); cuadro4; chisq.test(cuadro4) # No se puede Rech H0. p-valor>0.05
cuadro5 = table(X5,X4); cuadro5; chisq.test(cuadro5) # No se puede Rech H0. X5 y X4 son dep, luego habrá que quedarse con una sola, el árbol sugiere X5=Peso Nac
cuadro6 = table(X4,X10); cuadro6; chisq.test(cuadro6) # Rech H0. son dep, esto sugiere no usar X4 por tener cierta colinealidad con X10
cuadro7 = table(X5,X10); cuadro7; chisq.test(cuadro7) # No se puede Rech H0. p-valor>0.5, son ind.

#?mosaicplot
mosaicplot(cuadro1, color=TRUE, main="Mosaico de Fallecimientos vs Cidoten Prenatal", xlab="Fallecimientos", ylab="Cidoten Prenatal")
mosaicplot(cuadro2, color=TRUE, main="Mosaico de Fallecimientos vs Peso nacimiento", xlab="Fallecimientos", ylab="Cidoten Prenatal")
mosaicplot(cuadro3, color=TRUE, main="Mosaico de Fallecimientos vs Apgar 5'", xlab="Fallecimientos", ylab="Apgar 5'")
mosaicplot(cuadro4, color=TRUE, main="Mosaico de Fallecimientos vs Embarazo Múltiple", xlab="Fallecimientos", ylab="Embarazo Múltiple")

par(mfrow=c(1,1))

############################
### i) Árbol de Decisión ###
############################

# En caso de que se requieran, instalamos y cargamos paquetes a usar, en este caso:
if (!require('readxl')) install.packages("readxl"); library(readxl) # Leer excel
if (!require('party')) install.packages("party"); library(party) # árboles de decisión.
if (!require('rpart')) install.packages("rpart"); library(rpart) # árboles de decisión.
if (!require('rpart.plot')) install.packages("rpart.plot"); library(rpart.plot) # Gráficos de árboles de decisión.
if (!require('C50')) install.packages("C50"); library(C50) # Algoritmos y datasets para árboles de decisión.

df = data.frame(Y,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16); df # Agregamos las variables al dataframe

# Proporción de Mails que son Spam 9.36%, luego 90.64% No son Spam.
Si.Fall = sum(df$Y=="Si")/length(df$Y); Si.Fall
No.Fall = 1-Si.Fall; No.Fall # Sobrevida = 0.8699187

# De la columna Spam, cambiamos los Sí y No por 1 y 0, resp. Pero no es necesario.
#Data$Spam[Data$Spam == "Sí"] = 1 
#Data$Spam[Data$Spam == "No"] = 0
#Data$Spam = as.factor(Data$Spam) # Esto es para que quede como categórica.

# Dividimos la base en dos muestras: training set y test set. El primero es para entrenar al modelo, el segundo es para probarlo.
ind = sample(2, nrow(df), replace = TRUE, prob=c(0.60, 0.40)) # 60% Training, 40% Testing
Train = df[ind==1,] # Training
Test  = df[ind==2,] # Testing

# Creamos el árbol, de entrenamiento (data=Train)
arbol = rpart(Y ~ ., method = "class", data = Train) # en data=Train, usamos la data de entrenamiento.
arbol
rpart.plot(arbol, extra=4, tweak=1.5)
# extra=4, Class models: probability per class of observations in the node (conditioned on the node, sum across a node is 1).
# tweak=1.7, sólo para agrandar los números.
#?rpart.plot
printcp(arbol)
plotcp(arbol) # El error no vuelve a subir, no necesita poda.

# Poda del árbol, en caso de que lo necesite
poda.arbol = prune(arbol, cp=arbol$cptable[which.min(arbol$cptable[,"xerror"]),"CP"])
poda.arbol = prune(arbol, cp=0.87281) # cp sería el valor menor de xerror que arroja printcp(arbol)
printcp(poda.arbol)

# Predecir los Spam en Test data, esto es para probar el modelo que entrenamos con Training data.
testPred = predict(arbol, newdata = Test, type = "class")

# Visualizamos la matriz de confusión, es decir, los falsos positivos que arrojó nuestro modelo.
table(testPred,Test$Y)
# El modelo ha dicho que NO fallecen 161 que efectivamente no lo hacen, y se ha equivocado con 7 diciendo que No lo harán cuando Sí lo hicieron.
# Por otro lado, el modelo ha dicho que SÍ fallecen 17 que efectivamente lo hacen, y se ha equivocado en 10 diciendo que Sí lo harían cuando No lo hicieron.

# Calculemos el % de aciertos, sumando todos los aciertos y dividiendo por la cantidad de predicciones.
sum(testPred == Test$Y) / length(Test$Y)*100

# Conclusión, el modelo arroja un 94.95413% de acierto.
# Esto puede deberse a que existe "imbalanced data", es decir, hay una proporción pequeña de datos en la respuesta.
# The problem starts when the actual costs that we assign to every error are not equal. If we deal with a rare but fatal disease, the cost of failing to diagnose the disease of a sick person is much higher than the cost of sending a healthy person to more tests.



# Creamos el árbol final del modelo, (data=df)
arbol.f = rpart(df$Y ~ ., method = "class", data = df) # en data=Train, usamos la data de entrenamiento.
arbol.f
rpart.plot(arbol.f, extra=101, tweak=1.1, type=4) # Guardar este.
# extra=4, Class models: probability per class of observations in the node (conditioned on the node, sum across a node is 1).
# tweak=1.7, sólo para agrandar los números.
#?rpart.plot
printcp(arbol.f)
plotcp(arbol.f, col="Red", lty = 4, main="cross-validation results") # El error no vuelve a subir, no necesita poda.
summary(arbol.f)
# Luego el modelo de árbol de decisión sugiere que las variables significativas
# que mejor explican los Fallecimientos (o Sobrevida) son Peso Nacimiento (split 658 [gr])
# y Apgar1' (split puntaje >= 4).
png("9.- Cross-validation del Árbol de Mortalidad.jpeg", height=500, width=600)
plotcp(arbol.f, col="Red", lty = 4, main="cross-validation results")
dev.off()

# Guardamos el gráfico.
png("8.- Árbol de Mortalidad.jpeg", height=500, width=600)
rpart.plot(arbol.f, extra=101, tweak=1.1, type=4)
dev.off()

# El árbol se intrepreta así:
# La sobrevida es 86.9% (428/492), de los cuales separamos en 2 ramas según Peso nacimiento,
# los que pesan <658 y los que pesan >=658, de los primeros (que son el 7% del total (36/492)),
# sobrevive un 17% (6/36). De los segundos (que son el 92.7% del total (456/492)), sobrevive un 92.5% (422/456).
# Volvemos a separar en 2 (la rama izquierda anterior de pesos mayores a 658) según Apgar 5',
# aquellos con puntaje <4 (que son el 3.7% de la rama anterior (17/456)) tienen una sobrevida del 23.5% (4/17).
# aquellos con puntaje >=4 (que son el 89.2% de la rama anterior (439/492)) tienen una sobrevida del 95.2% (418/439).
# Estas 2 ramificaciones son producto de que X4 en presencia de X5 presenta cierta colinealidad, por tanto, nos quedamos con X5 dado que explica mejor Y.

# Pregunta: Cómo sería el árbol sin Peso nacimiento? Probar.


# Tenemos todo ok con Fallec. Escribirlo como publicación en LaTeX PLOS
# Hay que hacer lo mismo con Estadías.
# 1.- Eliminando Estadía y Visita médica del Neonatólogo (porque pesan mucho), qué Factores X explican el mayor costo en consumos.
## Hacer la regresion logística desde aquí.

dev.off()
dev.off()


####################################
###                              ###
### Regresión logística binomial ###
###                              ###
####################################

rm(list = ls()) ## Borra memoria

# 1 Funciones de interes
fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  # Begin construction of chart
  
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    
    #theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 2 Leemos las librerias que necesitaremos
#####
if (!require('plyr')) install.packages('plyr'); library('plyr')
if (!require('purrr')) install.packages('purrr'); library('purrr')
if (!require('corrplot')) install.packages('corrplot'); library('corrplot')
if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('gridExtra')) install.packages('gridExtra'); library('gridExtra')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('readxl')) install.packages('readxl'); library('readxl')
if (!require('RColorBrewer')) install.packages('RColorBrewer'); library('RColorBrewer')
if (!require('showtext')) install.packages('showtext'); library('showtext')
if (!require('jsonlite')) install.packages('jsonlite'); library('jsonlite')
if (!require('curl')) install.packages('curl'); library('curl')
if (!require('knitr')) install.packages('knitr'); library('knitr')
if (!require('kableExtra')) install.packages('kableExtra'); library('kableExtra')
if (!require('magick')) install.packages('magick'); library('magick')
if (!require('cluster')) install.packages('cluster'); library('cluster')
if (!require('FactoMineR')) install.packages('FactoMineR'); library('FactoMineR')
if (!require('factoextra')) install.packages('factoextra'); library('factoextra')
if (!require('magrittr')) install.packages('magrittr'); library('magrittr')
if (!require('party')) install.packages("party"); library(party) # arboles de decision.
if (!require('rpart')) install.packages("rpart"); library(rpart) # arboles de decision.
if (!require('rpart.plot')) install.packages("rpart.plot"); library(rpart.plot) # Graficos de arboles de decision.
if (!require('C50')) install.packages("C50"); library(C50) # Algoritmos y datasets para arboles de decision.
if (!require('Amelia')) install.packages("Amelia"); library(Amelia) # Algoritmos y datasets para arboles de decision.
if (!require('ROCR')) install.packages('ROCR'); library(ROCR) # Curva de ROC

# 3 Definimos la carpeta de trabajo:
#####
setwd("C:/Hamlet/Indisa things/7.- Neo Cifuentes")

# 3 Cargamos los datos
#####
require('readxl')
Data = read_excel("2018-08-14 - resultados por EG.xlsx", sheet = "FR")
Data = as.data.frame(Data)
# Damos una primera mirada a los datos
colnames(Data)
head(Data)
# Una mirada desde arriba a la data
sapply(Data, function(x) sum(is.na(x))) # No hay datos perdidos
sapply(Data, function(x) length(unique(x))) # Cantidad de valores unicos

cols.dont.want <- c("ALTA CON OXIGENO", "Oxíg a los 28días") # if you want to remove multiple columns
SubData <- Data[, ! names(Data) %in% cols.dont.want, drop = F]

length(Data[1,]) # cantidad de columnas de Data
length(SubData[1,]) # cantidad de columnas de SubData
colnames(SubData)

length(SubData[,1])
SubData = SubData[complete.cases(SubData), ] # El mejor compactador.
length(SubData[,1]) # La data se redujo en 2 filas, que son donde X1 (Edad Materna) tiene 2 NA.
missmap(SubData, main = "Missing values in SubData") # el df está llenito
# 511 filas se redujo a 492. 19 filas menos que tenían missdata.


# Exportamos para usar en PBI (ya que no tiene missdata).
library(xlsx)
write.xlsx(SubData, 'NeoNoMiss.xlsx')

# Redefinimos las variables, porque para Logit las queremos con 0 y 1 esta vez.
Y = SubData$"FALLECIMIENTO"; Y  # FALLECIMIENTO, del bebé
X1= SubData$"TRASLADO"; X1 # TRASLADO. Si No
X2= SubData$"Múltiple"; X2 # embarazo Múltiple. Si No
X3= SubData$"periodo"; X3 # periodo. 1 o 2
X4= SubData$"Cidoten Prenatal"; X4 # correlacionada con Antibióticos Prenatales. Si No
X5= SubData$"Peso Nacimiento"; X5 # Peso Nacimiento, significativa (negativa)
X6= SubData$"PEG"; X6 # Si No
X7= SubData$"Rango Peso Num"; X7 # ya transformadas a categoría num en la base
X8= SubData$"Categ Peso Num"; X8 # ya transformadas a categoría num en la base
X9= SubData$"Apgar 1'"; X9 # numeric
X10= SubData$"Apgar 5'"; X10 # numeric
X11= SubData$"masaje cardiaco"; X11 # Si No
X12= SubData$"adrenalina"; X12 # Si No
X13= SubData$"Perforaciòn Intestinal"; X13 # Si No
X14= SubData$"Sepsis precoz"; X14 # Si No
X15= SubData$"Sepsis tardia"; X15 # Si No
X16= SubData$"EG al nacer"; X16 # Numeric, Edad gestacional, semanas al nacer.

Y[Y != "Si"] = 0
Y[Y == "Si"] = 1; Y
X1[X1 != "Si"] = 0
X1[X1 == "Si"] = 1; X1
X2[X2 != "Si"] = 0
X2[X2 == "Si"] = 1; X2
X4[X4 != "Si"] = 0
X4[X4 == "Si"] = 1; X4
X6[X6 != "Si"] = 0
X6[X6 == "Si"] = 1; X6
#X7 #categorías de Rango de pesos
#X8 #categorías de Categorías de peso
X11[X11 != "Si"] = 0
X11[X11 == "Si"] = 1; X11
X12[X12 != "Si"] = 0
X12[X12 == "Si"] = 1; X12
X13[X13 != "Si"] = 0
X13[X13 == "Si"] = 1; X13
X14[X14 != "Si"] = 0
X14[X14 == "Si"] = 1; X14
X15[X15 != "Si"] = 0
X15[X15 == "Si"] = 1; X15

df = data.frame(Y,X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16); df # Agregamos las variables al dataframe
# X7 o X8


# Ver primero si las variables son significativas.
df

# Train y Test
ind = sample(2, nrow(df), replace = TRUE, prob=c(0.60, 0.40)) # 60% Training, 40% Testing
Train = df[ind==1,] # Training data
Test  = df[ind==2,] # Testing data

# Ajuste del modelo de regresión logística binomial en Train
model = glm(Y ~., family=binomial(link='logit'), data=Train)
summary(model)

# Al ser muchas variables, ordenamos todo por significancia para verlo mejor.
library(xtable)
idx = order(coef(summary(model))[,4])  # ordeno los coef de menor a mayor p-valor
out = coef(summary(model))[idx,]       # reordeno coef, SE, etc. de menor a mayor p-valor
out

# Veamos la performance del modelo.
install.packages('ggplot2'); library("ggplot2")
install.packages('ROCR'); library("ROCR")

prob = predict(model, newdata=Test, type='response')
pred = prediction(prob, Test$Y)  
perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc = performance(pred, "auc", fpr.stop=1) # AUC=0.9348799
tp  = performance(pred, measure="tpr" )
#auc.name()
png("5.2.- ROC_Y_Train.jpeg")
plot(perf, col=rainbow(7), main="ROC curve",
     xlab="1-Specificity (false positives)", 
     ylab="Sensitivity (true positives)")    
abline(0, 1) # añade la diagonal, que representa la apuesta random (lanzar una moneda)
dev.off()


# El modelo es bueno, pero sería mejor si usamos sólo algunas variables,
# Aquí debería hacer forward o backward. Poniendo variables o quitándolas para ver el mejor ajuste.
# Este es el modelo heurístico
model2 = glm(Y ~ X5+X10+X4, family=binomial(link='logit'), data=df)
summary(model2)
# buscar modelos logit backward or forward

idx = order(coef(summary(model2))[,4])  # ordeno los coef de menor a mayor p-valor
out = coef(summary(model2))[idx,]       # reordeno coef, SE, etc. de menor a mayor p-valor
out

prob = predict(model2, newdata=df, type='response')
pred = prediction(prob, df$Y) # en todo el df
perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc = performance(pred, "auc", fpr.stop=1) # AUC=0.8984344
tp  = performance(pred, measure="tpr" )
#auc.name()
png("5.3.- ROC_Y_model2.jpeg")
plot(perf, col=rainbow(7), main="ROC curve",
     xlab="1-Specificity (false positives)", 
     ylab="Sensitivity (true positives)")    
abline(0, 1) # añade la diagonal, que representa la apuesta random (lanzar una moneda)
dev.off()

## Plots de model2
install.packages('jtools'); library("jtools") # se demora
install.packages('ggstance'); library("ggstance")

summ(model2)
effect_plot(model2, pred = X5, interval = TRUE) # 
#plot_summs(model2, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
effect_plot(model2, pred = X5, interval = TRUE, plot.points = TRUE)
#export_summs(model2, scale = TRUE)

## you can plot a normal distribution along the width of your specified interval to convey the uncertainty. This is also great for didactic purposes.
png("6.1.- Comparación de modelos.jpeg")
plot_summs(model, model2, scale = TRUE, plot.distributions = TRUE
           #,omit.coefs=c("(Intercept)","X11","X12") # Omitir variables.
)
dev.off()

## Guardamos un ejemplo de la Y vs X importante, en este caso "Peso al nacer"
png("7.1.- Fallecimiento vs Peso al nacer.jpeg")
effect_plot(model2, pred = X5, interval = TRUE, #plot.points = TRUE,
            x.label="X5: Peso al nacer", 
            y.label="Y: Fallecimiento")
abline(0, 1) # Now you know that when mpg is from about 30 and higher, it no longer is giving any appreciable increase in probability because it has already reached 1.
dev.off()

png("7.2.- Fallecimiento vs EG.jpeg")
effect_plot(model2, pred = X10, interval = TRUE, #plot.points = TRUE,
            x.label="X6: EG, Etapa Gestacional (Semanas)", 
            y.label="Y: Fallecimiento")
#abline(1300, 1) # añade la diagonal, que representa la apuesta random (lanzar una moneda)
dev.off()

png("7.3.- Fallecimiento vs Citoden Prenatal.jpeg")
cat_plot(model2, pred = X4, interval = TRUE, #plot.points = TRUE,
         x.label="X4: Citoden Prenatal", 
         y.label="Y: Fallecimiento")
#abline(1300, 1) # añade la diagonal, que representa la apuesta random (lanzar una moneda)
dev.off()

#install.packages('officer'); library("officer")
#install.packages('flextable'); library("flextable")
#export_summs(model2, scale = TRUE, to.file = "docx", file.name = "Logistic Regression.docx") # to.file="pdf"

# Al ser muchas variables, ordenamos todo por significancia para verlo mejor.
idx = order(coef(summary(model2))[,4])  # ordeno los coef de menor a mayor p-valor
out = coef(summary(model2))[idx,]       # reordeno coef, SE, etc. de menor a mayor p-valor
out

# Ajuste del modelo de regresión logística binomial en Train
model1 = glm(Y ~., family=binomial(link='logit'), data=df)
summary(model1)

# Stepwise forward and backward.
slm1 <- step(model1, scope = list(upper = as.formula(Y ~ .^2),
                                  lower = as.formula(Y ~ .)),
             direction = "both")
slm1
summary(slm1)
idx = order(coef(summary(slm1))[,4])  # ordeno los coef de menor a mayor p-valor
out = coef(summary(slm1))[idx,]       # reordeno coef, SE, etc. de menor a mayor p-valor
out

#                Estimate   Std. Error     z value     Pr(>|z|)
#(Intercept)  62.75460985 1.438421e+01  4.36274377 1.284413e-05 ***
#X16          -2.25551462 5.332254e-01 -4.22994617 2.337472e-05 ***
#X61:X8       -5.33513355 1.499797e+00 -3.55723613 3.747773e-04 ***
#X41          -2.78545196 8.275372e-01 -3.36595391 7.627945e-04 ***
#X131:X16      2.01515952 6.086430e-01  3.31090538 9.299465e-04 ***
#X8:X16        0.36570368 1.183905e-01  3.08896186 2.008572e-03 **
#X8:X131      -3.08725553 1.135385e+00 -2.71912694 6.545448e-03 **
#X8           -7.92977587 3.032678e+00 -2.61477712 8.928570e-03 **
#X11:X131      5.82554939 2.242975e+00  2.59724230 9.397561e-03 **
#X11:X151      4.13536699 1.860796e+00  2.22236463 2.625867e-02 *
#X11          -6.92468054 3.128290e+00 -2.21356736 2.685855e-02 *
#X131        -33.98637830 1.592703e+01 -2.13388088 3.285253e-02 *
#X11:X141      8.43138897 3.984690e+00  2.11594614 3.434939e-02 *
#X3:X151      -3.83929115 1.846411e+00 -2.07932684 3.758732e-02 *
#X3           -1.38081794 7.220499e-01 -1.91235803 5.583029e-02 .


slm2 <- step(model2, scope = list(upper = as.formula(Y ~ .^2),
                                  lower = as.formula(Y ~ .)),
             direction = "both")
slm2
summary(slm2)

# Model2 bueno, con algunas.
#             Estimate      Std. Error   z value     Pr(>|z|)
#(Intercept)  6.276467261 0.8275520961  7.584377 3.340879e-14
#X5:Peso Nac -0.003774434 0.0005950921 -6.342604 2.259130e-10
#X10:Apgar5' -0.440133281 0.0766589474 -5.741447 9.387095e-09
#X4:Cidoten  -1.270137960 0.4312534941 -2.945224 3.227214e-03


# Model2 anterior, con todas.
#                Estimate  Std. Error    z value     Pr(>|z|)
#(Intercept) 13.393427005 3.229680820  4.1469816 3.368871e-05 ***
#X6: PEG      1.820156895 0.586537095  3.1032255 1.914237e-03 **
#X10:Apgar5' -0.364667524 0.134998204 -2.7012769 6.907381e-03 **
#X4:Cidoten  -1.222656179 0.504209175 -2.4248987 1.531266e-02 *
#X16:EG nacer-0.339501831 0.143299453 -2.3691774 1.782770e-02 *
#X1:Traslado -1.029730609 0.542967291 -1.8964874 5.789563e-02 .
#X13:Perf Int 1.009227838 0.582197133  1.7334813 8.301016e-02 .

# Modelo entonces quedaría Y = X6 + X10 + X4 + X16 + X1 + X13
# Por qué no aparece X5 que es el más correlacionado, ¿es no significativo?.

# Model3 stepwise
#Coefficients:
#              Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  1.635e+01  7.245e+00   2.257  0.02399 * 
#X2           8.075e+01  3.395e+01   2.379  0.01738 * 
#X4          -2.052e+00  8.427e-01  -2.435  0.01489 * 
#X9          -1.074e+00  3.283e-01  -3.270  0.00107 **
#X10         -9.794e-01  3.929e-01  -2.493  0.01267 * 
#X2:X16      -3.144e+00  1.385e+00  -2.270  0.02324 * 
#X9:X10       1.158e-01  4.622e-02   2.506  0.01223 * 
#X2:X3       -4.897e+00  2.847e+00  -1.720  0.08540 . 
#X2:X5        9.954e-03  5.457e-03   1.824  0.06816 . 

model4 = glm(Y ~ X2+X4+X9+X2*X16+X9*X10+X2*X3+X2*X5, family=binomial(link='logit'), data=df)
summary(model4)

idx = order(coef(summary(model4))[,4])  # ordeno los coef de menor a mayor p-valor
out = coef(summary(model4))[idx,]       # reordeno coef, SE, etc. de menor a mayor p-valor
out

#                Estimate   Std. Error     z value     Pr(>|z|)
#X5          -0.003252800 0.0008609209 -3.77827949 0.0001579156 ***
#X10         -0.456126962 0.1606877629 -2.83859177 0.0045313087 **
#X4          -1.176536136 0.4553602376 -2.58374807 0.0097733154 **
#(Intercept)  6.583103816 2.7518537332  2.39224336 0.0167457349 *
#X2          11.843372977 7.0913338481  1.67011922 0.0948957776 .

model5 = glm(Y ~ X5+X10+X4, family=binomial(link='logit'), data=df)
summary(model5)

model6 = glm(Y ~ X4+X9+X10, family=binomial(link='logit'), data=df)
summary(model6)

AIC(model2,model3,model4,model5,model6)

# El modelo2, X5+X10+X4 fue el mejor modelo.
# Veamos como predice y probemos su ROC, respecto del model3 que es completo (con todas las variables en el orden óptimo).

######################
## Model2 vs Model3 ##
###################### Ver también significancia de las variables en los subgrupos de semanas, EG.

model2 # Y ~ X5+X10+X4
model3 # Y ~ X1+X2+X3+X4+X5+X6+X9+X10+X11+X12+X13+X14+X15+X16

df2 = data.frame(Y,X1,X2,X3,X4,X5,X6,X7, X8,X9, X10,X11,X12,X13,X14,X15,X16); df2
sapply(df2, function(x) sum(is.na(x))) # No hay datos perdidos
sapply(df2, function(x) length(unique(x))) # Cantidad de valores unicos

# transformar a numérico: No=1, Si=2. Falta transformar X7 y X8 que son las categorías de peso.
df2[, c(1:15)] = sapply(df2[, c(1:15)], as.numeric); df2

names(df2) = c("FALLECIMIENTO","TRASLADO","Múltiple","periodo","Cidoten Prenatal","Peso Nacimiento","PEG",
               "Rango Peso Num","Categ Peso Num",
               "Apgar 1'","Apgar 5'","masaje cardiaco","adrenalina",
               "Perforaciòn Intestinal" ,"Sepsis precoz","Sepsis tardia","EG al nacer")

prob = predict(model, newdata=Test, type='response')
pred = prediction(prob, Test$Y)  
perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc = performance(pred, "auc", fpr.stop=1) # AUC=0.8720635
tp  = performance(pred, measure="tpr" )
#auc.name()
png("5.2.- ROC_Y_Train.jpeg")
plot(perf, col=rainbow(7), main="ROC curve",
     xlab="1-Specificity (false positives)", 
     ylab="Sensitivity (true positives)")    
abline(0, 1) # añade la diagonal, que representa la apuesta random (lanzar una moneda)
dev.off()








