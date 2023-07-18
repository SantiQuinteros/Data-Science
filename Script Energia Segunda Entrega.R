# LIBERIRAS

#library(summarytools)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(FactoInvestigate)
library(plotly)
library(MASS)
library(MVN) 
library(GGally)
library(tidyverse)
library(pROC)
library(caret)

# BASE DE DATOS -> ENERGIA

datos <- read.table("C:\\Users\\Santiago\\OneDrive\\Escritorio\\Proyecto Analisis Multi\\Energia1.txt", header = TRUE, sep = "\t")

row.names(datos) <- datos$PAISES

datos$PAISES <- NULL

# VARIABLES -> CLASE Y RESUMEN

#str(datos)

#descr(datos)

# HISTOGRAMA DE LAS VARIABLES NUMERICAS

par(mfrow = c(4, 4))
par(mar = c(2, 2, 1, 1))

for (i in 1:14) {
  nombre_variable <- names(datos)[i]
  hist(
    datos[[i]],
    main = paste(nombre_variable),
    xlab = nombre_variable,
    ylab = "Frecuencia",
    breaks = seq(min(datos[[i]]), max(datos[[i]]), length.out = 11),
  )
}

# GRAFICO DE CAJA DE LAS VARIABLES NUMERICAS

par(mfrow = c(4, 4))
par(mar = c(2, 2, 2, 2))

for(i in 1:14) {
  nombre_variable <- names(datos)[i]
  boxplot(
    datos[[i]],
    main=paste(nombre_variable),
    xlab=nombre_variable,
    ylab="Valor",
    ylim=c(min(datos[[i]]), max(datos[[i]]))

  )
}

# OUTLIERS

paises_sup <- datos[datos$Prod_Ene > 1 | datos$O_PRIpc > 20000 | datos$C_ELECpc < 60, ]

datos_sup <- subset(datos, !rownames(datos) %in% rownames(paises_sup))

# GRAFICO CAJA SIN OUTLIERS

par(mfrow = c(4, 4))
par(mar = c(2, 2, 2, 2))

for(i in 1:14) {
  nombre_variable <- names(datos_sup)[i]
  boxplot(
    datos_sup[[i]],
    main=paste(nombre_variable),
    xlab=nombre_variable,
    ylab="Valor",
    ylim=c(min(datos_sup[[i]]), max(datos_sup[[i]]))
    
  )
}

# MATRIZ DE CORRELACION Y DISTRIBUCIONES DE LAS VARIABLES

corr_mat <- cor(datos_sup)

ggplot(
  data = reshape2::melt(corr_mat),
  aes(x = Var1, y = Var2, fill = value)
) +
  geom_tile() +
  geom_text(aes(label = round(value, 1)), color = "black", size = 3) +
  scale_fill_gradient2(low = "steelblue", high = "steelblue", mid = "white", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none") +
  labs(title = "Matriz de correlación", x = NULL, y = NULL)


ggpairs(datos_sup, 
        diag = list(continuous = wrap("densityDiag")), 
        lower = list(continuous = wrap("points", alpha = 0.9)))


# ANALISIS COMPONENTES PRINCIPALES GENERAL, SUPLEMENTANDO EL PIB_PC

acp <- PCA(datos_sup, scale.unit = TRUE, graph = FALSE, quanti.sup=c (4))

inercia_acp <- data.frame(acp[["eig"]])

### Calidad de representacion global ###

ggplot(inercia_acp
       , aes(x = reorder(row.names(inercia_acp), -inercia_acp$percentage.of.variance)
             , y = inercia_acp$percentage.of.variance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Componente Principal") +
  ylab("Proporción de Inercia") +
  ggtitle("Descomposición de la Inercia Total")

head(inercia_acp, 3)

#PROYECTAMOS A LOS PAISES SUPLEMENTARIOS

paises_proyectados <- predict(acp, newdata =  paises_sup)

# Base solo con las siglas de los paises y el año

coordenadas <- acp$ind$coord[, 1:3]
coordenadas_sup <- paises_proyectados$coord[, 1:3]
coord <- rbind(coordenadas, coordenadas_sup)

paises <- row.names(coord)

apodo_pais <- substr(paises, 1, 3)

apodo_pais[apodo_pais == "EL_"] <- "SAL"
apodo_pais[apodo_pais == "REP"] <- "DOM"

año <- substr(paises, nchar(paises)-1, nchar(paises))

sigla_pais <- paste0(apodo_pais, año)

row.names(coord) <- sigla_pais

# CALIDAD DE REPRESENTACION 

coord_var <- round(acp[["var"]][["coord"]][, 1:3], 2)
cos2_var <- round(acp[["var"]][["cos2"]][, 1:3], 2)
co2_var_tot <- rowSums(cos2_var) 

variables <- cbind(coord_var,cos2_var,co2_var_tot) 

colnames(variables)<- c('Cood1', 'Cood2', 'Cood3', 'Cos2_comp1', 'Cos2_comp2', 'Cos2_comp3', 'Cos2_Total') 

acp <- PCA(datos_sup, scale.unit = TRUE, graph = FALSE, quanti.sup=c (4),  ncp = 3)

dist_ind <- round(acp[["ind"]][["dist"]], 2)
coord_ind <- round(acp[["ind"]][["coord"]][, 1:3], 2)
co2_ind <- round(acp[["ind"]][["cos2"]][, 1:3], 2)
co2_ind_tot <- rowSums(co2_ind) 

ind_v <- cbind(dist_ind,coord_ind,co2_ind,co2_ind_tot)

dist_ind_sup <- round(paises_proyectados[["dist"]], 2)
coord_ind_sup <- round(paises_proyectados[["coord"]][,1:3], 2)
co2_ind_sup <-round(paises_proyectados[["cos2"]][,1:3], 2)
co2_ind_tot_sup <- rowSums(co2_ind_sup)

ind_proy <- cbind(dist_ind_sup,coord_ind_sup ,co2_ind_sup,co2_ind_tot_sup)

individuos <-rbind(ind_v, ind_proy)

colnames(individuos)<- c('Dist', 'Cood1', 'Cood2', 'Cood3', 'Cos2_comp1', 'Cos2_comp2', 'Cos2_comp3', 'Cos2_Total') 

##### Calidad representacion varaibles con 3 componentes #####

variables

##### Calidad representacion paises con 3 componentes #####

head(individuos)

##### Grafico Variables #####

plot.PCA(acp, axes=c(1, 2), choix="var")

plot.PCA(acp, axes=c(1, 3), choix="var")

# Grafico Individuos 

# componente 1 y 2

#ggplot(as.data.frame(coord), aes(x = Dim.1, y = Dim.2)) +
#  geom_text(aes(label = rownames(coord)), hjust = -0.2, vjust = 0.5, color = "black", size = 2) +
#  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
#  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
#  labs(x = "Componente 1", y = "Componente 2") +
#  theme_minimal()

# componente 1 y 3

#ggplot(as.data.frame(coord), aes(x = Dim.1, y = Dim.3)) +
#  geom_text(aes(label = rownames(coord)), hjust = -0.2, vjust = 0.5, color = "black", size = 2) +
#  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
#  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
#  labs(x = "Componente 1", y = "Componente 3") +
#  theme_minimal()

#### Clusters con los componentes ####

### coord es la tabla con los 3 primeros componentes principales por individuos ###

#head(coord)

### estandarizado de datos ###

coord_st<-scale(coord) 

### Calculo de la matriz de distancias euclideanas ###

distancia <- get_dist(coord_st, method = "euclidean")

### Generar diferentes metodos jerarquicos ###

cluster_ward <- hclust(distancia, method = "ward.D2")

cluster_VMC <- hclust(distancia, method = "single")

cluster_VML <- hclust(distancia, method = "complete")

### Generar un dendrogramas ###

plot(cluster_ward, cex = 0.5)

plot(cluster_VMC, cex = 0.5)

plot(cluster_VML, cex = 0.5)

### Indicadores de los diferentes metodos ###

source('C:\\Users\\Santiago\\OneDrive\\Escritorio\\Ejercicios Analisis Multivariado\\indicadores.R')       

Indicadores_Ward <- indicadores(cluster_ward$merge , coord_st, 10)

# Opción 1: WARD 4 clusters

Indicadores_VMC <- indicadores(cluster_VMC$merge , coord_st,10 )

# Opcion 2: VMC 5 clustes

Indicadores_VML <- indicadores(cluster_VML$merge , coord_st,10 )

# Opcion 3: VML 6 clusters


### Comparar diferentes opciones con la silueta ###

#silueta4<-silhouette(cutree(cluster_ward , 4) , distancia)
#silueta5<-silhouette(cutree(cluster_VMC , 5) , distancia)
#silueta6<-silhouette(cutree(cluster_VML , 6) , distancia)

#fviz_silhouette(silueta4)
#fviz_silhouette(silueta5)
#fviz_silhouette(silueta6)

Cluster_Paises <- data.frame(coord)

grupos <- data.frame(cutree(cluster_VML, k = 6))

colnames(grupos) <- c('Grupo') 

Cluster_Paises$Grupo <- grupos$Grupo

### Grafico de los paises diferenciados por cluster en el espacio de componntes ###

ggplot(as.data.frame(Cluster_Paises), aes(x = Dim.1, y = Dim.2, color = as.factor(Grupo))) +
  geom_text(aes(label = rownames(Cluster_Paises)), hjust = -0.2, vjust = 0.5, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = "Componente 1", y = "Componente 2") +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "black")) +
  theme_minimal()


ggplot(as.data.frame(Cluster_Paises), aes(x = Dim.1, y = Dim.3, color = as.factor(Grupo))) +
  geom_text(aes(label = rownames(Cluster_Paises)), hjust = -0.2, vjust = 0.5, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(x = "Componente 1", y = "Componente 3") +
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "black")) +
  theme_minimal()


### Analisis de descriminte ###

### Necesitamos reducir las emisiones a 20 mil Mtons a 2050. Si para 2050 seremos 9 mil millones -> 2 toneladas por habitante. ###

datos_sup$Sostenible <- ifelse(datos_sup$CO2pc > 2000, 0, 1)

base <-  subset(datos_sup, select = -CO2pc)

base <-  subset(base, select = -CO2_cons)

head(base)

#cor(base[,-13])

#ggpairs(base, 1:12, mapping = ggplot2::aes(color = ifelse(Sostenible == 1, "si", "no"), alpha = 0.5), 
#        diag = list(continuous = wrap("densityDiag")), 
#        lower = list(continuous = wrap("points", alpha = 0.9)))


### Utilizamos la funcion testes para realizar test de igualdad de medias, homogeneidad de varianzas y multinormalidad ###   

source("C:\\Users\\Santiago\\OneDrive\\Escritorio\\Ejercicios Analisis Multivariado\\testes.R")

testes(base[,-13], base$Sostenible)

## Se rechaza hipotesis nula de igualdad de medias. Pr -> 0 -> Medias distintas

## Se rechaza la hipotesis nula de homogenidad -> Las varianzas y covarinzas son diferentes. No se puede lineal.

## Se rechaza hipotesis nula de que Multinormalidad > No se puede usar cuadratico.

### Test de Normalidad para cada variable ###

Sostenibles<-filter(base,base$Sostenible == 1)
NoSostenibles<-filter(base,base$Sostenible == 0)

mvn(Sostenibles[,-13], mvnTest = "mardia")$univariateNormality
mvn(NoSostenibles[,-13], mvnTest = "mardia")$univariateNormality

### HAY QUE HACER EL DISCRIMINANTE LOGISTICO ###

## Utilizamos entrenamiento y testeo estratificado ##

set.seed(13)
muestra <- createDataPartition(base[, "Sostenible"], p = 0.75, list = FALSE)

entrenamiento <- base[muestra, ]
testeo  <- base[-muestra, ]

mean(entrenamiento$Sostenible)

mean(testeo$Sostenible)

## Logisitco ##

log1 <- glm(Sostenible ~ Prod_Ind + Prod_Ene + ConsENpc + PBIpc + O_PRIpc + O_SECpc + X.C_RES + X.C_TRAN + X.C_IND + X.C_SEC+ C_ELECpc + X.IMPORTA
            , family = binomial
            , data = entrenamiento
            )

## Vemos los niveles de significación de las variables ##

summary(log1)

## Vemos la significación conjunta ##

D1 <- log1$null.deviance-log1$deviance
pchisq(D1, df = 13, lower.tail = F)

## Probabilidad estimada por el modelo de cada uno de los paises de ser sostenible ##

predicciones_entrenamiento <- predict(log1, newdata = entrenamiento, type = "response")

predicciones_testeo <- predict(log1, newdata = testeo, type = "response")

predicciones <- predict(log1, newdata = base, type = "response")

## Clasificamos los pasises en base al umbral 0.5 ##

clasificacion_entrenamiento <- ifelse(predicciones_entrenamiento > 0.5, 1, 0)

clasificacion_testeo <- ifelse(predicciones_testeo > 0.5, 1, 0) 

clasificacion <- ifelse(predicciones > 0.5, 1, 0) 

## Matriz de confusion ##

confusion_entrenamiento <- table(entrenamiento$Sostenible, clasificacion_entrenamiento) 

confusion_entrenamiento

confusion_testeo <- table(testeo$Sostenible, clasificacion_testeo) 

confusion_testeo

confusion <- table(base$Sostenible, clasificacion) 

confusion

prop.table(confusion_entrenamiento, m=1)

prop.table(confusion_testeo, m=1)

prop.table(confusion, m=1)

## Curva ROC y punto de corte que maximiza sensitividad y especificidad ##

roclog <- roc(base$Sostenible ~ predicciones, plot = TRUE, print.auc = TRUE)

plot(roclog, print.thres = "best", print.thres.best.method = "closest.topleft", add = TRUE, col = "red")

## El punto que mejor proporciona la sensitividad y especificidad es el umbral 0.492 ##





