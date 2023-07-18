# LIBERIRAS

library(summarytools)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(FactoInvestigate)
library(plotly)

# BASE DE DATOS -> ENERGIA

datos <- read.table("C:\\Users\\Santiago\\OneDrive\\Escritorio\\Proyecto Analisis Multi\\Energia1.txt", header = TRUE, sep = "\t")

row.names(datos) <- datos$PAISES

datos$PAISES <- NULL

# VARIABLES -> CLASE Y RESUMEN

str(datos)

descr(datos)

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

# EN LOS GRAFICOS SE PUEDE OBSERVAR QUE HAY PAISES CON MUY ALTA PRODCUCCION DE ENERGIA PRIMARIA PER CAPITA RESPECTO AL RESTO
# ESTOS PAISES SE PUEDE CONSIDERAR COMO OUTLIERS Y EXLUIRLOS, PARA LUEGO PROYECTORALOS UNA VEZ ARMADOS LOS COMPONENTES}


# PAISES CON GRAN OFERTA DE ENERGIA PRIMARIA PER CAPITA

paises_sup = datos[datos$O_PRIpc > 20000, ]

datos_sup <- datos[datos$O_PRIpc <= 20000, ]

#VENEZUELA Y TRINIDAD

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

# MATRIZ DE CORRELACION

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

# ANALISIS COMPONENTES PRINCIPALES GENERAL

res = PCA(datos_sup, scale.unit=TRUE, graph=FALSE, ncp = 14)

# Inercia de cada componente 

inercia <- data.frame(res[["eig"]])

ggplot(inercia
       , aes(x = reorder(row.names(inercia), -inercia$percentage.of.variance)
             , y = inercia$percentage.of.variance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Componente Principal") +
  ylab("Proporción de Inercia") +
  ggtitle("Descomposición de la Inercia Total")

# Grafico Variables

plot.PCA(res, axes=c(1, 2), choix="var")

# Grafico Individuos

plot.PCA(res, axes=c(1, 2), choix="ind")

# Grafico Dinamico Individuos


plot_ly(x = res$ind$coord[, 1], y = res$ind$coord[, 2], text = rownames(res$ind$coord), type = "scatter", mode = "none") %>%
  add_text(x = res$ind$coord[, 1], y = res$ind$coord[, 2], text = rownames(res$ind$coord),
           textfont = list(size = 8),
           hoverinfo = "text",
           showlegend = FALSE) %>%
  layout(title = "Gráfico Dinámico ACP Individuos",
         xaxis = list(title = paste("PC1 (", round(res$eig[1, 2], 2), "%)", sep = "")),
         yaxis = list(title = paste("PC2 (", round(res$eig[2, 2], 2), "%)", sep = "")))


# ANALISIS COMPONENTES PRINCIPALES CON VARIABLES SUPLEMENTARIAS PIB PER CAPITA, CONSUMO TOTAL PER CAPITA Y %ENERGIA TRASPORTE

acp <- PCA(datos_sup, scale.unit = TRUE, graph = FALSE, quanti.sup=c (4))

inercia_acp <- data.frame(acp[["eig"]])

ggplot(inercia_acp
       , aes(x = reorder(row.names(inercia_acp), -inercia_acp$percentage.of.variance)
             , y = inercia_acp$percentage.of.variance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  xlab("Componente Principal") +
  ylab("Proporción de Inercia") +
  ggtitle("Descomposición de la Inercia Total")

# Grafico Variables

plot.PCA(acp, axes=c(1, 2), choix="var")

plot.PCA(acp, axes=c(1, 3), choix="var")

#PROYECTAMOS A VENEZUELA Y TRINIDAD

paises_proyectados <- predict(acp, newdata =  paises_sup)

# Grafico Dinamico Individuos

grafico_ind_1 <- plot_ly(x = acp$ind$coord[, 1], y = acp$ind$coord[, 2], text = rownames(acp$ind$coord), type = "scatter", mode = "none") %>%
                add_text(x = acp$ind$coord[, 1], y = acp$ind$coord[, 2], text = rownames(acp$ind$coord),
                  textfont = list(size = 8),
                  hoverinfo = "text",
                  showlegend = FALSE) %>%
                layout(title = "Gráfico Dinámico ACP Individuos",
                  xaxis = list(title = paste("PC1 (", round(res$eig[1, 2], 2), "%)", sep = "")),
                  yaxis = list(title = paste("PC2 (", round(res$eig[2, 2], 2), "%)", sep = "")))

grafico_combinado_1 <- grafico_ind_1 %>%
  add_trace(x = paises_proyectados$coord[, 1], 
            y = paises_proyectados$coord[, 2], 
            text = rownames(paises_proyectados$coord),
            type = "scatter",
            mode = "none",
            textfont = list(color = "red", size = 8),
            hoverinfo = "text",
            showlegend = FALSE)

grafico_combinado_1

                 
grafico_ind_2 <- plot_ly(x = acp$ind$coord[, 3], y = acp$ind$coord[, 4], text = rownames(acp$ind$coord), type = "scatter", mode = "none") %>%
  add_text(x = acp$ind$coord[, 3], y = acp$ind$coord[, 4], text = rownames(acp$ind$coord),
           textfont = list(size = 8),
           hoverinfo = "text",
           showlegend = FALSE) %>%
  layout(title = "Gráfico Dinámico ACP Individuos",
         xaxis = list(title = paste("PC1 (", round(res$eig[1, 2], 2), "%)", sep = "")),
         yaxis = list(title = paste("PC2 (", round(res$eig[2, 2], 2), "%)", sep = "")))

grafico_combinado_2 <- grafico_ind_2 %>%
  add_trace(x = paises_proyectados$coord[, 3], 
            y = paises_proyectados$coord[, 4], 
            text = rownames(paises_proyectados$coord),
            type = "scatter",
            mode = "none",
            textfont = list(color = "red", size = 8),
            hoverinfo = "text",
            showlegend = FALSE)

grafico_combinado_2


dimdesc(acp, axes=c(1,2))

# CALIDAD DE REPRESENTACION 

# VARIABLES

round(acp[["var"]][["contrib"]][, 1:4], 2)
round(acp[["var"]][["coord"]][, 1:4], 2)
round(acp[["var"]][["cos2"]][, 1:4], 2)
rowSums(acp[["var"]][["cos2"]][, 1:4])
rowSums(round(acp[["var"]][["contrib"]][, 1:4], 2))

# INDIVIDUOS

acp[["ind"]][["contrib"]][, 1:4]
round(acp[["ind"]][["coord"]][, 1:4], 2)
round(acp[["ind"]][["cos2"]][, 1:4], 2)
rowSums(round(acp[["ind"]][["cos2"]][, 1:4], 2))
rowSums(round(acp[["ind"]][["contrib"]][, 1:4], 2))



#Investigate(acp, language = "en")





