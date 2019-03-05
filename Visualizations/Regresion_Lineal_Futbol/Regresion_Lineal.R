#==========================================================
# Regresion Lineal en R #### 
#==========================================================


# Equipo: Equipos de las 5 grandes ligas de Europa
# GolesF: Goles a Favor
# GolesC: Goles en Contra
# DF: Diferencia de Goles
# Puntos: Puntos acumulados a final de temporada
# W: Win (Victorias)
# D: Draw (Empates)
# L: Lost (Derrotas)
# GolesMT_F: Goles 1ª Parte a Favor
# GolesMT_C: Goles 1ª Parte en Contra
# TirosF: Tiros a Favor
# TirosC: Tiros en Contra
# TirosPF: Tiros a Puerta a Favor
# TirosPC: Tiros a Puerta en Contra
# Fouls_Prov: Fouls Provocadas
# Fouls_Rec: Faltas Recibidas
# Corners_Fav: Corners a Favor
# Corners_Con: Corners en Contra
# TA_Prov: Tarjetas Amarillas Provocadas
# TA_Rec: Tarjetas Amarillas Recibidas
# TR_Prov: Tarjetas Rojas Provocadas
# TR_Rec: Tarjetas Rojas Recibidas





#Cargar paquetes
library(tidyverse)
library(GGally)
library(car)
library(gvlma)
#devtools::install_github("drsimonj/corrr")
library("corrr")
library(ggcorrplot)

library(RColorBrewer)


data <- read_csv("Data_Regresion.csv")

#GGpairs
ggpairs(data = data, columns = 2:ncol(data), title = "Todas las variables")

#Seleccionar columnas
columnas_seleccionadas <- data %>%
  select(Puntos, GolesF, GolesC, TirosF, TirosC, TirosPF, TirosPC)

#GGpairs
ggpairs(data = columnas_seleccionadas, 
        columns = 2:ncol(columnas_seleccionadas), 
        title = "Algunas variables")

# GGpairs
# ggpairs(data = columnas_seleccionadas, 
#        columns = 2:ncol(For_ggpairs), 
#        title = "Algunas variables")


#Utilizando corrr, quitar variables y prepararlo para una matriz
correlate(data[,-1], diagonal = 1) %>%
  focus(-W, -D, -L, mirror = TRUE) %>%
  remove_rownames() %>%
  column_to_rownames(var = "rowname") -> M

#Crear matriz y valores P
cor_matrix <- as.matrix(M)
p_mat <- cor_pmat(cor_matrix)

#ggcorrplot
ggcorrplot(cor_matrix, p.mat = p_mat, hc.order = TRUE,
           type = "lower", 
           lab = TRUE,
           lab_size = 4,
           ggtheme = ggplot2::theme_classic,
           colors = brewer.pal(n = 3, name = "RdYlBu"))



#Modelo ####

#Crear el modelo de regresion lineal
Modelo <- lm(Puntos ~ GolesF + GolesC + TirosF + TirosC + TirosPF + TirosPC
               + Fouls_Prov + Fouls_Rec + Corners_Fav + Corners_Con, data = data)

summary(Modelo)



#Residuales ####

#Residuals vs Fitted
plot(Modelo, which=1)

#Normality Q-Q plot
plot(Modelo, which = 2)

#Scale-Location
plot(Modelo, which = 3)

#Residual vs Leverage
plot(Modelo, which = 5)


#VIF
vif(Modelo)  #Multicolinearity


# Breush-Pagan test
ncvTest(Modelo)


#Durbin-Watson
durbinWatsonTest(Modelo)  #Autocorrelation residuals


#gvlma package
gvmodel <- gvlma(Modelo)
summary(gvmodel)



# Modelo 2


Modelo2 <- lm(Puntos ~ TirosF +TirosC+TirosPF+TirosPC+Fouls_Prov+Fouls_Rec +Corners_Fav +Corners_Con, data = data)

summary(Modelo2)

