library(readxl)
library(dplyr)
library(bvarsv)

#Leemos datos y convertimos a serie temporal
datos <- read_excel("/Datos/Tabla_Consolidada.xlsx")

#Tipo_Interes tiene NA. Agrupamos por año y calculamos media
media_grupo <- tapply(datos$Tipo_Interes, rep(1:(nrow(datos)/4), each = 4), function(x) mean(x, na.rm = TRUE))

# Rellenar los NA con la media del grupo correspondiente
for (i in 1:length(media_grupo)) {
  inicio <- (i - 1) * 4 + 1
  fin <- inicio + 3
  datos$Tipo_Interes[inicio:fin] <- ifelse(is.na(datos$Tipo_Interes[inicio:fin]), media_grupo[i], datos$Tipo_Interes[inicio:fin])
}


datos_st <- ts(data = datos[, -1], start = c(1988, 1), frequency = 4) 

str(datos_st) #1988-2024 (2023T4). 144 filas
plot(datos_st)

par(mfrow=c(2,2))
######################
#Model Estimation
#####################

set.seed(1)

#number of Gibbs sampler
nburn.vignette <-5000
nrep.vignetee <-50000

###############
#1988-2023 #144
#############
fit <- bvar.sv.tvp(datos_st,p=2, nburn = nburn.vignette, nrep=nrep.vignetee) #t=102 -> 10.5 años-> 1998-2024
fit$Beta.postmean
t=dim(fit$Beta.postmean)[3]

beta_postmean=fit$Beta.postmean
# Extraer la intersección c de 3x1 (primera columna)
c_1 <- beta_postmean[, 1, ,drop=FALSE]


# Extraer B1 de la columna 2 a la 4
B1_1 <- beta_postmean[, 2:4, ,drop=FALSE]

# Extraer B2 de la columna 5 a la 7
B2_1 <- beta_postmean[, 5:7, ,drop=FALSE ]


#Modelo
identidad <- array(diag(3), dim = c(3, 3, t))
resta <- array(0, dim = c(3, 3, t))
modelo_1 <- array(0, dim = c(3, 1, t))
y_1 <- numeric(t)

for (i in 1:t) {
  resta[, , i] <- solve(identidad[, , i] - B1_1[, , i] - B2_1[, , i]) 
  modelo_1[, , i] <- resta[, , i] %*% c_1[, , i]
  y_1[i] <- modelo_1[,1,i]
}
y_1
x <- seq(1998.5, 2023.75, 0.25)
plot(x, y_1, type = "l", xlab = "Tiempo", ylab = "Valor de Ay_1", main = "1998.5-2023")

###############
#1988-2023 tau=20 
#############

fit_t20 <- bvar.sv.tvp(datos_st,p=2, tau=20, nburn = nburn.vignette, nrep=nrep.vignetee) #122->5.5 años
beta_postmean2=fit_t20$Beta.postmean
t=dim(fit_t20$Beta.postmean)[3]
# Extraer la intersección c de 3x1 (primera columna)
c_2 <- beta_postmean2[, 1, ,drop=FALSE]


# Extraer B1 de la columna 2 a la 4
B1_2 <- beta_postmean2[, 2:4, ,drop=FALSE]

# Extraer B2 de la columna 5 a la 7
B2_2<- beta_postmean2[, 5:7, ,drop=FALSE ]


#Modelo
identidad <- array(diag(3), dim = c(3, 3, t))
resta2 <- array(0, dim = c(3, 3, t))
modelo_2 <- array(0, dim = c(3, 1, t))
y_2 <-numeric(t)

for (i in 1:t) {
  resta2[, , i] <- solve(identidad[, , i] - B1_2[, , i] - B2_2[, , i]) 
  modelo_2[, , i] <- resta2[, , i] %*% c_2[, , i]
  y_2[i] <- modelo_2[,1,i]
}
y_2
x2<-seq(1993.5,2023.75,0.25)
plot(x2, y_2, type = "l", xlab = "Tiempo", ylab = "Valor de Ay_2", main = "1993.5-2023")

###############
#BVAR 1998-2019
##############
# Filtrar datos desde 1988t1
datos_st_2  <- window(datos_st, start = c(1998, 1), end = c(2019, 4))
str(datos_st_2) #1998-2020 (2019T4). 88 filas
plot(datos_st_2)

fit_3 <- bvar.sv.tvp(datos_st_2,p=2, nburn = nburn.vignette, nrep=nrep.vignetee) #46->10.5 años
beta_postmean3=fit_3$Beta.postmean
t=dim(fit_3$Beta.postmean)[3]
# Extraer la intersección c de 3x1 (primera columna)
c_3 <- beta_postmean3[, 1, ,drop=FALSE]


# Extraer B1 de la columna 2 a la 4
B1_3 <- beta_postmean3[, 2:4, ,drop=FALSE]

# Extraer B2 de la columna 5 a la 7
B2_3 <- beta_postmean3[, 5:7, ,drop=FALSE ]


#Modelo
identidad <- array(diag(3), dim = c(3, 3, t))
resta3 <- array(0, dim = c(3, 3, t))
modelo_3 <- array(0, dim = c(3, 1, t))
y_3 <- numeric(t)

for (i in 1:t) {
  resta3[, , i] <- solve(identidad[, , i] - B1_3[, , i] - B2_3[, , i]) 
  modelo_3[, , i] <- resta3[, , i] %*% c_3[, , i]
  y_3[i] <- modelo_3[,1,i]
}
y_3
x3<- seq(2008.5, 2019.75, 0.25)
plot(x3, y_3, type = "l", xlab = "Tiempo", ylab = "Valor de Ay_3", main = "2008.5-2019")

#####################
#BVAR 1998-2019 tau=20
#####################

fit_3_t20 <- bvar.sv.tvp(datos_st_2,p=2, tau=20, nburn = nburn.vignette, nrep=nrep.vignetee) #66 -> 5.5 años
beta_postmean4=fit_3_t20$Beta.postmean
t=dim(fit_3_t20$Beta.postmean)[3]

# Extraer la intersección c de 3x1 (primera columna)
c_4 <- beta_postmean4[, 1, ,drop=FALSE]


# Extraer B1 de la columna 2 a la 4
B1_4 <- beta_postmean4[, 2:4, ,drop=FALSE]

# Extraer B2 de la columna 5 a la 7
B2_4 <- beta_postmean4[, 5:7, ,drop=FALSE ]


#Modelo
identidad <- array(diag(3), dim = c(3, 3, t))
resta4 <- array(0, dim = c(3, 3, t))
modelo_4 <- array(0, dim = c(3, 1, t))
y_4 <- numeric(t)

for (i in 1:t) {
  resta4[, , i] <- solve(identidad[, , i] - B1_4[, , i] - B2_4[, , i]) 
  modelo_4[, , i] <- resta4[, , i] %*% c_4[, , i]
  y_4[i] <- modelo_4[,1,i]
}
y_4
x4<- seq(2003.5,2019.75,0.25)

plot(x4, y_4, type = "l", xlab = "Tiempo", ylab = "Valor de Ay_4", main = "2003.5-2019")


#Guardamos todo en excel

wb <- createWorkbook()


addWorksheet(wb, "y_1")
writeData(wb, "y_1", y_1)

addWorksheet(wb, "y_2")
writeData(wb, "y_2", y_2)

addWorksheet(wb, "y_3")
writeData(wb, "y_3", y_3)

addWorksheet(wb, "y_4")
writeData(wb, "y_4", y_4)

# Guardar el archivo Excel
saveWorkbook(wb, "/Users/mariamolinadiaz/Downloads/modelos/resultados_y.xlsx", overwrite = TRUE)
