# Emission-model-Agriculture_-HN2024
# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(tidyr)

# Leer los datos desde el archivo Excel
data <- read_excel("C:/Users/DEES-JULIO/Desktop/GIZ/Agricultura/data/data.xlsx",sheet = "data")

# Definir las variables necesarias
years <- data$Year
poblacion_animal <- data$Poblacion_Animal  # Población animal en el dataset
superficie_cultivada <- data$Superficie_Cultivada  # Superficie cultivada en hectáreas
superficie_pastizales <- data$Superficie_Pastizales  # Superficie de pastizales en hectáreas
N_total_aplicado <- data$N_total_aplicado_p  # N aplicado en estiércol
N_aplicado <- data$N_aplicado_p  # N aplicado en suelos gestionados




#############################################################

factor <- read_excel("C:/Users/DEES-JULIO/Desktop/GIZ/Agricultura/data/data.xlsx",sheet = "Factor_calculado")
# Definir los factores de emisión y GWP (Global Warming Potential)
factor_emision_CH4_fermentacion <- 56  # Ejemplo de factor de emisión para CH₄ (Fermentación entérica, kg CH₄/cabeza/año)
factor_emision_CH4_estiércol <- 0.01  # Ejemplo de factor de emisión CH₄ para estiércol (kg CH₄/cabeza/año)
factor_emision_N2O_estiércol <- 0.01  # Factor de emisión N₂O estiércol (kg N₂O-N/kg N aplicado)
factor_emision_N2O_suelos <- 0.01  # Factor de emisión N₂O suelos gestionados (kg N₂O-N/kg N aplicado)
factor_emision_CO2_cultivo <- 1.0  # Factor de emisión CO₂ para tierras de cultivo (t CO₂/ha)
factor_emision_CO2_pastizales <- 7  # Factor de emisión CO₂ para pastizales (t CO₂/ha)
factor_emision_GHG_biomasa <- 0.5  # Factor de emisión por quemado de biomasa (valor supuesto)
GWP_CH4 <- 25  # GWP para CH₄
GWP_N2O <- 298  # GWP para N₂O

# Definir tasa de cambio de uso del suelo (por ejemplo, 4% anual)
tasa_cambio_suelo <- 0.03

# Ajustar la superficie de cultivo y pastizales por la tasa de cambio de uso del suelo
for (i in 2:length(years)) {
  cambio_suelo <- superficie_cultivada[i - 1] * tasa_cambio_suelo
  superficie_cultivada[i] <- superficie_cultivada[i - 1] - cambio_suelo
  superficie_pastizales[i] <- superficie_pastizales[i - 1] + cambio_suelo
}

# Calcular emisiones por categoría

# Fermentación entérica (3A1)
emisiones_CH4_fermentacion <- poblacion_animal * factor_emision_CH4_fermentacion / 1000  # kg CH₄/año convertidos a ton
emisiones_CO2e_fermentacion <- emisiones_CH4_fermentacion * GWP_CH4 / 1000  # Convertir a toneladas CO₂e

# Gestión de estiércol (3A2)
emisiones_CH4_estiércol <- poblacion_animal * factor_emision_CH4_estiércol  # kg CH₄/año
emisiones_N2O_estiércol <- N_total_aplicado * factor_emision_N2O_estiércol  # kg N₂O-N/año
emisiones_CO2e_CH4_estiércol <- emisiones_CH4_estiércol * GWP_CH4 / 1000  # Convertir CH₄ a CO₂e (toneladas)
emisiones_CO2e_N2O_estiércol <- emisiones_N2O_estiércol * GWP_N2O / 1000  # Convertir N₂O a CO₂e (toneladas)

# Pastizales (3B3)
emisiones_CO2_pastizales <- superficie_pastizales * factor_emision_CO2_pastizales  # t CO₂/año

# Quemado de biomasa (3C1)
emisiones_GHG_biomasa <- superficie_cultivada * factor_emision_GHG_biomasa/1000  # t CO₂/año

# Emisiones directas de N₂O de suelos gestionados (3C4)
emisiones_N2O_suelos <- N_aplicado * factor_emision_N2O_suelos # kg N₂O-N/año
emisiones_CO2e_N2O_suelos <- emisiones_N2O_suelos * GWP_N2O / 1000  # Convertir N₂O a CO₂e (toneladas)

# Crear un dataframe con las emisiones por categoría
data_local <- data.frame(
  Year = years,
  `3A1 - Fermentación entérica` = emisiones_CO2e_fermentacion,
  `3A2 - Gestión de estiércol` = emisiones_CO2e_CH4_estiércol/1e2 + emisiones_CO2e_N2O_estiércol/1e2,
  `3B3 - Pastizales` = emisiones_CO2_pastizales/1e2,
  `3C1 - Emisiones de GHG por quemado de biomasa` = emisiones_GHG_biomasa/1e2,
  `3C4 - Emisiones directas de N2O de suelos gestionados` = emisiones_CO2e_N2O_suelos/1e2
)

# Calcular las emisiones totales por año
data_local <- data_local %>%
  mutate(Emisiones_Totales_Gt = rowSums(across(starts_with("3"))))

# Filtrar datos para años específicos
años_especificos <- c(2000,2005,2010,2015,2020)
data_filtrada <- data_local %>%
  filter(Year %in% años_especificos)

# Transformar a formato largo para crear gráfico apilado
data_long_filtrada <- data_filtrada %>%
  pivot_longer(cols = c("X3A1...Fermentación.entérica", "X3A2...Gestión.de.estiércol", 
                        "X3B3...Pastizales", "X3C1...Emisiones.de.GHG.por.quemado.de.biomasa", 
                        "X3C4...Emisiones.directas.de.N2O.de.suelos.gestionados"), 
               names_to = "Categoría", values_to = "Emisiones")

# Visualización
ggplot(data_long_filtrada, aes(x = factor(Year), y = Emisiones, fill = Categoría)) +
  geom_bar(stat = "identity", width = 0.6) +  # Ajuste del ancho de las barras
  geom_text(aes(label = round(Emisiones, 2)), position = position_stack(vjust = 0.5), size = 3) +  # Etiquetas en las barras
  labs(title = "",
       x = "Año", y = "Emisiones Totales (Gg CO2e)", fill = "Categoría") +
  scale_fill_manual(values = c("X3A1...Fermentación.entérica" = "orange", 
                               "X3A2...Gestión.de.estiércol" = "grey", 
                               "X3B3...Pastizales" = "blue", 
                               "X3C1...Emisiones.de.GHG.por.quemado.de.biomasa" = "yellow",
                               "X3C4...Emisiones.directas.de.N2O.de.suelos.gestionados" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 360, hjust = 0.5),  # Rotar los textos del eje x
        legend.position = "bottom")

# Guardar los resultados en Excel


#Ajustar regresión lineal para obtener tasa de crecimiento de cada variable

# Población animal (Fermentación entérica)
modelo_poblacion_animal <- lm(poblacion_animal ~ years)
tasa_crecimiento_poblacion_animal <- coef(modelo_poblacion_animal)["years"] / mean(poblacion_animal)

# N total aplicado (Gestión de estiércol)
modelo_N_total_aplicado <- lm(N_total_aplicado ~ years)
tasa_crecimiento_N_total_aplicado <- coef(modelo_N_total_aplicado)["years"] / mean(N_total_aplicado)

# Superficie de pastizales (Pastizales)
modelo_superficie_pastizales <- lm(superficie_pastizales ~ years)
tasa_crecimiento_superficie_pastizales <- coef(modelo_superficie_pastizales)["years"] / mean(superficie_pastizales)

# Superficie cultivada (Quemado de biomasa)
modelo_superficie_cultivada <- lm(superficie_cultivada ~ years)
tasa_crecimiento_superficie_cultivada <- coef(modelo_superficie_cultivada)["years"] / mean(superficie_cultivada)

# N aplicado en suelos gestionados (Emisiones de N₂O suelos gestionados)
modelo_N_aplicado <- lm(N_aplicado ~ years)
tasa_crecimiento_N_aplicado <- coef(modelo_N_aplicado)["years"] / mean(N_aplicado)

# Mostrar las tasas de crecimiento estimadas
tasa_crecimiento_poblacion_animal
tasa_crecimiento_N_total_aplicado
tasa_crecimiento_superficie_pastizales
tasa_crecimiento_superficie_cultivada
tasa_crecimiento_N_aplicado


# Proyección de las variables explicativas hasta 2050
future_years <- 2000:2050

# Población animal proyectada
poblacion_animal_futuro <- predict(modelo_poblacion_animal, newdata = data.frame(years = future_years))

# N total aplicado proyectado
N_total_aplicado_futuro <- predict(modelo_N_total_aplicado, newdata = data.frame(years = future_years))

# Superficie de pastizales proyectada
superficie_pastizales_futuro <- predict(modelo_superficie_pastizales, newdata = data.frame(years = future_years))

# Superficie cultivada proyectada
superficie_cultivada_futuro <- predict(modelo_superficie_cultivada, newdata = data.frame(years = future_years))

# N aplicado proyectado para suelos gestionados
N_aplicado_futuro <- predict(modelo_N_aplicado, newdata = data.frame(years = future_years)

                             
# Extend emisiones_CO2e_N2O_suelos_futuro to match future_years
modelo_N2O_suelos <- lm(emisiones_CO2e_N2O_suelos ~ years)
emisiones_CO2e_N2O_suelos_futuro <- predict(modelo_N2O_suelos, newdata = data.frame(years = future_years))

# Extend N_aplicado_futuro to match future_years
modelo_N_aplicado <- lm(N_aplicado ~ years)
N_aplicado_futuro <- predict(modelo_N_aplicado, newdata = data.frame(years = future_years))

# Now check the lengths again
length(emisiones_CO2e_N2O_suelos_futuro) # Should now be 51
length(N_aplicado_futuro) # Should now be 51


# Cálculo de emisiones futuras utilizando las variables proyectadas
# Fermentación entérica (3A1) - Emisiones futuras
emisiones_CH4_fermentacion_futuro <- poblacion_animal_futuro * factor_emision_CH4_fermentacion / 1000
 emisiones_CO2e_fermentacion_futuro <- emisiones_CH4_fermentacion_futuro * GWP_CH4 / 1000
                             
# Gestión de estiércol (3A2) - Emisiones futuras
 emisiones_CH4_estiércol_futuro <- poblacion_animal_futuro * factor_emision_CH4_estiércol
 emisiones_N2O_estiércol_futuro <- N_total_aplicado_futuro * factor_emision_N2O_estiércol
 emisiones_CO2e_CH4_estiércol_futuro <- emisiones_CH4_estiércol_futuro * GWP_CH4 / 1000
 emisiones_CO2e_N2O_estiércol_futuro <- emisiones_N2O_estiércol_futuro * GWP_N2O / 1000
                             
# Pastizales (3B3) - Emisiones futuras
emisiones_CO2_pastizales_futuro <- superficie_pastizales_futuro * factor_emision_CO2_pastizales
                             
# Quemado de biomasa (3C1) - Emisiones futuras
 emisiones_GHG_biomasa_futuro <- superficie_cultivada_futuro * factor_emision_GHG_biomasa / 1000
                             
# Emisiones directas de N₂O de suelos gestionados (3C4) - Emisiones futuras
emisiones_N2O_suelos_futuro <- N_aplicado_futuro * factor_emision_N2O_suelos
emisiones_CO2e_N2O_suelos_futuro <- emisiones_N2O_suelos_futuro * GWP_N2O / 1000
                             
# Verificar las longitudes de las proyecciones
length(future_years)
length(emisiones_CO2e_fermentacion_futuro)
length(emisiones_CO2e_CH4_estiércol_futuro)
length(emisiones_CO2e_N2O_estiércol_futuro)
length(emisiones_CO2_pastizales_futuro)
length(emisiones_GHG_biomasa_futuro)
length(emisiones_CO2e_N2O_suelos_futuro)
length(poblacion_animal_futuro)
length(N_total_aplicado_futuro)
length(superficie_cultivada_futuro)
length(N_aplicado_futuro)

# Asegurarse de que todas las proyecciones tengan la misma longitud que 'future_years'
if (length(emisiones_CO2e_fermentacion_futuro) != length(future_years)) {
  emisiones_CO2e_fermentacion_futuro <- rep(emisiones_CO2e_fermentacion_futuro, length.out = length(future_years))
}
if (length(emisiones_CO2e_CH4_estiércol_futuro) != length(future_years)) {
  emisiones_CO2e_CH4_estiércol_futuro <- rep(emisiones_CO2e_CH4_estiércol_futuro, length.out = length(future_years))
}
if (length(emisiones_CO2_pastizales_futuro) != length(future_years)) {
  emisiones_CO2_pastizales_futuro <- rep(emisiones_CO2_pastizales_futuro, length.out = length(future_years))
}
if (length(emisiones_GHG_biomasa_futuro) != length(future_years)) {
  emisiones_GHG_biomasa_futuro <- rep(emisiones_GHG_biomasa_futuro, length.out = length(future_years))
}
if (length(emisiones_CO2e_N2O_suelos_futuro) != length(future_years)) {
  emisiones_CO2e_N2O_suelos_futuro <- rep(emisiones_CO2e_N2O_suelos_futuro, length.out = length(future_years))
}

# Ahora crear el dataframe asegurando que todas las columnas tengan la misma longitud
proyeccion_BAU_futuro <- data.frame(
  Year = future_years,
  `3A1 - Fermentación entérica` = emisiones_CO2e_fermentacion_futuro,
  `3A2 - Gestión de estiércol` = emisiones_CO2e_CH4_estiércol_futuro/1e2 + emisiones_CO2e_N2O_estiércol_futuro/1e2,
  `3B3 - Pastizales` = emisiones_CO2_pastizales_futuro/1e2,
  `3C1 - Emisiones de GHG por quemado de biomasa` = emisiones_GHG_biomasa_futuro/1e2,
  `3C4 - Emisiones directas de N₂O de suelos gestionados` = emisiones_CO2e_N2O_suelos_futuro/1e2,
  poblacion_animal_futuro=poblacion_animal_futuro,
  N_total_aplicado_futuro=N_total_aplicado_futuro,
  superficie_cultivada_futuro=superficie_cultivada_futuro,
  N_aplicado_futuro=N_aplicado_futuro
  
)
                             
# Calcular las emisiones totales proyectadas
proyeccion_BAU_futuro <- proyeccion_BAU_futuro %>%
mutate(Emisiones_Totales_Gt = rowSums(across(starts_with("3"))))


# Filtrar para años específicos
años_especificos_futuro <- c(2018, 2025, 2030, 2040, 2050)
data_filtrada_futuro <- proyeccion_BAU_futuro %>%
  filter(Year %in% años_especificos_futuro)

# Transformar los datos a formato largo para visualización
data_long_filtrada_futuro <- data_filtrada_futuro %>%
  pivot_longer(cols = c("X3A1...Fermentación.entérica", "X3A2...Gestión.de.estiércol", 
                        "X3B3...Pastizales", "X3C1...Emisiones.de.GHG.por.quemado.de.biomasa", 
                        "X3C4...Emisiones.directas.de.N.O.de.suelos.gestionados"), 
               names_to = "Categoría", values_to = "Emisiones")

# Visualización del escenario futuro
ggplot(data_long_filtrada_futuro, aes(x = factor(Year), y = Emisiones, fill = Categoría)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(Emisiones, 2)), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "",
       x = "Año", y = "Emisiones Totales (Gg CO2e)", fill = "Categoría") +
  scale_fill_manual(values = c("X3A1...Fermentación.entérica" = "orange", 
                               "X3A2...Gestión.de.estiércol" = "grey", 
                               "X3B3...Pastizales" = "blue", 
                               "X3C1...Emisiones.de.GHG.por.quemado.de.biomasa" = "yellow",
                               "X3C4...Emisiones.directas.de.N.O.de.suelos.gestionados" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 360, hjust = 0.5),
        legend.position = "bottom")



write.xlsx(proyeccion_BAU_futuro, "C:/Users/DEES-JULIO/Desktop/GIZ/Agricultura/Emisiones_BAU_Agricultura.xlsx", row.names = FALSE)






##### Descripción de variables

descriptivos <- read_excel("C:/Users/DEES-JULIO/Desktop/GIZ/descriptivos.xlsx", sheet = "Hoja16")

# Convertir Year a numérico
descriptivos$Year <- as.numeric(descriptivos$Year)

# Asegurarse de que "Categorías de emisiones" es un factor
descriptivos$Categorías <- as.factor(descriptivos$Categorías)



# Crear el gráfico de barras apiladas con etiquetas en cada barra y leyenda debajo
ggplot(descriptivos, aes(x = Year, y = emisiones_totales, fill = Categorías)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(emisiones_totales, 1)), 
            position = position_stack(vjust = 0.5), size = 3) +  # Etiquetas centradas en las barras
  labs(title = "Emisiones agricultura y ganadería",
       x = "Año",
       y = "Emisiones Totales (Gg CO2e)",
       fill = "Sector") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Coloca la leyenda debajo del gráfico





