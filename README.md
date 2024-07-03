# Emission-model-Agriculture_-HN2024
# Instalar y cargar los paquetes necesarios
library(lpSolve)
library(readxl)
library(ggplot2)
# Instalar y cargar los paquetes necesarios

library(ggplot2)

# Definir el año inicial y final
anio_inicio <- 2000
anio_fin <- 2050
anios <- anio_inicio:anio_fin

# Crear un data frame para almacenar las trayectorias de emisión y absorción
trayectorias <- data.frame(
  Año = anios
)

# Extender las variables hasta 2050
extender_variable <- function(variable, crecimiento_anual, anios) {
  extendida <- rep(NA, length(anios))
  extendida[1:length(variable)] <- variable
  for (i in (length(variable) + 1):length(anios)) {
    extendida[i] <- extendida[i - 1] * (1 + crecimiento_anual)
  }
  return(extendida)
}
# Definir tasas de crecimiento anual para las variables (supuestos)
crecimiento_stock <- 0.01
crecimiento_export <- 0.02
crecimiento_produccion <- 0.015
crecimiento_cpi <- 0.02
crecimiento_area <- 0.005
crecimiento_yield <- 0.01
crecimiento_stocks <- 0.01
crecimiento_gpv <- 0.03

# Extender cada variable
trayectorias$Stock_Variation <- extender_variable(datos$Stock_Variation, crecimiento_stock, anios)
trayectorias$Export_Quantity <- extender_variable(datos$Export_Quantity, crecimiento_export, anios)
trayectorias$Production <- extender_variable(datos$Production, crecimiento_produccion, anios)
trayectorias$Consumer_Price_Indices <- extender_variable(datos$Consumer_Price_Indices, crecimiento_cpi, anios)
trayectorias$Area_Harvested <- extender_variable(datos$Area_Harvested, crecimiento_area, anios)
trayectorias$Yield <- extender_variable(datos$Yield, crecimiento_yield, anios)
trayectorias$Stocks <- extender_variable(datos$Stocks, crecimiento_stocks, anios)
trayectorias$Gross_Production_Value <- extender_variable(datos$Gross_Production_Value, crecimiento_gpv, anios)

# Calcular la tasa de crecimiento anual promedio de las emisiones históricas
emisiones_historicas <- datos$Emisiones_Totales_Tierra
tasa_crecimiento_emisiones <- (emisiones_historicas[length(emisiones_historicas)] / emisiones_historicas[1])^(1 / (length(emisiones_historicas) - 1)) - 1

# Crear columnas para los escenarios BAU, Moderado y Ambicioso
trayectorias$Emisiones_BAU <- rep(emisiones_historicas[1], length(anios))
trayectorias$Emisiones_Moderado <- rep(NA, length(anios))
trayectorias$Emisiones_Ambicioso <- rep(NA, length(anios))

# Llenar el primer año con las emisiones iniciales
trayectorias$Emisiones_Moderado[1] <- emisiones_historicas[1]
trayectorias$Emisiones_Ambicioso[1] <- emisiones_historicas[1]

# Calcular las trayectorias de emisiones para cada escenario
for (i in 2:length(anios)) {
  # Emisiones en el escenario BAU crecen con la tasa histórica calculada
  trayectorias$Emisiones_BAU[i] <- trayectorias$Emisiones_BAU[i - 1] * (1 + tasa_crecimiento_emisiones)
  
  # Moderado: cero emisiones en 2050
  trayectorias$Emisiones_Moderado[i] <- trayectorias$Emisiones_Moderado[i - 1] * (1 - 1/(2050 - 2020))
  
  # Ambicioso: cero emisiones en 2030
  trayectorias$Emisiones_Ambicioso[i] <- trayectorias$Emisiones_Ambicioso[i - 1] * (1 - 1/(2030 - 2020))
}

# Visualizar los resultados
# Visualizar los resultados
ggplot(trayectorias, aes(x = Año)) +
  geom_line(aes(y = Emisiones_BAU, color = "Emisiones Totales (BAU)"), size = 1) +
  geom_line(aes(y = Emisiones_Moderado, color = "Emisiones Moderado"), size = 1, linetype = "dotdash") +
  geom_line(aes(y = Emisiones_Ambicioso, color = "Emisiones Ambicioso"), size = 1, linetype = "dotted") +
  labs(title = "Trayectorias de Emisión y Absorción en el Sector Agrícola",
       x = "Año",
       y = "Emisiones y Absorciones Totales de CO2 (toneladas)",
       color = "Escenario") +
  theme_minimal() +
  scale_color_manual(values = c("Emisiones Totales (BAU)" = "blue", 
                                "Emisiones Moderado" = "orange",
                                "Emisiones Ambicioso" = "red"))



#################### Ahora vamos a incluir factores exogenos como politicas y tecnologías ##############################################


# Definir el año inicial y final con mayor resolución entre 2024 y 2050
anio_inicio <- 2000
anio_fin <- 2050
anios <- c(anio_inicio:2023, seq(2024, anio_fin, by = 0.25))

# Crear un data frame para almacenar las trayectorias de emisión y absorción
trayectorias <- data.frame(Año = anios)

# Extender las variables hasta 2050
extender_variable <- function(variable, crecimiento_anual, anios) {
  extendida <- rep(NA, length(anios))
  extendida[1:length(variable)] <- variable
  for (i in (length(variable) + 1):length(anios)) {
    extendida[i] <- extendida[i - 1] * (1 + crecimiento_anual)
  }
  return(extendida)
}

# Definir tasas de crecimiento anual para las variables (supuestos) # Estos se pueden calcular con un crecimiento promedio de la serie.
crecimiento_stock <- 0.01
crecimiento_export <- 0.02
crecimiento_produccion <- 0.015
crecimiento_cpi <- 0.02
crecimiento_area <- 0.005
crecimiento_yield <- 0.01
crecimiento_stocks <- 0.01
crecimiento_gpv <- 0.03
crecimiento_emisiones <- 0.02
crecimiento_absorciones <- 0.015

# Extender cada variable
trayectorias$Stock_Variation <- extender_variable(datos$Stock_Variation, crecimiento_stock, anios)
trayectorias$Export_Quantity <- extender_variable(datos$Export_Quantity, crecimiento_export, anios)
trayectorias$Production <- extender_variable(datos$Production, crecimiento_produccion, anios)
trayectorias$Consumer_Price_Indices <- extender_variable(datos$Consumer_Price_Indices, crecimiento_cpi, anios)
trayectorias$Area_Harvested <- extender_variable(datos$Area_Harvested, crecimiento_area, anios)
trayectorias$Yield <- extender_variable(datos$Yield, crecimiento_yield, anios)
trayectorias$Stocks <- extender_variable(datos$Stocks, crecimiento_stocks, anios)
trayectorias$Gross_Production_Value <- extender_variable(datos$Gross_Production_Value, crecimiento_gpv, anios)
trayectorias$Emisiones_Totales_Tierra <- extender_variable(datos$Emisiones_Totales_Tierra, crecimiento_emisiones, anios)
trayectorias$Emisiones_Ganadería <- extender_variable(datos$Emisiones_Ganadería, crecimiento_emisiones, anios)
trayectorias$Absorciones_Ganadería <- extender_variable(datos$Absorciones_Ganadería, crecimiento_absorciones, anios)
trayectorias$Emisiones_Tierras_Totales <- extender_variable(datos$Emisiones_Tierras_Totales, crecimiento_emisiones, anios)
trayectorias$Absorciones_Tierras_Totales <- extender_variable(datos$Absorciones_Tierras_Totales, crecimiento_absorciones, anios)
trayectorias$Emisiones_Tierra_de_Cultivo <- extender_variable(datos$Emisiones_Tierra_de_Cultivo, crecimiento_emisiones, anios)
trayectorias$Absorciones_Tierra_de_Cultivo <- extender_variable(datos$Absorciones_Tierra_de_Cultivo, crecimiento_absorciones, anios)
trayectorias$Emisiones_Pastizales <- extender_variable(datos$Emisiones_Pastizales, crecimiento_emisiones, anios)
trayectorias$Absorciones_Pastizales <- extender_variable(datos$Absorciones_Pastizales, crecimiento_absorciones, anios)
trayectorias$Emisiones_Humedales <- extender_variable(datos$Emisiones_Humedales, crecimiento_emisiones, anios)
trayectorias$Absorciones_Humedales <- extender_variable(datos$Absorciones_Humedales, crecimiento_absorciones, anios)
trayectorias$Emisiones_Asentamiento <- extender_variable(datos$Emisiones_Asentamiento, crecimiento_emisiones, anios)
trayectorias$Absorciones_Asentamientos <- extender_variable(datos$Absorciones_Asentamientos, crecimiento_absorciones, anios)
trayectorias$Emisiones_Otras_Tierras <- extender_variable(datos$Emisiones_Otras_Tierras, crecimiento_emisiones, anios)
trayectorias$Absorciones_Otras_Tierra <- extender_variable(datos$Absorciones_Otras_Tierra, crecimiento_absorciones, anios)
trayectorias$Emisiones_Totales <- extender_variable(datos$Emisiones_Totales, crecimiento_emisiones, anios)
trayectorias$Absorciones_Totales <- extender_variable(datos$Absorciones_Totales, crecimiento_absorciones, anios)

# Calcular la tasa de crecimiento anual promedio de las emisiones históricas
emisiones_historicas <- datos$Emisiones_Totales_Tierra
tasa_crecimiento_emisiones <- (emisiones_historicas[length(emisiones_historicas)] / emisiones_historicas[1])^(1 / (length(emisiones_historicas) - 1)) - 1

# Ajustar la tasa de crecimiento para que sea realista
tasa_crecimiento_emisiones <- min(tasa_crecimiento_emisiones, 0.01)  # Ajustar a un máximo de 1%

# Crear columnas para los escenarios BAU, Moderado y Ambicioso
trayectorias$Emisiones_BAU <- rep(emisiones_historicas[1], length(anios))
trayectorias$Emisiones_Moderado <- rep(NA, length(anios))
trayectorias$Emisiones_Ambicioso <- rep(NA, length(anios))

# Llenar el primer año con las emisiones iniciales
trayectorias$Emisiones_Moderado[1] <- emisiones_historicas[1]
trayectorias$Emisiones_Ambicioso[1] <- emisiones_historicas[1]

# Factores de reducción de emisiones (tecnologías y políticas)
factor_agricultura_precision <- 0.95  # Reducción del 1%
factor_energias_renovables <- 0.5    # Reducción del 1%
factor_subsidios_sostenibles <- 0.995  # Reducción del 0.5%
factor_regulacion_emisiones <- 0.98   # Reducción del 2%
otro_factor_1<- 0.98   # Reducción del 2%
otro_factor_2<- 0.98   # Reducción del 2%
otro_factor_3<- 0.98   # Reducción del 2%
otro_factor_4<- 0.98   # Reducción del 2%
otro_factor_5<- 0.98   # Reducción del 2%

# Aplicar los factores de reducción en los escenarios Moderado y Ambicioso
factor_moderado <-  factor_regulacion_emisiones*otro_factor_1*otro_factor_2
factor_ambicioso <- factor_agricultura_precision * factor_energias_renovables * factor_regulacion_emisiones*otro_factor_1*otro_factor_2

# Definir periodo de transición y reducción
transicion_inicio <- 2025
transicion_fin_moderado <- 2040
transicion_fin_ambicioso <- 2028

# Calcular las trayectorias de emisiones para cada escenario
calcular_trayectoria <- function(emisiones_iniciales, factor_moderado, factor_ambicioso, transicion_inicio, transicion_fin_moderado, transicion_fin_ambicioso, anios, tasa_crecimiento_emisiones) {
  BAU <- rep(emisiones_iniciales[1], length(anios))
  Moderado <- rep(NA, length(anios))
  Ambicioso <- rep(NA, length(anios))
  
  Moderado[1] <- emisiones_iniciales[1]
  Ambicioso[1] <- emisiones_iniciales[1]
  
  for (i in 2:length(anios)) {
    BAU[i] <- BAU[i - 1] * (1 + tasa_crecimiento_emisiones)
    
    if (anios[i] >= transicion_inicio) {
      if (anios[i] <= transicion_fin_moderado) {
        Moderado[i] <- Moderado[i - 1] * (1 + tasa_crecimiento_emisiones)
      } else {
        Moderado[i] <- Moderado[i - 1] * factor_moderado
        factor_moderado <- factor_moderado * 0.99  # Reducción gradual adicional
      }
      
      if (anios[i] <= transicion_fin_ambicioso) {
        Ambicioso[i] <- Ambicioso[i - 1] * (1 + tasa_crecimiento_emisiones)
      } else {
        Ambicioso[i] <- Ambicioso[i - 1] * factor_ambicioso
        factor_ambicioso <- factor_ambicioso * 0.98  # Reducción gradual adicional
      }
    } else {
      Moderado[i] <- Moderado[i - 1] * (1 + tasa_crecimiento_emisiones)
      Ambicioso[i] <- Ambicioso[i - 1] * (1 + tasa_crecimiento_emisiones)
    }
  }
  
  return(data.frame(BAU, Moderado, Ambicioso))
}

# Aplicar la función para cada tipo de emisión
emisiones_tierra <- calcular_trayectoria(datos$Emisiones_Totales_Tierra, factor_moderado, factor_ambicioso, transicion_inicio, transicion_fin_moderado, transicion_fin_ambicioso, anios, tasa_crecimiento_emisiones)
emisiones_ganaderia <- calcular_trayectoria(datos$Emisiones_Ganadería, factor_moderado, factor_ambicioso, transicion_inicio, transicion_fin_moderado, transicion_fin_ambicioso, anios, tasa_crecimiento_emisiones)
emisiones_pastizales <- calcular_trayectoria(datos$Emisiones_Pastizales, factor_moderado, factor_ambicioso, transicion_inicio, transicion_fin_moderado, transicion_fin_ambicioso, anios, tasa_crecimiento_emisiones)

# Agregar los resultados al data frame de trayectorias
trayectorias$Emisiones_Tierra_BAU <- emisiones_tierra$BAU
trayectorias$Emisiones_Tierra_Moderado <- emisiones_tierra$Moderado
trayectorias$Emisiones_Tierra_Ambicioso <- emisiones_tierra$Ambicioso

trayectorias$Emisiones_Ganadería_BAU <- emisiones_ganaderia$BAU
trayectorias$Emisiones_Ganadería_Moderado <- emisiones_ganaderia$Moderado
trayectorias$Emisiones_Ganadería_Ambicioso <- emisiones_ganaderia$Ambicioso

trayectorias$Emisiones_Pastizales_BAU <- emisiones_pastizales$BAU
trayectorias$Emisiones_Pastizales_Moderado <- emisiones_pastizales$Moderado
trayectorias$Emisiones_Pastizales_Ambicioso <- emisiones_pastizales$Ambicioso

# Visualizar los resultados
plot_emisiones <- function(data, title, subtitle, y_label) {
  ggplot(data, aes(x = Año)) +
    geom_line(aes(y = Emisiones_Tierra_BAU, color = "Emisiones Tierra (BAU)"), size = 1) +
    geom_line(aes(y = Emisiones_Tierra_Moderado, color = "Emisiones Tierra Moderado"), size = 1, linetype = "dashed") +
    geom_line(aes(y = Emisiones_Tierra_Ambicioso, color = "Emisiones Tierra Ambicioso"), size = 1, linetype = "dotted") +
    geom_line(aes(y = Emisiones_Ganadería_BAU, color = "Emisiones Ganadería (BAU)"), size = 1) +
    geom_line(aes(y = Emisiones_Ganadería_Moderado, color = "Emisiones Ganadería Moderado"), size = 1, linetype = "dashed") +
    geom_line(aes(y = Emisiones_Ganadería_Ambicioso, color = "Emisiones Ganadería Ambicioso"), size = 1, linetype = "dotted") +
    geom_line(aes(y = Emisiones_Pastizales_BAU, color = "Emisiones Pastizales (BAU)"), size = 1) +
    geom_line(aes(y = Emisiones_Pastizales_Moderado, color = "Emisiones Pastizales Moderado"), size = 1, linetype = "dashed") +
    geom_line(aes(y = Emisiones_Pastizales_Ambicioso, color = "Emisiones Pastizales Ambicioso"), size = 1, linetype = "dotted") +
    labs(title = title, subtitle = subtitle, x = "Año", y = y_label, color = "Escenario") +
    theme_minimal() +
    scale_color_manual(values = c("Emisiones Tierra (BAU)" = "red", 
                                  "Emisiones Tierra Moderado" = "green", 
                                  "Emisiones Tierra Ambicioso" = "blue",
                                  "Emisiones Ganadería (BAU)" = "orange",
                                  "Emisiones Ganadería Moderado" = "purple",
                                  "Emisiones Ganadería Ambicioso" = "pink",
                                  "Emisiones Pastizales (BAU)" = "brown",
                                  "Emisiones Pastizales Moderado" = "cyan",
                                  "Emisiones Pastizales Ambicioso" = "magenta"))
}

plot1 ## Es el original
plot2<-plot_emisiones(trayectorias, "Trayectorias de Emisión en el Sector Agrícola", "Comparación de escenarios de mitigación hasta 2050", "Emisiones Totales de CO2 (toneladas)")### Con cambios en energías renovable
