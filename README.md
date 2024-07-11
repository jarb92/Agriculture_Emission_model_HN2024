# Emission-model-Agriculture_-HN2024
# Instalar y cargar los paquetes necesarios
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# Cargar los datos desde el archivo Excel
data <- read_excel("C:/Users/DEES-JULIO/Desktop/GIZ/Agricultura/agricultural_emissions_2000_2020.xlsx")
View(data)
GPV <- read_excel("data/Value of Agricultural Production.xls")

# Preparar los datos
resultado <- GPV %>%
  filter(Year >= 2000, Year <= 2020) %>%
  group_by(Year, Categoría) %>%
  summarise(total_value = sum(Value, na.rm = TRUE)) %>%
  arrange(Year, Categoría)

resultado_wide <- resultado %>%
  pivot_wider(names_from = Categoría, values_from = total_value, names_prefix = "total_value_")

nGPV <- data.frame(resultado_wide)
View(nGPV)

# Definir las variables del archivo principal
years <- data$Year

#variables economicas 

export_value <- data$Export_Value
import_value <- data$Import_Value
consumer_price_indices <- data$Consumer_Price_Indices
area_harvested <- data$Area_Harvested
yield <- data$Yield


# Variables activas en el modelos

gross_production_value <- data$Gross_Production_Value
production_agricultura <- nGPV$total_value_Agricultura
production_ganaderia <- nGPV$total_value_Ganadería

## Variables de emisiones

emisiones_totales_tierra <- data$Emisiones_Totales_Tierra
emisiones_ganaderia <- data$Emisiones_Ganadería
absorciones_ganaderia <- data$Absorciones_Ganadería
absorciones_tierras_totales <- data$Absorciones_Tierras_Totales
emisiones_tierra_de_cultivo <- data$Emisiones_Tierra_de_Cultivo
absorciones_tierra_de_cultivo <- data$Absorciones_Tierra_de_Cultivo
emisiones_pastizales <- data$Emisiones_Pastizales
absorciones_pastizales <- data$Absorciones_Pastizales
emisiones_humedales <- data$Emisiones_Humedales
absorciones_humedales <- data$Absorciones_Humedales
emisiones_asentamiento <- data$Emisiones_Asentamiento
absorciones_asentamientos <- data$Absorciones_Asentamientos
emisiones_totales <- data$Emisiones_Totales
absorciones_totales <- data$Absorciones_Totales

# Definir parámetros de simulación
num_simulaciones <- 1000
anio_inicio <- 2000
anio_fin <- 2050
anios <- seq(anio_inicio, anio_fin, by = 1)

# Calcular las tasas de crecimiento promedio
crecimiento_produccion_agricultura <- mean((production_agricultura[-1] / production_agricultura[-length(production_agricultura)] - 1), na.rm = TRUE)
crecimiento_produccion_ganaderia <- mean((production_ganaderia[-1] / production_ganaderia[-length(production_ganaderia)] - 1), na.rm = TRUE)
crecimiento_area <- mean((area_harvested[-1] / area_harvested[-length(area_harvested)] - 1), na.rm = TRUE)

# Calcular los factores de emisión históricos
factor_emision_agricultura_hist <- emisiones_totales_tierra / production_agricultura
factor_emision_ganaderia_hist <- emisiones_ganaderia / production_ganaderia
factor_emision_area_hist <- emisiones_tierra_de_cultivo / area_harvested

# Calcular las desviaciones estándar (sigma)
sigma_emision_agricultura <- sd(factor_emision_agricultura_hist, na.rm = TRUE)
sigma_emision_ganaderia <- sd(factor_emision_ganaderia_hist, na.rm = TRUE)
sigma_emision_area <- sd(factor_emision_area_hist, na.rm = TRUE)

# Función para generar trayectorias estocásticas
generar_trayectorias <- function(num_simulaciones, anios, produccion_inicial, crecimiento_anual, sigma) {
  trayectorias <- matrix(NA, nrow = length(anios), ncol = num_simulaciones)
  trayectorias[1, ] <- produccion_inicial
  for (i in 2:length(anios)) {
    crecimiento_estocastico <- rnorm(num_simulaciones, mean = crecimiento_anual, sd = sigma)
    trayectorias[i, ] <- trayectorias[i - 1, ] * (1 + crecimiento_estocastico)
  }
  return(trayectorias)
}

# Generar trayectorias estocásticas para producción agrícola y ganadera
trayectorias_agricultura <- generar_trayectorias(num_simulaciones, anios, production_agricultura[1], crecimiento_produccion_agricultura, sigma_emision_agricultura)
trayectorias_ganaderia <- generar_trayectorias(num_simulaciones, anios, production_ganaderia[1], crecimiento_produccion_ganaderia, sigma_emision_ganaderia)
trayectorias_area <- generar_trayectorias(num_simulaciones, anios, area_harvested[1], crecimiento_area, sigma_emision_area)

# Calcular emisiones estocásticas para cada simulación
factor_emision_agricultura <- mean(factor_emision_agricultura_hist, na.rm = TRUE)  # kg CO2e por tonelada de producción agrícola
factor_emision_ganaderia <- mean(factor_emision_ganaderia_hist, na.rm = TRUE)    # kg CO2e por cabeza de ganado
factor_emision_area <- mean(factor_emision_area_hist, na.rm = TRUE)          # kg CO2e por hectárea

emisiones_agricultura <- trayectorias_agricultura * factor_emision_agricultura
emisiones_ganaderia <- trayectorias_ganaderia * factor_emision_ganaderia
emisiones_area <- trayectorias_area * factor_emision_area

emisiones_totales_simuladas <- emisiones_agricultura + emisiones_ganaderia + emisiones_area

# Calcular el promedio y percentiles de las emisiones simuladas
emisiones_promedio <- rowMeans(emisiones_totales_simuladas)
emisiones_p5 <- apply(emisiones_totales_simuladas, 1, quantile, probs = 0.05)
emisiones_p95 <- apply(emisiones_totales_simuladas, 1, quantile, probs = 0.95)

# Crear un data frame para las trayectorias de emisión promedio y percentiles
trayectorias <- data.frame(
  Año = anios,
  Emisiones_Promedio = emisiones_promedio,
  Emisiones_P5 = emisiones_p5,
  Emisiones_P95 = emisiones_p95
)

# Visualizar los resultados
plot_emisiones_estocasticas <- function(data, title, subtitle, y_label) {
  ggplot(data, aes(x = Año)) +
    geom_line(aes(y = Emisiones_Promedio, color = "Emisiones Promedio"), size = 1) +
    geom_ribbon(aes(ymin = Emisiones_P5, ymax = Emisiones_P95), alpha = 0.2) +
    labs(title = title, subtitle = subtitle, x = "Año", y = y_label, color = "Escenario") +
    theme_minimal() +
    scale_color_manual(values = c("Emisiones Promedio" = "red"))
}

plot1 <- plot_emisiones_estocasticas(trayectorias, "Trayectorias de Emisión en el Sector Agrícola (Enfoque Estocástico)", "Comparación de escenarios de mitigación hasta 2050", "Emisiones Totales de CO2 (toneladas)")
print(plot1)

#############################  Haremos un ejercicio para simular factores de reducción ########################

############################  Haremos un ejercicio para simular factores de reducción ########################

# Factores de reducción (ejemplos)
factor_agricultura_precision <- 0.99  # Reducción del 95%
factor_energias_renovables <- 0.95    # Reducción del 20%
factor_subsidios_sostenibles <- 0.97  # Reducción del 10%
factor_regulacion_emisiones <- 0.93   # Reducción del 15%
factor_CCS <- 0.90                    # Reducción del 30%

# Factores combinados para escenarios
factor_reduccion_agricultura_moderado <- factor_agricultura_precision * factor_subsidios_sostenibles
factor_reduccion_agricultura_ambicioso <- factor_agricultura_precision * factor_subsidios_sostenibles * factor_energias_renovables

factor_reduccion_ganaderia_moderado <- factor_regulacion_emisiones * factor_CCS
factor_reduccion_ganaderia_ambicioso <- factor_regulacion_emisiones * factor_CCS * factor_energias_renovables

factor_reduccion_area_moderado <- factor_agricultura_precision
factor_reduccion_area_ambicioso <- factor_agricultura_precision * factor_energias_renovables

# Calcular las trayectorias de emisiones para cada actividad y escenario
calcular_trayectoria_reduccion <- function(emisiones_iniciales, factores_reduccion, anios, transicion_inicio, transicion_fin) {
  trayectorias <- matrix(NA, nrow = length(anios), ncol = num_simulaciones)
  trayectorias[1, ] <- emisiones_iniciales[1, ]
  
  for (i in 2:length(anios)) {
    if (anios[i] >= transicion_inicio && anios[i] <= transicion_fin) {
      trayectorias[i, ] <- trayectorias[i - 1, ] * factores_reduccion
    } else if (anios[i] > transicion_fin) {
      trayectorias[i, ] <- trayectorias[i - 1, ] * factores_reduccion
    } else {
      trayectorias[i, ] <- emisiones_iniciales[i, ]
    }
  }
  return(trayectorias)
}

transicion_inicio <- 2025
transicion_fin_moderado <- 2050
transicion_fin_ambicioso <- 2050

# Emisiones agrícolas
trayectorias_agricultura_moderado <- calcular_trayectoria_reduccion(emisiones_agricultura, factor_reduccion_agricultura_moderado, anios, transicion_inicio, transicion_fin_moderado)
trayectorias_agricultura_ambicioso <- calcular_trayectoria_reduccion(emisiones_agricultura, factor_reduccion_agricultura_ambicioso, anios, transicion_inicio, transicion_fin_ambicioso)

# Emisiones ganaderas
trayectorias_ganaderia_moderado <- calcular_trayectoria_reduccion(emisiones_ganaderia, factor_reduccion_ganaderia_moderado, anios, transicion_inicio, transicion_fin_moderado)
trayectorias_ganaderia_ambicioso <- calcular_trayectoria_reduccion(emisiones_ganaderia, factor_reduccion_ganaderia_ambicioso, anios, transicion_inicio, transicion_fin_ambicioso)

# Emisiones por área
trayectorias_area_moderado <- calcular_trayectoria_reduccion(emisiones_area, factor_reduccion_area_moderado, anios, transicion_inicio, transicion_fin_moderado)
trayectorias_area_ambicioso <- calcular_trayectoria_reduccion(emisiones_area, factor_reduccion_area_ambicioso, anios, transicion_inicio, transicion_fin_ambicioso)

# Emisiones totales moderado
emisiones_totales_moderado <- trayectorias_agricultura_moderado + trayectorias_ganaderia_moderado + trayectorias_area_moderado

# Emisiones totales ambicioso
emisiones_totales_ambicioso <- trayectorias_agricultura_ambicioso + trayectorias_ganaderia_ambicioso + trayectorias_area_ambicioso

# Calcular el promedio y percentiles de las emisiones en los escenarios moderado y ambicioso
emisiones_moderado_promedio <- rowMeans(emisiones_totales_moderado)
emisiones_moderado_p5 <- apply(emisiones_totales_moderado, 1, quantile, probs = 0.05)
emisiones_moderado_p95 <- apply(emisiones_totales_moderado, 1, quantile, probs = 0.95)

emisiones_ambicioso_promedio <- rowMeans(emisiones_totales_ambicioso)
emisiones_ambicioso_p5 <- apply(emisiones_totales_ambicioso, 1, quantile, probs = 0.05)
emisiones_ambicioso_p95 <- apply(emisiones_totales_ambicioso, 1, quantile, probs = 0.95)

# Agregar los resultados al data frame de trayectorias
trayectorias <- trayectorias %>%
  mutate(
    Emisiones_Moderado_Promedio = emisiones_moderado_promedio,
    Emisiones_Moderado_P5 = emisiones_moderado_p5,
    Emisiones_Moderado_P95 = emisiones_moderado_p95,
    Emisiones_Ambicioso_Promedio = emisiones_ambicioso_promedio,
    Emisiones_Ambicioso_P5 = emisiones_ambicioso_p5,
    Emisiones_Ambicioso_P95 = emisiones_ambicioso_p95
  )

# Visualizar los resultados
plot_emisiones_estocasticas_con_mitigacion <- function(data, title, subtitle, y_label) {
  ggplot(data, aes(x = Año)) +
    geom_line(aes(y = Emisiones_Promedio, color = "Emisiones Promedio BAU"), size = 1) +
    geom_ribbon(aes(ymin = Emisiones_P5, ymax = Emisiones_P95), alpha = 0.2, fill = "red") +
    geom_line(aes(y = Emisiones_Moderado_Promedio, color = "Emisiones Promedio Moderado"), size = 1, linetype = "dashed") +
    geom_ribbon(aes(ymin = Emisiones_Moderado_P5, ymax = Emisiones_Moderado_P95), alpha = 0.2, fill = "green") +
    geom_line(aes(y = Emisiones_Ambicioso_Promedio, color = "Emisiones Promedio Ambicioso"), size = 1, linetype = "dotted") +
    geom_ribbon(aes(ymin = Emisiones_Ambicioso_P5, ymax = Emisiones_Ambicioso_P95), alpha = 0.2, fill = "blue") +
    scale_x_continuous(breaks = c(seq(2000, 2030, by = 5), seq(2030, 2050, by = 5))) +
    labs(title = title, subtitle = subtitle, x = "Año", y = y_label, color = "Escenario") +
    theme_minimal() +
    scale_color_manual(values = c("Emisiones Promedio BAU" = "red", 
                                  "Emisiones Promedio Moderado" = "green", 
                                  "Emisiones Promedio Ambicioso" = "blue"))
}

plot3 <- plot_emisiones_estocasticas_con_mitigacion(trayectorias, "Trayectorias de Emisión en el Sector Agrícola (Enfoque Estocástico con Mitigación)", "Comparación de escenarios de mitigación hasta 2050", "Emisiones Totales de CO2 (toneladas)")
print(plot3)


