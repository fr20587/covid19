## Cargando paquetes

library(tidyverse)
library(jsonlite)
library(readxl)
library(lubridate)
library(ggrepel)
library(ggforce)
library(gganimate)
library(Cairo)
library(magick)
library(scales)
library(d3heatmap)
library(hrbrthemes)
library(ggdark)

## Creando recurso de logo para vizualizaciones
logo_profdat <- "logo/logo.png"

## Lectura de los datos

cubadata <- read_csv(url("https://covid19cubadata.github.io/data/covid19-casos.csv"))
casosprov <- fromJSON(url("https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/affected_provinces.json"))
act <- fromJSON(url("https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/evolution_of_cases_by_days.json"))
muer <- fromJSON(url("https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/evolution_of_deaths_by_days.json"))
rec <- fromJSON(url("https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/evolution_of_recovered_by_days.json"))
muertes <- read_excel("data/muertes.xlsx")
poblacionmun <- read_excel("data/poblacion.cuba.2018.onei.xlsx")

## Creando función para inserción de logo en las visualizaciones

agregar_logo <- function(plot_path, logo_path, posicion_logo, logo_scale = 10){
  if (!posicion_logo %in% c("superior derecha", "superior izquierda", "inferior derecha", "inferior izquierda")) {
    stop("Error: Posición del Logo no reconocida\n  
         Pruebe: logo_positon = 'superior derecha' 'superior izquierda' 'inferior derecha' 'inferior izquierda'")
    }
  
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  if (posicion_logo == "superior derecha") {
    x_pos = plot_width - logo_width - 0.02 * plot_width
    y_pos = 0.045 * plot_height
  } else if (posicion_logo == "inferior derecha") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (posicion_logo == "superior izquierda") {
    x_pos = plot_width - logo_width - 0.02 * plot_width
    y_pos = plot_height - logo_height - 0.02 * plot_height
  } else if (posicion_logo == "inferior izquierda") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.02 * plot_height
  }
  
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
}

### Expreción para guardar gráfico con logo

##  plot_with_logo <- agregar_logo(
##    plot_path = paste0(Sys.Date(), "_", short_name, "_peak-years.png"), # url or local file for the plot
##    logo_path = logo_file_name, # url or local file for the logo
##    logo_position = "top right", # choose a corner
##    # 'top left', 'top right', 'bottom left' or 'bottom right'
##    logo_scale = 7
##  )

# save the image and write to working directory
## magick::image_write(plot_with_logo, paste0(Sys.Date(), "_", short_name, "_peak-years.png"))


## Manejo de los datos

                                                        ### Trabajando con cubadata ###
  ### Definiendo y dando formato a las variables
  ### TCI <- Tiempo de confirmación luego del ingreso
  ### TC <- Tipo de Caso si es: Turista o Cubano
  ### rango <- Rango Etario
  ### rango2 z- Rengo Etario por decenas

cubadata <- as_tibble(cubadata)
cubadata <- cubadata %>% mutate(sexo = as.factor(sexo),
                                edad = as.numeric(edad),
                                pais = as.factor(pais),
                                municipio = as.factor(municipio),
                                provincia = as.factor(provincia),
                                fecha_confirmacion = as.Date(fecha_confirmacion),
                                fecha_ingreso = as.Date(fecha_ingreso),
                                TCI = as.integer(fecha_confirmacion - fecha_ingreso),
                                TCaso = as.factor(if_else(pais == "cu", "Cubano", "Turista")),
                                rango = cut(edad,
                                            breaks = c(0,19,39,59,79,Inf),
                                            right = T, 
                                            labels =c("0-19", "20-39", "40-59","60-79","80 o más")),
                                rango2 = cut(edad,
                                             breaks = c(0,9,19,29,39,49,59,69,79,89,99,Inf),
                                             right = T, 
                                             labels =c("0-9","10-19","20-29", "30-39","40-49", "50-59","60-69", "70-79","80-89",
                                                       "90-99", "100 o más")))


                                                        ### Creando el recurso casos ###
fecha <- c(act[["date"]][["values"]])
nuevos <- c(act[["daily"]][["values"]])
acumulados <- c(act[["accumulated"]][["values"]])
activos <- c(act[["active"]][["values"]])
muertos <- c(muer[["accumulated"]][["values"]])
recuperados <- c(rec[["accumulated"]][["values"]])

casos <- tibble(fecha, nuevos, activos, acumulados, recuperados, muertos) %>% 
  mutate(fecha = as.Date(fecha))

                                                        ### Trabajando con casosprov ###

casosprov <- casosprov %>% rename(Casos = value)

casosprov <- casosprov %>% rename(Provincia = name)

casosprov <- casosprov %>% rename(Población = population) 

casosprov <- casosprov %>% mutate(`Tasa.10^5Hab`= 10^5*Casos/Población)

casosprov %>% select(Provincia, Casos, Población, `Tasa.10^5Hab`) %>% arrange(desc(`Tasa.10^5Hab`))

                                                        ### Trabajando con muertes ###
muertes <- muertes %>% mutate(FechaF = as.Date(`Fecha de fallecimiento`),
                              sexo = as_factor(sexo),
                              Nacionalidad = as_factor(Nacionalidad),
                              municipio = as_factor(municipio),
                              provincia = as_factor(provincia),                                
                              rango = cut(edad,
                                          breaks = c(0,19,39,59,79,Inf),
                                          right = T,
                                          labels =c("0-19", "20-39", "40-59","60-79","80 o más")),
                              rango2 = cut(edad,
                                           breaks = c(0,9,19,29,39,49,59,69,79,89,99,Inf),
                                           right = T, 
                                           labels =c("0-9","10-19","20-29", "30-39","40-49", "50-59","60-69", "70-79","80-89",
                                                     "90-99", "100 o más")))

  ### Creando Tabla de Factores de Riesgos
factor <- c(colnames(select(muertes, 
                            -`Fecha de fallecimiento`, 
                            -`Fecha de detección`, 
                            -sexo, -edad, 
                            -Nacionalidad, 
                            -diasevolucion, 
                            -municipio,
                            -provincia,
                            -rango,
                            -rango2,
                            -FechaF)))

total <- c(rowSums(t(select(muertes, 
                            -`Fecha de fallecimiento`, 
                            -`Fecha de detección`, 
                            -sexo,
                            -edad, 
                            -Nacionalidad, 
                            -diasevolucion, 
                            -municipio,
                            -provincia,
                            -rango,
                            -rango2,
                            -FechaF)), na.rm = T))

Factores <- tibble(Factor.Riesgo = as.factor(factor), Total = total)

# Explorando tiempo de detección de COVID-19 vs Fecha de fallecimiento en Pacientes fallecidos

class.muertes <- muertes %>%  select(`Fecha de fallecimiento`, `Fecha de detección`, Nacionalidad, sexo, edad) %>% 
  mutate(tiempo.detección = as.numeric(`Fecha de fallecimiento` - `Fecha de detección`)/(60*60*24),
         clasificación = as.factor(ifelse(tiempo.detección > 0, "Antes del deceso", 
                                          ifelse(tiempo.detección < 0, "Después del deceso", "Mismo día del deceso"))),
         rango = cut(edad,
                     breaks = c(0,19,39,59,79,Inf),
                     right = T,
                     labels =c("0-19", "20-39", "40-59","60-79","80 o más")),
         rango2 = cut(edad,
                      breaks = c(0,9,19,29,39,49,59,69,79,89,99,Inf),
                      right = T, 
                      labels =c("0-9","10-19","20-29", "30-39","40-49", "50-59","60-69", "70-79","80-89",
                                "90-99", "100 o más")))


                                          ## Creando recurso de población por municipio

casosmun <-  count(cubadata, municipio)

casospoblmun <- merge(poblacionmun, casosmun, by = "municipio", all = T)

casospoblmun <- casospoblmun %>% 
  mutate(`Tasa.10^5Hab` = round(n*10^5/poblacion, 2)) %>% 
  arrange(desc(`Tasa.10^5Hab`))



## Salva de los datos para su análisis

save(cubadata, file = "rda/cubadata.rda")
save(casos, file = "rda/casos.rda")
save(casosprov, file = "rda/casosprov.rda")
save(Factores, file = "rda/factores.riesgos.rda")
save(class.muertes, file = "rda/class.muertes.rda")
save(casospoblmun, file = "rda/casospoblmun.rda")


