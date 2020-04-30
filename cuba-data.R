library(tidyverse)
library(jsonlite)
library(readxl)


## Lectura de los datos

cubadata <- read_csv(url("https://covid19cubadata.github.io/data/covid19-casos.csv"))
casosprov <- fromJSON(url("https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/affected_provinces.json"))
act <- fromJSON(url("https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/evolution_of_cases_by_days.json"))
muer <- fromJSON(url("https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/evolution_of_deaths_by_days.json"))
rec <- fromJSON(url("https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/evolution_of_recovered_by_days.json"))
muertes <- read_excel("data/muertes.xlsx")

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

casosprov <- casosprov %>% mutate(Tasa= 10^5*Casos/Población)

casosprov %>% select(Provincia, Casos, Población, Tasa) %>% arrange(desc(Tasa))

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

  ### Creando Tabla de Factores de Reiesgos
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





## Salva de los datos para su análisis

save(cubadata, file = "rda/cubadata.rda")
save(casos, file = "rda/casos.rda")
save(casosprov, file = "rda/casosprov.rda")
save(Factores, file = "rda/factores.riesgos.rda")
save(class.muertes, file = "rda/class.muertes.rda")



