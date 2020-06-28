## Cargando paquetes

library(tidyverse)
library(jsonlite)
library(readxl)
library(lubridate)
library(ggrepel)
library(ggforce)
library(gganimate)
library(ggimage)
library(Cairo)
library(magick)
library(scales)
library(d3heatmap)
library(hrbrthemes)
library(ggdark)
library(ggflags)
library(gifski)

## Lectura de los datos

cubadata <- read_csv(url("https://covid19cubadata.github.io/data/covid19-casos.csv"))
casosprov <- fromJSON(url("https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/affected_provinces.json"))
act <- fromJSON(url("https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/evolution_of_cases_by_days.json"))
muer <- fromJSON(url("https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/evolution_of_deaths_by_days.json"))
fallecidos <- fromJSON(url("https://covid19cubadata.github.io/data/covid19-fallecidos.json"))
rec <- fromJSON(url("https://covid19cuba.github.io/covid19cubadata.github.io/api/v1/evolution_of_recovered_by_days.json"))
muertes <- read_excel("data/muertes.xlsx")
poblacionmun <- read_excel("data/poblacion.cuba.2018.onei.xlsx")
distribucion.municipios.provincia <- read_excel("data/distribucion.municipios.provincia.xlsx")
#covidcuba <- fromJSON(url("https://covid19cubadata.github.io/data/covid19-cuba.json"))

# Importacion de información internacional de la base de datos de ECDC
url <- url("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
casos.ecdc <- read_csv(url) %>% mutate(dateRep = dmy(dateRep))

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


                                                        ### Creando el recurso cantidad de casos por evento en el tiempo ###
fecha <- c(act[["date"]][["values"]])
nuevos <- c(act[["daily"]][["values"]])
acumulados <- c(act[["accumulated"]][["values"]])
activos <- c(act[["active"]][["values"]])
muertos <- c(muer[["accumulated"]][["values"]])
recuperados <- c(rec[["accumulated"]][["values"]])

casos <- tibble(fecha, nuevos, activos, acumulados, recuperados, muertos) %>% 
  mutate(fecha = as.Date(fecha))

casos.tiempo.eventos <- casos %>% 
  rename("Fecha" = "fecha",
         "Casos Nuevos" = "nuevos",
         "Casos Acumulados" = "acumulados",
         "Casos Activos" = "activos",
         "Casos Recuperados" = "recuperados",
         "Casos Fallecidos" = "muertos") %>% 
  gather("Casos Nuevos", 
         "Casos Acumulados", 
         "Casos Activos", 
         "Casos Recuperados",
         "Casos Fallecidos",
         key = "Evento", 
         value = "Cantidad")

                                                        ### Trabajando con casosprov ###

casosprov <- casosprov %>% mutate(Casos = value,
                                  Provincia = name,
                                  Población = population,
                                  `Tasa.10^5Hab`= 10^5*Casos/Población) %>% 
  select(Provincia, Casos, Población, `Tasa.10^5Hab`) %>% arrange(desc(`Tasa.10^5Hab`))

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

casosmun <- as.tibble(count(cubadata, municipio))

casospoblmun <- merge(poblacionmun, casosmun, by = "municipio", all = T)

casospoblmun <- casospoblmun %>% 
  mutate(`Tasa.10^5Hab` = round(n*10^5/poblacion, 2)) %>% 
  arrange(desc(`Tasa.10^5Hab`))

casospoblmun <- merge(casospoblmun, distribucion.municipios.provincia, by = "municipio")
                                          ## Creando recurso de casos Cuba vs TOP10

casos.top.10.cu <- casos.ecdc %>% 
  filter(geoId %in% c("CU", "US", "ES", "RU", "UK", "IT", "BR", "FR", "DE", "TR", "IR")) %>%
  filter(cases > 0) %>% 
  arrange(dateRep) %>% 
  arrange(geoId)

casos.cu <- casos.top.10.cu %>% filter(geoId == "CU") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.)) %>% 
  mutate(geoId = ifelse(geoId == "CU", "cu", ""))

casos.cu$geoId[casos.cu$geoId == "CU"] <- "cu"

casos.us <- casos.top.10.cu %>% filter(geoId == "US") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.)) %>% 
  mutate(geoId = ifelse(geoId == "US", "us", ""))

casos.es <- casos.top.10.cu %>% filter(geoId == "ES") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.)) %>% 
  mutate(geoId = ifelse(geoId == "ES", "es", ""))

casos.ru <- casos.top.10.cu %>% filter(geoId == "RU") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.)) %>% 
  mutate(geoId = ifelse(geoId == "RU", "ru", ""))

casos.uk <- casos.top.10.cu %>% filter(geoId == "UK") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.)) %>% 
  mutate(geoId = ifelse(geoId == "UK", "gb", ""))

casos.it <- casos.top.10.cu %>% filter(geoId == "IT") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.)) %>% 
  mutate(geoId = ifelse(geoId == "IT", "it", ""))

casos.br <- casos.top.10.cu %>% filter(geoId == "BR") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.)) %>% 
  mutate(geoId = ifelse(geoId == "BR", "br", ""))

casos.fr <- casos.top.10.cu %>% filter(geoId == "FR") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.)) %>% 
  mutate(geoId = ifelse(geoId == "FR", "fr", ""))

casos.tr <- casos.top.10.cu %>% filter(geoId == "TR") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.)) %>% 
  mutate(geoId = ifelse(geoId == "TR", "tr", ""))

casos.ir <- casos.top.10.cu %>% filter(geoId == "IR") %>% 
  mutate(casos.acum = cumsum(cases)) %>%
  mutate(dia = rownames(.)) %>% 
  mutate(geoId = ifelse(geoId == "IR", "ir", ""))

casos.top.10.cu <- rbind(casos.cu, casos.us, casos.es, casos.ru, casos.uk, casos.it, casos.br, casos.fr, casos.tr, casos.ir) %>% 
  mutate(`tasa.10^5.hab.acum` = 10^5*casos.acum/popData2019)

## Creando data frame con casos por municipios y provincias
casos.prov.mun <- as_tibble(left_join(distribucion.municipios.provincia, casosmun, by = "municipio")) %>% 
  rename(casos = n) 

casos.prov.mun$casos[is.na(casos.prov.mun$casos)] <- 0

## Creando data frame con casos acumulados por provincias en el tiempo
casos.prov.tiempo <- 
  count(cubadata, fecha_confirmacion, provincia) %>% 
  rename(casos = n)

casos.art <- casos.prov.tiempo %>% 
  filter(provincia == "Artemisa") %>% 
  mutate(casos.acum = cumsum(casos))

casos.cmg <- casos.prov.tiempo %>% 
  filter(provincia == "Camagüey") %>% 
  mutate(casos.acum = cumsum(casos))

casos.cav <- casos.prov.tiempo %>% 
  filter(provincia == "Ciego de Ávila") %>% 
  mutate(casos.acum = cumsum(casos))

casos.cfg <- casos.prov.tiempo %>% 
  filter(provincia == "Cienfuegos") %>% 
  mutate(casos.acum = cumsum(casos))

casos.grm <- casos.prov.tiempo %>% 
  filter(provincia == "Granma") %>% 
  mutate(casos.acum = cumsum(casos))

casos.gtm <- casos.prov.tiempo %>% 
  filter(provincia == "Guantánamo") %>% 
  mutate(casos.acum = cumsum(casos))

casos.hol <- casos.prov.tiempo %>% 
  filter(provincia == "Holguín") %>% 
  mutate(casos.acum = cumsum(casos))

casos.juv <- casos.prov.tiempo %>% 
  filter(provincia == "Isla de la Junventud") %>% 
  mutate(casos.acum = cumsum(casos))

casos.hab <- casos.prov.tiempo %>% 
  filter(provincia == "La Habana") %>% 
  mutate(casos.acum = cumsum(casos))

casos.ltu <- casos.prov.tiempo %>% 
  filter(provincia == "Las Tunas") %>% 
  mutate(casos.acum = cumsum(casos))

casos.mtz <- casos.prov.tiempo %>% 
  filter(provincia == "Matanzas") %>% 
  mutate(casos.acum = cumsum(casos))

casos.may <- casos.prov.tiempo %>% 
  filter(provincia == "Mayabeque") %>% 
  mutate(casos.acum = cumsum(casos))

casos.pri <- casos.prov.tiempo %>% 
  filter(provincia == "Pinar del Río") %>% 
  mutate(casos.acum = cumsum(casos))

casos.ssp <- casos.prov.tiempo %>% 
  filter(provincia == "Sancti Spíritus") %>% 
  mutate(casos.acum = cumsum(casos))

casos.scu <- casos.prov.tiempo %>% 
  filter(provincia == "Santiago de Cuba") %>% 
  mutate(casos.acum = cumsum(casos))

casos.vlc <- casos.prov.tiempo %>% 
  filter(provincia == "Villa Clara") %>% 
  mutate(casos.acum = cumsum(casos))

casos.prov.tiempo <- rbind(casos.art, 
                           casos.cav, 
                           casos.cfg, 
                           casos.grm, 
                           casos.gtm, 
                           casos.hol, 
                           casos.juv, 
                           casos.hab, 
                           casos.ltu, 
                           casos.mtz, 
                           casos.may, 
                           casos.pri, 
                           casos.ssp, 
                           casos.scu, 
                           casos.vlc) 

casos.prov.tiempo <- casos.prov.tiempo %>% 
  rename(casos.acum.prov = casos.acum) %>% 
  arrange(fecha_confirmacion)

## Salva de los datos para su análisis

save(cubadata, file = "rda/cubadata.rda")
save(casos, file = "rda/casos.rda")
save(casosprov, file = "rda/casosprov.rda")
save(Factores, file = "rda/factores.riesgos.rda")
save(class.muertes, file = "rda/class.muertes.rda")
save(casospoblmun, file = "rda/casospoblmun.rda")
save(casos.top.10.cu, file = "rda/casos.top.10.cu.rda")
save(casos.ecdc, file = "rda/casos.ecdc.rda")
save(casos.prov.tiempo, file = "rda/casos.prov.tiempo.rda")
save(casos.tiempo.eventos, file = "rda/casos.tiempo.eventos.rda")

write_json(cubadata, "data/cubadata.json")
write_json(casos, "data/casos.json")
write_json(rango.c2, "data/casos.rango.json")
write_json(casosprov, "data/casosprov.json")
write_json(Factores, "data/factores.riesgos.json")
write_json(class.muertes, "data/class.muertes.json")
write_json(casospoblmun, "data/casospoblmun.json")
write_json(casos.top.10.cu, "data/casos.top.10.cu.json")
write_json(casos.ecdc, "data/casos.ecdc.json")
write_json(casos.prov.mun, "data/casos.prov.mun.json")
