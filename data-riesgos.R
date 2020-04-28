library(tidyverse)
library(readxl)
muertes <- read_excel("data/muertes.xlsx")

p <- t(select(muertes, -`Fecha de fallecimiento`, -`Fecha de detección`, -sexo, -edad, -Nacionalidad, -diasevolucion, -municipio, -provincia))

factor <- c(colnames(select(muertes, -`Fecha de fallecimiento`, -`Fecha de detección`, -sexo, -edad, -Nacionalidad, -diasevolucion, -municipio, -provincia)))

total <- c(rowSums(p, na.rm = T))

Factores <- tibble(Factor.Riesgo = as.factor(factor), Total = total)


