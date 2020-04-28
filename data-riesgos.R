library(tidyverse)
library(readxl)
muertes <- read_excel("data/muertes.xlsx")

summary(muertes, na.rm = T)
t(muertes)

p <- t(select(muertes, -`Fecha de fallecimiento`, -`Fecha de detección`, -sexo, -edad, -Nacionalidad, -diasevolucion, -municipio, -provincia))

colSums(p, na.rm = T)

b <- rowSums(p, na.rm = T)
class(b)

b
p
 table(b)

c <- as_tibble(as.data.frame(p)) %>% mutate(total = rowSums(p, na.rm = T)) %>% select(total)

  head(c)
