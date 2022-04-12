library(dplyr)
library(writexl)
library(readxl)
library(tidyverse)

#Cargar datos-----
baseservidores <- read_excel("./Data/cuestionario_unicef_servidorespublicos.xlsx")
labels <- read_excel("./Data/labels_unicefservidores.xlsx")

#Arreglar nombres vars----
nombresviejos <- colnames(baseservidores)
colnames(baseservidores) <- pull(labels, label)
#base reducida
basereducida <- baseservidores %>%
  select(institucion:impacto_comunicacion)

#Extraer duplicados----
#Nota: extraje duplicados de la manera mas estricta (que todas las respuestas coincidieran)
#porque no tenia ningun nombre de la persona para controlar por esa var)
duplicados <- basereducida[duplicated(basereducida),] #dataframe sin dups
nrow(duplicados) #checkear numero de duplicados
basereducida <- basereducida %>% filter(!duplicated(basereducida)) #base sin dups


# Arreglar nombres de instituciones ---------------------------------------
basereducida <- basereducida %>%
  mutate(institucion2 = ifelse(institucion == "OTRAS ENTIDADES", institucion_otra,
                               institucion),
         institucion2 = str_to_upper(institucion2)) %>%
  relocate(institucion2, .after = institucion_otra)


# Ficha tecnica servidores publicos ---------------------------------------
#Observaciones por institucion
obs <- basereducida %>%
  group_by(institucion2) %>%
  count() %>%
  rename(institucion = institucion2, observaciones = n)

#resumen edad y anios de experiencia
ficha <- basereducida %>%
  group_by(sexo) %>%
  summarise(prom = mean(edad, na.rm=T),
            median = median(edad, na.rm = T),
            sd = sd(edad, na.rm=T),
            max = max(edad, na.rm = T),
            min = min(edad, na.rm = T)) %>%
  pivot_longer(prom:min, names_to = "Indicador", 
               values_to = "Edad")

aniosexp <- basereducida %>%
  group_by(sexo) %>%
  summarise(prom = mean(anios_experiencia, na.rm = T),
            median = median(anios_experiencia, na.rm = T),
            sd = sd(anios_experiencia, na.rm = T),
            max = max(anios_experiencia, na.rm = T),
            min = min(anios_experiencia, na.rm = T)) %>%
  pivot_longer(prom:min, names_to = "Indicador", 
               values_to = "Años de experiencia")

ficha <- merge(ficha, aniosexp, by = c("sexo", "Indicador"), all.x = T) %>%
  rename(Sexo = sexo)


#tabla de genero
gender <- basereducida %>%
  count(sexo)

#tabla sector
sector <- basereducida %>%
  select(formacion_leyes:diseno_implementacion_estrategia) %>%
  summarise(across(everything(), ~ sum(., na.rm = T)/n())) %>%
  pivot_longer(formacion_leyes:diseno_implementacion_estrategia, 
               names_to = "Sector", values_to = "Pctg")

#exportar tablas
vars <- list(obs = obs, ficha = ficha, gender = gender, sector = sector)
write_xlsx(vars, "./Tablas/ficha_servidores.xlsx")

# Exportar base limpia ----------------------------------------------------
write_xlsx(basereducida, "./Data/base_servidores.xlsx")

