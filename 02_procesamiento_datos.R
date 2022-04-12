library(dplyr)
library(writexl)
library(readxl)
library(tidyverse)


# cargar datos ------------------------------------------------------------
base <- read_excel("./Data/base_servidores.xlsx")

# Factores ----------------------------------------------------------------
#Tabla factores
factores <- base %>%
  select(institucion2:anios_experiencia,
         factores_covid:factores_desastres) %>%
  #porcentajes
  summarise(across(contains("factores"), ~ sum(., na.rm = T)/n())) %>%
  #formatear datos
  pivot_longer(factores_covid:factores_desastres, names_to = "Factores",
               values_to = "Pctg") %>%
  #etiquetas
  mutate(Factores = case_when(Factores == "factores_covid" ~ "Pandemia por virus COVID-19",
                              Factores == "factores_socioeconomicos" ~ "Socio-económicos",
                              Factores == "factores_politicas" ~ "Políticos",
                              Factores == "factores_administrativos" ~ "Administrativos",
                              Factores == "factores_desastres" ~ "Desastres naturales"))
#Tabla influencia
influencia <- base %>%
  select(institucion2, contains("influencia")) %>%
  summarise(across(contains("influencia"), ~ mean(., na.rm= T))) %>%
  #tabla longer
  pivot_longer(influencia_factores_covid:influencia_factores_desastres,
               names_to = "Factores", values_to = "Influencia") %>%
  #arreglar etiquetas
  mutate(Factores = case_when(Factores == "influencia_factores_covid" ~ "Pandemia por virus COVID-19",
                              Factores == "influencia_factores_socioeconomico" ~ "Socio-económicos",
                              Factores == "influencia_factores_politico" ~ "Políticos",
                              Factores == "influencia_factores_administrativo" ~ "Administrativos",
                              Factores == "influencia_factores_desastres" ~ "Desastres naturales"))

#Tabla capacidad respuesta
capacidad <- base %>%
  select(institucion2, contains("capacidad"), -apoyo_desarrollo_capacidades) %>%
  summarise(across(contains("capacidad"), ~ mean(., na.rm= T))) %>%
  #tabla longer
  pivot_longer(capacidad_factores_covid:capacidad_factores_desastres,
               names_to = "Factores", values_to = "Capacidad") %>%
  #arreglar etiquetas
  mutate(Factores = case_when(Factores == "capacidad_factores_covid" ~ "Pandemia por virus COVID-19",
                              Factores == "capacidad_factores_socioeconomico" ~ "Socio-económicos",
                              Factores == "capacidad_factores_politico" ~ "Políticos",
                              Factores == "capacidad_factores_administrativo" ~ "Administrativos",
                              Factores == "capacidad_factores_desastres" ~ "Desastres naturales"))


factores <- merge(factores, influencia, by = "Factores", all.x = T) %>%
  merge(capacidad, by = "Factores", all.x = T) %>%
  rename("Influencia del factor externo" = Influencia, 
         "Capacidad de adaptación de UNICEF al factor externo")
  
  
# Enfoques transversales --------------------------------------------------
transversales <- base %>%
  select(institucion2,
         apoyo_genero,
         apoyo_equidad,
         apoyo_interculturalidad_ddhh) %>%
  summarise(across(contains("apoyo"), ~ mean(., na.rm= T))) %>%
  #tabla longer
  pivot_longer(apoyo_genero:apoyo_interculturalidad_ddhh,
               names_to = "Ejes", values_to = "Apoyo para introduccion de ejes transversales") %>%
  #arreglar etiquetas
  mutate(Ejes = case_when(Ejes == "apoyo_genero" ~ "Enfoque de género",
                              Ejes == "apoyo_equidad" ~ "Enfoque de equidad",
                              Ejes == "apoyo_interculturalidad_ddhh" ~ "Interculturalidad y derechos humanos"))


# Unicef y NNAs, pregunta 5.2 y 8 ------------------------
iniciativas <- base %>%
  summarise(iniciativas = mean(iniciativas, na.rm=T),
            dinamicas_gobierno = mean(dinamicas_unicef_gobierno, na.rm = T)) %>%
  pivot_longer(iniciativas:dinamicas_gobierno, names_to = "Pregunta",
               values_to = "Valor") %>%
  mutate(Pregunta = case_when(Pregunta == "iniciativas" ~ "5.2 Las iniciativas de UNICEF respondieron a las necesidades, políticas y prioridades de NNAs especialmente los más marginados",
                              Pregunta == "dinamicas_gobierno" ~ "8. Las acciones de UNICEF se están transformando en dinámicas permanentes y siendo adoptadas, replicadas y escaladas por el Gobierno"))


# Exportar datos ----------------------------------------------------------
preguntas <- list(factores = factores, capacidad = capacidad, 
                  influencia = influencia, transversales = transversales, 
                  iniciativas = iniciativas)
write_xlsx(preguntas, "./Tablas/preguntas_investigacion.xlsx")



