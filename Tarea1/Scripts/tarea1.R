#install.packages(c("dplyr", "stargazer", "readr"))

#Liberías necesarias para el script
library(dplyr)
library(readr)   
library(readxl)     
library(stargazer)  
library(ggplot2)

#Aquí definimos los dataframes que nos van a servir de la carpeta datos, puede que dsp agreguemos más.
#Importante notar que se utiliza la coma como el separador de decimales.
postulacion_oferta_academica <- read_excel("Tarea1/Datos/postulacion/ofertaAcademica/OfertaAcadémica_Admisión2025.xlsx")
rendicion_archivo_c <- read_delim("Tarea1/Datos/rendicion/archivoC/ArchivoC_Adm2025.csv", delim = ";", 
                                  locale = locale(decimal_mark = ","))
matricula <- read_delim("Tarea1/Datos/matricula/ArchivoMatr_Adm2025.csv", delim = ";", 
                        locale = locale(decimal_mark = ","))
inscripcion <- read_delim("Tarea1/Datos/inscripcion/archivoB/ArchivoB_Adm2025.csv", delim = ";",
                          locale = locale(decimal_mark = ","))


#Aquí creamos un nuevo df con lo que pide la tarea, creando una nueva variable para agregarla que es M2_FINAL, la cual
#es una nueva columna que considera el puntaje máx de el alumno en M2 en los diversos períodos donde dio la prueba
#Aparte de esa nueva columna, consideramos más variables que estaban en el df original y que nos van a servir para el lm(modelo lineal)
#Y por último borramos los valores nulos en M2_FINAL.
rendicion_clean <- rendicion_archivo_c %>%
  mutate(M2_FINAL = pmax(MATE2_REG_ACTUAL, MATE2_INV_ACTUAL, 
                         MATE2_REG_ANTERIOR, MATE2_INV_ANTERIOR, na.rm = TRUE)) %>%
  filter(!is.na(M2_FINAL)) %>% 
  select(ID_aux, M2_FINAL, PTJE_NEM, PTJE_RANKING)
# *) problema en caso todos NA, solucionar!!!!!
#----->POSIBLE SOLUCIÓN:
# rendicion_clean <- rendicion_archivo_c %>%
#   mutate(M2_FINAL = pmax(
#     MATE2_REG_ACTUAL, MATE2_INV_ACTUAL, 
#     MATE2_REG_ANTERIOR, MATE2_INV_ANTERIOR,
#     na.rm = TRUE
#   )) %>%
#   mutate(M2_FINAL = na_if(M2_FINAL, -Inf)) %>%  
#   filter(!is.na(M2_FINAL)) %>% 
#   select(ID_aux, M2_FINAL, PTJE_NEM, PTJE_RANKING)

#Observamos los promedios y medianas con los datos de gente que no dio su ingreso percapita
rendicionfiltrada <- rendicion_archivo_c %>%
  mutate(
    M2_FINAL = pmax(
      MATE2_REG_ACTUAL, MATE2_INV_ACTUAL, 
      MATE2_REG_ANTERIOR, MATE2_INV_ANTERIOR,
      na.rm = TRUE
    ),
    M2_FINAL = na_if(M2_FINAL, -Inf)  # corrige caso todos NA
  ) %>%
  select(ID_aux, M2_FINAL, PTJE_NEM, PTJE_RANKING) %>%
  filter(
    !is.na(M2_FINAL),
    M2_FINAL >= 100,
    PTJE_NEM >= 100,
    PTJE_RANKING >= 100
  )
summary(rendicionfiltrada)
#Nuevo df considerando solo los códigos de carreras tecnológicas o científicas.
carreras_stem <- postulacion_oferta_academica %>%
  filter(CAR_CIENCIAS_TECNOLOGIA == "S") %>%
  select(CODIGO_CARRERA)

#Hace match entre los alumnos de el df matricula y solo los que postularon a carreras STEM-
#Solo entrega el ID_aux de aquellos que cumplan el requisito.
postulantes_stem <- matricula %>%
  inner_join(carreras_stem, by = c("CODIGO" = "CODIGO_CARRERA")) %>%
  distinct(ID_aux)


#Base final que considera todas las variables anteriores
base_final <- rendicion_clean %>%
  #Une las tablas de inscripcion y postulantes stem ocupando ID_aux como llave primaria
  inner_join(postulantes_stem, by = "ID_aux") %>%
  inner_join(inscripcion, by = "ID_aux") %>%
  
  mutate(
    #Hace una limpieza en la var ingreso per capita, ya que esta va del 1 al 10 pero los que tienen 99 son los que no respondieron
    INGRESO_PERCAPITA_GRUPO_FA = ifelse(INGRESO_PERCAPITA_GRUPO_FA == 99, NA, INGRESO_PERCAPITA_GRUPO_FA),
    #En la base dice PACE en la casilla si entró con PACE, si no, aparece vacía, lo mismo con BEA
    #Acá transformamos en 0 si no entró por beca, 1 si sí y así trabajamos con números y no palabras
    PACE = ifelse(is.na(PACE), 0, 1),
    BEA  = ifelse(is.na(BEA), 0, 1)
  ) %>%
  
  #Filtro para eliminar datos nulos o sin sentido
  filter(
    !is.na(INGRESO_PERCAPITA_GRUPO_FA),
    !is.na(GRUPO_DEPENDENCIA),          
    !is.na(CODIGO_REGION),               
    M2_FINAL >= 100,  #El puntaje mín es 100, por lo que hay que eliminar los 0 que son los que no dieron la prueba
    PTJE_NEM >= 100, 
    PTJE_RANKING >= 100
  ) %>%
  
  #Elegimos las variables que estarán en la tabla
  select(
    M2_FINAL,                   
    INGRESO_PERCAPITA_GRUPO_FA, 
    PTJE_NEM,                   
    PTJE_RANKING,
    GRUPO_DEPENDENCIA,    #Vale 1 si es particular
    #Vale 2 si es particular subenciado
    #Vale 3 si es municipal
    #Vale 4 si es Servicio Local de Educación
    
    SEXO,                       
    CODIGO_REGION,              
    PACE,                       
    BEA                         
  )

#peos locos
#no mas xd


#Resumen de la base
summary(base_final)

#p3
# distribucion variable dependiente
hist(base_final$M2_FINAL,
     main = "Distribución Puntaje M2",
     xlab = "Puntaje M2")

# variables categoricas
#sexo
table(base_final$SEXO)
prop.table(table(base_final$SEXO))

#tipo de colegio
table(base_final$GRUPO_DEPENDENCIA)
prop.table(table(base_final$GRUPO_DEPENDENCIA))

#region
table(base_final$CODIGO_REGION)

#variables numericas
#ingreso
summary(base_final$INGRESO_PERCAPITA_GRUPO_FA)

#NEM y RANKING
summary(base_final$PTJE_NEM)
summary(base_final$PTJE_RANKING)

# diagramas de caja boxplot
boxplot(base_final$M2_FINAL, main="M2")
boxplot(base_final$PTJE_NEM, main="NEM")
boxplot(base_final$INGRESO_PERCAPITA_GRUPO_FA, main="Ingreso")


#variable dependiente (M2) con su variable independiente principal (Ingreso)
#los anteriores quedaban como muy simples y no decian mucho
boxplot(M2_FINAL ~ INGRESO_PERCAPITA_GRUPO_FA, 
        data = base_final,
        main = "Distribución de Puntaje M2 por Tramo de Ingreso",
        xlab = "Tramo de Ingreso Per Cápita",
        ylab = "Puntaje M2",
        col = "lightblue")

#p4
#creamos gráfico con variable dependiente M2 y variable independiente ingreso per capita
ggplot(base_final, aes(x = INGRESO_PERCAPITA_GRUPO_FA, y = M2_FINAL)) +
  geom_jitter(alpha = 0.3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relación entre ingreso per cápita y puntaje M2",
    x = "Tramo de ingreso per cápita",
    y = "Puntaje M2"
  )

ggplot(base_final, aes(x = INGRESO_PERCAPITA_GRUPO_FA, y = M2_FINAL)) +
  geom_jitter(alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ GRUPO_DEPENDENCIA) +
  labs(
    title = "Relación ingreso–puntaje M2 por tipo de establecimiento",
    x = "Ingreso per cápita",
    y = "Puntaje M2"
  )
#modelo 1, solo ingreso
modelo_0 <- lm(M2_FINAL ~ INGRESO_PERCAPITA_GRUPO_FA, data = base_final)
summary(modelo_0)

#modelo 1, solo ingreso
modelo_1 <- lm(M2_FINAL ~ INGRESO_PERCAPITA_GRUPO_FA + factor(GRUPO_DEPENDENCIA), data = base_final)
summary(modelo_1)

#modelo 2, agregamos medidas academicas
modelo_2 <- lm(M2_FINAL ~ INGRESO_PERCAPITA_GRUPO_FA + PTJE_NEM + factor(GRUPO_DEPENDENCIA), data = base_final)
summary(modelo_2)

#modelo 2, agregamos medidas academicas
modelo_5 <- lm(M2_FINAL ~ INGRESO_PERCAPITA_GRUPO_FA + PTJE_RANKING + factor(GRUPO_DEPENDENCIA), data = base_final)
summary(modelo_5)

#modelo 3, agregamos datos de la poblacion
modelo_3 <- lm(M2_FINAL ~ INGRESO_PERCAPITA_GRUPO_FA + PTJE_NEM +
                 factor(SEXO) + factor(CODIGO_REGION) + factor(GRUPO_DEPENDENCIA),
               data = base_final)
summary(modelo_3)

#modelo 4, el final considerando toas las variables
modelo_4 <- lm(M2_FINAL ~ INGRESO_PERCAPITA_GRUPO_FA + PTJE_NEM +
                 factor(SEXO) + factor(GRUPO_DEPENDENCIA) + factor(CODIGO_REGION) +
                 PACE,
               data = base_final)
summary(modelo_4)
stargazer(modelo_0, modelo_1, modelo_2, modelo_5, modelo_3, modelo_4,
          type = "text",
          df = FALSE)
