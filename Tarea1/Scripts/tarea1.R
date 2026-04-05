#install.packages(c("dplyr", "stargazer", "readr"))

#Liberías necesarias para el script
library(dplyr)
library(readr)   
library(readxl)     
library(stargazer)  


#Aquí definimos los dataframes que nos van a servir de la carpeta datos, puede que dsp agreguemos más.
postulacion_oferta_academica <- read_excel("Datos/postulacion/ofertaAcademica/OfertaAcadémica_Admisión2025.xlsx")
rendicion_archivo_c <- read_delim("Datos/rendicion/archivoC/ArchivoC_Adm2025.csv", delim = ";")
matricula <- read_delim("Datos/matricula/ArchivoMatr_Adm2025.csv", delim = ";")
inscripcion <- read_delim("Datos/inscripcion/archivoB/ArchivoB_Adm2025.csv", delim = ";")


#Aquí creamos un nuevo df con lo que pide la tarea, creando una nueva variable para agregarla que es M2_FINAL, la cual
#es una nueva columna que considera el puntaje máx de el alumno en M2 en los diversos períodos donde dio la prueba
#Aparte de esa nueva columna, consideramos más variables que estaban en el df original y que nos van a servir para el lm
#Y por último borramos los valores nulos en M2_FINAL
rendicion_clean <- rendicion_archivo_c %>%
  mutate(M2_FINAL = pmax(MATE2_REG_ACTUAL, MATE2_INV_ACTUAL, 
                         MATE2_REG_ANTERIOR, MATE2_INV_ANTERIOR, na.rm = TRUE)) %>%
  filter(!is.na(M2_FINAL)) %>% 
  select(ID_aux, M2_FINAL, PTJE_NEM, PTJE_RANKING)


#Nuevo df considerando solo los códigos de carreras tecnológicas o científicas.
carreras_stem <- postulacion_oferta_academica %>%
  filter(CAR_CIENCIAS_TECNOLOGIA == "S") %>%
  select(CODIGO_CARRERA)

#Hace match entre los alumnos de el df matricula y solo los que postularon a carreras STEM
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
