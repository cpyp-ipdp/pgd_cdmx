##Estimación de indicadores de población indígena en la Ciudad de México
#Se borra todo lo que se encuentra en el entorno

rm(list=ls())

# Librerías ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, showtext, srvyr, haven, googledrive)

#Crear directorio para guardar los archivos
dir.create("data", showWarnings = FALSE)

url_basica<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/"
segunda_parte<-"microdatos/enigh"
fin1<-"_ns_"
fin2<-"_csv.zip"

years<-c("2016", "2018", "2020")

# Función genérica para descargar, descomprimir, renombrar y eliminar
procesar_archivo <- function(year, tipo, url_base, segunda_parte, fin1, fin2, carpeta_destino) {
  # Construir URL y ruta de destino
  url <- paste0(url_base, year, "/", segunda_parte, year, fin1, tipo, fin2)
  destfile <- paste0(carpeta_destino, "/", year, fin1, tipo, fin2)
  
  # Descargar el archivo
  download.file(url, destfile = destfile)
  
  # Descomprimir el archivo
  unzip(destfile, exdir = carpeta_destino)
  
  # Renombrar el archivo descomprimido
  file.rename(
    from = paste0(carpeta_destino, "/", tipo, ".csv"),
    to = paste0(carpeta_destino, "/", tipo, "_", year, ".csv")
  )
  
  # Eliminar el archivo descargado
  file.remove(destfile)
}

# Iterar sobre los años y procesar cada tipo de archivo
for (i in 1:length(years)) {
  procesar_archivo(years[i], "poblacion", url_basica, segunda_parte, fin1, fin2, "data")
  procesar_archivo(years[i], "viviendas", url_basica, segunda_parte, fin1, fin2, "data")
  procesar_archivo(years[i], "trabajos", url_basica, segunda_parte, fin1, fin2, "data")
}

estimacion <- function(path, variable) {
  # Cargar los datos de población
  datos <- read_csv(path, show_col_types = FALSE)
  
  # Si son datos de 2016 a 2020, unir con la tabla de viviendas
  if (substr(path, 16, 19) %in% c("2016", "2018", "2020")) {
    # Cargar tabla viviendas
    vivienda <- read_csv(str_replace(path, "poblacion", "viviendas"), show_col_types = FALSE)
    
    # Pegar factor, upm y est_dis de la tabla vivienda
    datos <- datos %>%
      left_join(vivienda %>% select(folioviv, factor, upm, est_dis), by = "folioviv")
  }
  
  # Unir con la tabla trabajos si es "ocupacion" o si la persona debe ser identificada como indígena
  if (variable %in% c("ocupacion", "ocupacion_mujer")) {
    # Cargar tabla trabajos
    trabajos <- read_csv(str_replace(path, "poblacion", "trabajos"), show_col_types = FALSE)
    
    # Pegar datos relevantes de la tabla trabajos
    datos <- datos %>%
      left_join(trabajos %>% select(folioviv, foliohog, numren, id_trabajo, trapais),
                by = c("folioviv", "foliohog", "numren"))
  }
  
  # Crear variable cve_ent
  datos <- datos %>%
    mutate(cve_ent = case_when(
      nchar(folioviv) == 9  ~ paste0("0", substr(folioviv, 1, 1)),
      nchar(folioviv) == 10 ~ substr(folioviv, 1, 2)
    )) %>%
    # Filtrar si entidad es CDMX (09)
    filter(cve_ent == "09")
  
  # Definir diseño muestral
  mydesign <- datos %>%
    as_survey_design(ids = upm,
                     strata = est_dis,
                     weights = factor)

  # Estimar población hablante indígena en CDMX
  if (variable == "hablante") {
    hablante <- mydesign %>%
      group_by(hablaind) %>%
      summarise(
        poblacion = survey_total(vartype = "cv"),
        porcentaje = survey_prop(vartype = "cv")
      ) %>%
      mutate(porcentaje = round(porcentaje * 100, 2)) %>%
      # Añadir año
      mutate(year = substr(path, 16, 19)) %>%
      filter(hablaind == 1)
    
    return(hablante)
  }
  
  # Estimar población hablante indígena en CDMX
  if (variable == "indigena") {
    indigena <- mydesign %>%
      group_by(etnia) %>%
      summarise(
        poblacion = survey_total(vartype = "cv"),
        porcentaje = survey_prop(vartype = "cv")
      ) %>%
      mutate(porcentaje = round(porcentaje * 100, 2)) %>%
      # Añadir año
      mutate(year = substr(path, 16, 19)) %>%
      filter(etnia == 1)
    
    return(indigena)
  }
  
  
  # Estimar población ocupada, considerando si la persona es indígena
  if (variable == "ocupacion") {
    ocupacion <- mydesign %>%
      group_by(trapais,etnia) %>%
      summarise(
        ocupados = survey_total(vartype = "cv"),
        porcentaje = survey_prop(vartype = "cv")
      ) %>%
      mutate(porcentaje = round(porcentaje * 100, 2)) %>%
      # Añadir año
      mutate(year = substr(path, 16, 19)) %>%
      filter(trapais == 1)%>%
      filter(etnia == 1)
    return(ocupacion)
  }
  
  
  
  # Estimar población ocupada, considerando si la persona es indígena
  if (variable == "ocupacion_mujer") {
    ocupacion_muj <- mydesign %>%
      filter(trapais==1)%>%
      group_by(etnia,sexo) %>%
      summarise(
        ocupados = survey_total(vartype = "cv"),
        porcentaje = survey_prop(vartype = "cv")
      ) %>%
      mutate(porcentaje = round(porcentaje * 100, 2)) %>%
      # Añadir año
      mutate(year = substr(path, 16, 19)) %>%
      #Filtro población indígena
      filter(etnia == 1) %>%
      #Filtro mujer
      filter(sexo==2)
    return(ocupacion_muj)
  }
  
  
  # Estimar población analfabeta
  if (variable == "analfabetismo") {
    analfa <- mydesign %>%
      group_by(etnia,alfabetism) %>%
      summarise(
        analfabetas = survey_total(vartype = "cv"),
        porcentaje = survey_prop(vartype = "cv")
      ) %>%
      mutate(porcentaje = round(porcentaje * 100, 2)) %>%
      # Añadir año
      mutate(year = substr(path, 16, 19)) %>%
      filter(etnia == 1)%>%
      filter(alfabetism == 2)
    return(analfa)
  }
  
}

#Población hablante indígena en CDMX
tabla_hablante<-map_dfr(
  list.files("data", full.names = TRUE, pattern = "poblacion_.*.csv"),
  estimacion,
  variable = "hablante"
)
print("Población hablante de lengua indígena en la CDMX")
tabla_hablante

#Población que se autoadscribe indígena en CDMX
tabla_indigena<-map_dfr(
  list.files("data", full.names = TRUE, pattern = "poblacion_.*.csv"),
  estimacion,
  variable = "indigena"
)
print("Porcentaje de población que se autodescribe como indígena en la CDMX")
tabla_indigena

#Población ocupada indígena en CDMX
tabla_ocupacion<-map_dfr(
  list.files("data", full.names = TRUE, pattern = "poblacion_.*.csv"),
  estimacion,
  variable = "ocupacion"
)
print("Población que se autodescribe como indígena y que se encuentra ocupada en la CDMX")
tabla_ocupacion

#Población ocupada indígena mujer en CDMX 
tabla_ocupacion_mujer<-map_dfr(
  list.files("data", full.names = TRUE, pattern = "poblacion_.*.csv"),
  estimacion,
  variable = "ocupacion_mujer"
)
print("Población que se autodescribe como indígena que es mujer y que se encuentra ocupada en la CDMX")
tabla_ocupacion_mujer

#Población analfabeta indígena en CDMX 
tabla_analfabetismo<-map_dfr(
  list.files("data", full.names = TRUE, pattern = "poblacion_.*.csv"),
  estimacion,
  variable = "analfabetismo"
)
print("Población que se autodescribe como indígena y que es analfabeta en la CDMX")
tabla_analfabetismo

##Estimación de metas 
#Metas población indígena analfabeta 
#Tabla de analfabetismo
datos<-data.frame(tabla_analfabetismo$year,tabla_analfabetismo$porcentaje)%>%
  rename(year=tabla_analfabetismo.year,
         porcentaje=tabla_analfabetismo.porcentaje)%>%
  mutate(year=as.numeric(year))
datos

##Calcular tasa de crecimiento promedio anual de 2016 a 2022
valor_inicial<-datos$porcentaje[1]
valor_final <- datos$porcentaje[length(datos$porcentaje)]
n <- datos$year[length(datos$year)] - datos$year[1]

tasa_crecimiento <- (valor_final / valor_inicial)^(1 / n) - 1
print("Se estima una tasa de crecimiento promedio anual para utilizarse como base en la proyección de metas a 2025")
tasa_crecimiento

proyecciones <- data.frame(year = seq(2023, 2045, by = 1))
proyecciones$porcentaje <- valor_final * (1 + tasa_crecimiento)^(proyecciones$year - 2022)
proyecciones

datos_proyectados <- rbind(datos, proyecciones)

#Gráfica
ggplot(datos_proyectados, aes(x = year, y = porcentaje)) +
  geom_line() +
  geom_point() +
  labs(title = "Porcentaje de población indígena analfabeta en la CDMX",
       x = "Año",
       y = "Porcentaje") +
  theme_minimal()

#Datos 2030, 2035 y 2045
datos_proyectados %>% filter(year %in% c(2030, 2035, 2045))

#Metas población hablante indígena 
#Tabla de Población hablante de lengua indígena en la CDMX
hablante<-data.frame(tabla_hablante$year,tabla_hablante$porcentaje)%>%
  rename(year=tabla_hablante.year,
         porcentaje=tabla_hablante.porcentaje)%>%
  mutate(year=as.numeric(year))
hablante

##Calcular tasa de crecimiento promedio anual de 2016 a 2022
valor_inicial_hablante<-hablante$porcentaje[1]
valor_final_hablante <- hablante$porcentaje[length(hablante$porcentaje)]
n <- hablante$year[length(hablante$year)] - hablante$year[1]

tasa_crecimiento_hablante <- (valor_final_hablante / valor_inicial_hablante)^(1 / n) - 1
print("Se estima una tasa de crecimiento promedio anual para utilizarse como base en la proyección de metas a 2025")
tasa_crecimiento_hablante

proyecciones_hablante <- data.frame(year = seq(2023, 2045, by = 1))
proyecciones_hablante$porcentaje <- valor_final_hablante * (1 + tasa_crecimiento_hablante)^(proyecciones_hablante$year - 2022)
proyecciones_hablante

datos_proyectados_hablante <- rbind(hablante, proyecciones_hablante)

#Gráfica
ggplot(datos_proyectados_hablante, aes(x = year, y = porcentaje)) +
  geom_line() +
  geom_point() +
  labs(title = "Población hablante de lengua indígena en la CDMX",
       x = "Año",
       y = "Porcentaje") +
  theme_minimal()

#Datos 2030, 2035 y 2045
datos_proyectados_hablante %>% filter(year %in% c(2030, 2035, 2045))

#Metas población adscribe indígena 
#Tabla de Población que se auto adscribe indígena en la CDMX
adscribe <- data.frame(tabla_indigena$year, tabla_indigena$porcentaje) %>%
  rename(year = tabla_indigena.year,
         porcentaje = tabla_indigena.porcentaje) %>%
  mutate(year = as.numeric(year))

# Excluir el año 2022 para el cálculo de la tasa de crecimiento
adscribe_sin_2022 <- adscribe %>% filter(year != 2022)

## Calcular tasa de crecimiento promedio anual de 2016 a 2021 (sin incluir 2022)
valor_inicial_adscribe <- adscribe_sin_2022$porcentaje[1]
valor_final_adscribe <- adscribe_sin_2022$porcentaje[length(adscribe_sin_2022$porcentaje)]
n <- adscribe_sin_2022$year[length(adscribe_sin_2022$year)] - adscribe_sin_2022$year[1]

tasa_crecimiento_adscribe <- (valor_final_adscribe / valor_inicial_adscribe)^(1 / n) - 1
print("Se estima una tasa de crecimiento promedio anual para utilizarse como base en la proyección de metas a 2025")
tasa_crecimiento_adscribe

proyecciones_adscribe <- data.frame(year = seq(2023, 2045, by = 1))
proyecciones_adscribe$porcentaje <- valor_final_adscribe * (1 + tasa_crecimiento_adscribe)^(proyecciones_adscribe$year - 2020)
proyecciones_adscribe

datos_proyectados_adscribe <- rbind(adscribe, proyecciones_adscribe)

#Gráfica
ggplot(datos_proyectados_adscribe, aes(x = year, y = porcentaje)) +
  geom_line() +
  geom_point() +
  labs(title = "Población que se auto adscribe indígena en la CDMX",
       x = "Año",
       y = "Porcentaje") +
  theme_minimal()

#Datos 2030, 2035 y 2045
datos_proyectados_adscribe %>% filter(year %in% c(2030, 2035, 2045))

#Cálculo de la tendencia (regresión lineal) 
# Asegurarse de que la variable "year" es numérica
tabla_indigena$year <- as.numeric(tabla_indigena$year)

# Crear un modelo de regresión lineal usando lm()
modelo <- lm(porcentaje ~ year, data = tabla_indigena)

# Mostrar el resumen del modelo para ver la relación y los coeficientes
summary(modelo)

# Predecir para los años 2023, 2030, 2045
predicciones <- data.frame(year = c(2030, 2035, 2045))
predicciones$prediccion <- predict(modelo, newdata = predicciones)

# Ver las predicciones
print(predicciones)

#Metas población ocupada 
#Tabla de Población ocupada indígena en la CDMX
ocupada <- data.frame(tabla_ocupacion$year, tabla_ocupacion$porcentaje) %>%
  rename(year = tabla_ocupacion.year,
         porcentaje = tabla_ocupacion.porcentaje) %>%
  mutate(year = as.numeric(year))

# Excluir el año 2022 para el cálculo de la tasa de crecimiento
ocupada_sin_2022 <- ocupada %>% filter(year != 2022)

## Calcular tasa de crecimiento promedio anual de 2016 a 2021 (sin incluir 2022)
valor_inicial_ocupada <- ocupada_sin_2022$porcentaje[1]
valor_final_ocupada <- ocupada_sin_2022$porcentaje[length(ocupada_sin_2022$porcentaje)]
n <- ocupada_sin_2022$year[length(ocupada_sin_2022$year)] - ocupada_sin_2022$year[1]

tasa_crecimiento_ocupada <- (valor_final_ocupada / valor_inicial_ocupada)^(1 / n) - 1
print("Se estima una tasa de crecimiento promedio anual para utilizarse como base en la proyección de metas a 2025")
tasa_crecimiento_ocupada

proyecciones_ocupada <- data.frame(year = seq(2023, 2045, by = 1))
proyecciones_ocupada$porcentaje <- valor_final_ocupada * (1 + tasa_crecimiento_ocupada)^(proyecciones_ocupada$year - 2021)
proyecciones_ocupada

datos_proyectados_ocupada <- rbind(ocupada, proyecciones_ocupada)

#Gráfica
ggplot(datos_proyectados_ocupada, aes(x = year, y = porcentaje)) +
  geom_line() +
  geom_point() +
  labs(title = "Población ocupada en pueblos originarios en la CDMX",
       x = "Año",
       y = "Porcentaje") +
  theme_minimal()

#Datos 2030, 2035 y 2045
datos_proyectados_ocupada %>% filter(year %in% c(2030, 2035, 2045))


#Metas población ocupada mujeres
#Tabla de Población ocupada indígena mujeres en la CDMX
ocupadam <- data.frame(tabla_ocupacion_mujer$year, tabla_ocupacion_mujer$porcentaje) %>%
  rename(year = tabla_ocupacion_mujer.year,
         porcentaje = tabla_ocupacion_mujer.porcentaje) %>%
  mutate(year = as.numeric(year))

# Excluir el año 2022 para el cálculo de la tasa de crecimiento
ocupadam_sin_2022 <- ocupadam %>% filter(year != 2022)

## Calcular tasa de crecimiento promedio anual de 2016 a 2021 (sin incluir 2022)
valor_inicial_ocupadam <- ocupadam_sin_2022$porcentaje[1]
valor_final_ocupadam <- ocupadam_sin_2022$porcentaje[length(ocupadam_sin_2022$porcentaje)]
n <- ocupadam_sin_2022$year[length(ocupadam_sin_2022$year)] - ocupadam_sin_2022$year[1]

tasa_crecimiento_ocupadam <- (valor_final_ocupadam / valor_inicial_ocupadam)^(1 / n) - 1
print("Se estima una tasa de crecimiento promedio anual para utilizarse como base en la proyección de metas a 2025")
tasa_crecimiento_ocupadam

proyecciones_ocupadam <- data.frame(year = seq(2023, 2045, by = 1))
proyecciones_ocupadam$porcentaje <- valor_final_ocupadam * (1 + tasa_crecimiento_ocupadam)^(proyecciones_ocupadam$year - 2021)
proyecciones_ocupadam

datos_proyectados_ocupadam <- rbind(ocupadam, proyecciones_ocupadam)

#Gráfica
ggplot(datos_proyectados_ocupadam, aes(x = year, y = porcentaje)) +
  geom_line() +
  geom_point() +
  labs(title = "Población ocupada mujer en pueblos originarios en la Ciudad",
       x = "Año",
       y = "Porcentaje") +
  theme_minimal()

#Datos 2030, 2035 y 2045
datos_proyectados_ocupadam %>% filter(year %in% c(2030, 2035, 2045))


