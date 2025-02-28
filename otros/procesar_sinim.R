library(dplyr)
library(readxl)
library(purrr)
library(janitor)
library(tidyr)
library(stringr)

# lista con nombres de comunas
cut_comunas <- read.csv2("comunas_chile_cut.csv") |> 
  mutate(cut_comuna = as.character(cut_comuna))

# vector con nombres de archivos descargados desde plataforma de sinim, una sola variable por archivo, puede ser un año o varios (para tapar datos faltantes)
archivos <- dir("datos_corregidos")

# loop por cada archivo descargado de sinim
datos_sinim <- map(archivos, ~{
  message(.x)
  
  # cargar archivo
  archivo <- paste0("datos_corregidos/", .x) |>
    read_excel()
  
  # obtener el nombre de la variable desde una de las celdas
  nombre_variable <- archivo[3][[1]][1]
  
  # limpiar archivos y pivotar a long
  archivo_1 <- archivo |> 
    row_to_names(2) |> 
    clean_names() |> 
    select(-municipio) |> 
    pivot_longer(cols = starts_with("x"), names_to = "año", values_to = "variable") |> 
    mutate(año = str_remove(año, "x"),
           variable = as.numeric(variable))
  
  # rellenar datos faltantes con años anteriores
  archivo_2 <- archivo_1 |> 
    fill(variable, .direction = "updown") |> 
    group_by(codigo) |> 
    slice_max(año) |> 
    ungroup()
  
  # agregar nombres de comunas y variables
  archivo_3 <- archivo_2 |> 
    mutate(nombre_variable) |> 
    rename(cut_comuna = codigo) |> 
    left_join(cut_comunas, by = "cut_comuna")
  
  return(archivo_3)
})
# retorna una lista

# volver a obtener nombres de variables
variables_lista <- datos_sinim |> 
  map(~select(.x, nombre_variable) |> pull() |> unique()) |> 
  unlist()

# nombrar la lista
names(datos_sinim) <- variables_lista

datos_sinim

# correciones ----
datos_sinim |> names()

# datos que vienen en porcentaje, pasar a proporción
# [16] "IADM02 (%) Participación de Ingresos Propios Permanentes (IPP) en el Ingreso Total"                                                                                                                      
# [17] "IADM75 (%) Dependencia del Fondo Común Municipal sobre los Ingresos Propios" 

datos_sinim[["IADM02 (%) Participación de Ingresos Propios Permanentes (IPP) en el Ingreso Total"]] <- datos_sinim[["IADM02 (%) Participación de Ingresos Propios Permanentes (IPP) en el Ingreso Total"]] |> 
  mutate(variable = variable/100)

datos_sinim[["IADM75 (%) Dependencia del Fondo Común Municipal sobre los Ingresos Propios"]] <- datos_sinim[["IADM75 (%) Dependencia del Fondo Común Municipal sobre los Ingresos Propios"]] |> 
  mutate(variable = variable/100)

datos_sinim[["RSHPORH40i (%) Porcentaje de Hogares de 0-40% de Ingresos respecto del Total Regional (RSH)"]] <- datos_sinim[["RSHPORH40i (%) Porcentaje de Hogares de 0-40% de Ingresos respecto del Total Regional (RSH)"]] |> 
  mutate(variable = variable/100)



# guardar ----
saveRDS(datos_sinim, "datos_sinim.rds", compress = F)
