library(tidyverse)
source("funciones.R")

# cargar resultado del scraping
datos_sinim <- readr::read_rds("datos/datos_originales/sinim_scraping_2019-2023.rds")

# revisar variables
datos_sinim |> 
  distinct(variable, area, subarea)

# revisiar municipios
datos_sinim |> 
  distinct(municipio, municipio_name)

datos_sinim |> distinct(unit)  

# agregar códigos únicos territoriales
datos_sinim_2 <- datos_sinim |> 
  left_join(municipios |> select(id_municipio, cut_comuna = idLegal),
            by = join_by(municipio == id_municipio))

# limpieza
datos_sinim_3 <- datos_sinim_2 |> 
  # sacar columnas irrelevantes
  select(-class_type, -col_info) |> 
  # reordenar columnas de municipio
  select(-municipio) |> 
  rename(municipio = municipio_name) |> 
  relocate(municipio, cut_comuna, contains("year"), .before = var_code) |> 
  # limpiar variables
  mutate(unit = str_trim(unit)) |> 
  # convertir cifras a numérico
  mutate(value = parse_number(value, locale = locale(decimal_mark = ",", grouping_mark = ".")))

# renombrar
datos_sinim_4 <- datos_sinim_3 |> 
  rename(año_id = sinim_year_code,
         año = user_year,
         variable_id = var_code,
         variable_desc = description,
         unidad = unit,
         valor = value)

# guardar ----
datos_sinim_4 |> arrow::write_parquet("datos/sinim_2019-2023.parquet")
datos_sinim_4 |> readr::write_rds("datos/sinim_2019-2023.rds", compress = "gz")
datos_sinim_4 |> writexl::write_xlsx("datos/sinim_2019-2023.xlsx")
