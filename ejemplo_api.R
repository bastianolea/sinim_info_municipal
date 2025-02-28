library(tidyverse)
source("funciones.R")

variables <- sinim_obtener_variables()

# ver todas las áreas
variables |> distinct(area, subarea) |> print(n=Inf)

variables_educacion <- variables %>%
  filter(area == "03.  EDUCACION MUNICIPAL")

# With progress bar and debug messages
datos_educacion <- sinim_obtener_datos(
  var_codes        = variables_educacion$code[1],
  years            = c(2001:2023),
  municipios       = "183", # IQUIQUE
  corrmon          = TRUE,
  limit            = 5000,
  region           = "T",
  parallel_workers = 4,
  show_progress    = TRUE
)

datos_educacion |> 
  distinct(variable, area, subarea)

datos_educacion |> 
  filter(user_year > 2010) |> 
  select(variable, user_year, value)
