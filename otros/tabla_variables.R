datos_sinim |> 
  distinct(area, subarea, variable) |> 
  knitr::kable(format = "markdown") |> # crear tabla en markdown
  clipr::write_clip() # copiar 
