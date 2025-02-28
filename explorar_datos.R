sinim <- arrow::read_parquet("datos/sinim_2022_2023.parquet")


sinim |> 
  count(variable, area, subarea)


sinim |> 
  filter(año == 2023) |> 
  filter(variable_id == 887) |> 
  filter(municipio == "LA FLORIDA")
  
