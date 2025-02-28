library(purrr)

# los archivos descargados de sinim vienen en un formato excel medio raro,
# así que hay que abrirlos con excel y guardarlos, de lo contrario usar este comando para arreglarlos con libreOffice

archivos <- dir("datos")

map(archivos, ~system(paste0("/Applications/LibreOffice.app/Contents/MacOS/soffice  --headless --convert-to xls --outdir datos_corregidos datos/", .x)))

# además hay que revisar manualmente los datos, por ejemplo, en metros cuadrados de parque urbano, La Pintana está errado por orden de magnitud (MMPQC (MTS²) Superficie Total (m2) de Parques Urbanos Existentes en la Comuna (a contar del 2010))