###########################################################################################
##############################      Información general      ##############################
###########################################################################################
# Autor: Luis Fernando Flores
# Fecha: 13/08/2025
#
# Descripción:
# Este script descarga datos de precios de gasolina desde la API de la IEA
# para un conjunto amplio de países. Los datos se almacenan en archivos JSON,
# luego se leen, combinan en un solo data.frame y finalmente se exportan
# a un archivo Excel para su posterior análisis.
###########################################################################################



###########################################################################################
##############################           Librerías           ##############################
###########################################################################################
library(httr)
library(jsonlite)
library(writexl)



###########################################################################################
##############################     Configuración inicial     ##############################
###########################################################################################
# Crear carpeta de salida si no existe
if (!dir.exists("datos_iea")) dir.create("datos_iea")



###########################################################################################
##############################        Lista de países        ##############################
###########################################################################################
paises <- c(
  "Afghanistan", "Africa", "Algeria", "Americas", "Andorra", "Argentina", "Armenia", "Asia",
  "Australia", "Austria", "Azerbaijan", "Bahrain", "Bangladesh", "Barbados", "Belarus", "Belgium",
  "Belize", "Benin", "Bolivarian Republic of Venezuela", "Botswana", "Brazil", "Bulgaria",
  "Burkina Faso", "Cabo Verde", "Cameroon", "Canada", "Chad", "Chile", "Chinese Taipei",
  "Colombia", "Costa Rica", "Cote d'Ivoire", "Croatia", "Cuba", "Cyprus", "Czech Republic",
  "Democratic Republic of the Congo", "Denmark", "Dominican Republic", "Ecuador", "Egypt",
  "El Salvador", "Estonia", "Ethiopia", "Europe", "European Union - 27", "Finland", "France",
  "G20", "Georgia", "Germany", "Ghana", "Greece", "Greenland", "Grenada", "Guatemala", "Guyana",
  "Haiti", "Honduras", "Hungary", "IEA and Accession/Association countries", "Iceland", "India",
  "Indonesia", "Ireland", "Islamic Republic of Iran", "Israel", "Italy", "Jamaica", "Japan",
  "Jordan", "Kazakhstan", "Kenya", "Korea", "Kyrgyzstan", "Lao People's Democratic Republic",
  "Latvia", "Lesotho", "Libya", "Lithuania", "Luxembourg", "Madagascar", "Malawi", "Malaysia",
  "Mali", "Malta", "Mauritius", "Mexico", "Mongolia", "Morocco", "Mozambique", "Namibia", "Nepal",
  "Netherlands", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Norway", "OECD", "Oceania",
  "Oman", "Pakistan", "Panama", "Papua New Guinea", "Paraguay", "People's Republic of China",
  "Peru", "Philippines", "Plurinational State of Bolivia", "Poland", "Portugal", "Qatar",
  "Republic of Moldova", "Republic of Türkiye", "Romania", "Russian Federation", "Rwanda",
  "Saudi Arabia", "Senegal", "Seychelles", "Singapore", "Slovak Republic", "Slovenia",
  "South Africa", "Spain", "Sri Lanka", "Sweden", "Switzerland", "Tajikistan", "Thailand",
  "Togo", "Trinidad and Tobago", "Tunisia", "Turkmenistan", "Uganda", "Ukraine",
  "United Arab Emirates", "United Kingdom", "United Republic of Tanzania", "United States",
  "Uruguay", "Uzbekistan", "Viet Nam", "World", "Zambia", "Zimbabwe"
)



###########################################################################################
##############################     Descarga de datos IEA     ##############################
###########################################################################################
# Recorrer todos los países
for (pais in paises) {
  pais_codificado <- URLencode(pais, reserved = TRUE)
  url <- paste0(
    "https://api.iea.org/prices?Country=", pais_codificado,
    "&CODE_INDICATOR=PRICE&CODE_SECTOR=TRANS&CODE_PRODUCT=GASOLINE&CODE_UNIT=USDCUR"
  )
  
  # Realizar la consulta
  res <- try(GET(url), silent = TRUE)
  
  # Revisar si fue exitosa
  if (!inherits(res, "try-error") && status_code(res) == 200) {
    contenido <- content(res, "text", encoding = "UTF-8")
    archivo <- paste0("datos_iea/", gsub("[ /]", "_", pais), ".json")
    writeLines(contenido, archivo)
    cat("✅ Guardado:", pais, "\n")
  } else {
    cat("❌ Error al descargar:", pais, "\n")
  }
  
  Sys.sleep(1)  # Espera 1 segundo entre solicitudes
}

cat("✔️ Descarga finalizada.\n")



###########################################################################################
##############################     Lectura de archivos       ##############################
###########################################################################################
# Leer lista de archivos
archivos <- list.files("datos_iea", pattern = "\\.json$", full.names = TRUE)



###########################################################################################
##############################     Unión de datos JSON       ##############################
###########################################################################################
# Leer y combinar todos los archivos
datos_todos <- lapply(archivos, function(archivo) {
  # Leer JSON
  datos <- fromJSON(archivo)
  
  # Si tiene datos, agregar columna del país (desde el nombre del archivo)
  if (length(datos) > 0) {
    pais <- gsub("datos_iea/|\\.json", "", archivo)
    pais <- gsub("_", " ", pais)
    datos$pais <- pais
    return(datos)
  } else {
    return(NULL)
  }
})

# Unir en un solo data.frame
datos_unidos <- bind_rows(datos_todos)



###########################################################################################
##############################     Limpieza y selección      ##############################
###########################################################################################
# Ver primeras filas
head(datos_unidos)

Precios_gasolina <- data.frame(
  pais = datos_unidos$pais,
  CODE_YEAR = datos_unidos$CODE_YEAR,
  Value = datos_unidos$Value
)



###########################################################################################
##############################     Exportación a Excel       ##############################
###########################################################################################
write_xlsx(
  Precios_gasolina,
  "C:/Users/Fernando Flores/OneDrive/Libros/MEFP/Proyecto/BIGprecios_gasolina2.xlsx"
)
