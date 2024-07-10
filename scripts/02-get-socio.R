# poverty_path <- "data/raw/poverty/"

# Monetary poverty --------------------------------------------------------
# Mapa de Pobreza Distrital y Provincial 2018
# monetary_poverty_url <- "https://cdn.www.gob.pe/uploads/document/file/3340937/Anexo%20Estad%C3%ADstico.xlsx"
# monetary_poverty_filename <- "monetary-poverty.xlsx"
# monetary_poverty_filepath <- fs::path(poverty_path, monetary_poverty_filename)
# download.file(monetary_poverty_url, monetary_poverty_filepath, mode = "wb")

# Non-monetary poverty ----------------------------------------------------
# Mapa de Necesidades Insatisfechas
# nonmonetary_poverty_url <- "https://www.inei.gob.pe/media/MenuRecursivo/publicaciones_digitales/Est/Lib1588/cuadros/01_1C.xlsx"
# nonmonetary_poverty_filename <- "nonmonetary-poverty.xlsx"
# nonmonetary_poverty_filepath <- fs::path(poverty_path, nonmonetary_poverty_filename)
# download.file(nonmonetary_poverty_url, nonmonetary_poverty_filepath, mode = "wb")

# Population --------------------------------------------------------------
population_url <- "https://cdn.www.gob.pe/uploads/document/file/5501252/3464927-anexo-1.xlsx?v=1701294767"
population_filepath <- "data/raw/socio/population.xlsx"
download.file(population_url, population_filepath, mode = "wb")


# Districts ---------------------------------------------------------------
districts_url <- "https://ide.inei.gob.pe/files/Distrito.rar"
districts_filepath <- "data/raw/districts.rar"
download.file(districts_url, districts_filepath, mode = "wb")


