# Limpieza del entorno
rm(list=ls())
gc()

# Instalar y cargar librerías
#install.packages(c("DBI", "RSQLite", "readxl", "dplyr", "ggplot2", "rmarkdown"))
library(DBI)
library(RSQLite)
library(readxl)
library(dplyr)
library(ggplot2)
library(rmarkdown)
library(stringr)
library(janitor)

if (!requireNamespace("rstudioapi", quietly = TRUE)) {
  install.packages("rstudioapi")
}

# Establecer el directorio de trabajo
set_working_directory_to_script_path <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE)) {
    script_path <- rstudioapi::getActiveDocumentContext()$path
    if (nchar(script_path) > 0) {
      script_dir <- dirname(script_path)
      setwd(script_dir)
      message("Directorio de trabajo cambiado a: ", script_dir)
    } else {
      warning("No se pudo obtener la ruta del script. Asegúrate de estar ejecutando esto en RStudio.")
    }
  } else {
    warning("El paquete 'rstudioapi' no está disponible.")
  }
}

# Ejecuta la función
set_working_directory_to_script_path()

# Conectar a la base de datos SQLite
con <- dbConnect(RSQLite::SQLite(), "Output/Adres_Prueba_Tecnica.sqlite")

# Ruta a los archivos .xlsx
ruta <- "input/"
archivos <- c(file.path(ruta, "Municipios.xlsx"), file.path(ruta, "Prestadores.xlsx"), file.path(ruta, "DANE.xlsx"))

# Leer y cargar las hojas de los archivos Excel en SQLite
for (archivo in archivos) {
  # Obtener los nombres de las hojas del archivo Excel
  hojas <- readxl::excel_sheets(archivo)

  # Leer cada hoja y cargarla en SQLite
  for (hoja in hojas) {
    nombre_tabla <- paste(tools::file_path_sans_ext(basename(archivo)), hoja, sep = "_")
    data <- readxl::read_excel(archivo, sheet = hoja)
    dbWriteTable(con, nombre_tabla, data, overwrite = TRUE)
  }
}

# Extraer datos de la tabla Municipios
query_municipios <- "SELECT * FROM Municipios_Minicipios"
datos_municipios <- dbGetQuery(con, query_municipios)

# Extraer datos de la tabla Prestadores
query_prestadores <- "SELECT * FROM Prestadores_Prestadores"
datos_prestadores <- dbGetQuery(con, query_prestadores)

# Extraer datos de la tabla DANE
query_DANE <- "SELECT * FROM DANE_DANE"
datos_DANE <- dbGetQuery(con, query_DANE)

# Función para eliminar tildes, cambiar 'Ñ' por 'N', y quitar espacios en blanco al inicio o final
eliminar_tildes <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- str_replace_all(x, "Ñ", "N")
  x <- str_replace_all(x, " D.C", "")
  x <- str_replace_all(x, "\\.", "")  # Escapar el punto con doble barra
  x <- str_replace_all(x, "ñ", "n")
  x <- str_trim(x)
  return(x)
}

# Limpiar y transformar la columna Municipio
datos_municipios <- datos_municipios %>%
  mutate(Departamento = str_to_upper(Departamento) %>% str_replace_all("%", "") %>% str_replace_all("&", "A") %>% str_replace_all("!", "") %>% str_replace_all("'", "") %>% str_replace_all("\\*", "") %>% str_replace_all("\\?", "") %>% str_replace_all("#", ""),
         Municipio = str_to_upper(eliminar_tildes(Municipio)),
         Departamento = str_to_upper(eliminar_tildes(Departamento)))

datos_DANE <- datos_DANE %>%
  mutate(across(where(is.character), ~str_to_upper(eliminar_tildes(.))))

# Join de los dos datasets usando el código del municipio
datos_municipios_limpio <- datos_municipios %>%
  left_join(datos_DANE, by = c("Depmun" = "DPTOC_MPIO"))

# Reemplazar las columnas de datos_municipios con las limpias de datos_DANE
datos_municipios_limpio <- datos_municipios_limpio %>%
  mutate(Departamento = NOM_DEPTO,
         Municipio = NOM_MPIO,
         Region = PROVINCIA) %>%
  select(Departamento, Dep, Municipio, Depmun, Superficie, Poblacion, Irural, Region)

# Eliminar tildes y convertir a mayúsculas la columna Municipio después del reemplazo
datos_municipios_limpio <- datos_municipios_limpio %>%
  mutate(Municipio = str_to_upper(eliminar_tildes(Municipio)))

# Aplicar la eliminación de tildes y conversión a mayúsculas a todas las columnas de tipo chr
datos_prestadores <- datos_prestadores %>%
  mutate(across(where(is.character), ~str_to_upper(eliminar_tildes(.))))

# Función para convertir números en formato 'YYYYMMDD' a fechas
convertir_fecha <- function(fecha) {
  as.Date(as.character(fecha), format = "%Y%m%d")
}

# Aplicar la función de conversión a las columnas fecha_radicacion y fecha_vencimiento
datos_prestadores <- datos_prestadores %>%
  mutate(fecha_radicacion = convertir_fecha(fecha_radicacion),
         fecha_vencimiento = convertir_fecha(fecha_vencimiento))

# Seleccionar datos de analisis
datos_prestadores_seleccionados <- datos_prestadores %>%
  select(depa_nombre, muni_nombre, codigo_habilitacion, nombre_prestador,
         nits_nit, clpr_nombre, fecha_radicacion, fecha_vencimiento,
         clase_persona, numero_sede_principal)

# Realizar las modificaciones especificadas
datos_prestadores_seleccionados <- datos_prestadores_seleccionados %>%
  mutate(muni_nombre = if_else(depa_nombre == "BOLIVAR" & muni_nombre == "SANTA ROSA", "SANTA ROSA DE LIMA", muni_nombre),
         muni_nombre = if_else(depa_nombre == "META" & muni_nombre == "CUBARRAL", "CUMARAL", muni_nombre))

# ARREGLO MANUAL DADO QUE EN PRESTACION NO HAY COINCIDENCIA CON DANE
# [1] "ANTIOQUIA EL CARMEN DE VIBORAL"           "ANTIOQUIA EL SANTUARIO"
# [3] "BARRANQUILLA BARRANQUILLA"                "BOLIVAR EL CARMEN DE BOLIVAR"
# [5] "BOLIVAR SANTA ROSA"                       "BOYACA SAN PABLO DE BORBUR"
# [7] "BOYACA SANTA ROSA DE VITERBO"             "BUENAVENTURA BUENAVENTURA"
# [9] "CALI CALI"                                "CAQUETA CURILLO"
# [11] "CARTAGENA CARTAGENA"                      "CAUCA GUACHENE"
# [13] "CHOCO EL CANTON DEL SAN PABLO"            "CHOCO ISTMINA"
# [15] "CUNDINAMARCA SAN ANTONIO DEL TEQUENDAMA"  "CUNDINAMARCA VILLA DE SAN DIEGO DE UBATE"
# [17] "MAGDALENA PUEBLOVIEJO"                    "META CUBARRAL"
# [19] "META SAN CARLOS DE GUAROA"                "META VISTAHERMOSA"
# [21] "NARINO SANTACRUZ"                         "PUTUMAYO LEGUIZAMO"
# [23] "PUTUMAYO VILLAGARZON"                     "QUINDIO MONTENEGRO"
# [25] "SAN ANDRES Y PROVIDENCIA PROVIDENCIA"     "SAN ANDRES Y PROVIDENCIA SAN ANDRES"
# [27] "SANTA MARTA SANTA MARTA"                  "SUCRE SAN JUAN DE BETULIA"
# [29] "VALLE DEL CAUCA GUADALAJARA DE BUGA"

# "CAUCA GUACHENE" NO FUE ENCONTRADO MANUALMENTE

# Renombrar columnas para facilitar la unión
datos_municipios_limpio <- datos_municipios_limpio %>%
  rename(depa_nombre = Departamento,
         muni_nombre = Municipio)

# Unir los dataframes usando las columnas comunes
datos_combinados <- datos_prestadores_seleccionados %>%
  left_join(datos_municipios_limpio, by = c("depa_nombre", "muni_nombre"))

# Agregar una variable que indique si se encontró la coincidencia o no
datos_combinados <- datos_combinados %>%
  mutate(coincidencia = if_else(is.na(Dep), "No encontrado", "Encontrado"))

# Crear nuevas tablas en la base de datos con el prefijo "limpia_"
dbWriteTable(con, "limpia_Municipios", datos_municipios_limpio, overwrite = TRUE)
dbWriteTable(con, "limpia_Prestadores", datos_combinados, overwrite = TRUE)

# Crear la tabla compilado en SQLite
dbExecute(con, "
  CREATE TABLE compilado AS
  SELECT
    a.depa_nombre,
    a.muni_nombre,
    a.codigo_habilitacion,
    a.nombre_prestador,
    a.nits_nit,
    a.clpr_nombre,
    a.fecha_radicacion,
    a.fecha_vencimiento,
    a.clase_persona,
    a.numero_sede_principal,
    b.Dep,
    b.Depmun,
    b.Superficie,
    b.Poblacion,
    b.Irural,
    b.Region,
    CASE WHEN b.Dep IS NULL THEN 'No encontrado' ELSE 'Encontrado' END AS coincidencia
  FROM limpia_Prestadores a
  LEFT JOIN limpia_Municipios b
  ON a.depa_nombre = b.depa_nombre AND a.muni_nombre = b.muni_nombre
")

# Extraer datos de la tabla completo
query_completo <- "SELECT * FROM compilado"
query_completo <- dbGetQuery(con, query_completo)

# Cerrar la conexión a la base de datos
dbDisconnect(con)

################################################################################
if (!requireNamespace("DataExplorer", quietly = TRUE)) {
  install.packages("DataExplorer")
}

library(DataExplorer)
library(dplyr)

# Función para realizar EDA en un data frame y guardar el reporte
generar_reporte_eda <- function(data, nombre) {
  tryCatch({
    create_report(
      data,
      config = configure_report(
        add_intro = TRUE,
        add_plot_str = TRUE,
        add_plot_missing = TRUE,
        add_plot_density = TRUE,
        add_plot_qq = TRUE,
        add_plot_bar = TRUE,
        add_plot_scatterplot = TRUE,
        add_plot_boxplot = TRUE,
        add_plot_prcomp = TRUE,
        add_plot_correlation = TRUE,
        add_plot_histogram = TRUE
      ),
      output_file = paste0("EDA_", nombre, ".html"),
      quiet = TRUE
    )
  }, error = function(e) {
    message("Error al generar el reporte para ", nombre, ": ", e)
  })
}

# Guardar el directorio de trabajo actual
original_dir <- getwd()

# Cambiar al directorio 'Outputh'
dir.create("Output", showWarnings = FALSE)  # Crear la carpeta si no existe
setwd("Output")

# Generar el reporte EDA para datos_combinados
generar_reporte_eda(query_completo, "query_completo")

# Instalar y cargar las librerías necesarias
if (!requireNamespace("sf", quietly = TRUE)) {
  install.packages("sf")
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("viridis", quietly = TRUE)) {
  install.packages("viridis")
}

library(sf)
library(ggplot2)
library(dplyr)
library(viridis)


# Ruta al shapefile descargado (asegúrate de ajustar la ruta según tu archivo)
#setwd(original_dir)
shapefile_path <- file.path(original_dir, "Input", "Servicios_Municipios", "Servicios_Departamentos.shp")

# Leer el shapefile
departamentos_shape <- st_read( shapefile_path)

# Mostrar la estructura del shapefile
print(st_geometry_type(departamentos_shape))
print(st_crs(departamentos_shape))
print(head(departamentos_shape))

# Contar cuántos nombre_prestador hay en cada departamento
conteo_prestadores_por_departamento <- datos_combinados %>%
  count(depa_nombre, Dep, name = "numero_prestadores")

# Cambiar el nombre de la columna Dep en conteo_prestadores_por_departamento para coincidir con el shapefile
names(conteo_prestadores_por_departamento)[2] <- "DPTO_CCDGO"

# Unir el shapefile con los datos de conteo
departamentos_shape1 <- departamentos_shape %>%
  mutate(
    DPTO_CCDGO = as.character(DPTO_CCDGO) # Asegúrate de que el tipo de datos coincida
  )

# Realizar la unión usando la columna DPTO_CCDGO
departamentos_shape1 <- departamentos_shape1 %>%
  left_join(conteo_prestadores_por_departamento, by = "DPTO_CCDGO")

# Reemplazar NAs en numero_prestadores con 0
departamentos_shape1$numero_prestadores[is.na(departamentos_shape$numero_prestadores)] <- 0

# Plotear el mapa
mapa <- ggplot(data = departamentos_shape1) +
  geom_sf(aes(fill = numero_prestadores), color = "gray", size = 0.1) +
  scale_fill_gradient(low = "gray", high = "blue", name = "Número de Prestadores") +
  labs(title = "Cantidad de Prestadores por Departamento en Colombia",
       subtitle = "Intensidad del color indica la cantidad de prestadores",
       caption = "Fuente: Datos combinados de departamentos y prestadores") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Guardar el plot en un archivo
ggsave("mapa_prestadores_colombia.png", plot = mapa, width = 8, height = 6)

# Contar cuántos nombre_prestador hay en cada departamento y sumar la población por departamento
razon_prestadores_por_departamento <- datos_combinados %>%
  group_by(depa_nombre, Dep) %>%
  summarise(
    numero_prestadores = n(),
    total_poblacion = sum(Poblacion, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(prestadores_por_1000_habitantes = (numero_prestadores / total_poblacion) * 1000)


# Cambiar el nombre de la columna Dep en conteo_prestadores_por_departamento para coincidir con el shapefile
names(razon_prestadores_por_departamento)[2] <- "DPTO_CCDGO"

# Unir el shapefile con los datos de conteo
departamentos_shape1 <- departamentos_shape %>%
  mutate(
    DPTO_CCDGO = as.character(DPTO_CCDGO) # Asegúrate de que el tipo de datos coincida
  )

# Realizar la unión usando la columna DPTO_CCDGO
departamentos_shape1 <- departamentos_shape1 %>%
  left_join(razon_prestadores_por_departamento, by = "DPTO_CCDGO")

# Reemplazar NAs en prestadores_por_1000_habitantes con 0
departamentos_shape1$prestadores_por_1000_habitantes[is.na(departamentos_shape$prestadores_por_1000_habitantes)] <- 0

# Plotear el mapa
mapa <- ggplot(data = departamentos_shape1) +
  geom_sf(aes(fill = prestadores_por_1000_habitantes), color = "gray", size = 0.1) +
  scale_fill_gradient(low = "gray", high = "blue", name = "Número de Prestadores") +
  labs(title = "Cantidad de Prestadores cada 1000 habitantes por Departamento en Colombia",
       subtitle = "Intensidad del color indica la cantidad de prestadores cada 1000 habitantes",
       caption = "Fuente: Datos combinados de departamentos y prestadores") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Guardar el plot en un archivo
ggsave("mapa_prestadores_por_1000_habitantes_colombia.png", plot = mapa, width = 8, height = 6)

abreviaciones <- c(
  "PROFESIONAL INDEPENDIENTE" = "PROFESIONAL INDEPENDIENTE",
  "INSTITUCIONES PRESTADORAS DE SERVICIOS DE SALUD - IPS" = "IPS",
  "OBJETO SOCIAL DIFERENTE A LA PRESTACION DE SERVICIOS DE SALUD" = "OBJETO SOCIAL DIFERENTE A PRESTA. SALUD.",
  "TRANSPORTE ESPECIAL DE PACIENTES" = "TRANSPORTE ESPECIAL DE PACIENTES"
)

# Reemplazar los nombres en la columna clpr_nombre con los nombres abreviados
datos_combinados <- datos_combinados %>%
  mutate(clpr_nombre = abreviaciones[clpr_nombre])

# Contar cuántos nombre_prestador hay en cada departamento y sumar la población por departamento
razon_prestadores_por_departamento <- datos_combinados %>%
  group_by(depa_nombre, Dep, clpr_nombre) %>%
  summarise(
    numero_prestadores = n(),
    total_poblacion = sum(Poblacion, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(prestadores_por_1000_habitantes = (numero_prestadores / total_poblacion) * 1000)

# Cambiar el nombre de la columna Dep en razon_prestadores_por_departamento para coincidir con el shapefile
names(razon_prestadores_por_departamento)[2] <- "DPTO_CCDGO"

# Asegurarse de que el tipo de datos coincida
departamentos_shape1 <- departamentos_shape %>%
  mutate(DPTO_CCDGO = as.character(DPTO_CCDGO))

# Realizar la unión usando la columna DPTO_CCDGO
departamentos_shape1 <- departamentos_shape1 %>%
  left_join(razon_prestadores_por_departamento, by = "DPTO_CCDGO")

# Reemplazar NAs en prestadores_por_1000_habitantes con 0
departamentos_shape1$prestadores_por_1000_habitantes[is.na(departamentos_shape1$prestadores_por_1000_habitantes)] <- 0

# Plotear el mapa con facet_wrap por clpr_nombre
mapa <- ggplot(data = departamentos_shape1) +
  geom_sf(aes(fill = prestadores_por_1000_habitantes), color = "gray", size = 0.1) +
  scale_fill_gradient(low = "gray", high = "blue", name = "Número de Prestadores por 1000 habitantes") +
  labs(title = "Cantidad de Prestadores cada 1000 habitantes por Departamento en Colombia",
       subtitle = "Intensidad del color indica la cantidad de prestadores cada 1000 habitantes",
       caption = "Fuente: Datos combinados de departamentos y prestadores") +
  facet_wrap(~ clpr_nombre) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Guardar el plot en un archivo
ggsave("mapa_prestadores_por_1000_habitantes_por_tipo_colombia.png", plot = mapa, width = 16, height = 10)


# Contar cuántos nombre_prestador hay en cada departamento y sumar la población por departamento
razon_prestadores_por_departamento <- datos_combinados %>%
  group_by(depa_nombre, Dep, clase_persona ) %>%
  summarise(
    numero_prestadores = n(),
    total_poblacion = sum(Poblacion, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(prestadores_por_1000_habitantes = (numero_prestadores / total_poblacion) * 1000)

# Cambiar el nombre de la columna Dep en razon_prestadores_por_departamento para coincidir con el shapefile
names(razon_prestadores_por_departamento)[2] <- "DPTO_CCDGO"

# Asegurarse de que el tipo de datos coincida
departamentos_shape1 <- departamentos_shape %>%
  mutate(DPTO_CCDGO = as.character(DPTO_CCDGO))

# Realizar la unión usando la columna DPTO_CCDGO
departamentos_shape1 <- departamentos_shape1 %>%
  left_join(razon_prestadores_por_departamento, by = "DPTO_CCDGO")

# Reemplazar NAs en prestadores_por_1000_habitantes con 0
departamentos_shape1$prestadores_por_1000_habitantes[is.na(departamentos_shape1$prestadores_por_1000_habitantes)] <- 0

# Plotear el mapa con facet_wrap por clpr_nombre
mapa <- ggplot(data = departamentos_shape1) +
  geom_sf(aes(fill = prestadores_por_1000_habitantes), color = "gray", size = 0.1) +
  scale_fill_gradient(low = "gray", high = "blue", name = "Número de Prestadores por 1000 habitantes") +
  labs(title = "Cantidad de Prestadores cada 1000 habitantes por Departamento en Colombia",
       subtitle = "Intensidad del color indica la cantidad de prestadores cada 1000 habitantes",
       caption = "Fuente: Datos combinados de departamentos y prestadores") +
  facet_wrap(~ clase_persona) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Guardar el plot en un archivo
ggsave("mapa_prestadores_por_1000_habitantes_por_clase_colombia.png", plot = mapa, width = 16, height = 10)

################################################################################
##Modelamiento Estadistico
library(FactoMineR)
library(factoextra)
library(data.table)
# Convertir el data frame a data.table y seleccionar las columnas de interés
datos_acm <- as.data.table(datos_combinados)[, .(depa_nombre, clpr_nombre, clase_persona)]

# Realizar el ACM
acm_result <- MCA(datos_acm, graph = FALSE)

# Asignar colores diferentes a cada variable
variable_colores <- c("depa_nombre" = "blue", "clpr_nombre" = "red", "clase_persona" = "green")

# Crear una paleta de colores para los niveles de las variables
var_colors <- c(rep("blue", length(unique(datos_acm$depa_nombre))),
                rep("red", length(unique(datos_acm$clpr_nombre))),
                rep("green", length(unique(datos_acm$clase_persona))))

# Visualizar las variables
plot_vars <- fviz_mca_var(acm_result, 
                          repel = TRUE, # Evita la superposición de etiquetas
                          col.var = var_colors, # Colores diferentes por variable
                          addEllipses = TRUE, # Agregar elipses
                          ellipse.level = 0.90) + # Nivel de confianza para las elipses
  labs(title = "Variables en el ACM",
       x = "Dimensión 1",
       y = "Dimensión 2") +
  theme_minimal()

# Mostrar el plot
print(plot_vars)

# Guardar el plot en un archivo
ggsave("variables_acm.png", plot = plot_vars, width = 10, height = 8)


datos_acm <- as.data.table(datos_combinados)[, .(clpr_nombre, clase_persona)]

# Realizar el ACM
acm_result <- MCA(datos_acm, graph = FALSE)

# Asignar colores diferentes a cada variable
variable_colores <- c("clpr_nombre" = "red", "clase_persona" = "green")

# Crear una paleta de colores para los niveles de las variables
var_colors <- c(rep("red", length(unique(datos_acm$clpr_nombre))),
                rep("green", length(unique(datos_acm$clase_persona))))

# Visualizar las variables
plot_vars <- fviz_mca_var(acm_result, 
                          repel = TRUE, # Evita la superposición de etiquetas
                          col.var = var_colors, # Colores diferentes por variable
                          addEllipses = TRUE, # Agregar elipses
                          ellipse.level = 0.90) + # Nivel de confianza para las elipses
  labs(title = "Variables en el ACM",
       x = "Dimensión 1",
       y = "Dimensión 2") +
  theme_minimal()

# Mostrar el plot
print(plot_vars)

# Guardar el plot en un archivo
ggsave("variables_acm_tipo.png", plot = plot_vars, width = 10, height = 8)
