
# README

## Descripción del Proyecto

Este proyecto tiene como objetivo procesar y analizar datos de municipios y prestadores de servicios de salud en Colombia. Utilizando archivos Excel y una base de datos SQLite, se realiza la limpieza y transformación de los datos, se generan informes de análisis exploratorio de datos (EDA) y se crean visualizaciones geográficas para entender la distribución de los prestadores de servicios de salud.

## Requisitos

Este proyecto utiliza R y las siguientes librerías:

- `DBI`
- `RSQLite`
- `readxl`
- `dplyr`
- `ggplot2`
- `rmarkdown`
- `stringr`
- `janitor`
- `rstudioapi`
- `DataExplorer`
- `sf`
- `viridis`
- `FactoMineR`
- `factoextra`
- `data.table`

## Instalación

Para instalar las librerías necesarias, puedes utilizar el siguiente comando en R:

```R
install.packages(c("DBI", "RSQLite", "readxl", "dplyr", "ggplot2", "rmarkdown", "stringr", "janitor", "rstudioapi", "DataExplorer", "sf", "viridis", "FactoMineR", "factoextra", "data.table"))
```

## Uso

### 1. Limpieza del Entorno

El script comienza limpiando el entorno de trabajo para asegurar que no haya datos residuales:

```R
rm(list=ls())
gc()
```

### 2. Establecer el Directorio de Trabajo

Se establece el directorio de trabajo al directorio del script actual utilizando `rstudioapi`:

```R
set_working_directory_to_script_path()
```

### 3. Conectar a la Base de Datos SQLite

Se establece una conexión a una base de datos SQLite ubicada en `Output/Adres_Prueba_Tecnica.sqlite`:

```R
con <- dbConnect(RSQLite::SQLite(), "Output/Adres_Prueba_Tecnica.sqlite")
```

### 4. Leer y Cargar Datos desde Archivos Excel

Se leen y cargan hojas de archivos Excel en la base de datos SQLite:

```R
ruta <- "input/"
archivos <- c(file.path(ruta, "Municipios.xlsx"), file.path(ruta, "Prestadores.xlsx"), file.path(ruta, "DANE.xlsx"))

for (archivo in archivos) {
  hojas <- readxl::excel_sheets(archivo)
  for (hoja in hojas) {
    nombre_tabla <- paste(tools::file_path_sans_ext(basename(archivo)), hoja, sep = "_")
    data <- readxl::read_excel(archivo, sheet = hoja)
    dbWriteTable(con, nombre_tabla, data, overwrite = TRUE)
  }
}
```

### 5. Transformación y Limpieza de Datos

Se realizan diversas transformaciones y limpiezas, incluyendo la eliminación de tildes y la conversión de texto a mayúsculas:

```R
eliminar_tildes <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- str_replace_all(x, "Ñ", "N")
  x <- str_replace_all(x, " D.C", "")
  x <- str_replace_all(x, "\.", "") 
  x <- str_replace_all(x, "ñ", "n")
  x <- str_trim(x)
  return(x)
}
```

### 6. Unión de Datos y Creación de Nuevas Tablas

Se combinan los datos de municipios y prestadores, se realiza la unión usando campos comunes, y se crea una nueva tabla compilada en la base de datos SQLite:

```R
datos_combinados <- datos_prestadores_seleccionados %>%
  left_join(datos_municipios_limpio, by = c("depa_nombre", "muni_nombre")) %>%
  mutate(coincidencia = if_else(is.na(Dep), "No encontrado", "Encontrado"))
```

### 7. Análisis Exploratorio de Datos (EDA)

Se genera un reporte de EDA utilizando la librería `DataExplorer`:

```R
generar_reporte_eda <- function(data, nombre) {
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
}
```

### 8. Visualización Geográfica

Se crean mapas para visualizar la distribución de prestadores de servicios de salud por departamento y por cada 1000 habitantes:

```R
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

ggsave("mapa_prestadores_colombia.png", plot = mapa, width = 8, height = 6)
```

### 9. Análisis de Correspondencias Múltiples (ACM)

Se realiza un análisis de correspondencias múltiples para visualizar las relaciones entre variables categóricas:

```R
acm_result <- MCA(datos_acm, graph = FALSE)
plot_vars <- fviz_mca_var(acm_result, 
                          repel = TRUE, 
                          col.var = var_colors, 
                          addEllipses = TRUE, 
                          ellipse.level = 0.90) + 
  labs(title = "Variables en el ACM",
       x = "Dimensión 1",
       y = "Dimensión 2") +
  theme_minimal()

ggsave("variables_acm.png", plot = plot_vars, width = 10, height = 8)
```

## Conclusión

Este proyecto proporciona una metodología completa para la limpieza, transformación, análisis y visualización de datos de municipios y prestadores de servicios de salud en Colombia. Utilizando R y diversas librerías, se logra un análisis detallado y se generan visualizaciones útiles para entender la distribución y características de los prestadores de servicios de salud en diferentes regiones del país.

## Contacto

Para más información, puedes contactarme en: rmaciasb@unal.edu.co.
