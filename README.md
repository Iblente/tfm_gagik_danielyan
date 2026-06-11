Se trabaja con la base de datos STATS19 de accidentes de tráfico de Gran Bretaña.

Para ejecutar el script hay que seleccionar todo el código y darle a Run tres veces seguidas, porque en la primera pasada se instalan los paquetes, en la segunda se cargan, y en la tercera el código ya puede ejecutarse completo sin errores. Las figuras y tablas se guardan en la carpeta output/, que el script crea automáticamente si no existe.

Los datos históricos de colisiones deben descargarse manualmente desde https://www.gov.uk/government/statistical-data-sets/road-safety-open-data y modificar la ruta en el script. Ahora mismo está como csv_hist <- "cambiar/ruta/dft-road-casualty-statistics-collision-1979-latest-published-year.csv". En la misma página hay que descargar también los archivos de vehículos y víctimas, con rutas csv_veh <- "cambiar/ruta/dft-road-casualty-statistics-vehicle-1979-latest-published-year.csv" y csv_cas <- "cambiar/ruta/dft-road-casualty-statistics-casualty-1979-latest-published-year.csv". Los datos más recientes (2020-2024) se descargan automáticamente mediante el paquete stats19.

El índice de condiciones socioeconómicas IMD 2019 se descarga desde https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019, concretamente el archivo "File 7: All IoD2019 Scores, Ranks, Deciles and Population Denominators". La ruta en el script está como csv_imd <- "cambiar/ruta/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv".

Los datos de tráfico del DfT se descargan desde https://roadtraffic.dft.gov.uk/downloads. Hay que descargar dos archivos de la sección "Data at the count point level": "Count points" y "Annual average daily flow". Las rutas en el script están como count_pts <- read.csv("cambiar/ruta/count_points.csv") y aadf <- read.csv("cambiar/ruta/dft_traffic_counts_aadf.csv").

El archivo shapefile de los polígonos LSOA se descarga desde https://geoportal.statistics.gov.uk buscando "LSOA Dec 2011 EW BGC V3" y descargando el shapefile. La ruta en el script está como lsoa_sf <- st_read("cambiar/ruta/LSOA_2011_EW_BGC_V3.shp").

Los paquetes necesarios son INLA, dplyr, ggplot2, spdep, sf, stats19, data.table, lubridate, Rcpp y spatstat. El script instala automáticamente los que falten en la primera ejecución. INLA no está en CRAN y se instala desde su propio repositorio. 

Los modelos BYM2 y Hawkes pueden tardar varios minutos cada uno dependiendo del equipo. El código fue ejecutado en un ordenador de sobremesa equipado con un procesador AMD Ryzen 9 5900X (12 núcleos, 3,70 GHz), 32 GB de RAM y 800 GB de almacenamiento disponible.
