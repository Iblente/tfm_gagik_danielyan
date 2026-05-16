# LIBRERÍAS 
if(!require(dplyr)) install.packages("dplyr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(spdep)) install.packages("spdep")
if(!require(sf)) install.packages("sf")
if(!require(stats19)) install.packages("stats19")
if(!require(data.table)) install.packages("data.table")

library(dplyr)
library(lubridate)
library(ggplot2)
library(spdep)
library(sf)
library(stats19)
library(data.table)

# Carpeta donde guardar figuras y tablas
dir.create("output", showWarnings = FALSE)

# CARGA DE DATOS

# Descarga manual desde https://www.gov.uk/government/statistical-data-sets/road-safety-open-data
csv_hist <- "cambiar/ruta/descargando/antes/dft-road-casualty-statistics-collision-1979-latest-published-year.csv"

# Variables que voy a utilizar
variables <- c(
  "collision_year",
  "collision_index",
  "collision_ref_no",
  "date",
  "time",
  "day_of_week",
  "police_force",
  "collision_severity",
  "number_of_vehicles",
  "number_of_casualties",
  "local_authority_district",
  "local_authority_ons_district",
  "first_road_class",
  "road_type",
  "speed_limit",
  "junction_detail",
  "light_conditions",
  "weather_conditions",
  "road_surface_conditions",
  "urban_or_rural_area",
  "location_easting_osgr",
  "location_northing_osgr",
  "lsoa_of_accident_location",
  "enhanced_severity_collision"
)

# Lectura del csv
col_hist <- fread(
  csv_hist,
  select = variables,
  nThread = 1,
  showProgress = TRUE
)

# Me quedo únicamente con 2015-2019
col_hist <- col_hist %>%
  filter(collision_year %in% 2015:2019)

# Conversión de formatos
col_hist$date <- as.Date(
  col_hist$date,
  format = "%d/%m/%Y"
)

col_hist$collision_year <- as.numeric(col_hist$collision_year)

col_hist$location_easting_osgr <-
  as.numeric(col_hist$location_easting_osgr)

col_hist$location_northing_osgr <-
  as.numeric(col_hist$location_northing_osgr)

col_hist$enhanced_severity_collision <-
  as.numeric(col_hist$enhanced_severity_collision)

# Variables categóricas formato
col_hist$day_of_week <- as.character(col_hist$day_of_week)
col_hist$police_force <- as.character(col_hist$police_force)
col_hist$collision_severity <- as.character(col_hist$collision_severity)
col_hist$number_of_vehicles <- as.character(col_hist$number_of_vehicles)
col_hist$number_of_casualties <- as.character(col_hist$number_of_casualties)
col_hist$local_authority_district <- as.character(col_hist$local_authority_district)
col_hist$first_road_class <- as.character(col_hist$first_road_class)
col_hist$road_type <- as.character(col_hist$road_type)
col_hist$speed_limit <- as.character(col_hist$speed_limit)
col_hist$junction_detail <- as.character(col_hist$junction_detail)
col_hist$light_conditions <- as.character(col_hist$light_conditions)
col_hist$weather_conditions <- as.character(col_hist$weather_conditions)
col_hist$road_surface_conditions <- as.character(col_hist$road_surface_conditions)
col_hist$urban_or_rural_area <- as.character(col_hist$urban_or_rural_area)


# DESCARGA DE DATOS AUTOMÁTICA 2020 - 2024 

lista_reciente <- list()

for(a in 2020:2024){
  
  cat("Descarga año:", a, "\n")
  
  datos_temp <- tryCatch(
    
    get_stats19(
      year = a,
      type = "collision",
      ask = FALSE
    ),
    
    error = function(e) NULL
    
  )
  
  lista_reciente[[as.character(a)]] <- datos_temp
}

# Unión de años recientes
col_reciente <- bind_rows(lista_reciente)

# Base de datos completa
col <- bind_rows(col_hist, col_reciente)

# Guardado
saveRDS(
  col,
  file.path(
    dirname(csv_hist),
    "col_completo_2015_2024.rds"
  )
)


# LIMPIEZA

# Elimino coordenadas erróneas
col <- col %>%
  filter(
    location_easting_osgr > 0,
    location_northing_osgr > 0
  )

# Elimino fechas vacías
col <- col %>%
  filter(!is.na(date))


# CREACIÓN DE LA FECHA

# Algunos años ya tienen variable de fecha creada
# Para los antiguos lo construyo manualmente

col$datetime <- ifelse(
  
  is.na(col$datetime),
  
  as.POSIXct(
    paste(col$date, col$time),
    format = "%Y-%m-%d %H:%M",
    tz = "Europe/London"
  ),
  
  col$datetime
  
)

col$datetime <- as.POSIXct(
  col$datetime,
  origin = "1970-01-01",
  tz = "Europe/London"
)

# Variables temporales
col$anio <- year(col$datetime)
col$mes <- month(col$datetime)
col$dia_sem <- wday(col$datetime)
col$hora_dia <- hour(col$datetime)

# Elimino NA en datetime
col <- col %>%
  filter(!is.na(datetime))


# TIEMPO CONTINUO

t_origen <- min(col$datetime, na.rm = TRUE)

col$t_cont <- as.numeric(
  difftime(
    col$datetime,
    t_origen,
    units = "hours"
  )
)

# EXCLUIR AÑOS COVID

# Excluyo 2020 y 2021
anios <- c(
  2015:2019,
  2022:2024
)

col_tfm <- col %>%
  filter(collision_year %in% anios)

# VARIABLE SEVERIDAD

# Históricos:
# 1 Fatal
# 2 Serious
# 3 Slight

# Recientes:
# Fatal / Serious / Slight

col_tfm$sev_num <- NA

col_tfm$sev_num[
  col_tfm$collision_severity %in% c("1", "Fatal")
] <- 1

col_tfm$sev_num[
  col_tfm$collision_severity %in% c("2", "Serious")
] <- 2

col_tfm$sev_num[
  col_tfm$collision_severity %in% c("3", "Slight")
] <- 3


# CAPÍTULO 4 DEL TFM

# DESCRIPTIVOS

# Número total de accidentes
print(nrow(col_tfm))

# Años incluidos
print(sort(unique(col_tfm$collision_year)))

# Distribución de severidad
sev <- col_tfm %>%
  filter(!is.na(sev_num)) %>%
  count(sev_num)

sev$pct <- round(
  sev$n / sum(sev$n) * 100,
  1
)

print(sev)

# Intensidad media del proceso
intensidad <- nrow(col_tfm) /
  max(col_tfm$t_cont, na.rm = TRUE)

print(intensidad)


# PATRONES TEMPORALES

meses_es <- c(
  "Enero",
  "Febrero",
  "Marzo",
  "Abril",
  "Mayo",
  "Junio",
  "Julio",
  "Agosto",
  "Septiembre",
  "Octubre",
  "Noviembre",
  "Diciembre"
)

col_temp <- col_tfm %>%
  filter(!is.na(datetime))


# ACCIDENTES POR HORA

por_hora <- col_temp %>%
  count(hora_dia)

por_hora$media_anual <-
  por_hora$n / length(anios)

print(por_hora)

# Hora con más accidentes
hora_max <- por_hora$hora_dia[
  which.max(por_hora$n)
]

# Hora con menos accidentes
hora_min <- por_hora$hora_dia[
  which.min(por_hora$n)
]

# Ratio máximo/mínimo
ratio_hora <- round(
  max(por_hora$n) / min(por_hora$n),
  2
)

# Gráfico
ggplot(
  por_hora,
  aes(
    x = hora_dia,
    y = media_anual
  )
) +
  
  geom_col(fill = "blue") +
  
  scale_x_continuous(
    breaks = 0:23
  ) +
  
  labs(
    title = "Accidentes medios por hora",
    x = "Hora",
    y = "Media anual"
  )

ggsave(
  "output/fig4_1_distribucion_horaria.png",
  dpi = 300,
  width = 10,
  height = 5
)


# ACCIDENTES POR DÍA DE LA SEMANA

col_temp$dia <- lubridate::wday(
  col_temp$datetime,
  label = TRUE,
  abbr = FALSE
)

por_dia <- col_temp %>%
  count(dia_sem, dia)

por_dia <- por_dia %>%
  arrange(dia_sem)

por_dia$media_anual <-
  por_dia$n / length(anios)

print(
  por_dia[, c("dia", "media_anual")]
)


# ACCIDENTES POR MES

por_mes <- col_temp %>%
  count(mes)

por_mes$media_anual <-
  por_mes$n / length(anios)

por_mes$nombre <- meses_es[
  por_mes$mes
]

print(
  por_mes[, c("nombre", "media_anual")]
)


# VARIACIÓN INTERANUAL

por_anio <- col_temp %>%
  count(anio)

por_anio$var_pct <- round(
  
  (por_anio$n - lag(por_anio$n)) /
    lag(por_anio$n) * 100,
  
  1
  
)

print(por_anio)


# DÍA LABORAL O FIN DE SEMANA

patron_hora_dia <- col_temp

patron_hora_dia$tipo_dia <- ifelse(
  
  patron_hora_dia$dia_sem %in% c(1, 7),
  
  "Fin de semana",
  
  "Laborable"
  
)

patron_hora_dia <- patron_hora_dia %>%
  
  group_by(tipo_dia, hora_dia) %>%
  
  summarise(
    n = n(),
    .groups = "drop"
  )

patron_hora_dia$media_anual <-
  patron_hora_dia$n / length(anios)

# Gráfico
ggplot(
  patron_hora_dia,
  aes(
    x = hora_dia,
    y = media_anual,
    fill = tipo_dia
  )
) +
  
  geom_col(
    position = "dodge",
    alpha = 0.8
  ) +
  
  scale_x_continuous(
    breaks = 0:23
  ) +
  
  scale_fill_manual(
    values = c(
      "Laborable" = "blue",
      "Fin de semana" = "red"
    )
  ) +
  
  labs(
    title = "Patrón horario según tipo de día",
    x = "Hora",
    y = "Media anual",
    fill = NULL
  )

ggsave(
  "output/fig4_1b_hora_laborable_finde.png",
  dpi = 300,
  width = 11,
  height = 5
)


# ESTACIONALIDAD MESES

patron_mes_anio <- col_temp %>%
  count(anio, mes)

patron_mes_anio$mes_abb <- factor(
  
  month.abb[patron_mes_anio$mes],
  
  levels = month.abb
  
)

ggplot(
  patron_mes_anio,
  aes(
    x = mes_abb,
    y = n,
    group = anio,
    color = factor(anio)
  )
) +
  
  geom_line(alpha = 0.7) +
  
  scale_color_viridis_d(
    name = "Año"
  ) +
  
  labs(
    title = "Accidentes por mes y año",
    x = NULL,
    y = "Accidentes"
  ) 

ggsave(
  "output/fig4_1c_patron_mensual_por_anio.png",
  dpi = 300,
  width = 11,
  height = 5
)


# COVID-19

# Número de accidentes por año
n_2019 <- sum(
  col$collision_year == 2019,
  na.rm = TRUE
)

n_2020 <- sum(
  col$collision_year == 2020,
  na.rm = TRUE
)

n_2021 <- sum(
  col$collision_year == 2021,
  na.rm = TRUE
)

# Caída porcentual respecto a 2019
caida_2020 <- round(
  (1 - n_2020 / n_2019) * 100,
  1
)

caida_2021 <- round(
  (1 - n_2021 / n_2019) * 100,
  1
)

print(caida_2020)
print(caida_2021)


# ABRIL 2020 FRENTE A ABRIL DE 2017-2019

n_abril_2020 <- col %>%
  
  filter(collision_year == 2020) %>%
  
  filter(month(date) == 4) %>%
  
  nrow()

n_abril_base <- col %>%
  
  filter(collision_year %in% 2017:2019) %>%
  
  filter(month(date) == 4) %>%
  
  nrow()

n_abril_base <- n_abril_base / 3

caida_abril <- round(
  
  (1 - n_abril_2020 / n_abril_base) * 100,
  
  1
  
)

print(caida_abril)


# SEVERIDAD ESTRUCTURA

sev_anio <- col %>%
  
  filter(collision_year %in% 2017:2019)

sev_anio$sev_num <- NA

sev_anio$sev_num[
  sev_anio$collision_severity %in% c("1", "Fatal")
] <- 1

sev_anio$sev_num[
  sev_anio$collision_severity %in% c("2", "Serious")
] <- 2

sev_anio$sev_num[
  sev_anio$collision_severity %in% c("3", "Slight")
] <- 3

sev_anio <- sev_anio %>%
  
  group_by(collision_year) %>%
  
  summarise(
    
    pct_fatal = round(
      mean(sev_num == 1, na.rm = TRUE) * 100,
      2
    ),
    
    pct_grave = round(
      mean(sev_num == 2, na.rm = TRUE) * 100,
      2
    ),
    
    pct_leve = round(
      mean(sev_num == 3, na.rm = TRUE) * 100,
      2
    ),
    
    .groups = "drop"
    
  )

print(sev_anio)


# PATRÓN HORARIO COVID

patron_covid <- col %>%
  
  filter(
    
    !is.na(datetime),
    
    collision_year == 2019 |
      
      (
        datetime >= as.POSIXct("2020-03-23") &
          datetime <= as.POSIXct("2020-06-30")
      )
    
  )

patron_covid$periodo <- ifelse(
  
  patron_covid$collision_year == 2019,
  
  "2019",
  
  "Confinamiento 2020"
  
)

patron_covid$hora_dia <-
  hour(patron_covid$datetime)

patron_covid <- patron_covid %>%
  
  group_by(periodo, hora_dia) %>%
  
  summarise(
    n = n(),
    .groups = "drop"
  )

patron_covid <- patron_covid %>%
  
  group_by(periodo) %>%
  
  mutate(
    pct = n / sum(n) * 100
  ) %>%
  
  ungroup()

# Gráfico
ggplot(
  patron_covid,
  aes(
    x = hora_dia,
    y = pct,
    color = periodo,
    group = periodo
  )
) +
  
  geom_line(linewidth = 1) +
  
  geom_point(size = 1.5) +
  
  scale_x_continuous(
    breaks = 0:23
  ) +
  
  scale_color_manual(
    values = c(
      "2019" = "blue",
      "Confinamiento 2020" = "red"
    )
  ) +
  
  labs(
    title = "Patrón horario: 2019 - confinamiento 2020",
    x = "Hora",
    y = "% del total diario",
    color = NULL
  ) 

ggsave(
  "output/fig4_2b_patron_horario_covid.png",
  dpi = 300,
  width = 10,
  height = 5
)


# SERIE SEMANAL COVID

serie_semanal <- col %>%
  
  filter(collision_year >= 2015)

serie_semanal <- serie_semanal %>%
  
  filter(!is.na(date))

serie_semanal$semana <- floor_date(
  serie_semanal$date,
  "week"
)

serie_semanal <- serie_semanal %>%
  count(semana)

serie_semanal <- serie_semanal %>%
  
  filter(!is.na(semana))

serie_semanal$periodo <- ifelse(
  
  serie_semanal$semana >= as.Date("2020-03-23") &
    serie_semanal$semana <= as.Date("2021-07-19"),
  
  "COVID",
  
  "Normal"
  
)

# Gráfico
ggplot(
  serie_semanal,
  aes(
    x = semana,
    y = n,
    fill = periodo
  )
) +
  
  geom_col() +
  
  scale_fill_manual(
    values = c(
      "Normal" = "blue",
      "COVID" = "red"
    )
  ) +
  
  labs(
    title = "Accidentes semanales 2015-2024",
    x = NULL,
    y = "Accidentes",
    fill = NULL
  )

ggsave(
  "output/fig4_2_serie_semanal_covid.png",
  dpi = 300,
  width = 12,
  height = 5
)


# ACCIDENTES POR LSOA

# Escocia usa Data Zones y no LSOA
# Por tanto este análisis queda restringido
# a Inglaterra y Gales

acc_lsoa <- col_tfm %>%
  
  filter(!is.na(lsoa_of_accident_location))

acc_lsoa <- acc_lsoa %>%
  
  filter(lsoa_of_accident_location != "")

acc_lsoa <- acc_lsoa %>%
  
  filter(lsoa_of_accident_location != "-1")

acc_lsoa <- acc_lsoa %>%
  
  count(lsoa_of_accident_location)

acc_lsoa$tasa_anual <-
  acc_lsoa$n / length(anios)

print(nrow(acc_lsoa))

print(summary(acc_lsoa$tasa_anual))

print(
  
  quantile(
    
    acc_lsoa$tasa_anual,
    
    c(0.75, 0.90, 0.95, 0.99)
    
  )
  
)


# CONCENTRACIÓN ZONAS

acc_ord <- sort(
  acc_lsoa$n,
  decreasing = TRUE
)

n_50 <- which(
  
  cumsum(acc_ord) / sum(acc_ord) >= 0.50
  
)[1]

n_90 <- which(
  
  cumsum(acc_ord) / sum(acc_ord) >= 0.90
  
)[1]

print(
  
  round(
    n_50 / nrow(acc_lsoa) * 100,
    1
  )
  
)

print(
  
  round(
    n_90 / nrow(acc_lsoa) * 100,
    1
  )
  
)

# LSOAs con más accidentes
print(
  
  acc_lsoa %>%
    
    arrange(desc(tasa_anual)) %>%
    
    head(20)
  
)


# HISTOGRAMA

ggplot(
  acc_lsoa,
  aes(x = tasa_anual)
) +
  
  geom_histogram(
    bins = 80,
    fill = "blue",
    color = "white"
  ) +
  
  scale_x_continuous(
    
    limits = c(
      0,
      quantile(acc_lsoa$tasa_anual, 0.99)
    )
    
  ) +
  
  geom_vline(
    xintercept = mean(acc_lsoa$tasa_anual),
    color = "red",
    linetype = "dashed"
  ) +
  
  geom_vline(
    xintercept = median(acc_lsoa$tasa_anual),
    color = "orange",
    linetype = "dotted"
  ) +
  
  labs(
    title = "Tasa anual de accidentes por LSOA",
    x = "Accidentes/año",
    y = "LSOAs"
  ) 

ggsave(
  "output/fig4_3_hist_tasa_lsoa.png",
  dpi = 300,
  width = 9,
  height = 5
)

# CURVA DE LORENZ Y GINI

lorenz <- data.frame(
  
  x = c(
    0,
    seq_len(length(acc_ord)) / length(acc_ord)
  ),
  
  y = c(
    0,
    cumsum(acc_ord) / sum(acc_ord)
  )
  
)

gini_esp <- 1 - 2 * sum(
  
  diff(lorenz$x) *
    
    (
      lorenz$y[-1] +
        lorenz$y[-nrow(lorenz)]
    ) / 2
  
)

print(
  round(gini_esp, 3)
)

# Gráfico Lorenz
ggplot(
  lorenz,
  aes(x = x, y = y)
) +
  
  geom_line(color = "blue") +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "gray",
    linetype = "dashed"
  ) +
  
  labs(
    title = paste0(
      "Curva de Lorenz — Gini = ",
      round(gini_esp, 3)
    ),
    x = "Proporción de LSOAs",
    y = "Proporción de accidentes"
  ) 

ggsave(
  "output/fig4_4_lorenz_lsoa.png",
  dpi = 300,
  width = 8,
  height = 7
)


# INDICE DE MORAN I

# Accidentes 2019
acc_2019 <- col_tfm %>%
  
  filter(collision_year == 2019)

acc_2019 <- acc_2019 %>%
  
  filter(!is.na(lsoa_of_accident_location))

acc_2019 <- acc_2019 %>%
  
  filter(lsoa_of_accident_location != "")

acc_2019 <- acc_2019 %>%
  
  filter(lsoa_of_accident_location != "-1")

acc_2019 <- acc_2019 %>%
  
  count(
    lsoa_of_accident_location,
    name = "n_acc"
  )


# Histórico completo por LSOA
acc_hist_lsoa <- col_tfm %>%
  
  filter(!is.na(lsoa_of_accident_location))

acc_hist_lsoa <- acc_hist_lsoa %>%
  
  filter(lsoa_of_accident_location != "")

acc_hist_lsoa <- acc_hist_lsoa %>%
  
  filter(lsoa_of_accident_location != "-1")

acc_hist_lsoa <- acc_hist_lsoa %>%
  
  count(
    lsoa_of_accident_location,
    name = "n_hist"
  )


# Base GLM
datos_glm <- left_join(
  
  acc_2019,
  
  acc_hist_lsoa,
  
  by = "lsoa_of_accident_location"
  
)

datos_glm$n_hist[
  is.na(datos_glm$n_hist)
] <- 1


# MODELO POISSON

glm_base <- glm(
  
  n_acc ~ offset(log(n_hist)),
  
  family = poisson(link = "log"),
  
  data = datos_glm
  
)

print(summary(glm_base))


# Residuos Pearson
datos_glm$residuo <- residuals(
  glm_base,
  type = "pearson"
)

print(summary(datos_glm$residuo))


# CENTROIDES

centroides <- col_tfm %>%
  
  filter(!is.na(lsoa_of_accident_location))

centroides <- centroides %>%
  
  filter(lsoa_of_accident_location != "")

centroides <- centroides %>%
  
  filter(location_easting_osgr > 0)

centroides <- centroides %>%
  
  group_by(lsoa_of_accident_location) %>%
  
  summarise(
    
    cx = mean(
      location_easting_osgr,
      na.rm = TRUE
    ),
    
    cy = mean(
      location_northing_osgr,
      na.rm = TRUE
    ),
    
    .groups = "drop"
    
  )


# Unir centroides
datos_glm <- left_join(
  
  datos_glm,
  
  centroides,
  
  by = "lsoa_of_accident_location"
  
)

datos_glm <- datos_glm %>%
  
  filter(!is.na(cx))

datos_glm <- datos_glm %>%
  
  filter(!is.na(cy))


# MATRIZ DE ACCIDENTES ENTRE LSOAS

coords <- as.matrix(
  
  datos_glm[, c("cx", "cy")]
  
)

# 6 vecinos más próximos
nb <- knn2nb(
  
  knearneigh(
    coords,
    k = 6
  )
  
)

W <- nb2listw(
  nb,
  style = "W"
)


# MORAN I

moran_res <- moran.test(
  
  datos_glm$residuo,
  
  listw = W,
  
  alternative = "greater"
  
)

moran_tasa <- moran.test(
  
  datos_glm$n_acc,
  
  listw = W,
  
  alternative = "greater"
  
)

print(moran_tasa)

print(moran_res)


# DIAGRAMA DE MORAN

png(
  "output/fig4_5_diagrama_moran.png",
  width = 1800,
  height = 1800,
  res = 300
)

moran.plot(
  
  datos_glm$residuo,
  
  listw = W,
  
  xlab = "Residuo (zona i)",
  
  ylab = "Media residuos vecinos",
  
  main = "Diagrama de Moran — 2019",
  
  pch = 16,
  
  cex = 0.3,
  
  col = "blue"
  
)

dev.off()


# LISA

set.seed(2024)

lisa <- localmoran_perm(
  
  datos_glm$residuo,
  
  listw = W,
  
  nsim = 999,
  
  alternative = "two.sided"
  
)


# Resultados LISA
datos_glm$lisa_I <-
  lisa[, "Ii"]

datos_glm$lisa_pval <-
  lisa[, "Pr(z != E(Ii))"]


# CATEGORÍAS O CUADRANTES

z_res <- scale(
  datos_glm$residuo
)[, 1]

lag_z <- lag.listw(
  W,
  z_res
)

datos_glm$cuadrante <- case_when(
  
  z_res > 0 &
    lag_z > 0 &
    datos_glm$lisa_pval < 0.05 ~ "HH",
  
  z_res > 0 &
    lag_z < 0 &
    datos_glm$lisa_pval < 0.05 ~ "HL",
  
  z_res < 0 &
    lag_z > 0 &
    datos_glm$lisa_pval < 0.05 ~ "LH",
  
  z_res < 0 &
    lag_z < 0 &
    datos_glm$lisa_pval < 0.05 ~ "LL",
  
  TRUE ~ "No significativo"
  
)


tabla_cuadrantes <- datos_glm %>%
  
  count(cuadrante)

tabla_cuadrantes$pct <- round(
  
  tabla_cuadrantes$n /
    sum(tabla_cuadrantes$n) * 100,
  
  1
  
)

tabla_cuadrantes <- tabla_cuadrantes %>%
  
  arrange(desc(n))

print(tabla_cuadrantes)


# MAPA LISA

mapa_lisa <- datos_glm

mapa_lisa$cx_km <- mapa_lisa$cx / 1000
mapa_lisa$cy_km <- mapa_lisa$cy / 1000

mapa_lisa$cuadrante <- factor(
  
  mapa_lisa$cuadrante,
  
  levels = c(
    "HH",
    "HL",
    "LH",
    "LL",
    "No significativo"
  )
  
)

ggplot(
  mapa_lisa,
  aes(
    x = cx_km,
    y = cy_km,
    color = cuadrante
  )
) +
  
  geom_point(
    size = 0.3,
    alpha = 0.7
  ) +
  
  scale_color_manual(
    values = c(
      "HH" = "red",
      "LL" = "blue",
      "HL" = "orange",
      "LH" = "green",
      "No significativo" = "gray"
    )
  ) +
  
  coord_fixed() +
  
  labs(
    title = "Cuadrantes LISA (2019)",
    x = "Easting (km)",
    y = "Northing (km)",
    color = NULL
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(size = 3)
    )
  ) 

ggsave(
  "output/fig4_6_mapa_lisa.png",
  dpi = 300,
  width = 7,
  height = 10
)

# GETIS-ORD

# Estadístico local Gi*
gi <- as.numeric(
  
  localG(
    datos_glm$n_acc,
    listw = W
  )
  
)

datos_glm$gi_star <- gi

print(summary(gi))


# HOTSPOTS Y COLDSPOTS

# Hotspots 95%
print(
  sum(gi > 1.96)
)

# Hotspots 99%
print(
  sum(gi > 2.576)
)

# Hotspots 99.9%
print(
  sum(gi > 3.29)
)

# Coldspots
print(
  sum(gi < -1.96)
)


# HISTOGRAMA Gi*

ggplot(
  datos_glm,
  aes(x = gi_star)
) +
  
  geom_histogram(
    bins = 60,
    fill = "blue",
    color = "white"
  ) +
  
  geom_vline(
    xintercept = c(-1.96, 1.96),
    color = "red",
    linetype = "dashed"
  ) +
  
  geom_vline(
    xintercept = c(-2.576, 2.576),
    color = "red",
    linetype = "dotted"
  ) +
  
  labs(
    title = "Estadístico Gi* (Getis-Ord)",
    x = "Gi*",
    y = "LSOAs"
  ) 

ggsave(
  "output/fig4_7_histograma_gi_star.png",
  dpi = 300,
  width = 8,
  height = 5
)


# MAPA Gi*

mapa_gi <- datos_glm

mapa_gi$cx_km <- mapa_gi$cx / 1000
mapa_gi$cy_km <- mapa_gi$cy / 1000

mapa_gi$categoria <- case_when(
  
  mapa_gi$gi_star > 2.576 ~ "Hotspot p<0.01",
  
  mapa_gi$gi_star > 1.96 ~ "Hotspot p<0.05",
  
  mapa_gi$gi_star < -1.96 ~ "Coldspot",
  
  TRUE ~ "No significativo"
  
)

mapa_gi$categoria <- factor(
  
  mapa_gi$categoria,
  
  levels = c(
    "Hotspot p<0.01",
    "Hotspot p<0.05",
    "No significativo",
    "Coldspot"
  )
  
)

ggplot(
  mapa_gi,
  aes(
    x = cx_km,
    y = cy_km,
    color = categoria
  )
) +
  
  geom_point(
    size = 0.3,
    alpha = 0.7
  ) +
  
  scale_color_manual(
    values = c(
      "Hotspot p<0.01" = "red",
      "Hotspot p<0.05" = "orange",
      "No significativo" = "gray",
      "Coldspot" = "blue"
    )
  ) +
  
  coord_fixed() +
  
  labs(
    title = "Hotspots Gi* (2019)",
    x = "Easting (km)",
    y = "Northing (km)",
    color = NULL
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(size = 3)
    )
  ) 

ggsave(
  "output/fig4_8_mapa_gi_star.png",
  dpi = 300,
  width = 7,
  height = 10
)


# FUNCIÓN K DE RIPLEY

# Greater London
# Metropolitan Police = 1
# Octubre 2019

col_muestra <- col_tfm %>%
  
  filter(police_force == 1)

col_muestra <- col_muestra %>%
  
  filter(collision_year == 2019)

col_muestra <- col_muestra %>%
  
  filter(mes == 10)

col_muestra <- col_muestra %>%
  
  filter(!is.na(datetime))

col_muestra <- col_muestra %>%
  
  filter(location_easting_osgr > 0)

print(nrow(col_muestra))


# VARIABLES ESPACIO-TEMPORALES

x_km <- col_muestra$location_easting_osgr / 1000

y_km <- col_muestra$location_northing_osgr / 1000

t_h <- col_muestra$t_cont


# FUNCIÓN K

K_st <- function(x, y, t, r, tv) {
  
  n <- length(x)
  
  area <- diff(range(x)) *
    diff(range(y))
  
  T <- diff(range(t))
  
  rho <- n / (area * T)
  
  conteo <- 0
  
  for (i in seq_len(n)) {
    
    d2 <- (x - x[i])^2 +
      (y - y[i])^2
    
    dt <- abs(t - t[i])
    
    conteo <- conteo + sum(
      
      d2 <= r^2 &
        dt <= tv &
        (d2 + dt^2) > 0
      
    )
    
  }
  
  K_obs <- conteo / (n * rho)
  
  K_pois <- pi * r^2 * 2 * tv
  
  ratio <- ifelse(
    K_pois > 0,
    K_obs / K_pois,
    NA
  )
  
  data.frame(
    
    r_km = r,
    
    t_h = tv,
    
    K_obs = K_obs,
    
    K_pois = K_pois,
    
    ratio = ratio
    
  )
  
}


# PARÁMETROS

r_vals <- c(
  0.5,
  1,
  2,
  3,
  5
)

t_vals <- c(
  0.5,
  1,
  2,
  6,
  12,
  24
)


# CÁLCULO RIPLEY

resultados_k <- do.call(
  
  rbind,
  
  lapply(r_vals, function(r) {
    
    do.call(
      
      rbind,
      
      lapply(t_vals, function(tv) {
        
        K_st(
          x_km,
          y_km,
          t_h,
          r = r,
          tv = tv
        )
        
      })
      
    )
    
  })
  
)

print(resultados_k)


# EXPORTAR RESULTADOS

write.csv(
  resultados_k,
  "output/tabla4_1_ripley_K.csv",
  row.names = FALSE
)


# MAPA DE CALOR RIPLEY

ggplot(
  resultados_k,
  aes(
    x = factor(t_h),
    y = factor(r_km),
    fill = ratio
  )
) +
  
  geom_tile(color = "white") +
  
  geom_text(
    aes(
      label = round(ratio, 1)
    ),
    size = 3.5,
    color = "white",
    fontface = "bold"
  ) +
  
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 1,
    name = "K/K_Poisson"
  ) +
  
  labs(
    title = "K observado / K Poisson — Londres, oct. 2019",
    x = "Ventana temporal (h)",
    y = "Radio (km)"
  ) 

ggsave(
  "output/fig4_9_mapa_de_calor_ripley.png",
  dpi = 300,
  width = 9,
  height = 5
)


# CURVA SEGÚN RADIO

graf_radio <- resultados_k %>%
  
  filter(t_h %in% c(0.5, 1, 2, 6))

graf_radio$t_label <- paste0(
  "t=",
  graf_radio$t_h,
  "h"
)

ggplot(
  graf_radio,
  aes(
    x = r_km,
    y = ratio,
    color = t_label,
    group = t_label
  )
) +
  
  geom_line() +
  
  geom_point() +
  
  geom_hline(
    yintercept = 1,
    color = "gray",
    linetype = "dashed"
  ) +
  
  labs(
    title = "K/K_Poisson según radio",
    x = "Radio (km)",
    y = "Ratio",
    color = NULL
  ) 

ggsave(
  "output/fig4_10_curva_ripley_radio.png",
  dpi = 300,
  width = 9,
  height = 5
)

# CURVA SEGÚN VENTANA TEMPORAL

graf_tiempo <- resultados_k %>%
  
  filter(r_km %in% c(0.5, 1, 2, 3))

graf_tiempo$r_label <- paste0(
  "r=",
  graf_tiempo$r_km,
  "km"
)

ggplot(
  graf_tiempo,
  aes(
    x = t_h,
    y = ratio,
    color = r_label,
    group = r_label
  )
) +
  
  geom_line() +
  
  geom_point() +
  
  geom_hline(
    yintercept = 1,
    color = "gray",
    linetype = "dashed"
  ) +
  
  scale_x_continuous(
    breaks = t_vals
  ) +
  
  labs(
    title = "K/K_Poisson según ventana temporal",
    x = "Horas",
    y = "Ratio",
    color = NULL
  ) 

ggsave(
  "output/fig4_11_curva_ripley_tiempo.png",
  dpi = 300,
  width = 9,
  height = 5
)


# CLÚSTERS SEVERIDAD

resultados_sev <- do.call(
  
  rbind,
  
  lapply(c(1, 2, 3), function(s) {
    
    # Etiquetas
    etiq <- c(
      "1" = "Fatal",
      "2" = "Grave",
      "3" = "Leve"
    )[as.character(s)]
    
    
    # Filtrado
    sub <- col_tfm %>%
      
      filter(police_force == 1)
    
    sub <- sub %>%
      
      filter(collision_year == 2019)
    
    sub <- sub %>%
      
      filter(mes == 10)
    
    sub <- sub %>%
      
      filter(sev_num == s)
    
    sub <- sub %>%
      
      filter(!is.na(datetime))
    
    sub <- sub %>%
      
      filter(location_easting_osgr > 0)
    
    
    # Con pocos eventos la estimación es inestable
    if (nrow(sub) < 30) {
      return(NULL)
    }
    
    xs <- sub$location_easting_osgr / 1000
    
    ys <- sub$location_northing_osgr / 1000
    
    ts <- sub$t_cont
    
    
    # Ripley
    do.call(
      
      rbind,
      
      lapply(c(0.5, 1, 2), function(r) {
        
        do.call(
          
          rbind,
          
          lapply(c(0.5, 1), function(tv) {
            
            res <- tryCatch(
              
              K_st(
                xs,
                ys,
                ts,
                r,
                tv
              ),
              
              error = function(e) NULL
              
            )
            
            if (is.null(res)) {
              return(NULL)
            }
            
            cbind(
              
              severidad = etiq,
              
              n_eventos = nrow(sub),
              
              res
              
            )
            
          })
          
        )
        
      })
      
    )
    
  })
  
)

print(resultados_sev)


# RESUMEN

ratio_sev <- resultados_sev %>%
  
  filter(r_km <= 1)

ratio_sev <- ratio_sev %>%
  
  filter(t_h <= 1)

ratio_sev <- ratio_sev %>%
  
  group_by(severidad) %>%
  
  summarise(
    
    ratio_medio = round(
      mean(ratio, na.rm = TRUE),
      3
    ),
    
    n = first(
      as.integer(n_eventos)
    ),
    
    .groups = "drop"
    
  )

ratio_sev <- ratio_sev %>%
  
  arrange(desc(ratio_medio))

print(ratio_sev)


# EXPORTAR TABLA

write.csv(
  
  resultados_sev,
  
  "output/tabla4_2_ripley_severidad.csv",
  
  row.names = FALSE
  
)


# GRÁFICO SEVERIDAD

graf_sev <- resultados_sev %>%
  
  filter(t_h == 1)

ggplot(
  graf_sev,
  aes(
    x = r_km,
    y = ratio,
    color = severidad,
    group = severidad
  )
) +
  
  geom_line() +
  
  geom_point() +
  
  geom_hline(
    yintercept = 1,
    color = "gray",
    linetype = "dashed"
  ) +
  
  scale_color_manual(
    values = c(
      "Fatal" = "red",
      "Grave" = "orange",
      "Leve" = "blue"
    )
  ) +
  
  labs(
    title = "K/K_Poisson por severidad (t = 1h)",
    x = "Radio (km)",
    y = "Ratio",
    color = NULL
  ) 

ggsave(
  "output/fig4_12_ripley_severidad.png",
  dpi = 300,
  width = 9,
  height = 5
)
