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
install.packages(
  "fmesher",
  repos = c(INLA = "https://inla.r-inla-download.org/R/testing"),
  type  = "binary"
)

library(INLA)

# Carpeta donde guardar figuras y tablas
dir.create("output", showWarnings = FALSE)

# CARGA DE DATOS

# Descarga manual desde https://www.gov.uk/government/statistical-data-sets/road-safety-open-data
csv_hist <- "C:/Users/gagik/Downloads/TFM CODIGO/dft-road-casualty-statistics-collision-1979-latest-published-year.csv"

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
# 1 Fatal o Mortal
# 2 Serious o Grave
# 3 Slight o Leve

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

gini_esp <- abs(1 - 2 * sum(
  diff(lorenz$x) *
    (lorenz$y[-1] + lorenz$y[-nrow(lorenz)]) / 2
))

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
  
  filter(police_force %in% c("1", "Metropolitan Police"))

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
      
      filter(police_force %in% c("1", "Metropolitan Police"))
    
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

# CARGA DE VEHÍCULOS Y VÍCTIMAS

csv_veh <- "C:/Users/gagik/Downloads/TFM CODIGO/dft-road-casualty-statistics-vehicle-1979-latest-published-year.csv"
csv_cas <- "C:/Users/gagik/Downloads/TFM CODIGO/dft-road-casualty-statistics-casualty-1979-latest-published-year.csv"

veh <- fread(
  csv_veh,
  select = c(
    "collision_index",
    "collision_year",
    "journey_purpose_of_driver",
    "age_of_driver",
    "sex_of_driver",
    "vehicle_type",
    "lsoa_of_driver",
    "driver_imd_decile"
  ),
  nThread = 1
) %>%
  filter(collision_year %in% 2015:2019)

cas <- fread(
  csv_cas,
  select = c(
    "collision_index",
    "collision_year",
    "casualty_class",
    "casualty_severity",
    "casualty_type",
    "lsoa_of_casualty"
  ),
  nThread = 1
) %>%
  filter(collision_year %in% 2015:2019) %>%
  select(-collision_year)

veh_acc <- veh %>%
  group_by(collision_index) %>%
  slice(1) %>%
  ungroup() %>%
  select(-collision_year)

col_v <- col_tfm %>%
  filter(collision_year %in% 2015:2019) %>%
  left_join(veh_acc, by = "collision_index")

col_v$journey_cat <- case_when(
  col_v$journey_purpose_of_driver %in% c("1", "Journey as part of work")       ~ "Laboral",
  col_v$journey_purpose_of_driver %in% c("2", "Commuting to/from work")         ~ "Desplazamiento",
  col_v$journey_purpose_of_driver %in% c("3", "Taking pupil to/from school")    ~ "Escolar",
  col_v$journey_purpose_of_driver %in% c("4", "5", "Other/personal")            ~ "Otros",
  TRUE ~ NA_character_
)

print(table(col_v$journey_cat, useNA = "always"))


# PATRÓN HORARIO POR PROPÓSITO DEL VIAJE

patron_journey <- col_v %>%
  filter(!is.na(journey_cat)) %>%
  group_by(journey_cat, hora_dia) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(journey_cat) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ungroup()

ggplot(
  patron_journey,
  aes(x = hora_dia, y = pct, color = journey_cat, group = journey_cat)
) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Patrón horario según propósito del viaje",
    x = "Hora",
    y = "% del total diario",
    color = NULL
  )

ggsave("output/fig4_13_patron_journey.png", dpi = 300, width = 11, height = 5)


# CLUSTERING POR PROPÓSITO DEL VIAJE

for (jcat in c("Laboral", "Desplazamiento", "Otros")) {
  
  sub_j <- col_v %>%
    filter(
      journey_cat == jcat,
      police_force %in% c("1", "Metropolitan Police"),
      collision_year == 2019,
      mes == 10,
      !is.na(datetime),
      location_easting_osgr > 0
    )
  
  if (nrow(sub_j) < 30) next
  
  res_j <- K_st(
    sub_j$location_easting_osgr / 1000,
    sub_j$location_northing_osgr / 1000,
    sub_j$t_cont,
    r = 0.5,
    tv = 0.5
  )
  
  cat(jcat, "n =", nrow(sub_j),
      "ratio Kobs/Kpois (r=0.5km, t=30min):", round(res_j$ratio, 2), "\n")
}


# CONDUCTOR LOCAL - FORÁNEO

col_v$conductor_local <- col_v$lsoa_of_driver == col_v$lsoa_of_accident_location

cat("Proporción conductores locales:",
    round(mean(col_v$conductor_local, na.rm = TRUE) * 100, 1), "%\n")


# PEATONES POR LSOA

peatones_lsoa <- cas %>%
  filter(casualty_type %in% c("0", "Pedestrian")) %>%
  left_join(
    col_tfm %>% select(collision_index, lsoa_of_accident_location),
    by = "collision_index"
  ) %>%
  filter(!is.na(lsoa_of_accident_location)) %>%
  count(lsoa_of_accident_location, name = "n_peatones")

print(nrow(peatones_lsoa))
print(summary(peatones_lsoa$n_peatones))


# CAPÍTULO 5 - MODELO ESPACIAL BYM2

library(INLA)
library(dplyr)
library(ggplot2)
library(spdep)
library(scales)

# Descarga manual desde
# https://www.gov.uk/government/statistics/english-indices-of-deprivation-2019
csv_imd <- "C:/Users/gagik/Downloads/TFM CODIGO/File_7_-_All_IoD2019_Scores__Ranks__Deciles_and_Population_Denominators_3.csv"

imd_raw <- read.csv(csv_imd)

# Me quedo con el código de zona, la puntuación y el decil
imd <- imd_raw[, c(
  "LSOA.code..2011.",
  "Index.of.Multiple.Deprivation..IMD..Score",
  "Index.of.Multiple.Deprivation..IMD..Decile..where.1.is.most.deprived.10..of.LSOAs."
)]

names(imd) <- c(
  "lsoa_of_accident_location",
  "imd_score",
  "imd_decile"
)


# DATOS DEL MODELO

# Años de estimación
anios_bym <- 2015:2019

# Accidentes por zona en el periodo de estimación
acc_bym <- col_tfm %>%
  
  filter(collision_year %in% anios_bym) %>%
  
  filter(!is.na(lsoa_of_accident_location)) %>%
  
  filter(lsoa_of_accident_location != "") %>%
  
  filter(lsoa_of_accident_location != "-1") %>%
  
  count(
    lsoa_of_accident_location,
    name = "n_acc"
  )


# Histórico completo por zona para la exposición
acc_total <- col_tfm %>%
  
  filter(!is.na(lsoa_of_accident_location)) %>%
  
  filter(lsoa_of_accident_location != "") %>%
  
  filter(lsoa_of_accident_location != "-1") %>%
  
  count(
    lsoa_of_accident_location,
    name = "n_total"
  )


# Velocidad límite media por zona
vel_media <- col_tfm %>%
  
  filter(collision_year %in% anios_bym) %>%
  
  filter(!is.na(lsoa_of_accident_location)) %>%
  
  filter(lsoa_of_accident_location != "") %>%
  
  filter(!is.na(speed_limit)) %>%
  
  mutate(
    speed_limit = as.numeric(speed_limit)
  ) %>%
  
  filter(!is.na(speed_limit)) %>%
  
  filter(speed_limit > 0) %>%
  
  group_by(lsoa_of_accident_location) %>%
  
  summarise(
    vel_media = mean(speed_limit, na.rm = TRUE),
    .groups = "drop"
  )


# Proporción de accidentes en zona urbana
prop_urb <- col_tfm %>%
  
  filter(collision_year %in% anios_bym) %>%
  
  filter(!is.na(lsoa_of_accident_location)) %>%
  
  filter(lsoa_of_accident_location != "") %>%
  
  filter(!is.na(urban_or_rural_area)) %>%
  
  group_by(lsoa_of_accident_location) %>%
  
  summarise(
    
    prop_urbano = mean(
      urban_or_rural_area %in% c("1", "Urban"),
      na.rm = TRUE
    ),
    
    .groups = "drop"
    
  )


# Proporción de conductores menores de 25 años
jovenes_lsoa <- col_v %>%
  
  filter(collision_year %in% anios_bym) %>%
  
  filter(!is.na(lsoa_of_accident_location)) %>%
  
  filter(lsoa_of_accident_location != "") %>%
  
  filter(!is.na(age_of_driver)) %>%
  
  mutate(
    age_of_driver = as.numeric(age_of_driver)
  ) %>%
  
  filter(!is.na(age_of_driver)) %>%
  
  group_by(lsoa_of_accident_location) %>%
  
  summarise(
    prop_joven = mean(age_of_driver < 25, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  distinct(
    lsoa_of_accident_location,
    .keep_all = TRUE
  )


# Proporción de accidentes en viaje laboral
laboral_lsoa <- col_v %>%
  
  filter(collision_year %in% anios_bym) %>%
  
  filter(!is.na(lsoa_of_accident_location)) %>%
  
  filter(lsoa_of_accident_location != "") %>%
  
  filter(!is.na(journey_cat)) %>%
  
  group_by(lsoa_of_accident_location) %>%
  
  summarise(
    prop_laboral = mean(journey_cat == "Laboral", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  distinct(
    lsoa_of_accident_location,
    .keep_all = TRUE
  )


# Unión de todas las variables
datos_bym <- acc_bym %>%
  
  left_join(
    acc_total,
    by = "lsoa_of_accident_location"
  ) %>%
  
  mutate(
    esperado = n_total * length(anios_bym) / length(anios)
  ) %>%
  
  left_join(
    imd,
    by = "lsoa_of_accident_location"
  ) %>%
  
  left_join(
    vel_media,
    by = "lsoa_of_accident_location"
  ) %>%
  
  left_join(
    prop_urb,
    by = "lsoa_of_accident_location"
  ) %>%
  
  left_join(
    jovenes_lsoa,
    by = "lsoa_of_accident_location"
  ) %>%
  
  left_join(
    laboral_lsoa,
    by = "lsoa_of_accident_location"
  ) %>%
  
  mutate(
    
    prop_laboral = ifelse(
      is.na(prop_laboral),
      0,
      prop_laboral
    ),
    
    prop_joven = ifelse(
      is.na(prop_joven),
      0,
      prop_joven
    )
    
  ) %>%
  
  filter(!is.na(esperado)) %>%
  
  filter(!is.na(imd_score)) %>%
  
  filter(!is.na(vel_media)) %>%
  
  filter(!is.na(prop_urbano)) %>%
  
  filter(esperado > 0)


# Número de zonas en el modelo
print(nrow(datos_bym))


# VECINDAD

# Centroide de cada zona a partir de sus accidentes
centroides_bym <- col_tfm %>%
  
  filter(!is.na(lsoa_of_accident_location)) %>%
  
  filter(lsoa_of_accident_location != "") %>%
  
  filter(location_easting_osgr > 0) %>%
  
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

datos_bym <- datos_bym %>%
  
  left_join(
    centroides_bym,
    by = "lsoa_of_accident_location"
  ) %>%
  
  filter(!is.na(cx)) %>%
  
  filter(!is.na(cy))


# 6 vecinos más próximos, igual que en el capítulo 4
coords_bym <- as.matrix(
  datos_bym[, c("cx", "cy")]
)

nb_bym <- make.sym.nb(
  
  knn2nb(
    knearneigh(
      coords_bym,
      k = 6
    )
  )
  
)

# INLA necesita el grafo en su propio formato
nb2INLA(
  "output/grafo_vecindad.adj",
  nb_bym
)

grafo <- inla.read.graph(
  "output/grafo_vecindad.adj"
)


# ESTANDARIZACIÓN

datos_bym$imd_std <- scale(datos_bym$imd_score)[, 1]
datos_bym$vel_std <- scale(datos_bym$vel_media)[, 1]
datos_bym$urb_std <- scale(datos_bym$prop_urbano)[, 1]

datos_bym$idx <- 1:nrow(datos_bym)


# MODELO BYM2

formula_bym <- n_acc ~ 1 + imd_std + vel_std + urb_std +
  
  f(
    idx,
    model = "bym2",
    graph = grafo,
    scale.model = TRUE,
    constr = TRUE,
    
    hyper = list(
      phi  = list(prior = "pc",      param = c(0.5, 0.5)),
      prec = list(prior = "pc.prec", param = c(1, 0.01))
    )
    
  )

modelo_bym <- inla(
  
  formula_bym,
  
  family = "poisson",
  
  data = datos_bym,
  
  E = datos_bym$esperado,
  
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  ),
  
  control.predictor = list(
    compute = TRUE
  ),
  
  control.inla = list(
    strategy = "simplified.laplace",
    int.strategy = "eb",
    h = 0.01
  ),
  
  control.mode = list(
    theta = c(
      log(1963),
      log(0.279 / (1 - 0.279))
    ),
    restart = TRUE
  )
  
)

print(summary(modelo_bym))

print(round(modelo_bym$summary.fixed, 4))

print(round(modelo_bym$summary.hyperpar, 4))

cat("DIC:", modelo_bym$dic$dic, "\n")
cat("WAIC:", modelo_bym$waic$waic, "\n")


# RIESGO RELATIVO POR ZONA

datos_bym$rr_medio <-
  modelo_bym$summary.fitted.values$mean /
  datos_bym$esperado

datos_bym$rr_q025 <-
  modelo_bym$summary.fitted.values$`0.025quant` /
  datos_bym$esperado

datos_bym$rr_q975 <-
  modelo_bym$summary.fitted.values$`0.975quant` /
  datos_bym$esperado

print(summary(datos_bym$rr_medio))


# COMPARACIÓN CON UN GLM SIN COMPONENTE ESPACIAL

glm_simple <- glm(
  
  n_acc ~ imd_std + vel_std + urb_std +
    offset(log(esperado)),
  
  family = poisson(link = "log"),
  
  data = datos_bym
  
)

datos_bym$residuo_glm <- residuals(
  glm_simple,
  type = "pearson"
)

datos_bym$residuo_bym <-
  (datos_bym$n_acc - modelo_bym$summary.fitted.values$mean) /
  sqrt(modelo_bym$summary.fitted.values$mean)

W_bym <- nb2listw(
  nb_bym,
  style = "W"
)

# Moran sobre los residuos de cada modelo
print(
  moran.test(
    datos_bym$residuo_glm,
    listw = W_bym,
    alternative = "greater"
  )
)

print(
  moran.test(
    datos_bym$residuo_bym,
    listw = W_bym,
    alternative = "greater"
  )
)


# FIGURAS DEL MODELO ESPACIAL

datos_bym$cx_km <- datos_bym$cx / 1000
datos_bym$cy_km <- datos_bym$cy / 1000

# Corto la escala en el percentil 99 para que
# los valores extremos no aplasten el resto
p99_rr <- quantile(datos_bym$rr_medio, 0.99)

ggplot(
  datos_bym,
  aes(
    x = cx_km,
    y = cy_km,
    color = rr_medio
  )
) +
  
  geom_point(
    size = 0.3,
    alpha = 0.7
  ) +
  
  scale_color_gradientn(
    colours = c("grey", "yellow", "orange", "red", "brown"),
    limits = c(0, p99_rr),
    oob = squish,
    name = "RR"
  ) +
  
  theme_minimal() +
  
  coord_fixed() +
  
  labs(
    title = "Riesgo relativo por zona",
    x = "Easting (km)",
    y = "Northing (km)"
  )

ggsave(
  "output/fig5_1_mapa_riesgo_relativo.png",
  dpi = 300,
  width = 7,
  height = 10
)


# Histograma del riesgo relativo
ggplot(
  datos_bym,
  aes(x = rr_medio)
) +
  
  geom_histogram(
    bins = 80,
    fill = "blue",
    color = "white"
  ) +
  
  geom_vline(
    xintercept = 1,
    color = "red",
    linetype = "dashed"
  ) +
  
  labs(
    title = "Distribucion del riesgo relativo",
    x = "Riesgo relativo",
    y = "Zonas"
  )

ggsave(
  "output/fig5_2_hist_riesgo_relativo.png",
  dpi = 300,
  width = 9,
  height = 5
)


# Diagrama de Moran de los residuos del BYM
png(
  "output/fig5_3_moran_residuos_bym.png",
  width = 1800,
  height = 1800,
  res = 300
)

moran.plot(
  
  datos_bym$residuo_bym,
  
  listw = W_bym,
  
  xlab = "Residuo BYM (zona i)",
  
  ylab = "Media residuos vecinos",
  
  main = "Diagrama de Moran - residuos BYM2",
  
  pch = 16,
  
  cex = 0.3,
  
  col = "blue"
  
)

dev.off()


# Efectos fijos con sus intervalos
ef <- as.data.frame(modelo_bym$summary.fixed)

ef$variable <- rownames(ef)

ef <- ef[ef$variable != "(Intercept)", ]

ef$variable <- c(
  "IMD",
  "Velocidad media",
  "Proporcion urbana"
)

ggplot(
  ef,
  aes(
    x = variable,
    y = mean
  )
) +
  
  geom_point(
    size = 3,
    color = "blue"
  ) +
  
  geom_errorbar(
    aes(
      ymin = `0.025quant`,
      ymax = `0.975quant`
    ),
    width = 0.2,
    color = "blue"
  ) +
  
  geom_hline(
    yintercept = 0,
    color = "red",
    linetype = "dashed"
  ) +
  
  labs(
    title = "Efectos fijos del modelo BYM2",
    x = NULL,
    y = "Estimacion"
  ) +
  
  coord_flip()

ggsave(
  "output/fig5_4_efectos_fijos.png",
  dpi = 300,
  width = 8,
  height = 5
)


# SUPERFICIE DE RIESGO PARA EL HAWKES

# Guardo el riesgo relativo normalizado de cada zona
# para usarlo después como tasa base
mu_lsoa <- datos_bym %>%
  
  select(
    lsoa_of_accident_location,
    rr_medio
  ) %>%
  
  mutate(
    mu_norm = rr_medio / mean(rr_medio)
  )

saveRDS(
  mu_lsoa,
  "output/mu_lsoa_bym.rds"
)

print(nrow(mu_lsoa))
print(round(mean(mu_lsoa$mu_norm), 4))
print(round(range(mu_lsoa$mu_norm), 3))


# PERFIL DEL CONDUCTOR POR QUINTIL DE RIESGO

perfil_conductor <- col_v %>%
  
  filter(!is.na(lsoa_of_accident_location)) %>%
  
  left_join(
    mu_lsoa,
    by = "lsoa_of_accident_location"
  ) %>%
  
  filter(!is.na(mu_norm)) %>%
  
  mutate(
    age_of_driver = as.numeric(age_of_driver),
    quintil_riesgo = ntile(mu_norm, 5)
  ) %>%
  
  group_by(quintil_riesgo) %>%
  
  summarise(
    
    edad_media = round(
      mean(age_of_driver, na.rm = TRUE),
      1
    ),
    
    pct_hombre = round(
      mean(sex_of_driver %in% c("1", "Male"), na.rm = TRUE) * 100,
      1
    ),
    
    pct_laboral = round(
      mean(journey_cat == "Laboral", na.rm = TRUE) * 100,
      1
    ),
    
    pct_local = round(
      mean(conductor_local, na.rm = TRUE) * 100,
      1
    ),
    
    .groups = "drop"
    
  )

print(perfil_conductor)

write.csv(
  perfil_conductor,
  "output/tabla5_perfil_conductor.csv",
  row.names = FALSE
)


# CAPÍTULO 5 - PROCESO DE HAWKES TEMPORAL

library(dplyr)
library(lubridate)
library(ggplot2)
library(Rcpp)

mu_lsoa <- readRDS("output/mu_lsoa_bym.rds")

# Valor inicial de beta a partir de una vida media de 23 minutos
beta_init <- log(2) / (23/60)


# Eventos con zona, tiempo y severidad
eventos <- col_tfm %>%
  
  filter(collision_year %in% 2015:2019) %>%
  
  filter(!is.na(t_cont)) %>%
  
  filter(!is.na(sev_num)) %>%
  
  filter(location_easting_osgr > 0) %>%
  
  filter(!is.na(lsoa_of_accident_location)) %>%
  
  filter(lsoa_of_accident_location != "") %>%
  
  filter(lsoa_of_accident_location != "-1") %>%
  
  left_join(
    mu_lsoa,
    by = "lsoa_of_accident_location"
  ) %>%
  
  filter(!is.na(mu_norm)) %>%
  
  arrange(t_cont) %>%
  
  mutate(
    x_km = location_easting_osgr / 1000,
    y_km = location_northing_osgr / 1000
  )

print(nrow(eventos))
print(table(eventos$sev_num))


# FUNCIONES DEL MODELO

# A es la suma de los efectos de los eventos anteriores
# Con kernel exponencial se calcula de forma recursiva
calcular_A <- function(t, beta) {
  
  n <- length(t)
  A <- numeric(n)
  
  for (i in 2:n) {
    A[i] <- exp(-beta * (t[i] - t[i-1])) * (A[i-1] + 1)
  }
  
  A
}


# Log-verosimilitud con K distinto por severidad
log_verosimilitud_temporal <- function(params, t, sev, mu, T_obs) {
  
  K_leve  <- exp(params[1])
  K_grave <- exp(params[2])
  K_fatal <- exp(params[3])
  beta    <- exp(params[4])
  
  n <- length(t)
  
  K_vec <- ifelse(
    sev == 1,
    K_fatal,
    ifelse(sev == 2, K_grave, K_leve)
  )
  
  A <- calcular_A(t, beta)
  
  lambda_total <- mu + K_vec * A
  
  if (any(lambda_total <= 0)) return(1e10)
  
  term1 <- sum(log(lambda_total))
  
  term2 <- sum(mu) * T_obs / n +
    sum(K_vec * (1 - exp(-beta * (T_obs - t))) / beta)
  
  -(term1 - term2)
}


# Tiempos transformados para el diagnóstico de Ogata
calcular_tau <- function(t, sev, mu, params) {
  
  K_vec <- ifelse(
    sev == 1,
    params["K_fatal"],
    ifelse(sev == 2, params["K_grave"], params["K_leve"])
  )
  
  beta <- params["beta"]
  
  n <- length(t)
  tau <- numeric(n)
  
  for (i in 1:n) {
    
    tau_fondo <- sum(mu[1:i]) * t[i] / i
    
    tau_cluster <- if (i > 1) {
      sum(
        K_vec[1:(i-1)] *
          (1 - exp(-beta * (t[i] - t[1:(i-1)]))) / beta
      )
    } else 0
    
    tau[i] <- tau_fondo + tau_cluster
  }
  
  tau
}


# Versión en C++ para la parte espacio-temporal
# El doble bucle en R puro tardaría demasiado
cppFunction('
NumericVector calcular_cluster(
    NumericVector t, NumericVector x, NumericVector y,
    NumericVector K_vec, double beta, double sigma,
    double max_dt, double max_d2) {

  int n = t.size();
  NumericVector lambda_cluster(n, 0.0);
  double sigma2 = 2.0 * sigma * sigma;
  double norm   = 2.0 * M_PI * sigma * sigma;

  for (int i = 1; i < n; i++) {
    for (int j = i - 1; j >= 0; j--) {
      double dt = t[i] - t[j];
      if (dt > max_dt) break;
      double d2 = pow(x[i] - x[j], 2) + pow(y[i] - y[j], 2);
      if (d2 > max_d2) continue;
      double g = beta * exp(-beta * dt);
      double f = exp(-d2 / sigma2) / norm;
      lambda_cluster[i] += K_vec[j] * g * f;
    }
  }
  return lambda_cluster;
}
')


# ESTIMACIÓN SOBRE LONDRES

eventos_est <- eventos %>%
  
  filter(police_force %in% c("1", "Metropolitan Police")) %>%
  
  filter(collision_year %in% 2015:2019) %>%
  
  arrange(t_cont)


# Factor horario a partir de la distribución del capítulo 4
factor_horario <- por_hora$media_anual / mean(por_hora$media_anual)

eventos_est <- eventos_est %>%
  
  mutate(
    mu_horario = mu_norm * factor_horario[hora_dia + 1]
  )

cat("Accidentes Londres estimacion:", nrow(eventos_est), "\n")

t_est   <- eventos_est$t_cont - min(eventos_est$t_cont)
sev_est <- eventos_est$sev_num
mu_est  <- eventos_est$mu_horario
T_est   <- max(t_est)


# Optimización en escala logarítmica para evitar negativos
resultado <- optim(
  
  par = log(c(0.01, 0.01, 0.01, beta_init)),
  
  fn = log_verosimilitud_temporal,
  
  t = t_est,
  sev = sev_est,
  mu = mu_est,
  T_obs = T_est,
  
  method = "L-BFGS-B",
  
  lower = log(c(1e-6, 1e-6, 1e-6, 0.1)),
  upper = log(c(0.99, 0.99, 0.99, 1000)),
  
  control = list(
    maxit = 500,
    trace = 1
  )
  
)

params_est <- exp(resultado$par)

names(params_est) <- c(
  "K_leve",
  "K_grave",
  "K_fatal",
  "beta"
)

print(round(params_est, 4))

cat("Convergencia:", resultado$convergence, "\n")
cat("Log-verosimilitud:", -resultado$value, "\n")


# COMPARACIÓN CON UN MODELO SIN DEPENDENCIA TEMPORAL

# Misma tasa base pero accidentes independientes
loglik_base <- sum(log(mu_est)) -
  sum(mu_est) * T_est / length(t_est)

cat("Log-verosimilitud modelo base:", round(loglik_base, 2), "\n")
cat("Mejora al añadir Hawkes:", round(-resultado$value - loglik_base, 2), "\n")


# INTERVALOS DE CONFIANZA

# Hessiana en el óptimo para el error estándar
hess <- optimHess(
  
  par = resultado$par,
  
  fn = log_verosimilitud_temporal,
  
  t = t_est,
  sev = sev_est,
  mu = mu_est,
  T_obs = T_est
  
)

se_log <- sqrt(diag(solve(hess)))

ic_lower <- exp(resultado$par - 1.96 * se_log)
ic_upper <- exp(resultado$par + 1.96 * se_log)

ic_tabla <- data.frame(
  parametro = c("K_leve", "K_grave", "K_fatal", "beta"),
  estimacion = round(params_est, 4),
  ic_lower = round(ic_lower, 4),
  ic_upper = round(ic_upper, 4)
)

print(ic_tabla)


# Eta global como media de los K ponderada por severidad
prop_sev <- prop.table(table(sev_est))

eta_global <- params_est["K_leve"]  * prop_sev["3"] +
  params_est["K_grave"] * prop_sev["2"] +
  params_est["K_fatal"] * prop_sev["1"]

# Error estándar de eta por el método delta
eta_grad <- c(prop_sev["3"], prop_sev["2"], prop_sev["1"], 0)

se_eta <- sqrt(
  t(eta_grad) %*% solve(hess) %*% eta_grad
)

eta_val <- as.numeric(eta_global)

cat("Eta global:", round(eta_val, 4), "\n")

cat("Eta IC 95%:",
    round(eta_val - 1.96 * se_eta, 4),
    "-",
    round(eta_val + 1.96 * se_eta, 4), "\n")


# Vida media en minutos con su intervalo
vm_val <- log(2) / params_est["beta"] * 60

se_vm <- abs(-log(2) * 60 / params_est["beta"]^2) *
  sqrt(solve(hess)[4, 4]) * params_est["beta"]

cat("Vida media:", round(vm_val, 1), "minutos\n")

cat("Vida media IC 95%:",
    round(vm_val - 1.96 * se_vm, 1),
    "-",
    round(vm_val + 1.96 * se_vm, 1), "minutos\n")


# DIAGNÓSTICO DE OGATA

tau <- calcular_tau(t_est, sev_est, mu_est, params_est)

diferencias_tau <- diff(tau)

# Si el modelo ajusta bien deberían ser Exp(1)
ks_test <- ks.test(
  diferencias_tau,
  "pexp",
  rate = 1
)

print(ks_test)


# QQ plot
tau_ord <- sort(diferencias_tau)

n_tau <- length(tau_ord)

exp_teo <- qexp(
  seq(
    1 / (n_tau + 1),
    n_tau / (n_tau + 1),
    length.out = n_tau
  )
)

ggplot(
  data.frame(
    teo = exp_teo,
    obs = tau_ord
  ),
  aes(
    x = teo,
    y = obs
  )
) +
  
  geom_point(
    size = 0.3,
    alpha = 0.5,
    color = "blue"
  ) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    color = "red",
    linetype = "dashed"
  ) +
  
  labs(
    title = "QQ plot residuos de Ogata",
    x = "Cuantiles teoricos Exp(1)",
    y = "Cuantiles observados"
  )

ggsave(
  "output/fig6_1_ogata_qqplot.png",
  dpi = 300,
  width = 7,
  height = 7
)

saveRDS(
  
  list(
    params = params_est,
    eta_global = eta_global,
    vida_media_min = vm_val,
    loglik = -resultado$value,
    convergencia = resultado$convergence,
    ks_ogata = ks_test
  ),
  
  "output/resultados_hawkes.rds"
  
)


# COMPROBACIÓN CON FACTOR METEOROLÓGICO

# Lluvia con o sin viento fuerte
col_tfm$lluvia <- col_tfm$weather_conditions %in% c(
  "2", "Raining no high winds",
  "5", "Raining + high winds"
)

# Peso de los accidentes en lluvia frente a tiempo seco
factor_meteo <- col_tfm %>%
  
  filter(collision_year %in% 2015:2019) %>%
  
  filter(police_force %in% c("1", "Metropolitan Police")) %>%
  
  filter(!is.na(weather_conditions)) %>%
  
  filter(weather_conditions != "-1") %>%
  
  group_by(lluvia) %>%
  
  summarise(
    n = n(),
    .groups = "drop"
  ) %>%
  
  mutate(
    factor = n / mean(n)
  )

factor_lluvia_seco <- factor_meteo$factor

names(factor_lluvia_seco) <- as.character(factor_meteo$lluvia)

cat("Factor lluvia:", round(factor_lluvia_seco["TRUE"], 3), "\n")
cat("Factor seco:", round(factor_lluvia_seco["FALSE"], 3), "\n")


# Tasa base con zona, hora y clima
eventos_est_meteo <- eventos_est %>%
  
  mutate(
    
    lluvia = weather_conditions %in% c(
      "2", "Raining no high winds",
      "5", "Raining + high winds"
    ),
    
    factor_met = ifelse(
      lluvia,
      factor_lluvia_seco["TRUE"],
      factor_lluvia_seco["FALSE"]
    ),
    
    mu_meteo = mu_norm *
      factor_horario[hora_dia + 1] *
      factor_met
    
  )

t_met   <- eventos_est_meteo$t_cont - min(eventos_est_meteo$t_cont)
sev_met <- eventos_est_meteo$sev_num
mu_met  <- eventos_est_meteo$mu_meteo
T_met   <- max(t_met)

resultado_meteo <- optim(
  
  par = log(c(0.01, 0.01, 0.01, beta_init)),
  
  fn = log_verosimilitud_temporal,
  
  t = t_met,
  sev = sev_met,
  mu = mu_met,
  T_obs = T_met,
  
  method = "L-BFGS-B",
  
  lower = log(c(1e-6, 1e-6, 1e-6, 0.1)),
  upper = log(c(0.99, 0.99, 0.99, 1000)),
  
  control = list(
    maxit = 500,
    trace = 1
  )
  
)

params_meteo <- exp(resultado_meteo$par)

names(params_meteo) <- c(
  "K_leve",
  "K_grave",
  "K_fatal",
  "beta"
)

print(round(params_meteo, 4))

cat("Convergencia:", resultado_meteo$convergence, "\n")

prop_sev_met <- prop.table(table(sev_met))

eta_meteo <- params_meteo["K_leve"]  * prop_sev_met["3"] +
  params_meteo["K_grave"] * prop_sev_met["2"] +
  params_meteo["K_fatal"] * prop_sev_met["1"]

cat("Eta con meteorologia:", round(eta_meteo, 4), "\n")
cat("Eta sin meteorologia:", round(eta_global, 4), "\n")

cat("Diferencia:",
    round(as.numeric(eta_global) - as.numeric(eta_meteo), 4), "\n")

cat("Vida media con meteorologia:",
    round(log(2) / params_meteo["beta"] * 60, 1), "minutos\n")


# HAWKES POR PROPÓSITO DEL VIAJE

eventos_journey <- eventos %>%
  
  filter(police_force %in% c("1", "Metropolitan Police")) %>%
  
  filter(collision_year %in% 2015:2019) %>%
  
  left_join(
    
    veh %>%
      group_by(collision_index) %>%
      slice(1) %>%
      ungroup() %>%
      select(collision_index, journey_purpose_of_driver),
    
    by = "collision_index"
    
  ) %>%
  
  mutate(
    
    journey_cat = case_when(
      journey_purpose_of_driver %in% c("1", "Journey as part of work")     ~ "Laboral",
      journey_purpose_of_driver %in% c("2", "Commuting to/from work")      ~ "Desplazamiento",
      journey_purpose_of_driver %in% c("3", "Taking pupil to/from school") ~ "Escolar",
      journey_purpose_of_driver %in% c("4", "5", "Other/personal")         ~ "Otros",
      TRUE ~ NA_character_
    )
    
  ) %>%
  
  filter(!is.na(journey_cat)) %>%
  
  mutate(
    
    journey_num = case_when(
      journey_cat == "Laboral"        ~ 1L,
      journey_cat == "Desplazamiento" ~ 2L,
      TRUE                            ~ 3L
    ),
    
    mu_horario = mu_norm * factor_horario[hora_dia + 1]
    
  ) %>%
  
  arrange(t_cont)

print(nrow(eventos_journey))
print(table(eventos_journey$journey_cat))


# Igual que la log-verosimilitud por severidad
# pero con K según el propósito del viaje
log_verosimilitud_journey <- function(params, t, journey, mu, T_obs) {
  
  K_lab  <- exp(params[1])
  K_desp <- exp(params[2])
  K_otro <- exp(params[3])
  beta   <- exp(params[4])
  
  n <- length(t)
  
  K_vec <- ifelse(
    journey == 1L,
    K_lab,
    ifelse(journey == 2L, K_desp, K_otro)
  )
  
  A <- calcular_A(t, beta)
  
  lambda_total <- mu + K_vec * A
  
  if (any(lambda_total <= 0)) return(1e10)
  
  term1 <- sum(log(lambda_total))
  
  term2 <- sum(mu) * T_obs / n +
    sum(K_vec * (1 - exp(-beta * (T_obs - t))) / beta)
  
  -(term1 - term2)
}

t_j  <- eventos_journey$t_cont - min(eventos_journey$t_cont)
j_j  <- eventos_journey$journey_num
mu_j <- eventos_journey$mu_horario
T_j  <- max(t_j)

resultado_journey <- optim(
  
  par = log(c(0.01, 0.01, 0.01, beta_init)),
  
  fn = log_verosimilitud_journey,
  
  t = t_j,
  journey = j_j,
  mu = mu_j,
  T_obs = T_j,
  
  method = "L-BFGS-B",
  
  lower = log(c(1e-6, 1e-6, 1e-6, 0.1)),
  upper = log(c(0.99, 0.99, 0.99, 1000)),
  
  control = list(
    maxit = 500,
    trace = 1
  )
  
)

params_journey <- exp(resultado_journey$par)

names(params_journey) <- c(
  "K_laboral",
  "K_desplazamiento",
  "K_otro",
  "beta"
)

print(round(params_journey, 4))

cat("Convergencia:", resultado_journey$convergence, "\n")

prop_journey <- prop.table(table(j_j))

eta_journey <- params_journey["K_laboral"] * prop_journey["1"] +
  params_journey["K_desplazamiento"] * prop_journey["2"]

cat("Eta journey:", round(eta_journey, 4), "\n")

cat("Vida media journey:",
    round(log(2) / params_journey["beta"] * 60, 1), "minutos\n")


# CAPÍTULO 6 - VALIDACIÓN CON OTRO PERIODO

# VALIDACIÓN ESPACIAL

# Accidentes por zona en 2022-2024
acc_val <- col_tfm %>%
  
  filter(collision_year %in% 2022:2024) %>%
  
  filter(!is.na(lsoa_of_accident_location)) %>%
  
  filter(lsoa_of_accident_location != "") %>%
  
  filter(lsoa_of_accident_location != "-1") %>%
  
  count(
    lsoa_of_accident_location,
    name = "n_val"
  )


val_bym <- mu_lsoa %>%
  
  left_join(
    acc_val,
    by = "lsoa_of_accident_location"
  ) %>%
  
  left_join(
    
    datos_bym %>%
      select(
        lsoa_of_accident_location,
        esperado,
        n_acc
      ),
    
    by = "lsoa_of_accident_location"
    
  ) %>%
  
  filter(!is.na(n_val))


# Correlación entre los dos periodos
cor_val <- cor(
  val_bym$n_acc,
  val_bym$n_val,
  method = "spearman"
)

cat("Correlacion Spearman 2015-2019 / 2022-2024:",
    round(cor_val, 4), "\n")


# Gráfico de dispersión en escala logarítmica
ggplot(
  val_bym,
  aes(
    x = n_acc,
    y = n_val
  )
) +
  
  geom_point(
    alpha = 0.3,
    size = 0.5,
    color = "blue"
  ) +
  
  geom_smooth(
    method = "lm",
    color = "red",
    se = FALSE
  ) +
  
  scale_x_log10() +
  
  scale_y_log10() +
  
  labs(
    title = "Accidentes por zona: estimacion 2015-2019 y validacion 2022-2024",
    x = "Accidentes 2015-2019 (log)",
    y = "Accidentes 2022-2024 (log)"
  )

ggsave(
  "output/fig6_2_validacion_bym.png",
  dpi = 300,
  width = 8,
  height = 6
)


# Comparación por quintiles
val_bym$quintil <- ntile(val_bym$n_acc, 5)

resumen_quintiles <- val_bym %>%
  
  group_by(quintil) %>%
  
  summarise(
    
    acc_estimacion = round(mean(n_acc), 1),
    
    acc_validacion = round(mean(n_val), 1),
    
    n_zonas = n(),
    
    .groups = "drop"
    
  )

print(resumen_quintiles)


# VALIDACIÓN TEMPORAL

eventos_val <- col_tfm %>%
  
  filter(police_force %in% c("1", "Metropolitan Police")) %>%
  
  filter(collision_year %in% 2022:2024) %>%
  
  filter(!is.na(t_cont)) %>%
  
  filter(!is.na(sev_num)) %>%
  
  filter(location_easting_osgr > 0) %>%
  
  filter(!is.na(lsoa_of_accident_location)) %>%
  
  filter(lsoa_of_accident_location != "") %>%
  
  filter(lsoa_of_accident_location != "-1") %>%
  
  left_join(
    mu_lsoa,
    by = "lsoa_of_accident_location"
  ) %>%
  
  filter(!is.na(mu_norm)) %>%
  
  arrange(t_cont) %>%
  
  mutate(
    x_km = location_easting_osgr / 1000,
    y_km = location_northing_osgr / 1000,
    mu_horario = mu_norm * factor_horario[hora_dia + 1]
  )

cat("Accidentes Londres 2022-2024:", nrow(eventos_val), "\n")

t_val   <- eventos_val$t_cont - min(eventos_val$t_cont)
sev_val <- eventos_val$sev_num
mu_val  <- eventos_val$mu_horario
T_val   <- max(t_val)

resultado_val <- optim(
  
  par = log(c(0.01, 0.01, 0.01, beta_init)),
  
  fn = log_verosimilitud_temporal,
  
  t = t_val,
  sev = sev_val,
  mu = mu_val,
  T_obs = T_val,
  
  method = "L-BFGS-B",
  
  lower = log(c(1e-6, 1e-6, 1e-6, 0.1)),
  upper = log(c(0.99, 0.99, 0.99, 1000)),
  
  control = list(
    maxit = 500,
    trace = 1
  )
  
)

params_val <- exp(resultado_val$par)

names(params_val) <- c(
  "K_leve",
  "K_grave",
  "K_fatal",
  "beta"
)

print(round(params_val, 4))

prop_sev_val <- prop.table(table(sev_val))

eta_val_num <- params_val["K_leve"]  * prop_sev_val["3"] +
  params_val["K_grave"] * prop_sev_val["2"] +
  params_val["K_fatal"] * prop_sev_val["1"]

cat("Eta validacion:", round(eta_val_num, 4), "\n")

cat("Vida media validacion:",
    round(log(2) / params_val["beta"] * 60, 1), "minutos\n")


# INTENTOS DE HAWKES ESPACIO-TEMPORAL

# Versión con un solo K y sigma fijo
log_verosimilitud_st_sigma_fijo <- function(params, t, x, y, mu,
                                            T_obs, sigma_fijo,
                                            max_dt = 2, max_d = 1) {
  
  K    <- exp(params[1])
  beta <- exp(params[2])
  
  if (K >= 1) return(1e10)
  
  lambda_cluster <- calcular_cluster(
    t, x, y,
    rep(K, length(t)),
    beta, sigma_fijo,
    max_dt, max_d^2
  )
  
  lambda_total <- mu + lambda_cluster
  
  if (any(lambda_total <= 0)) return(1e10)
  
  term1 <- sum(log(lambda_total))
  
  term2 <- sum(mu) * T_obs / length(t) +
    sum(K * (1 - exp(-beta * (T_obs - t))) / beta)
  
  -(term1 - term2)
}


# Tasa base solo con la parte fija del BYM
coef_fijos <- modelo_bym$summary.fixed$mean

names(coef_fijos) <- rownames(modelo_bym$summary.fixed)

datos_bym$mu_fijo <- exp(
  coef_fijos["(Intercept)"] +
    coef_fijos["imd_std"] * datos_bym$imd_std +
    coef_fijos["vel_std"] * datos_bym$vel_std +
    coef_fijos["urb_std"] * datos_bym$urb_std
)

datos_bym$mu_fijo_norm <-
  datos_bym$mu_fijo / mean(datos_bym$mu_fijo)

mu_fijo_lsoa <- datos_bym %>%
  
  select(
    lsoa_of_accident_location,
    mu_fijo_norm
  )

eventos_st2 <- eventos %>%
  
  filter(police_force %in% c("1", "Metropolitan Police")) %>%
  
  filter(collision_year %in% 2015:2019) %>%
  
  filter(!is.na(mu_norm)) %>%
  
  left_join(
    mu_fijo_lsoa,
    by = "lsoa_of_accident_location"
  ) %>%
  
  filter(!is.na(mu_fijo_norm)) %>%
  
  arrange(t_cont) %>%
  
  mutate(
    mu_base = mu_fijo_norm * factor_horario[hora_dia + 1]
  )

t_st2  <- eventos_st2$t_cont - min(eventos_st2$t_cont)
x_st2  <- eventos_st2$x_km
y_st2  <- eventos_st2$y_km
mu_st2 <- eventos_st2$mu_base
T_st2  <- max(t_st2)


K_fijo    <- params_est["K_leve"]
beta_fijo <- params_est["beta"]

sigma_vals <- c(0.05, 0.1, 0.15, 0.2, 0.3, 0.5, 0.75, 1.0, 1.5, 2.0)

logliks_sigma <- sapply(sigma_vals, function(s) {
  
  cat("Sigma =", s, "km... ")
  
  val <- tryCatch(
    
    log_verosimilitud_st_sigma_fijo(
      params = log(c(K_fijo, beta_fijo)),
      t = t_st2,
      x = x_st2,
      y = y_st2,
      mu = mu_st2,
      T_obs = T_st2,
      sigma_fijo = s,
      max_dt = 2,
      max_d = max(1, 3 * s)
    ),
    
    error = function(e) NA
    
  )
  
  cat("loglik =", round(-val, 2), "\n")
  
  -val
})

resumen_sigma <- data.frame(
  sigma = sigma_vals,
  loglik = round(logliks_sigma, 2)
)

print(resumen_sigma)

cat("Sigma optimo:",
    sigma_vals[which.max(logliks_sigma)], "km\n")

saveRDS(
  
  list(
    resumen_sigma = resumen_sigma,
    K_fijo = K_fijo,
    beta_fijo = beta_fijo
  ),
  
  "output/resultados_hawkes_st.rds"
  
)


# EXPOSICIÓN CON TRÁFICO REAL DEL DFT

library(sf)

# Descarga manual desde https://roadtraffic.dft.gov.uk/downloads
count_pts <- read.csv(
  "C:/Users/gagik/Downloads/TFM CODIGO/count_points.csv"
)

aadf <- read.csv(
  "C:/Users/gagik/Downloads/TFM CODIGO/dft_traffic_counts_aadf.csv"
)

# Tráfico de 2019 con las coordenadas de cada punto
trafico_2019 <- aadf %>%
  
  filter(year == 2019) %>%
  
  select(
    count_point_id,
    all_motor_vehicles
  ) %>%
  
  left_join(
    
    count_pts %>%
      select(
        count_point_id,
        easting,
        northing
      ),
    
    by = "count_point_id"
    
  ) %>%
  
  filter(!is.na(easting)) %>%
  
  filter(!is.na(northing)) %>%
  
  filter(!is.na(all_motor_vehicles)) %>%
  
  filter(all_motor_vehicles > 0)

cat("Puntos de conteo 2019:", nrow(trafico_2019), "\n")


# Paso los puntos a objeto espacial
trafico_sf <- st_as_sf(
  trafico_2019,
  coords = c("easting", "northing"),
  crs = 27700
)

# Polígonos de las zonas, descarga manual desde
# https://geoportal.statistics.gov.uk
lsoa_sf <- st_read(
  "C:/Users/gagik/Downloads/TFM CODIGO/LSOA_2011_EW_BGC_V3.shp"
)

# Asigno cada punto de conteo a su zona
trafico_lsoa_sf <- st_join(
  
  trafico_sf,
  
  lsoa_sf %>% select(LSOA11CD),
  
  join = st_within
  
)

# Tráfico medio por zona
trafico_lsoa <- trafico_lsoa_sf %>%
  
  st_drop_geometry() %>%
  
  filter(!is.na(LSOA11CD)) %>%
  
  group_by(LSOA11CD) %>%
  
  summarise(
    
    trafico_medio = mean(all_motor_vehicles, na.rm = TRUE),
    
    n_puntos = n(),
    
    .groups = "drop"
    
  ) %>%
  
  rename(
    lsoa_of_accident_location = LSOA11CD
  )

cat("Zonas con dato de trafico:", nrow(trafico_lsoa), "\n")

cat("Zonas sin dato de trafico:",
    nrow(datos_bym) - sum(
      datos_bym$lsoa_of_accident_location %in%
        trafico_lsoa$lsoa_of_accident_location
    ), "\n")


# Más de la mitad de las zonas se queda sin dato
# Les imputo la media nacional
trafico_medio_nacional <- mean(trafico_lsoa$trafico_medio)

datos_bym_v2 <- datos_bym %>%
  
  left_join(
    trafico_lsoa,
    by = "lsoa_of_accident_location"
  ) %>%
  
  mutate(
    
    trafico_medio = ifelse(
      is.na(trafico_medio),
      trafico_medio_nacional,
      trafico_medio
    ),
    
    esperado_v2 = trafico_medio * length(anios_bym)
    
  )

cat("Media trafico nacional:",
    round(trafico_medio_nacional, 1), "\n")


# Mismo modelo con la nueva exposición
modelo_bym_v2 <- inla(
  
  formula_bym,
  
  family = "poisson",
  
  data = datos_bym_v2,
  
  E = datos_bym_v2$esperado_v2,
  
  control.compute = list(
    dic = TRUE,
    waic = TRUE,
    cpo = TRUE
  ),
  
  control.predictor = list(
    compute = TRUE
  ),
  
  control.inla = list(
    strategy = "simplified.laplace",
    int.strategy = "eb",
    h = 0.01
  )
  
)

print(summary(modelo_bym_v2))

print(round(modelo_bym_v2$summary.fixed, 4))

print(round(modelo_bym_v2$summary.hyperpar, 4))

cat("DIC v2:", modelo_bym_v2$dic$dic, "\n")
cat("DIC original:", modelo_bym$dic$dic, "\n")

cat("Diferencia DIC:",
    round(modelo_bym_v2$dic$dic - modelo_bym$dic$dic, 1), "\n")


# ÚLTIMO INTENTO: TASA BASE CON TRÁFICO REAL

library(spatstat)

coords_conteo <- trafico_2019 %>%
  
  select(
    easting,
    northing,
    all_motor_vehicles
  )

eventos_st3 <- eventos_est %>%
  
  mutate(
    x_osgr = x_km * 1000,
    y_osgr = y_km * 1000
  )


# A cada accidente le asigno el tráfico del punto de conteo más cercano
# Lo hago por bloques para no quedarme sin memoria
bloque <- 5000

n_acc <- nrow(eventos_st3)

trafico_acc <- numeric(n_acc)

for (i in seq(1, n_acc, by = bloque)) {
  
  idx_fin <- min(i + bloque - 1, n_acc)
  
  dx <- outer(
    eventos_st3$x_osgr[i:idx_fin],
    coords_conteo$easting,
    "-"
  )
  
  dy <- outer(
    eventos_st3$y_osgr[i:idx_fin],
    coords_conteo$northing,
    "-"
  )
  
  dist2 <- dx^2 + dy^2
  
  idx_min <- apply(dist2, 1, which.min)
  
  trafico_acc[i:idx_fin] <-
    coords_conteo$all_motor_vehicles[idx_min]
}

eventos_st3$trafico_cercano <- trafico_acc

eventos_st3$mu_trafico <-
  trafico_acc / mean(trafico_acc) *
  factor_horario[eventos_st3$hora_dia + 1]

cat("Media tasa base trafico:",
    round(mean(eventos_st3$mu_trafico), 4), "\n")

cat("Rango:",
    round(range(eventos_st3$mu_trafico), 4), "\n")


t_st3  <- eventos_st3$t_cont - min(eventos_st3$t_cont)
x_st3  <- eventos_st3$x_km
y_st3  <- eventos_st3$y_km
mu_st3 <- eventos_st3$mu_trafico
T_st3  <- max(t_st3)


# Versión con K, beta y sigma libres
log_verosimilitud_hawkes_st <- function(params, t, x, y, sev, mu, T_obs,
                                        max_dt = 2, max_d = 2) {
  
  K     <- exp(params[1])
  beta  <- exp(params[2])
  sigma <- exp(params[3])
  
  if (K >= 1) return(1e10)
  
  K_vec <- rep(K, length(t))
  
  lambda_cluster <- calcular_cluster(
    t, x, y,
    K_vec,
    beta, sigma,
    max_dt, max_d^2
  )
  
  lambda_total <- mu + lambda_cluster
  
  if (any(lambda_total <= 0)) return(1e10)
  
  term1 <- sum(log(lambda_total))
  
  term2 <- sum(mu) * T_obs / length(t) +
    sum(K * (1 - exp(-beta * (T_obs - t))) / beta)
  
  -(term1 - term2)
}

par_init_st <- log(c(0.3, log(2)/1, 0.3))

resultado_st3 <- optim(
  
  par = par_init_st,
  
  fn = log_verosimilitud_hawkes_st,
  
  t = t_st3,
  x = x_st3,
  y = y_st3,
  sev = eventos_st3$sev_num,
  mu = mu_st3,
  T_obs = T_st3,
  max_dt = 2,
  max_d = 2,
  
  method = "L-BFGS-B",
  
  lower = log(c(1e-4, 0.1,  0.05)),
  upper = log(c(0.95, 20,   3.0)),
  
  control = list(
    maxit = 1000,
    trace = 1
  )
  
)

params_st3 <- exp(resultado_st3$par)

names(params_st3) <- c(
  "K",
  "beta",
  "sigma_km"
)

print(round(params_st3, 4))

cat("Convergencia:", resultado_st3$convergence, "\n")

cat("Vida media:",
    round(log(2)/params_st3["beta"]*60, 1), "minutos\n")

cat("Alcance espacial:",
    round(params_st3["sigma_km"], 3), "km\n")

cat("Eta:", round(params_st3["K"], 4), "\n")
