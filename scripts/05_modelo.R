library(dplyr)
library(ggplot2)
library(lmtest)
library(car)
library(MASS)
library(pscl)
library(sf)
library(spdep)
library(DHARMa)
library(ggeffects)


#EXPLORACIÓN DE LOS DATOS
# Cargar base final
fa_final = readRDS("datos_procesados/fa_final_2007_2025.rds")

# Estructura y resumen
str(fa_final)
summary(fa_final)

# Distribución de presencia y ausencia
table(fa_final$FA)

# Distribución del número de casos
table(fa_final$Casos)

# Histograma de casos
ggplot(fa_final, aes(x = Casos)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(
    title = "Distribución de casos de fiebre amarilla",
    x = "Casos registrados por municipio",
    y = "Número de municipios"
  ) +
  theme_minimal()


# MODELO LINEAL EXPLORATORIO
# OLS inicial
mod_ols = lm(
  Casos ~ Temp_mean + Precip_mean + Altitud_min +
    Cantidad_PNH + porcentaje_bosques +
    porcentaje_agricultura + deforestacion,
  data = fa_final
)

summary(mod_ols)

# Especificación funcional
resettest(mod_ols)

# Multicolinealidad
vif(mod_ols)

# OLS sin altitud
mod_ols2 = lm(
  Casos ~ Temp_mean + Precip_mean +
    Cantidad_PNH + porcentaje_bosques +
    porcentaje_agricultura + deforestacion,
  data = fa_final
)

summary(mod_ols2)
resettest(mod_ols2)
vif(mod_ols2)

# AUTOCORRELACIÓN ESPACIAL DEL OLS
# Cargar centroides
centroides = st_read(
  "/Users/juanitaguerrero/Desktop/Proyecto FA/Fiebre_Amarilla_Colombia/datos_originales/Centroides_municipios.gpkg"
)

# Excluir área litigio
centroides = centroides[-113, ]

# Unir datos municipales
fa_new = left_join(
  centroides,
  fa_final,
  by = c("MpCodigo" = "Cod_mun")
)

# Coordenadas y vecinos
coords = st_coordinates(fa_new)
knn_obj = knearneigh(coords, k = 5)
knn_list = knn2nb(knn_obj)

# Matriz de pesos espaciales
weights_knn = nb2listw(knn_list, style = "W")

# Moran de los residuos OLS
lm.morantest(
  mod_ols2,
  weights_knn,
  alternative = "two.sided"
)


# MODELOS DE CONTEO
# Regresión binomial negativa
mod_nb = glm.nb(
  Casos ~ Temp_mean + Precip_mean +
    Cantidad_PNH + porcentaje_bosques +
    porcentaje_agricultura + deforestacion,
  data = fa_final
)

summary(mod_nb)

# Regresión Zero-Inflated Poisson
mod_zip = zeroinfl(
  Casos ~ Temp_mean + Precip_mean +
    Cantidad_PNH + porcentaje_bosques +
    porcentaje_agricultura + deforestacion | 1,
  data = fa_final,
  dist = "poisson"
)

summary(mod_zip)

# Comparación de modelos
AIC(mod_nb, mod_zip)


# DIAGNÓSTICOS DE LA NB
# Residuos simulados
set.seed(123)
res_nb = simulateResiduals(mod_nb, n = 1000)

# Diagnóstico general
plot(res_nb)

# Dispersión
testDispersion(res_nb)

# Exceso de ceros
testZeroInflation(res_nb)

# Autocorrelación espacial
testSpatialAutocorrelation(
  res_nb,
  x = coords[, 1],
  y = coords[, 2]
)

# Observaciones influyentes
plot(mod_nb, which = 4)

# Apalancamiento
plot(mod_nb, which = 5)

# ANÁLISIS SENSIBILIDAD
# Identificar observaciones influyentes
influencia_nb = data.frame(
  Cod_mun = fa_final$Cod_mun,
  Casos = fa_final$Casos,
  Cook = cooks.distance(mod_nb)
)

# Municipios con mayor influencia
head(
  influencia_nb[order(-influencia_nb$Cook), ],
  10
)

# Identificar las tres observaciones más influyentes
indices_influyentes = order(
  cooks.distance(mod_nb),
  decreasing = TRUE
)[1:3]

# Ajustar modelo sin esas observaciones
mod_nb_sensibilidad = update(
  mod_nb,
  data = fa_final[-indices_influyentes, ]
)

# Comparar coeficientes
comparacion_nb = data.frame(
  Variable = names(coef(mod_nb)),
  Beta_original = coef(mod_nb),
  Beta_sensibilidad = coef(mod_nb_sensibilidad),
  p_original = coef(summary(mod_nb))[, 4],
  p_sensibilidad = coef(summary(mod_nb_sensibilidad))[, 4]
)

print(comparacion_nb, digits = 4)


# INTERPRETACIÓN DE COEFICIENTES
# Razones de conteo e intervalos de confianza de Wald
resultados_nb = data.frame(
  Variable = names(coef(mod_nb)),
  Beta = coef(mod_nb),
  RC = exp(coef(mod_nb)),
  IC_inferior = exp(
    coef(mod_nb) - 1.96 * sqrt(diag(vcov(mod_nb)))
  ),
  IC_superior = exp(
    coef(mod_nb) + 1.96 * sqrt(diag(vcov(mod_nb)))
  ),
  p_valor = coef(summary(mod_nb))[, 4]
)

# Tabla de resultados
round(
  resultados_nb[, c("Beta", "RC", "IC_inferior",
                    "IC_superior", "p_valor")],
  4
)


# EFECTOS MARGINALES
# Temperatura
pred_temp = ggpredict(mod_nb, terms = "Temp_mean")
plot(pred_temp) +
  labs(
    title = "Asociación entre temperatura y casos",
    x = "Temperatura media (°C)",
    y = "Número esperado de casos"
  )

# Precipitación
pred_precip = ggpredict(mod_nb, terms = "Precip_mean")
plot(pred_precip) +
  labs(
    title = "Asociación entre precipitación y casos",
    x = "Precipitación media (mm)",
    y = "Número esperado de casos"
  )

# Géneros de primates no humanos
pred_pnh = ggpredict(mod_nb, terms = "Cantidad_PNH")
plot(pred_pnh) +
  labs(
    title = "Asociación entre primates y casos",
    x = "Número de géneros de primates",
    y = "Número esperado de casos"
  )

# Cobertura agrícola
pred_agricultura = ggpredict(
  mod_nb,
  terms = "porcentaje_agricultura"
)
plot(pred_agricultura) +
  labs(
    title = "Asociación entre agricultura y casos",
    x = "Proporción de cobertura agrícola",
    y = "Número esperado de casos"
  )

# Deforestación
pred_deforestacion = ggpredict(
  mod_nb,
  terms = "deforestacion"
)
plot(pred_deforestacion) +
  labs(
    title = "Asociación entre deforestación y casos",
    x = "Proporción de deforestación",
    y = "Número esperado de casos"
  )