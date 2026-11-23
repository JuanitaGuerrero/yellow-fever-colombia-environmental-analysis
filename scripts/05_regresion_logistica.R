# Cargar base final
fa_final = readRDS("datos_procesados/fa_final_2007_2025.rds")

str(fa_final)
table(fa_final$FA)
table(fa_final$Bioma, fa_final$FA)

# Correlaciones de Spearman
variables_ambientales = fa_final[, c(
  "Temp_mean", "Precip_mean", "Altitud_min",
  "Cantidad_PNH", "porcentaje_bosques",
  "porcentaje_agricultura", "deforestacion"
)]

round(cor(variables_ambientales, method = "spearman"), 2)

library(car)
library(ResourceSelection)

# GLM binomial
mod_log = glm(
  FA ~ Temp_mean + Precip_mean + Cantidad_PNH +
    porcentaje_bosques + deforestacion,
  family = binomial(link = "logit"),
  data = fa_final
)

summary(mod_log)

# Odds ratios e intervalos de confianza
exp(coef(mod_log))
exp(confint(mod_log))

# Términos de linealidad
fa_final$temp_log = fa_final$Temp_mean * log(fa_final$Temp_mean)
fa_final$precip_log = fa_final$Precip_mean * log(fa_final$Precip_mean)

fa_final$pnh_log = ifelse(
  fa_final$Cantidad_PNH > 0,
  fa_final$Cantidad_PNH * log(pmax(fa_final$Cantidad_PNH, 1)),
  0
)

# Modelo de linealidad
mod_linealidad = glm(
  FA ~ Temp_mean + Precip_mean + Cantidad_PNH +
    porcentaje_bosques + deforestacion +
    temp_log + precip_log + pnh_log,
  family = binomial(link = "logit"),
  data = fa_final
)

summary(mod_linealidad)

# Multicolinealidad
vif(mod_log)

# Bondad de ajuste
hoslem.test(
  model.response(model.frame(mod_log)),
  fitted(mod_log),
  g = 10
)

# Distancia de Cook
cook = cooks.distance(mod_log)
limite_cook = 4 / nobs(mod_log)

plot(cook, type = "h",
     main = "Distancia de Cook",
     ylab = "Distancia de Cook")

abline(h = limite_cook, col = "red", lty = 2)

which(cook > limite_cook)

# Leverage
leverage = hatvalues(mod_log)
limite_leverage = 2 * length(coef(mod_log)) / nobs(mod_log)

plot(leverage, type = "h",
     main = "Valores de leverage",
     ylab = "Leverage")

abline(h = limite_leverage, col = "blue", lty = 2)

which(leverage > limite_leverage)

# Residuos de deviance estandarizados
residuos_dev = rstandard(mod_log, type = "deviance")

plot(residuos_dev,
     main = "Residuos de deviance estandarizados",
     ylab = "Residuos estandarizados")

abline(h = c(-2, 2), col = "red", lty = 2)

which(abs(residuos_dev) > 2)

library(dplyr)
library(ggplot2)
library(mgcv)

# FA según riqueza de PNH
fa_final %>%
  group_by(Cantidad_PNH) %>%
  summarise(
    municipios = n(),
    positivos = sum(FA),
    proporcion = mean(FA),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = Cantidad_PNH, y = proporcion)) +
  geom_point(aes(size = municipios)) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Número de géneros de primates no humanos",
    y = "Municipios con registros de fiebre amarilla (%)",
    size = "Municipios"
  ) +
  theme_bw()

# GAM binomial
mod_gam = gam(
  FA ~ Temp_mean + Precip_mean +
    s(Cantidad_PNH, k = 4) +
    porcentaje_bosques + deforestacion,
  family = binomial(link = "logit"),
  data = fa_final,
  method = "REML"
)

summary(mod_gam)

# Comparación GLM y GAM
AIC(mod_log, mod_gam)

# Efecto suavizado de PNH
plot(
  mod_gam,
  select = 1,
  shade = TRUE,
  rug = TRUE,
  xlab = "Número de géneros de primates no humanos",
  ylab = "Efecto parcial sobre el logit de FA",
  main = "Efecto ajustado de la riqueza de PNH"
)

# Diagnóstico del GAM
gam.check(mod_gam)
concurvity(mod_gam)

library(spdep)

# Residuos de Pearson
res_glm = residuals(mod_log, type = "pearson")
res_gam = residuals(mod_gam, type = "pearson")

# Índice I de Moran
moran.test(res_glm, weights_knn, zero.policy = TRUE)
moran.test(res_gam, weights_knn, zero.policy = TRUE)

# Incorporar residuos
municipios_fa$res_gam = res_gam

# Distribución espacial
ggplot(municipios_fa) +
  geom_sf(aes(fill = res_gam), color = NA) +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  labs(
    title = "Distribución espacial de los residuos del GAM",
    fill = "Residuo"
  ) +
  theme_bw()
