library(dplyr)

# Cargar bases procesadas
fa_base = readRDS(
  "datos_procesados/fa_base_2007_2025.rds"
)

bioma_predominante = readRDS(
  "datos_procesados/fa_biomas_municipios.rds"
)

# Incorporar bioma predominante y cobertura
fa_final = fa_base %>%
  left_join(
    bioma_predominante,
    by = c("Cod_mun" = "MpCodigo")
  )

# Verificar dimensiones y variables
dim(fa_final)
names(fa_final)

# Comprobar municipios sin bioma
sum(is.na(fa_final$Bioma))

# Comprobar casos confirmados
table(fa_final$FA)
sum(fa_final$Casos)

# Guardar base definitiva para análisis estadístico
saveRDS(
  fa_final,
  "datos_procesados/fa_final_2007_2025.rds"
)