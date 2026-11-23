library(readxl)
library(dplyr)

# Importar base ambiental del proyecto original
fa_ambiental = read_excel(
  "datos_originales/FA_GEOAMB_MUNICIPIOS.xlsx"
)

# Revisar dimensiones y nombres de columnas
dim(fa_ambiental)
names(fa_ambiental)

# Examinar los primeros registros
head(fa_ambiental)

# Verificar municipios únicos en la base ambiental
n_distinct(fa_ambiental$Cod_mun)

# Comprobar códigos municipales duplicados
sum(duplicated(fa_ambiental$Cod_mun))

# Identificar municipios con FA que no estén en la base ambiental
municipios_faltantes = fa_municipios %>%
  filter(!COD_DANE %in% fa_ambiental$Cod_mun)

# Revisar resultados
nrow(municipios_faltantes)
View(municipios_faltantes)

# Seleccionar variables ambientales del estudio
fa_base = fa_ambiental %>%
  select(Cod_mun, Temp_mean, Precip_mean,
         Altitud_min, Cantidad_PNH)

# Integrar los casos confirmados por municipio
fa_base = fa_base %>%
  left_join(
    fa_municipios %>% select(COD_DANE, Casos),
    by = c("Cod_mun" = "COD_DANE")
  ) %>%
  mutate(
    Casos = coalesce(Casos, 0L),
    FA = if_else(Casos > 0, 1L, 0L)
  )

# Verificar dimensiones de la base
dim(fa_base)

# Contar municipios según presencia de FA
table(fa_base$FA)

# Comprobar el total de casos
sum(fa_base$Casos)

# Revisar primeros registros
head(fa_base)

# Verificar valores faltantes por variable
colSums(is.na(fa_base))

# Revisar estadísticas descriptivas
summary(fa_base)

# Importar base ambiental ampliada
fa_ampliada = read_excel(
  "datos_originales/FA_GEOAMB_AMPLIADA.xlsx"
)

# Incorporar variables de cobertura y deforestación
fa_base = fa_base %>%
  left_join(
    fa_ampliada %>%
      select(
        Cod_mun,
        porcentaje_bosques,
        porcentaje_agricultura,
        deforestacion
      ),
    by = "Cod_mun"
  )

# Excluir área en litigio Cauca-Huila
fa_base = fa_base %>%
  filter(Cod_mun != "00000")

# Guardar base municipal actualizada (sin biomas)
saveRDS(
  fa_base,
  "datos_procesados/fa_base_2007_2025.rds"
)

View(fa_base)