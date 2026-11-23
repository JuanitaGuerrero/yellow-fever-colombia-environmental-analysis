library(readxl)
library(dplyr)

# Importar casos de fiebre amarilla del INS (2007-2025)
fa_2007 = read_excel("datos_originales/Datos_2007_310.xlsx")
fa_2008 = read_excel("datos_originales/Datos_2008_310.xlsx")
fa_2009 = read_excel("datos_originales/Datos_2009_310.xlsx")
fa_2016 = read_excel("datos_originales/Datos_2016_310.xlsx")
fa_2023 = read_excel("datos_originales/Datos_2023_310.xlsx")
fa_2024 = read_excel("datos_originales/Datos_2024_310.xlsx")
fa_2025 = read_excel("datos_originales/Datos_2025_310.xlsx")

# Revisar nombres de columnas de cada archivo
lapply(
  list(fa_2007, fa_2008, fa_2009, fa_2016,
       fa_2023, fa_2024, fa_2025),
  names
)

# Unificar registros de fiebre amarilla (2007-2025)
fa_casos = bind_rows(
  fa_2007,
  fa_2008,
  fa_2009,
  fa_2016,
  fa_2023,
  fa_2024,
  fa_2025
)

# Verificar dimensiones y distribución anual
dim(fa_casos)
table(fa_casos$ANO)

# Revisar los códigos de ocurrencia
class(fa_casos$COD_DPTO_O)
class(fa_casos$COD_MUN_O)

# Examinar los primeros registros
head(fa_casos[, c("ANO", "COD_DPTO_O", "COD_MUN_O",
                  "Departamento_ocurrencia", "Municipio_ocurrencia")], 10)

# Verificar valores faltantes
sum(is.na(fa_casos$COD_DPTO_O))
sum(is.na(fa_casos$COD_MUN_O))

# Crear identificador DANE municipal de cinco dígitos
fa_casos = fa_casos %>%
  mutate(COD_DANE = paste0(COD_DPTO_O, COD_MUN_O))

# Contar municipios únicos con casos
n_distinct(fa_casos$COD_DANE)

# Revisar número de casos por municipio
fa_municipios = fa_casos %>%
  count(COD_DANE, Departamento_ocurrencia,
        Municipio_ocurrencia, name = "Casos") %>%
  arrange(desc(Casos))

# Conservar únicamente casos ocurridos en Colombia
fa_colombia = fa_casos %>%
  filter(Pais_ocurrencia == "COLOMBIA")

# Contar casos por municipio colombiano
fa_municipios = fa_colombia %>%
  count(COD_DANE, Departamento_ocurrencia,
        Municipio_ocurrencia, name = "Casos") %>%
  arrange(desc(Casos))

# Verificar resultados
nrow(fa_colombia)
nrow(fa_municipios)
n_distinct(fa_municipios$Departamento_ocurrencia)

# Abrir la tabla
View(fa_municipios)

# Guardar bases epidemiológicas procesadas
saveRDS(fa_colombia, "datos_procesados/fa_colombia_2007_2025.rds")

saveRDS(fa_municipios, "datos_procesados/fa_municipios_2007_2025.rds")