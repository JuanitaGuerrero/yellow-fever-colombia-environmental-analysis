library(sf)
library(dplyr)

# Importar información geográfica
ecoregiones = st_read(
  "datos_originales/Ecoregions2017/Ecoregions2017.shp"
)

municipios = st_read(
  "datos_originales/Municipios/Municipio, Distrito y Area no municipalizada.shp"
)

# Excluir área en litigio Cauca-Huila
municipios = municipios %>%
  filter(MpCodigo != "00000")


# Seleccionar ecorregiones de Colombia
# Transformar municipios a WGS 84
municipios_wgs84 = st_transform(
  municipios,
  st_crs(ecoregiones)
)

# Filtrar antes de proyectar las ecorregiones
sf_use_s2(FALSE)

ecoregiones_colombia = st_filter(
  ecoregiones,
  municipios_wgs84
)

# Transformar al sistema de coordenadas municipal
ecoregiones_colombia = st_transform(
  ecoregiones_colombia,
  st_crs(municipios)
)

# Reparar geometrías inválidas
ecoregiones_colombia = st_make_valid(
  ecoregiones_colombia
)

# Verificar ecorregiones seleccionadas
nrow(ecoregiones_colombia)
sum(!st_is_valid(ecoregiones_colombia))

ecoregiones_colombia %>%
  st_drop_geometry() %>%
  count(BIOME_NAME, sort = TRUE)


# Calcular superficies de biomas por municipio
municipios_biomas = st_intersection(
  municipios %>% select(MpCodigo),
  ecoregiones_colombia %>% select(BIOME_NAME)
)

municipios_biomas = municipios_biomas %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 1000000
  )

# Sumar superficies del mismo bioma
area_biomas = municipios_biomas %>%
  st_drop_geometry() %>%
  group_by(MpCodigo, BIOME_NAME) %>%
  summarise(
    area_km2 = sum(area_km2),
    .groups = "drop"
  )


# Verificar cobertura geográfica

area_municipios = municipios %>%
  mutate(
    area_municipio = as.numeric(st_area(geometry)) / 1000000
  ) %>%
  st_drop_geometry() %>%
  select(MpCodigo, area_municipio)

revision_areas = area_biomas %>%
  group_by(MpCodigo) %>%
  summarise(
    area_biomas = sum(area_km2),
    .groups = "drop"
  ) %>%
  left_join(area_municipios, by = "MpCodigo") %>%
  mutate(
    cobertura_pct = 100 * area_biomas / area_municipio
  )

# Revisar cobertura
summary(revision_areas$cobertura_pct)

# Identificar municipios con cobertura inferior al 99 %
revision_areas %>%
  filter(cobertura_pct < 99) %>%
  arrange(cobertura_pct)


# Identificar bioma predominante

bioma_predominante = area_biomas %>%
  group_by(MpCodigo) %>%
  slice_max(
    order_by = area_km2,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  rename(Bioma = BIOME_NAME) %>%
  select(MpCodigo, Bioma)

# Incorporar porcentaje de cobertura
bioma_predominante = bioma_predominante %>%
  left_join(
    revision_areas %>%
      select(MpCodigo, cobertura_pct),
    by = "MpCodigo"
  )

# Verificar clasificación
nrow(bioma_predominante)

bioma_predominante %>%
  count(Bioma, sort = TRUE)


# Guardar clasificación de biomas
saveRDS(
  bioma_predominante,
  "datos_procesados/fa_biomas_municipios.rds"
)

# Restaurar configuración de s2
sf_use_s2(TRUE)

# Cargar clasificación de biomas
biomas_municipios = readRDS(
  "datos_procesados/fa_biomas_municipios.rds"
)

# Visualizar tabla
View(biomas_municipios)
