# Environmental and Spatial Analysis of Human Yellow Fever in Colombia

**Author:** Juanita Guerrero  
**Status:** Research analysis / work in progress. Results and models have not been independently validated for this public repository.

## Overview

This R project explores municipality-level occurrence of reported human yellow fever cases in Colombia during 2007–2025 and their associations with environmental and geographic characteristics. It brings together epidemiological surveillance records, municipal geographic identifiers, environmental indicators, and biome information.

## Research workflow

1. `scripts/01_preparacion_casos_FA.R` — combine annual yellow fever surveillance files and prepare municipal identifiers and case summaries.
2. `scripts/02_preparacion_ambiental.R` — integrate municipal case summaries with environmental covariates.
3. `scripts/03_preparacion_biomas.R` — spatially intersect municipal boundaries with ecoregions and summarize biome coverage.
4. `scripts/04_base_final.R` — assemble the final municipality-level analysis dataset.
5. `scripts/05_regresion_logistica.R` — explore binary logistic regression for municipal case occurrence, including model diagnostics.
6. `scripts/05_modelo.R` — exploratory statistical models and spatial diagnostics. This is a separate exploratory script, not a validated final model.

The scripts are provided as research code, preserving the original analyses. They have not been fully tested in a clean environment. Some scripts assume objects from earlier steps exist in the R session; `05_modelo.R` also contains a machine-specific absolute path that must be updated before execution.

## Health outcome and predictors

- **Health outcome:** reported human yellow fever case occurrence and case counts by municipality.
- **Environmental variables considered:** temperature, precipitation, minimum elevation, non-human primate indicators, forest and agricultural cover, deforestation, and biome/ecoregion characteristics (as available in the project's source tables).
- **Methods:** data harmonization, spatial joins and overlays, exploratory regression, logistic regression, and model diagnostics.

## Data sources and availability

The original project uses annual yellow fever surveillance spreadsheets from Colombia's National Institute of Health (INS), project-specific environmental tables, municipal boundaries, and the *Ecoregions2017* spatial dataset. Source ownership, permissions, and licensing should be checked before redistribution.

**Individual surveillance records, processed health datasets, and large third-party spatial datasets are intentionally excluded from this public-sharing package.** This protects potentially sensitive records and avoids republishing datasets without verifying redistribution rights. Consequently, the scripts **will not run end-to-end from this repository alone**. The `datos_originales/` and `datos_procesados/` folders referenced by the scripts must be populated with appropriately obtained, authorized datasets before execution.

## Figures

The `figures/` folder contains selected PNG outputs from the original working directory. They are included as existing research artifacts; they are not independently verified or necessarily final publication-ready figures.

## Software

R; packages used across the scripts include `readxl`, `dplyr`, `sf`, `ggplot2`, `car`, `ResourceSelection`, `lmtest`, `MASS`, `pscl`, `spdep`, `DHARMa`, and `ggeffects`.

## Interpretation

This is an exploratory observational analysis. Associations should not be interpreted as causal, and the code and visualizations should not be used as an operational yellow fever risk prediction or early-warning tool without additional validation.
