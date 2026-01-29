# INLA Baseline Model for Simulation Study

**This model is for simulation study purposes only and should not be used for actual disease prediction.**

## Overview

A simple CHAP-compatible baseline model using INLA with:
- Poisson likelihood with log link
- Linear effects for `rainfall` and `mean_temperature` with a 3-month lag (shared across regions)
- Spatial IID random effect per location
- Monthly data support

## Model Formula

```r
disease_cases ~ rainfall_lag3 + mean_temperature_lag3 + f(location_id, model = "iid")
```

With `E = population` as the offset.

## Usage

```bash
# Local testing
Rscript isolated_run.R
```

## Purpose

This model serves as a baseline for comparing more sophisticated models in simulation studies. It intentionally uses a minimal specification to establish a performance floor.
