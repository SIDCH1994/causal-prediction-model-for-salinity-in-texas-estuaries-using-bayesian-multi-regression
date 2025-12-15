# Bayesian Causal Modeling of Salinity in Texas Estuaries

Bayesian multi-regression framework to **analyze and causally interpret salinity dynamics** in Texas estuaries using environmental and nutrient variables, with full uncertainty quantification via Bayesian inference.

> **Primary study area:** Baffin Bay, Texas (hypersaline estuary)

---

## Project Overview
Salinity is a key driver of estuarine ecosystem health. This project applies **Bayesian Multiple Linear Regression (BMLR)** to identify and quantify the environmental factors influencing salinity in Baffin Bay using observational data.

The modeling framework emphasizes:
- **Uncertainty-aware inference**
- **Causal interpretability under explicit assumptions**
- **Model comparison using WAIC and PSIS**

---

## Key Results (Quantified)
- Dataset: **206 observations**, **20 original variables**, collected between **Mar 2018 – Jul 2019**
- Final model predictors: **Temperature, DO%, pH, TOC, TN**
- Best model selected via **lowest WAIC and PSIS**
- All parameters showed **R-hat ≈ 1.00** and **ESS > 1200** (strong MCMC convergence)
- Final model residual uncertainty: **σ ≈ 0.63 PSU**

**Effect sizes (final model):**
- +1°C temperature → **+1.03 PSU salinity**
- Higher TOC → strong positive salinity effect
- Higher pH and TN → negative salinity association (freshwater dilution signal)

---

## Repository Structure
```
├── baffin_bay_code.R # Full Bayesian workflow (EDA → modeling → inference)
├── Baffin_bay.xlsx # Environmental dataset
├── bayesian_mlr_salinity_prediction.Rproj # RStudio project
└── plots/ # Generated diagnostics and figures
```


---

## Data Description
Source: **GRIIDC / Harte Research Institute (TAMU-CC)**

Variables include:
- Salinity (PSU)
- Temperature (°C)
- Dissolved Oxygen (%)
- pH
- Total Organic Carbon (TOC)
- Total Nitrogen (TN)
- Additional nutrient and chlorophyll variables (filtered during modeling)

---

## Methodology

### Exploratory Data Analysis
- Boxplots, histograms, scatterplot matrices
- Correlation analysis (e.g., salinity–conductivity r ≈ 0.96)
- Multicollinearity detection using **VIF**

### Feature Selection
- Removed redundant, post-treatment, or highly collinear variables
- Final predictors chosen for **causal relevance and statistical stability**

### Bayesian Modeling
- Implemented using `ulam()` from **rethinking (Stan backend)**
- Priors:
  - Intercept: Normal(0, 1.5)
  - Slopes: Normal(0, 0.5)
  - Error term: Exponential(2)
- Fitted with **4 MCMC chains** using **NUTS**

### Model Comparison
- Compared 3 candidate models
- Selected final model using **WAIC and PSIS**
- Validated via posterior predictive checks

---

## How to Run

### RStudio (Recommended)
1. Open `bayesian_mlr_salinity_prediction.Rproj`
2. Run `baffin_bay_code.R`

### R Console
```r
source("baffin_bay_code.R")
```

## Software Stack
- R (4.x)
- rethinking (Stan)
- ggplot2, dplyr, tidyr
- corrplot, GGally, car, readxl

## Author
- Siddhartha Chilukuri
- MS Data Science, Texas A&M University – Corpus Christi
