# Functional Clustering of Discount Functions for Behavioural Investor Profiling


## Overview

The paper Functional Clustering of Discount Functions for Behavioural Investor Profiling is currently in press at Applied Stochastic Models in Business and Industry. This repository contains the full, reproducible pipeline used in the article, including data preprocessing, functional representation, clustering procedures, and all figures reported in the manuscript.

Authors: 
Fabrizio Maturo - Department of Economics, Statistics and Business, Universitas Mercatorum, Rome, Italy. fabrizio.maturo@unimercatorum.it

Annamaria Porreca - Department for the Promotion of Human Science and Quality of Life, San Raffaele University, Rome, Italy
Unit of Clinical and Molecular Epidemiology IRCCS San Raffaele Roma, Rome, Italy.
Department of Mathematics and Physics, University of Campania Luigi Vanvitelli, Caserta, Italy

Viviana Ventre - Department of Mathematics and Physics, University of Campania Luigi Vanvitelli, Caserta, Italy
 
Roberta Martino - Department of Economics, Statistics and Business, Universitas Mercatorum, Rome, Italy. 

Salvador Cruz Rambaud - Department of Economics and Business, University of Almeria, Almeria, Spain.

## Summary

The study proposes a fully data-driven framework for modelling intertemporal discounting behaviour, starting from experimentally elicited choices collected through a structured questionnaire in which respondents express indifference between present and delayed monetary amounts across multiple time horizons. These responses are used to reconstruct individual discount factors, yielding discrete observations of each subject’s discount trajectory over time.

To ensure behavioural and economic coherence, the observed discount functions are transformed into smooth and strictly monotone decreasing curves through constrained smoothing techniques. This step is essential to enforce theoretical consistency with discounting principles, eliminate irregularities due to measurement noise, and obtain functional representations suitable for subsequent analysis.

Within this functional framework, each individual trajectory is treated as a continuous object, allowing the application of Functional Data Analysis methods that preserve the temporal ordering and shape characteristics of preferences. This representation enables the extraction of higher-order features, such as derivatives, which provide additional information on the speed and curvature of discounting behaviour.

The core methodological contribution lies in the use of functional clustering techniques to identify latent behavioural structures. In particular, functional k-means is applied both globally and conditionally. The global clustering captures the main sources of heterogeneity across the entire sample, while the conditional functional clustering, performed within Behavioural Investor Type categories, allows the identification of sub-profiles that refine and extend the classical BIT taxonomy.

This dual clustering strategy reveals that traditional behavioural classifications are not homogeneous, but rather contain internally differentiated patterns of temporal preferences. By combining monotone smoothing, functional representation, and conditional clustering, the analysis uncovers nuanced behavioural profiles that would remain hidden under standard discrete or cross-sectional approaches.

## Repository Structure

-   data_raw/: original dataset\
-   scripts/: R scripts for the analysis\
-   output/figures/: figures used in the manuscript\
-   paper/: manuscript

## Reproducibility

To reproduce the results, run the R scripts in sequence. All figures and
results are generated from the provided data.

## Data

The dataset contains responses from participants on intertemporal choice
tasks.

Observations were filtered based on completeness and internal
consistency criteria, as described in the manuscript.

## Dataset Description

The dataset combines demographic information, behavioural profiling, risk attitudes, and functional representations of intertemporal preferences. It is derived from a structured questionnaire and subsequent data transformation procedures.

### Demographic variables

- `AGE`: respondent’s age  
- `GENDER`: categorical variable (M/F)

These variables are used as standard covariates.

---

### Behavioural Investor Type (BIT) scores

- `Artigiano (Artisan)`
- `Idealista (Idealist)`
- `Guardiano (Guardian)`
- `Razionale (Rational)`

These are continuous scores obtained from a behavioural questionnaire designed to capture different investor profiles. Each score reflects the intensity of a latent behavioural dimension, computed as an aggregation of multiple questionnaire items.

---

### Derived ranking variables

- `Primotratto (Primary trait)`
- `Secondotratto (Secondary trait)`
- `Terzotratto (Tertiary trait)`
- `Quartotratto (Quaternary trait)`

These variables represent the ranking of the four behavioural scores for each individual. They are deterministic transformations of the original scores, where the highest score defines the primary trait and the lowest the quaternary trait.

**Important note:**  
These variables are not independent from the corresponding scores and are not jointly used in the statistical models.

---

### Risk attitude variables

- `Rischio (Risk)`: ordinal numerical scale (typically 1–4)  
- `r1`: categorical labels such as  
  - `Non investe (Does not invest)`  
  - `Neutro (Neutral)`  
  - `Propenso (Risk-seeking)`  
  - `Avverso (Risk-averse)`  
- `Rischio r2 (Risk, second measure)`: additional risk-related variable derived from a different question

These variables capture individual attitudes toward financial risk. Some redundancy is present, as numerical and categorical versions encode similar information.

---

### Functional discount variables

- `f`, `f(2)`, `f(4)`, `f(7)`, ..., `f(90)`

These variables represent the individual discount function evaluated over a discrete time grid.

Key properties:
- Values lie in the interval (0,1]  
- Typically monotonic decreasing  
- `f(0) = 1` by construction  

The discount functions are reconstructed from intertemporal choice questions using an iterative interpolation procedure based on indifference conditions between present and delayed rewards.

These functional observations constitute the main object of analysis in the paper.

---

### Additional derived variables

The dataset also includes additional columns such as:
H(0,6,500,12)
H(0,6,50,12)
H(0,1,50,1)
H(0,6,-500,12)
H(0,6,-500,12) GUADAGNO (Gain)

These variables are derived indicators related to discounting behaviour or payoff measures.

**Important note:**  
Some of these variables are included for completeness and transparency but are **not used in the analysis presented in the paper nor in the R code pipeline**.

---

### Overall structure

Each row corresponds to one individual and includes:

1. Demographic variables  
2. Behavioural scores and derived rankings  
3. Risk attitude measures  
4. A full discount function over time  
5. Additional derived indicators  

The dataset is therefore a hybrid structure combining scalar, categorical, and functional data.

---

### Reproducibility note

All analyses in the paper are based on:
- the reconstructed discount functions  
- selected behavioural and demographic variables  

Any additional variables included in the dataset but not explicitly used in the code are provided solely for completeness and reproducibility purposes.

## Notes

The repository is designed to ensure full reproducibility of the
results.
