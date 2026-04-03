# Functional Clustering of Discount Functions for Behavioural Investor Profiling

## Overview

This repository contains the full reproducible R pipeline associated
with the paper.

The analysis applies Functional Data Analysis (FDA) to model
intertemporal discounting behaviour and identify behavioural investor
types via functional clustering.

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
