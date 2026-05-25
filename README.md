1. Run 01_create_modelling_dataset.R
2. Run 04_diagnostics_psych_only.R
3. Run 02_model_training_psych_only.R
4. Run 03_model_interpretability_psych_only.R

# Predicting Esports Stress Reactivity from Baseline Psychology

This repository contains the reproducibility materials for the MSc thesis "Predicting Esports Stress Reactivity from Baseline Psychology Using Machine Learning" by Julian Greven.

## Data source

This project uses the Competitive Esports Physiological, Affective, and Video (CEPAV) dataset. The dataset is not redistributed in this repository. It can be obtained from the original CEPAV OSF repository:

https://osf.io/nbyv4/

The analyses use the processed CEPAV workbook. Users who want to reproduce the analyses should download the processed data from the original source and place it in the expected local data folder before running the scripts.

## Repository structure

```text
code/
  01_create_modelling_dataset.R
  02_model_training_psych_only.R
  03_model_interpretability_psych_only.R
  04_diagnostics_psych_only.R

outputs/
  diagnostics/
  interpretability/
  model_results/
  session_info/

# Script overview

01_create_modelling_dataset.R: constructs the participant-level modelling dataset from the processed CEPAV workbook. It scores psychological questionnaire features, engineers physiological features, aggregates match-level features to participant-level targets, and saves the final modelling dataset.

04_diagnostics_psych_only.R: produces diagnostic checks, including physiological data availability, HRV outlier sensitivity checks, predictor correlations, predictor-target correlations, target correlations, and condition-balance summaries.

02_model_training_psych_only.R: trains and evaluates the final psychological-only prediction models. It uses a participant-level 80/20 train-test split, repeated cross-validation, held-out test evaluation, and saves model-performance outputs, selected hyperparameters, predictions, group-error outputs, runtime information, and split identifiers.

03_model_interpretability_psych_only.R: extracts model-interpretability outputs, including Elastic Net coefficients, Random Forest feature importance, XGBoost feature importance, and combined descriptive feature-importance summaries.


## Recommended script order order

1. 01_create_modelling_dataset.R
2. 04_diagnostics_psych_only.R
3. 02_model_training_psych_only.R
4. 03_model_interpretability_psych_only.R

The diagnostics script can be run after dataset construction. The interpretability script should be run after model training because it depends on fitted model outputs.

## Outputs

The outputs/diagnostics/ folder contains diagnostic files related to physiological availability, target correlations, predictor-target correlations, predictor-correlation checks, and outlier checks.

The outputs/model_results/ folder contains cross-validation summaries, held-out test results, held-out predictions, selected hyperparameters, group-specific error outputs, runtime information, and train-test split identifiers.

The outputs/interpretability/ folder contains Elastic Net coefficients, Random Forest feature-importance outputs, XGBoost feature-importance outputs, and combined descriptive feature-importance summaries.

The outputs/session_info/ folder contains sessionInfo() outputs documenting the R version, package versions, and runtime environments used for the final analyses.

## Software environment

The final analyses were conducted in R version 4.5.2 on Windows 11. Main packages included readxl, dplyr, tidyr, stringr, janitor, purrr, readr, glmnet, ranger, xgboost, foreach, doParallel, and ggplot2.

Full package versions are provided in the outputs/session_info/ folder.




