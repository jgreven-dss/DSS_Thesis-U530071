
# Purpose:
#   Produce reproducible diagnostic outputs supporting target
#   selection, HRV exclusion from main models, predictor checks,
#   and interpretation of weak model performance.
#
# Inputs:
#   - CEPAV_data_29112024.xlsx
#   - model_data_scale_features.csv
#
# Outputs:
#   - physio_availability_by_measure_phase.csv
#   - hrv_outlier_checks.csv
#   - hr_outlier_checks.csv
#   - predictor_correlation_high_pairs_psych_only.csv
#   - predictor_target_correlations_psych_only.csv
#   - top_predictor_target_correlations_psych_only.csv
#   - hr_target_correlation_matrix_psych_only.csv
#   - hrv_target_correlation_matrix.csv
#   - target_summary_primary_hr.csv
#   - condition_balance_final_sample.csv
#   - session_info_diagnostics_psych_only.txt

# 1. Load packages ----------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(purrr)
library(readr)


# 2. Load data --------------------------------------------------------------

path <- "CEPAV_data_29112024.xlsx"

physio_behav <- read_excel(path, sheet = "physio_behav") |>
  clean_names() |>
  rename(participant_id = id)

model_data_scale <- read_csv("model_data_scale_features.csv", show_col_types = FALSE)


# 3. Define predictors and targets -----------------------------------------

scale_predictors <- c(
  "gad2_t1",
  "phq2_t1",
  "flourishing_t1",
  "swls3_t1",
  "stress_debilitating_mindset_t1",
  "fixed_mindset_t1",
  "gaming_disorder_t1",
  "alexithymia_t1",
  "body_awareness_t1",
  "ebq_negative_controllability_t1",
  "ebq_positive_controllability_t1",
  "ebq_negative_usefulness_t1",
  "ebq_positive_usefulness_t1",
  "positive_affect_t1",
  "negative_affect_t1",
  "ress_relaxation_t1",
  "ress_engagement_t1",
  "ress_rumination_t1",
  "ress_reappraisal_t1",
  "ress_distraction_t1",
  "ress_suppression_t1",
  "self_esteem_t1",
  "health_t1"
)

primary_targets <- c(
  "hr_reactivity_mean_across_matches",
  "hr_peak_reactivity_mean_across_matches",
  "hr_gameplay_sd_mean_across_matches",
  "hr_recovery_delta_mean_across_matches"
)

candidate_hrv_targets <- c(
  "hrv_reactivity_mean_across_matches",
  "hrv_gameplay_sd_mean_across_matches",
  "hrv_recovery_delta_mean_across_matches"
)


# 4. Final sample and condition balance ------------------------------------

condition_balance <- model_data_scale |>
  count(condition, name = "n") |>
  mutate(
    condition_label = case_when(
      condition == 0 ~ "Control",
      condition == 1 ~ "SMI",
      TRUE ~ as.character(condition)
    )
  ) |>
  select(condition, condition_label, n)

write_csv(condition_balance, "condition_balance_final_sample.csv")


# 5. Physiological availability by measure and phase -----------------------

physio_availability <- physio_behav |>
  select(
    participant_id,
    matches("^tournament[1-8]_(baseline|gameplay|recovery)_min[1-2]_(hrv|hr|sbp|dbp|co|tpr)$")
  ) |>
  pivot_longer(
    cols = -participant_id,
    names_to = c("match", "phase", "minute", "measure"),
    names_pattern = "^tournament([1-8])_(baseline|gameplay|recovery)_min([1-2])_(hrv|hr|sbp|dbp|co|tpr)$",
    values_to = "value"
  ) |>
  group_by(measure, phase) |>
  summarise(
    n_values = n(),
    n_non_missing = sum(!is.na(value)),
    n_missing = sum(is.na(value)),
    prop_non_missing = mean(!is.na(value)),
    .groups = "drop"
  ) |>
  arrange(measure, phase)

write_csv(physio_availability, "physio_availability_by_measure_phase.csv")


# 6. Outlier diagnostics for HR and HRV targets ----------------------------

flag_outliers_iqr <- function(data, variable, multiplier = 3) {
  x <- data[[variable]]
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - multiplier * iqr
  upper <- q3 + multiplier * iqr
  
  data |>
    filter(.data[[variable]] < lower | .data[[variable]] > upper) |>
    transmute(
      participant_id,
      condition,
      target = variable,
      value = .data[[variable]],
      lower_bound = lower,
      upper_bound = upper
    )
}

hr_outlier_checks <- map_dfr(
  primary_targets,
  ~ flag_outliers_iqr(model_data_scale, .x, multiplier = 3)
)

hrv_outlier_checks <- map_dfr(
  candidate_hrv_targets,
  ~ flag_outliers_iqr(model_data_scale, .x, multiplier = 3)
)

write_csv(hr_outlier_checks, "hr_outlier_checks.csv")
write_csv(hrv_outlier_checks, "hrv_outlier_checks.csv")


# 7. Target summaries -------------------------------------------------------

target_summary_primary_hr <- model_data_scale |>
  select(all_of(primary_targets)) |>
  summarise(
    across(
      everything(),
      list(
        n = ~ sum(!is.na(.x)),
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) |>
  pivot_longer(
    cols = everything(),
    names_to = c("target", ".value"),
    names_pattern = "(.*)_(n|mean|sd|min|median|max)$"
  )

write_csv(target_summary_primary_hr, "target_summary_primary_hr.csv")


# 8. Predictor multicollinearity check -------------------------------------

predictor_cor <- model_data_scale |>
  select(all_of(scale_predictors)) |>
  cor(use = "pairwise.complete.obs")

high_cor_pairs <- which(
  abs(predictor_cor) > 0.80 & abs(predictor_cor) < 1,
  arr.ind = TRUE
)

predictor_correlation_high_pairs <- tibble(
  var1 = rownames(predictor_cor)[high_cor_pairs[, 1]],
  var2 = colnames(predictor_cor)[high_cor_pairs[, 2]],
  correlation = predictor_cor[high_cor_pairs]
) |>
  filter(var1 < var2) |>
  arrange(desc(abs(correlation)))

write_csv(predictor_correlation_high_pairs, "predictor_correlation_high_pairs_psych_only.csv")


# 9. Predictor-target correlations -----------------------------------------

predictor_target_correlations <- map_dfr(
  primary_targets,
  function(target_name) {
    model_data_scale |>
      select(all_of(scale_predictors), target_value = all_of(target_name)) |>
      summarise(
        across(
          all_of(scale_predictors),
          ~ cor(.x, target_value, use = "pairwise.complete.obs")
        )
      ) |>
      pivot_longer(
        cols = everything(),
        names_to = "predictor",
        values_to = "correlation"
      ) |>
      mutate(
        target = target_name,
        abs_correlation = abs(correlation)
      )
  }
) |>
  arrange(target, desc(abs_correlation))

top_predictor_target_correlations <- predictor_target_correlations |>
  group_by(target) |>
  slice_max(abs_correlation, n = 10, with_ties = FALSE) |>
  ungroup() |>
  arrange(target, desc(abs_correlation))

write_csv(predictor_target_correlations, "predictor_target_correlations_psych_only.csv")
write_csv(top_predictor_target_correlations, "top_predictor_target_correlations_psych_only.csv")


# 10. Target correlation matrices --------------------------------------

hr_target_correlation_matrix <- model_data_scale |>
  select(all_of(primary_targets)) |>
  cor(use = "pairwise.complete.obs")

hr_target_correlation_matrix_long <- as.data.frame(as.table(hr_target_correlation_matrix)) |>
  as_tibble() |>
  rename(target_1 = Var1, target_2 = Var2, correlation = Freq)

write_csv(hr_target_correlation_matrix_long, "hr_target_correlation_matrix_psych_only.csv")

available_hrv_targets <- intersect(candidate_hrv_targets, names(model_data_scale))

if (length(available_hrv_targets) > 1) {
  hrv_target_correlation_matrix <- model_data_scale |>
    select(all_of(available_hrv_targets)) |>
    cor(use = "pairwise.complete.obs")
  
  hrv_target_correlation_matrix_long <- as.data.frame(as.table(hrv_target_correlation_matrix)) |>
    as_tibble() |>
    rename(target_1 = Var1, target_2 = Var2, correlation = Freq)
  
  write_csv(hrv_target_correlation_matrix_long, "hrv_target_correlation_matrix.csv")
}


# 11. Save session info ---------------------------------------------------

sink("session_info_diagnostics_psych_only.txt")
sessionInfo()
sink()


# 12. Console summary -------------------------------------------------------

message("Diagnostics completed. Key outputs saved:")
message("- physio_availability_by_measure_phase.csv")
message("- hrv_outlier_checks.csv")
message("- hr_outlier_checks.csv")
message("- predictor_correlation_high_pairs_psych_only.csv")
message("- predictor_target_correlations_psych_only.csv")
message("- top_predictor_target_correlations_psych_only.csv")
message("- hr_target_correlation_matrix_psych_only.csv")
message("- target_summary_primary_hr.csv")
message("- condition_balance_final_sample.csv")
