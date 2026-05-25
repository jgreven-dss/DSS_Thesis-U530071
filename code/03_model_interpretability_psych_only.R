

#   Extract and summarize model interpretability outputs for
#   the tuned primary HR models.
#
# Inputs:
#   - model_results_psych_only_primary_hr_targets.rds
#   - test_results_psych_only_primary_hr_targets.csv
#
# Outputs:
#   - elastic_net_coefficients_psych_only_primary_hr_targets.csv
#   - random_forest_importance_psych_only_primary_hr_targets.csv
#   - xgboost_importance_psych_only_primary_hr_targets.csv
#   - combined_feature_importance_psych_only_primary_hr_targets.csv
#   - top_combined_feature_importance_psych_only_primary_hr_targets.csv
#   - interpretability_model_performance_context_psych_only.csv
#   - feature_importance_plots_psych_only/*.png
#   - session_info_model_interpretability_psych_only.txt
#


# 1. Load packages ----------------------------------------------------------

library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(glmnet)
library(ranger)
library(xgboost)
library(ggplot2)


# 2. Load model objects and performance context -----------------------------

model_results <- readRDS("model_results_psych_only_primary_hr_targets.rds")
test_results <- read_csv("test_results_psych_only_primary_hr_targets.csv", show_col_types = FALSE)

primary_targets <- names(model_results)

# Predictor order used during modelling. This must match the training script.
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


# 3. Model-performance context ---------------------------------------------

# This table is useful because importance values should only be interpreted
# in light of each model's predictive performance.
performance_context <- test_results |>
  select(target, model, rmse, mae, rsq) |>
  arrange(target, rmse)

write_csv(
  performance_context,
  "interpretability_model_performance_context_psych_only.csv"
)


# 4. Elastic Net coefficients ----------------------------------------------

extract_elastic_net_coefficients <- function(result_object, target_name) {
  fit <- result_object$models$elastic_net
  coefs <- coef(fit, s = "lambda.min")
  coef_df <- tibble(
    predictor = rownames(coefs),
    coefficient = as.numeric(coefs[, 1])
  ) |>
    filter(predictor != "(Intercept)") |>
    mutate(
      target = target_name,
      model = "Elastic Net",
      abs_importance = abs(coefficient),
      direction = case_when(
        coefficient > 0 ~ "positive",
        coefficient < 0 ~ "negative",
        TRUE ~ "zero"
      )
    ) |>
    arrange(target, desc(abs_importance))
  
  coef_df
}

elastic_net_coefficients <- imap_dfr(
  model_results,
  extract_elastic_net_coefficients
)

write_csv(
  elastic_net_coefficients,
  "elastic_net_coefficients_psych_only_primary_hr_targets.csv"
)


# 5. Random Forest permutation importance ----------------------------------

extract_rf_importance <- function(result_object, target_name) {
  fit <- result_object$models$random_forest
  imp <- ranger::importance(fit)
  
  tibble(
    predictor = names(imp),
    importance = as.numeric(imp)
  ) |>
    mutate(
      target = target_name,
      model = "Random Forest",
      abs_importance = abs(importance)
    ) |>
    arrange(target, desc(abs_importance))
}

random_forest_importance <- imap_dfr(
  model_results,
  extract_rf_importance
)

write_csv(
  random_forest_importance,
  "random_forest_importance_psych_only_primary_hr_targets.csv"
)


# 6. XGBoost feature importance --------------------------------------------

extract_xgb_importance <- function(result_object, target_name) {
  fit <- result_object$models$xgboost
  imp <- xgboost::xgb.importance(
    feature_names = scale_predictors,
    model = fit
  )
  
  if (nrow(imp) == 0) {
    return(tibble(
      predictor = character(),
      gain = numeric(),
      cover = numeric(),
      frequency = numeric(),
      target = character(),
      model = character(),
      abs_importance = numeric()
    ))
  }
  
  as_tibble(imp) |>
    transmute(
      predictor = Feature,
      gain = Gain,
      cover = Cover,
      frequency = Frequency,
      target = target_name,
      model = "XGBoost",
      abs_importance = gain
    ) |>
    arrange(target, desc(abs_importance))
}

xgboost_importance <- imap_dfr(
  model_results,
  extract_xgb_importance
)

write_csv(
  xgboost_importance,
  "xgboost_importance_psych_only_primary_hr_targets.csv"
)


# 7. Combine importance rankings -------------------------------------------

# Importance values are on different scales across model families. Therefore,
# this script creates within-target, within-model ranks and normalized scores.
# These are intended as descriptive interpretability summaries, not causal effects.

en_ranked <- elastic_net_coefficients |>
  group_by(target, model) |>
  mutate(
    rank = rank(-abs_importance, ties.method = "average"),
    normalized_importance = ifelse(
      max(abs_importance, na.rm = TRUE) == 0,
      0,
      abs_importance / max(abs_importance, na.rm = TRUE)
    )
  ) |>
  ungroup() |>
  select(target, model, predictor, rank, normalized_importance)

rf_ranked <- random_forest_importance |>
  group_by(target, model) |>
  mutate(
    rank = rank(-abs_importance, ties.method = "average"),
    normalized_importance = ifelse(
      max(abs_importance, na.rm = TRUE) == 0,
      0,
      abs_importance / max(abs_importance, na.rm = TRUE)
    )
  ) |>
  ungroup() |>
  select(target, model, predictor, rank, normalized_importance)

xgb_ranked <- xgboost_importance |>
  group_by(target, model) |>
  mutate(
    rank = rank(-abs_importance, ties.method = "average"),
    normalized_importance = ifelse(
      max(abs_importance, na.rm = TRUE) == 0,
      0,
      abs_importance / max(abs_importance, na.rm = TRUE)
    )
  ) |>
  ungroup() |>
  select(target, model, predictor, rank, normalized_importance)

combined_feature_importance <- bind_rows(
  en_ranked,
  rf_ranked,
  xgb_ranked
) |>
  arrange(target, model, rank)

write_csv(
  combined_feature_importance,
  "combined_feature_importance_psych_only_primary_hr_targets.csv"
)


# 8. Top combined predictors ------------------------------------------------

# This summarizes which predictors repeatedly appear near the top across
# Elastic Net, Random Forest, and XGBoost for each target.

top_combined_feature_importance <- combined_feature_importance |>
  group_by(target, predictor) |>
  summarise(
    mean_rank = mean(rank, na.rm = TRUE),
    median_rank = median(rank, na.rm = TRUE),
    mean_normalized_importance = mean(normalized_importance, na.rm = TRUE),
    n_models_available = n_distinct(model),
    .groups = "drop"
  ) |>
  arrange(target, mean_rank)

top_combined_feature_importance_by_target <- top_combined_feature_importance |>
  group_by(target) |>
  slice_min(mean_rank, n = 10, with_ties = FALSE) |>
  ungroup()

write_csv(
  top_combined_feature_importance,
  "combined_feature_importance_summary_psych_only_primary_hr_targets.csv"
)

write_csv(
  top_combined_feature_importance_by_target,
  "top_combined_feature_importance_psych_only_primary_hr_targets.csv"
)


# 9. Plots -----------------------------------------------------------------

if (!dir.exists("feature_importance_plots_psych_only")) {
  dir.create("feature_importance_plots_psych_only")
}

plot_top_importance <- function(data, target_name, model_name, file_name) {
  plot_data <- data |>
    filter(target == target_name, model == model_name) |>
    arrange(desc(normalized_importance)) |>
    slice_head(n = 10) |>
    mutate(predictor = reorder(predictor, normalized_importance))
  
  if (nrow(plot_data) == 0) {
    return(NULL)
  }
  
  p <- ggplot(plot_data, aes(x = predictor, y = normalized_importance)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste(model_name, "feature importance"),
      subtitle = target_name,
      x = "Predictor",
      y = "Normalized importance"
    ) +
    theme_minimal()
  
  ggsave(
    filename = file.path("feature_importance_plots_psych_only", file_name),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300
  )
}

for (target_name in primary_targets) {
  for (model_name in unique(combined_feature_importance$model)) {
    safe_target <- gsub("[^A-Za-z0-9_]+", "_", target_name)
    safe_model <- gsub("[^A-Za-z0-9_]+", "_", model_name)
    file_name <- paste0("importance_", safe_target, "_", safe_model, ".png")
    plot_top_importance(combined_feature_importance, target_name, model_name, file_name)
  }
}


# 10. Save session info -----------------------------------------------------

sink("session_info_model_interpretability_psych_only.txt")
sessionInfo()
sink()


# 11. Print key outputs -----------------------------------------------------

message("\nModel performance context:")
print(performance_context)

message("\nTop combined feature-importance summary:")
print(top_combined_feature_importance_by_target)
