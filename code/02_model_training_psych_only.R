
# 02_model_training_psych_only.R
# Purpose:
#   Train and evaluate tuned regression models predicting
#   HR-based physiological stress-reactivity outcomes from
#   baseline psychological predictors only.
#
# Models:
#   - Baseline mean
#   - Linear regression
#   - Elastic Net
#   - Random Forest
#   - XGBoost
#
# Design:
#   - Same participant-level train/test split for all targets.
#   - Repeated cross-validation on the training set only.
#   - Median imputation and standardization learned inside folds.
#   - Final evaluation performed once on the held-out test set.

# 1. Load packages ----------------------------------------------------------

library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(glmnet)
library(ranger)
library(xgboost)
library(doParallel)
library(foreach)

set.seed(530071)


# 2. settings ----------------------------------------------------------

N_THREADS <- max(1, parallel::detectCores() - 2)

N_FOLDS <- 5
N_REPEATS <- 10

RF_NUM_TREES <- 2000
XGB_GRID_SIZE <- 120



# 3. Register parallel backend --------------------------------------------

cl <- parallel::makeCluster(N_THREADS)
doParallel::registerDoParallel(cl)
on.exit({
  try(parallel::stopCluster(cl), silent = TRUE)
}, add = TRUE)


# 4. Load modelling dataset ------------------------------------------------

model_data_scale <- read_csv("model_data_scale_features.csv")


# 5. Define predictors and primary HR targets ------------------------------

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


# 6. Basic checks -----------------------------------------------------------

missing_predictors <- setdiff(scale_predictors, names(model_data_scale))
missing_targets <- setdiff(primary_targets, names(model_data_scale))

if (length(missing_predictors) > 0) {
  stop("These predictors are missing from model_data_scale: ",
       paste(missing_predictors, collapse = ", "))
}

if (length(missing_targets) > 0) {
  stop("These targets are missing from model_data_scale: ",
       paste(missing_targets, collapse = ", "))
}

non_numeric_predictors <- scale_predictors[
  !sapply(model_data_scale[scale_predictors], is.numeric)
]

if (length(non_numeric_predictors) > 0) {
  stop("These predictors are not numeric and need encoding first: ",
       paste(non_numeric_predictors, collapse = ", "))
}


# 7. Helper functions -------------------------------------------------------

rmse <- function(truth, estimate) {
  sqrt(mean((truth - estimate)^2, na.rm = TRUE))
}

mae <- function(truth, estimate) {
  mean(abs(truth - estimate), na.rm = TRUE)
}

rsq_oos <- function(truth, estimate) {
  sse <- sum((truth - estimate)^2, na.rm = TRUE)
  sst <- sum((truth - mean(truth, na.rm = TRUE))^2, na.rm = TRUE)
  
  if (sst == 0) {
    return(NA_real_)
  }
  
  1 - (sse / sst)
}

compute_metrics <- function(truth, estimate) {
  tibble(
    rmse = rmse(truth, estimate),
    mae = mae(truth, estimate),
    rsq = rsq_oos(truth, estimate)
  )
}

get_preprocessing_params <- function(train_x) {
  medians <- train_x |>
    summarise(across(everything(), ~ median(.x, na.rm = TRUE)))
  
  means <- train_x |>
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
  
  sds <- train_x |>
    summarise(across(everything(), ~ sd(.x, na.rm = TRUE)))
  
  list(
    medians = medians,
    means = means,
    sds = sds
  )
}

apply_preprocessing <- function(x, params) {
  x_imp <- x
  
  for (col in names(x_imp)) {
    x_imp[[col]][is.na(x_imp[[col]])] <- params$medians[[col]]
  }
  
  x_scaled <- x_imp
  
  for (col in names(x_scaled)) {
    sd_value <- params$sds[[col]]
    
    if (is.na(sd_value) || sd_value == 0) {
      x_scaled[[col]] <- 0
    } else {
      x_scaled[[col]] <- (x_scaled[[col]] - params$means[[col]]) / sd_value
    }
  }
  
  x_scaled
}

make_train_test_split_ids <- function(data, prop = 0.80) {
  control_ids <- data |>
    filter(condition == 0) |>
    pull(participant_id)
  
  smi_ids <- data |>
    filter(condition == 1) |>
    pull(participant_id)
  
  train_control <- sample(control_ids, size = floor(length(control_ids) * prop))
  train_smi <- sample(smi_ids, size = floor(length(smi_ids) * prop))
  
  train_ids <- c(train_control, train_smi)
  test_ids <- setdiff(data$participant_id, train_ids)
  
  list(
    train_ids = train_ids,
    test_ids = test_ids
  )
}

make_repeated_folds <- function(data, v = 5, repeats = 5) {
  fold_list <- list()
  
  for (r in seq_len(repeats)) {
    fold_id <- rep(NA_integer_, nrow(data))
    
    for (cond in unique(data$condition)) {
      idx <- which(data$condition == cond)
      fold_id[idx] <- sample(rep(seq_len(v), length.out = length(idx)))
    }
    
    fold_list[[r]] <- tibble(
      row_id = seq_len(nrow(data)),
      repeat_id = r,
      fold_id = fold_id
    )
  }
  
  bind_rows(fold_list)
}


# 8. tuning grids ----------------------------------------------------

elastic_net_grid <- expand.grid(
  alpha = c(0, 0.25, 0.50, 0.75, 1)
)

rf_grid <- expand.grid(
  mtry = c(3, 5, 10, 15, 20),
  min.node.size = c(2, 5, 10, 20),
  sample.fraction = c(0.70, 0.90, 1.00)
) |>
  filter(mtry <= length(scale_predictors))

set.seed(530071)

xgb_grid_full <- expand.grid(
  nrounds = c(100, 300, 600),
  max_depth = c(1, 2, 3, 4),
  eta = c(0.005, 0.01, 0.03, 0.07),
  subsample = c(0.70, 0.90, 1.00),
  colsample_bytree = c(0.50, 0.80, 1.00),
  min_child_weight = c(1, 5, 10),
  lambda = c(1, 5, 10),
  alpha = c(0, 0.1)
)

xgb_grid <- xgb_grid_full[
  sample(
    seq_len(nrow(xgb_grid_full)),
    size = min(XGB_GRID_SIZE, nrow(xgb_grid_full))
  ),
]


# 9. Cross-validation tuning function --------------------------------------

cross_validate_tuned_models <- function(train_data, predictors, target,
                                        n_folds = 5, n_repeats = 5) {
  
  folds <- make_repeated_folds(train_data, v = n_folds, repeats = n_repeats)
  
  cv_results <- list()
  result_counter <- 1
  
  for (r in seq_len(n_repeats)) {
    
    for (fold in seq_len(n_folds)) {
      
      message("  Repeat ", r, "/", n_repeats, " | Fold ", fold, "/", n_folds)
      
      valid_rows <- folds |>
        filter(repeat_id == r, fold_id == fold) |>
        pull(row_id)
      
      fold_train <- train_data[-valid_rows, ]
      fold_valid <- train_data[valid_rows, ]
      
      x_train_raw <- fold_train |> select(all_of(predictors))
      x_valid_raw <- fold_valid |> select(all_of(predictors))
      
      y_train <- fold_train[[target]]
      y_valid <- fold_valid[[target]]
      
      prep_params <- get_preprocessing_params(x_train_raw)
      
      x_train <- apply_preprocessing(x_train_raw, prep_params)
      x_valid <- apply_preprocessing(x_valid_raw, prep_params)
      
      x_train_matrix <- as.matrix(x_train)
      x_valid_matrix <- as.matrix(x_valid)
      
      
      # Baseline mean -------------------------------------------------------
      
      pred_baseline <- rep(mean(y_train), length(y_valid))
      
      cv_results[[result_counter]] <- compute_metrics(y_valid, pred_baseline) |>
        mutate(
          model = "Baseline mean",
          target = target,
          repeat_id = r,
          fold_id = fold
        )
      result_counter <- result_counter + 1
      
      
      # Linear regression ---------------------------------------------------
      
      lm_data <- bind_cols(tibble(target_value = y_train), x_train)
      lm_fit <- lm(target_value ~ ., data = lm_data)
      pred_lm <- predict(lm_fit, newdata = x_valid)
      
      cv_results[[result_counter]] <- compute_metrics(y_valid, pred_lm) |>
        mutate(
          model = "Linear regression",
          target = target,
          repeat_id = r,
          fold_id = fold
        )
      result_counter <- result_counter + 1
      
      
      # Elastic Net ---------------------------------------------------------
      
      for (i in seq_len(nrow(elastic_net_grid))) {
        
        alpha_value <- elastic_net_grid$alpha[i]
        
        en_fit <- cv.glmnet(
          x = x_train_matrix,
          y = y_train,
          alpha = alpha_value,
          nfolds = 5,
          standardize = FALSE,
          parallel = TRUE
        )
        
        pred_en <- as.numeric(
          predict(en_fit, newx = x_valid_matrix, s = "lambda.min")
        )
        
        cv_results[[result_counter]] <- compute_metrics(y_valid, pred_en) |>
          mutate(
            model = "Elastic Net",
            target = target,
            repeat_id = r,
            fold_id = fold,
            alpha = alpha_value,
            lambda = en_fit$lambda.min
          )
        result_counter <- result_counter + 1
      }
      
      
      # Random Forest -------------------------------------------------------
      
      rf_train_data <- bind_cols(tibble(target_value = y_train), x_train)
      
      for (i in seq_len(nrow(rf_grid))) {
        
        rf_fit <- ranger(
          target_value ~ .,
          data = rf_train_data,
          num.trees = RF_NUM_TREES,
          mtry = rf_grid$mtry[i],
          min.node.size = rf_grid$min.node.size[i],
          sample.fraction = rf_grid$sample.fraction[i],
          importance = "permutation",
          num.threads = N_THREADS,
          seed = 530071
        )
        
        pred_rf <- predict(rf_fit, data = x_valid)$predictions
        
        cv_results[[result_counter]] <- compute_metrics(y_valid, pred_rf) |>
          mutate(
            model = "Random Forest",
            target = target,
            repeat_id = r,
            fold_id = fold,
            mtry = rf_grid$mtry[i],
            min.node.size = rf_grid$min.node.size[i],
            sample.fraction = rf_grid$sample.fraction[i]
          )
        result_counter <- result_counter + 1
      }
      
      
      # XGBoost -------------------------------------------------------------
      
      dtrain <- xgb.DMatrix(data = x_train_matrix, label = y_train)
      dvalid <- xgb.DMatrix(data = x_valid_matrix)
      
      for (i in seq_len(nrow(xgb_grid))) {
        
        xgb_params <- list(
          objective = "reg:squarederror",
          max_depth = xgb_grid$max_depth[i],
          eta = xgb_grid$eta[i],
          subsample = xgb_grid$subsample[i],
          colsample_bytree = xgb_grid$colsample_bytree[i],
          min_child_weight = xgb_grid$min_child_weight[i],
          lambda = xgb_grid$lambda[i],
          alpha = xgb_grid$alpha[i],
          nthread = N_THREADS,
          seed = 530071
        )
        
        xgb_fit <- xgb.train(
          params = xgb_params,
          data = dtrain,
          nrounds = xgb_grid$nrounds[i],
          verbose = 0
        )
        
        pred_xgb <- predict(xgb_fit, dvalid)
        
        cv_results[[result_counter]] <- compute_metrics(y_valid, pred_xgb) |>
          mutate(
            model = "XGBoost",
            target = target,
            repeat_id = r,
            fold_id = fold,
            nrounds = xgb_grid$nrounds[i],
            max_depth = xgb_grid$max_depth[i],
            eta = xgb_grid$eta[i],
            subsample = xgb_grid$subsample[i],
            colsample_bytree = xgb_grid$colsample_bytree[i],
            min_child_weight = xgb_grid$min_child_weight[i],
            lambda = xgb_grid$lambda[i],
            alpha = xgb_grid$alpha[i]
          )
        result_counter <- result_counter + 1
      }
    }
  }
  
  bind_rows(cv_results)
}


# 10. Select best hyperparameters ------------------------------------------

select_best_params <- function(cv_results) {
  
  best_en <- cv_results |>
    filter(model == "Elastic Net") |>
    group_by(alpha) |>
    summarise(
      mean_rmse = mean(rmse, na.rm = TRUE),
      mean_mae = mean(mae, na.rm = TRUE),
      mean_rsq = mean(rsq, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(mean_rmse) |>
    slice(1)
  
  best_rf <- cv_results |>
    filter(model == "Random Forest") |>
    group_by(mtry, min.node.size, sample.fraction) |>
    summarise(
      mean_rmse = mean(rmse, na.rm = TRUE),
      mean_mae = mean(mae, na.rm = TRUE),
      mean_rsq = mean(rsq, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(mean_rmse) |>
    slice(1)
  
  best_xgb <- cv_results |>
    filter(model == "XGBoost") |>
    group_by(
      nrounds,
      max_depth,
      eta,
      subsample,
      colsample_bytree,
      min_child_weight,
      lambda,
      alpha
    ) |>
    summarise(
      mean_rmse = mean(rmse, na.rm = TRUE),
      mean_mae = mean(mae, na.rm = TRUE),
      mean_rsq = mean(rsq, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(mean_rmse) |>
    slice(1)
  
  list(
    elastic_net = best_en,
    random_forest = best_rf,
    xgboost = best_xgb
  )
}


# 11. Final model fitting and test evaluation ------------------------------

fit_final_tuned_models <- function(train_data, test_data, predictors, target, best_params) {
  
  x_train_raw <- train_data |> select(all_of(predictors))
  x_test_raw <- test_data |> select(all_of(predictors))
  
  y_train <- train_data[[target]]
  y_test <- test_data[[target]]
  
  prep_params <- get_preprocessing_params(x_train_raw)
  
  x_train <- apply_preprocessing(x_train_raw, prep_params)
  x_test <- apply_preprocessing(x_test_raw, prep_params)
  
  x_train_matrix <- as.matrix(x_train)
  x_test_matrix <- as.matrix(x_test)
  
  final_metrics <- list()
  final_predictions <- list()
  
  
  # Baseline mean -----------------------------------------------------------
  
  pred_baseline <- rep(mean(y_train), length(y_test))
  
  final_metrics[["Baseline mean"]] <- compute_metrics(y_test, pred_baseline) |>
    mutate(model = "Baseline mean", target = target)
  
  final_predictions[["Baseline mean"]] <- tibble(
    participant_id = test_data$participant_id,
    condition = test_data$condition,
    truth = y_test,
    estimate = pred_baseline,
    model = "Baseline mean",
    target = target
  )
  
  
  # Linear regression -------------------------------------------------------
  
  lm_data <- bind_cols(tibble(target_value = y_train), x_train)
  lm_fit <- lm(target_value ~ ., data = lm_data)
  pred_lm <- predict(lm_fit, newdata = x_test)
  
  final_metrics[["Linear regression"]] <- compute_metrics(y_test, pred_lm) |>
    mutate(model = "Linear regression", target = target)
  
  final_predictions[["Linear regression"]] <- tibble(
    participant_id = test_data$participant_id,
    condition = test_data$condition,
    truth = y_test,
    estimate = pred_lm,
    model = "Linear regression",
    target = target
  )
  
  
  # Elastic Net -------------------------------------------------------------
  
  best_alpha_en <- best_params$elastic_net$alpha[1]
  
  en_fit <- cv.glmnet(
    x = x_train_matrix,
    y = y_train,
    alpha = best_alpha_en,
    nfolds = 10,
    standardize = FALSE,
    parallel = TRUE
  )
  
  pred_en <- as.numeric(
    predict(en_fit, newx = x_test_matrix, s = "lambda.min")
  )
  
  final_metrics[["Elastic Net"]] <- compute_metrics(y_test, pred_en) |>
    mutate(model = "Elastic Net", target = target)
  
  final_predictions[["Elastic Net"]] <- tibble(
    participant_id = test_data$participant_id,
    condition = test_data$condition,
    truth = y_test,
    estimate = pred_en,
    model = "Elastic Net",
    target = target
  )
  
  
  # Random Forest -----------------------------------------------------------
  
  rf_params <- best_params$random_forest
  
  rf_train_data <- bind_cols(tibble(target_value = y_train), x_train)
  
  rf_fit <- ranger(
    target_value ~ .,
    data = rf_train_data,
    num.trees = RF_NUM_TREES,
    mtry = rf_params$mtry[1],
    min.node.size = rf_params$min.node.size[1],
    sample.fraction = rf_params$sample.fraction[1],
    importance = "permutation",
    num.threads = N_THREADS,
    seed = 530071
  )
  
  pred_rf <- predict(rf_fit, data = x_test)$predictions
  
  final_metrics[["Random Forest"]] <- compute_metrics(y_test, pred_rf) |>
    mutate(model = "Random Forest", target = target)
  
  final_predictions[["Random Forest"]] <- tibble(
    participant_id = test_data$participant_id,
    condition = test_data$condition,
    truth = y_test,
    estimate = pred_rf,
    model = "Random Forest",
    target = target
  )
  
  
  # XGBoost -----------------------------------------------------------------
  
  xgb_best <- best_params$xgboost
  
  dtrain <- xgb.DMatrix(data = x_train_matrix, label = y_train)
  dtest <- xgb.DMatrix(data = x_test_matrix)
  
  xgb_params <- list(
    objective = "reg:squarederror",
    max_depth = xgb_best$max_depth[1],
    eta = xgb_best$eta[1],
    subsample = xgb_best$subsample[1],
    colsample_bytree = xgb_best$colsample_bytree[1],
    min_child_weight = xgb_best$min_child_weight[1],
    lambda = xgb_best$lambda[1],
    alpha = xgb_best$alpha[1],
    nthread = N_THREADS,
    seed = 530071
  )
  
  xgb_fit <- xgb.train(
    params = xgb_params,
    data = dtrain,
    nrounds = xgb_best$nrounds[1],
    verbose = 0
  )
  
  pred_xgb <- predict(xgb_fit, dtest)
  
  final_metrics[["XGBoost"]] <- compute_metrics(y_test, pred_xgb) |>
    mutate(model = "XGBoost", target = target)
  
  final_predictions[["XGBoost"]] <- tibble(
    participant_id = test_data$participant_id,
    condition = test_data$condition,
    truth = y_test,
    estimate = pred_xgb,
    model = "XGBoost",
    target = target
  )
  
  
  # Combine outputs ---------------------------------------------------------
  
  metrics <- bind_rows(final_metrics)
  
  predictions <- bind_rows(final_predictions) |>
    mutate(
      residual = truth - estimate,
      absolute_error = abs(residual)
    )
  
  models <- list(
    linear_regression = lm_fit,
    elastic_net = en_fit,
    random_forest = rf_fit,
    xgboost = xgb_fit
  )
  
  list(
    metrics = metrics,
    predictions = predictions,
    models = models,
    preprocessing = prep_params
  )
}


# 12. Create one global train/test split -----------------------------------

global_split <- make_train_test_split_ids(model_data_scale, prop = 0.80)

write_csv(
  tibble(
    participant_id = c(global_split$train_ids, global_split$test_ids),
    split = c(
      rep("train", length(global_split$train_ids)),
      rep("test", length(global_split$test_ids))
    )
  ),
  "global_train_test_split.csv"
)


# 13. Run tuned modelling pipeline -----------------------------------------

all_results <- list()

start_time <- Sys.time()

for (target in primary_targets) {
  
  message("\n============================================================")
  message("Running tuned models for target: ", target)
  message("============================================================")
  
  target_data <- model_data_scale |>
    select(
      participant_id,
      condition,
      all_of(scale_predictors),
      all_of(target)
    ) |>
    drop_na(all_of(target))
  
  train_data <- target_data |>
    filter(participant_id %in% global_split$train_ids)
  
  test_data <- target_data |>
    filter(participant_id %in% global_split$test_ids)
  
  cv_results <- cross_validate_tuned_models(
    train_data = train_data,
    predictors = scale_predictors,
    target = target,
    n_folds = N_FOLDS,
    n_repeats = N_REPEATS
  )
  
  best_params <- select_best_params(cv_results)
  
  final_results <- fit_final_tuned_models(
    train_data = train_data,
    test_data = test_data,
    predictors = scale_predictors,
    target = target,
    best_params = best_params
  )
  
  all_results[[target]] <- list(
    target = target,
    train_data = train_data,
    test_data = test_data,
    cv_results = cv_results,
    best_params = best_params,
    test_metrics = final_results$metrics,
    test_predictions = final_results$predictions,
    models = final_results$models,
    preprocessing = final_results$preprocessing
  )
}

end_time <- Sys.time()
runtime <- end_time - start_time


# 14. Collect and save outputs ---------------------------------------------

cv_results_all <- map_dfr(all_results, "cv_results")

test_results_all <- map_dfr(all_results, "test_metrics")

test_predictions_all <- map_dfr(all_results, "test_predictions")

best_params_all <- imap_dfr(
  all_results,
  function(x, target_name) {
    bind_rows(
      x$best_params$elastic_net |>
        mutate(model = "Elastic Net"),
      x$best_params$random_forest |>
        mutate(model = "Random Forest"),
      x$best_params$xgboost |>
        mutate(model = "XGBoost")
    ) |>
      mutate(target = target_name)
  }
)

cv_summary_all <- cv_results_all |>
  group_by(target, model) |>
  summarise(
    mean_rmse = mean(rmse, na.rm = TRUE),
    sd_rmse = sd(rmse, na.rm = TRUE),
    mean_mae = mean(mae, na.rm = TRUE),
    sd_mae = sd(mae, na.rm = TRUE),
    mean_rsq = mean(rsq, na.rm = TRUE),
    sd_rsq = sd(rsq, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(target, mean_rmse)

write_csv(cv_results_all, "cv_results_psych_only_primary_hr_targets_detailed.csv")
write_csv(cv_summary_all, "cv_results_psych_only_primary_hr_targets_summary.csv")
write_csv(test_results_all, "test_results_psych_only_primary_hr_targets.csv")
write_csv(test_predictions_all, "test_predictions_psych_only_primary_hr_targets.csv")
write_csv(best_params_all, "best_hyperparameters_psych_only_primary_hr_targets.csv")

saveRDS(all_results, "model_results_psych_only_primary_hr_targets.rds")


# 15. Group/disparate error analysis ---------------------------------------

group_error_results <- test_predictions_all |>
  group_by(target, model, condition) |>
  summarise(
    n = n(),
    rmse = sqrt(mean(residual^2, na.rm = TRUE)),
    mae = mean(absolute_error, na.rm = TRUE),
    mean_residual = mean(residual, na.rm = TRUE),
    sd_residual = sd(residual, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(target, model, condition)

group_error_diff <- group_error_results |>
  select(target, model, condition, rmse, mae, mean_residual) |>
  pivot_wider(
    names_from = condition,
    values_from = c(rmse, mae, mean_residual),
    names_prefix = "condition_"
  ) |>
  mutate(
    rmse_diff_smi_minus_control = rmse_condition_1 - rmse_condition_0,
    mae_diff_smi_minus_control = mae_condition_1 - mae_condition_0,
    mean_residual_diff_smi_minus_control =
      mean_residual_condition_1 - mean_residual_condition_0
  ) |>
  arrange(target, model)

write_csv(group_error_results, "group_error_results_psych_only_primary_hr_targets.csv")
write_csv(group_error_diff, "group_error_differences_psych_only_primary_hr_targets.csv")


# 16. Save runtime and session info ----------------------------------------

runtime_info <- tibble(
  start_time = as.character(start_time),
  end_time = as.character(end_time),
  runtime = as.character(runtime),
  n_threads = N_THREADS,
  n_folds = N_FOLDS,
  n_repeats = N_REPEATS,
  rf_num_trees = RF_NUM_TREES,
  xgb_grid_size = XGB_GRID_SIZE
)

write_csv(runtime_info, "runtime_info_psych_only_primary_hr_targets.csv")

sink("session_info_model_training_psych_only.txt")
sessionInfo()
sink()
