

# 1. Load packages ----------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(purrr)


# 2. Load CEPAV workbook ---------------------------------------------------

path <- "CEPAV_data_29112024.xlsx"

self_reports <- read_excel(path, sheet = "self_reports") |>
  clean_names() |>
  rename(participant_id = lab_id)

self_codes <- read_excel(path, sheet = "self_reports_codes") |>
  clean_names()

physio_behav <- read_excel(path, sheet = "physio_behav") |>
  clean_names() |>
  rename(participant_id = id)

physio_codes <- read_excel(path, sheet = "physio_behav_codes") |>
  clean_names()

missing_data <- read_excel(path, sheet = "missing") |>
  clean_names()

snr <- read_excel(path, sheet = "SNR") |>
  clean_names()


# 3. Helper functions ------------------------------------------------------

row_mean_min <- function(data, vars, min_prop = 0.80) {
  selected <- dplyr::select(data, dplyr::all_of(vars))
  n_valid <- rowSums(!is.na(selected))
  n_required <- ceiling(length(vars) * min_prop)
  
  score <- rowMeans(selected, na.rm = TRUE)
  score[n_valid < n_required] <- NA_real_
  score
}

row_sum_min <- function(data, vars, min_prop = 0.80) {
  selected <- dplyr::select(data, dplyr::all_of(vars))
  n_valid <- rowSums(!is.na(selected))
  n_required <- ceiling(length(vars) * min_prop)
  
  score <- rowSums(selected, na.rm = TRUE)
  score[n_valid < n_required] <- NA_real_
  score
}

safe_mean <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

safe_max <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

safe_sd <- function(x) {
  if (length(x) == 0 || sum(!is.na(x)) < 2) {
    return(NA_real_)
  }
  sd(x, na.rm = TRUE)
}

safe_phase_minute <- function(value, phase, minute, target_phase, target_minute) {
  x <- value[phase == target_phase & minute == target_minute]
  if (length(x) == 0 || all(is.na(x))) {
    return(NA_real_)
  }
  x[which(!is.na(x))[1]]
}


# 4. Psychological scale-level predictors ---------------------------------

psych_scale_features <- self_reports |>
  transmute(
    participant_id,
    condition,
    
    # Demographic / gaming background variables
    age = lab_age,
    total_csgo_hours,
    total_csgo_years,
    hours_csgo_week,
    esport_level,
    prof_level,
    rank_12months,
    
    # GAD-2 and PHQ-2
    # CEPAV uses 1-4 response values; standard scoring is 0-3.
    gad2_t1 = (gad_1_t1 - 1) + (gad_2_t1 - 1),
    phq2_t1 = (phq_1_t1 - 1) + (phq_2_t1 - 1),
    
    # Flourishing Scale
    flourishing_t1 = row_mean_min(
      pick(everything()),
      paste0("flo_", 1:8, "_t1"),
      min_prop = 0.80
    ),
    
    # 3-item Satisfaction With Life Scale
    swls3_t1 = row_mean_min(
      pick(everything()),
      paste0("swls_", 1:3, "_t1"),
      min_prop = 1
    ),
    
    # Stress-is-debilitating mindset
    # Higher scores indicate stronger beliefs that stress is harmful,
    # inhibits learning/development, and worsens performance/productivity.
    stress_debilitating_mindset_t1 = row_mean_min(
      pick(everything()),
      paste0("sms_", 1:3, "_t1"),
      min_prop = 1
    ),
    
    # Fixed mindset
    # Kept in raw scoring direction. Higher values = stronger fixed mindset.
    fixed_mindset_t1 = row_mean_min(
      pick(everything()),
      paste0("gfm_", 1:3, "_t1"),
      min_prop = 1
    ),
    
    # Gaming Disorder Test
    gaming_disorder_t1 = row_sum_min(
      pick(everything()),
      paste0("gdt_", 1:4, "_t1"),
      min_prop = 1
    ),
    
    # PAQ-S-style alexithymia score
    # CEPAV includes six ALE items, not the full 24-item PAQ.
    alexithymia_t1 = row_mean_min(
      pick(everything()),
      paste0("ale_", 1:6, "_t1"),
      min_prop = 0.80
    ),
    
    # Body Awareness Questionnaire
    # BAQ item 10 is reverse-scored using baq_10_t1_r.
    body_awareness_t1 = row_mean_min(
      pick(everything()),
      c(
        paste0("baq_", 1:9, "_t1"),
        "baq_10_t1_r",
        paste0("baq_", 11:18, "_t1")
      ),
      min_prop = 0.80
    ),
    
    # Emotion Beliefs Questionnaire subscales
    # Higher values indicate more maladaptive emotion beliefs.
    ebq_negative_controllability_t1 = row_sum_min(
      pick(everything()),
      c("ebq_1_t1", "ebq_5_t1", "ebq_9_t1", "ebq_13_t1"),
      min_prop = 1
    ),
    
    ebq_positive_controllability_t1 = row_sum_min(
      pick(everything()),
      c("ebq_2_t1", "ebq_6_t1", "ebq_10_t1", "ebq_14_t1"),
      min_prop = 1
    ),
    
    ebq_negative_usefulness_t1 = row_sum_min(
      pick(everything()),
      c("ebq_3_t1", "ebq_7_t1", "ebq_11_t1", "ebq_15_t1"),
      min_prop = 1
    ),
    
    ebq_positive_usefulness_t1 = row_sum_min(
      pick(everything()),
      c("ebq_4_t1", "ebq_8_t1", "ebq_12_t1", "ebq_16_t1"),
      min_prop = 1
    ),
    
    # Positive and negative affect
    positive_affect_t1 = row_mean_min(
      pick(everything()),
      c("aff_amu_t1", "aff_exc_t1", "aff_joy_t1", "aff_pro_t1"),
      min_prop = 1
    ),
    
    negative_affect_t1 = row_mean_min(
      pick(everything()),
      c("aff_ang_t1", "aff_fea_t1", "aff_ove_t1", "aff_str_t1"),
      min_prop = 1
    ),
    
    # RESS emotion-regulation strategy items
    # Kept separate because they represent distinct strategies.
    ress_relaxation_t1  = ress_1_t1,
    ress_engagement_t1  = ress_2_t1,
    ress_rumination_t1  = ress_3_t1,
    ress_reappraisal_t1 = ress_4_t1,
    ress_distraction_t1 = ress_5_t1,
    ress_suppression_t1 = ress_6_t1,
    
    # Single-item measures
    self_esteem_t1 = ses_t1,
    health_t1 = hea_t1
  )


# 5. Reshape tournament HR/HRV data ----------------------------------------

physio_long <- physio_behav |>
  select(
    participant_id,
    matches("^tournament[1-8]_(baseline|gameplay|recovery)_min[1-2]_(hr|hrv)$")
  ) |>
  pivot_longer(
    cols = -participant_id,
    names_to = c("match", "phase", "minute", "measure"),
    names_pattern = "^tournament([1-8])_(baseline|gameplay|recovery)_min([1-2])_(hr|hrv)$",
    values_to = "value"
  ) |>
  mutate(
    match = as.integer(match),
    minute = as.integer(minute)
  )


# 6. Compute match-level physiological features ----------------------------

match_features <- physio_long |>
  group_by(participant_id, match, measure) |>
  summarise(
    baseline_mean = safe_mean(value[phase == "baseline"]),
    gameplay_mean = safe_mean(value[phase == "gameplay"]),
    gameplay_max  = safe_max(value[phase == "gameplay"]),
    gameplay_sd   = safe_sd(value[phase == "gameplay"]),
    recovery_mean = safe_mean(value[phase == "recovery"]),
    
    recovery_min1 = safe_phase_minute(value, phase, minute, "recovery", 1),
    recovery_min2 = safe_phase_minute(value, phase, minute, "recovery", 2),
    
    n_baseline = sum(!is.na(value[phase == "baseline"])),
    n_gameplay = sum(!is.na(value[phase == "gameplay"])),
    n_recovery = sum(!is.na(value[phase == "recovery"])),
    
    .groups = "drop"
  ) |>
  mutate(
    # Gameplay-baseline activation
    reactivity_mean = gameplay_mean - baseline_mean,
    
    # Highest gameplay value relative to baseline
    peak_reactivity = gameplay_max - baseline_mean,
    
    # Recovery relative to gameplay
    # Negative values indicate downregulation after gameplay.
    recovery_delta = recovery_mean - gameplay_mean,
    
    # Change from recovery minute 1 to recovery minute 2
    recovery_slope = recovery_min2 - recovery_min1
  )


# 7. Restrict to complete HR/HRV tournament participants -------------------

complete_hr_hrv_ids <- match_features |>
  filter(measure %in% c("hr", "hrv")) |>
  group_by(participant_id, measure) |>
  summarise(
    n_matches_available = sum(!is.na(reactivity_mean)),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = measure,
    values_from = n_matches_available,
    names_prefix = "n_"
  ) |>
  filter(
    n_hr == 8,
    n_hrv == 8
  ) |>
  pull(participant_id)

match_features_complete <- match_features |>
  filter(
    participant_id %in% complete_hr_hrv_ids,
    measure %in% c("hr", "hrv")
  )


# 8. Aggregate match-level features to participant-level targets -----------

physio_participant_features <- match_features_complete |>
  group_by(participant_id, measure) |>
  summarise(
    reactivity_mean_across_matches = mean(reactivity_mean, na.rm = TRUE),
    reactivity_median_across_matches = median(reactivity_mean, na.rm = TRUE),
    reactivity_iqr_across_matches = IQR(reactivity_mean, na.rm = TRUE),
    
    peak_reactivity_mean_across_matches = mean(peak_reactivity, na.rm = TRUE),
    peak_reactivity_median_across_matches = median(peak_reactivity, na.rm = TRUE),
    
    gameplay_sd_mean_across_matches = mean(gameplay_sd, na.rm = TRUE),
    gameplay_sd_median_across_matches = median(gameplay_sd, na.rm = TRUE),
    
    recovery_delta_mean_across_matches = mean(recovery_delta, na.rm = TRUE),
    recovery_slope_mean_across_matches = mean(recovery_slope, na.rm = TRUE),
    
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = measure,
    values_from = -c(participant_id, measure),
    names_glue = "{measure}_{.value}"
  )


# 9. Create final modelling dataset ----------------------------------------

model_data_scale <- psych_scale_features |>
  inner_join(
    physio_participant_features,
    by = "participant_id"
  )


# 10. Define primary and secondary targets ---------------------------------

primary_targets <- c(
  "hr_reactivity_mean_across_matches",
  "hr_peak_reactivity_mean_across_matches",
  "hr_gameplay_sd_mean_across_matches",
  "hr_recovery_delta_mean_across_matches"
)

secondary_targets <- c(
  "hrv_reactivity_mean_across_matches",
  "hrv_gameplay_sd_mean_across_matches",
  "hrv_recovery_delta_mean_across_matches"
)


# 11. Save output files -----------------------------------------------------

write.csv(
  psych_scale_features,
  "psych_scale_features.csv",
  row.names = FALSE
)

write.csv(
  physio_participant_features,
  "physio_participant_features.csv",
  row.names = FALSE
)

write.csv(
  model_data_scale,
  "model_data_scale_features.csv",
  row.names = FALSE
)


# 13. Save reproducibility information -------------------------------------

sink("session_info.txt")
sessionInfo()
sink()

