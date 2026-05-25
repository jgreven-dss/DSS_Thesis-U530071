library(readr)
library(dplyr)
library(ggplot2)

test_predictions <- read_csv("test_predictions_psych_only_primary_hr_targets.csv")

plot_data <- test_predictions |>
  filter(model == "XGBoost") |>
  mutate(
    target_label = case_when(
      target == "hr_reactivity_mean_across_matches" ~ "Mean HR reactivity",
      target == "hr_peak_reactivity_mean_across_matches" ~ "Peak HR reactivity",
      target == "hr_gameplay_sd_mean_across_matches" ~ "HR gameplay variability",
      target == "hr_recovery_delta_mean_across_matches" ~ "HR recovery delta",
      TRUE ~ target
    )
  )

p <- ggplot(plot_data, aes(x = truth, y = estimate)) +
  geom_point(
    alpha = 0.65,
    size = 1.8
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  facet_wrap(~ target_label, scales = "free", ncol = 2) +
  labs(
    x = "Observed value",
    y = "Predicted value"
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10),
    panel.border = element_rect(fill = NA, linewidth = 0.4),
    panel.spacing = unit(1.2, "lines")
  )

p

ggsave(
  filename = "figure_predicted_vs_observed_hr_outcomes.pdf",
  plot = p,
  width = 7.2,
  height = 5.2,
  units = "in"
)

ggsave(
  filename = "figure_predicted_vs_observed_hr_outcomes.png",
  plot = p,
  width = 7.2,
  height = 5.2,
  units = "in",
  dpi = 300
)