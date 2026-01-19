# ============================================================
# 06_enrolments_step_logistic.R
# Logistic regression: predict completion from engagement
# Requires in environment:
#   enrolments_clean
#   step_activity_clean_1, step_activity_clean_3, step_activity_clean_5, step_activity_clean_7
# Outputs: CSVs in reports/output + graphs in graphs/
# ============================================================

library(dplyr)
library(ggplot2)

# folders
if (!dir.exists("reports/output")) dir.create("reports/output", recursive = TRUE)
if (!dir.exists("graphs")) dir.create("graphs", recursive = TRUE)

# 1) learner-level engagement
engagement_summary <- bind_rows(
  step_activity_clean_1,
  step_activity_clean_3,
  step_activity_clean_5,
  step_activity_clean_7
) %>%
  group_by(run, learner_id) %>%
  summarise(
    steps_visited = sum(visited, na.rm = TRUE),
    steps_completed = sum(completed_step, na.rm = TRUE),
    .groups = "drop"
  )

# 2) merge with enrolments
df <- enrolments_clean %>%
  select(run, learner_id, completed) %>%
  left_join(engagement_summary, by = c("run", "learner_id")) %>%
  mutate(
    steps_visited = ifelse(is.na(steps_visited), 0, steps_visited),
    steps_completed = ifelse(is.na(steps_completed), 0, steps_completed),
    completed = as.integer(completed),
    run = factor(run)
  )

# 3) logistic regression models
m1 <- glm(completed ~ steps_completed + run, data = df, family = binomial())
m2 <- glm(completed ~ steps_completed + steps_visited + run, data = df, family = binomial())

# save model summaries
sink("reports/output/logit_model_summary.txt")
cat("=== Model A: completed ~ steps_completed + run ===\n")
print(summary(m1))
cat("\n\n=== Model B: completed ~ steps_completed + steps_visited + run ===\n")
print(summary(m2))
sink()

# 4) odds ratios (Model B)
coef_table <- summary(m2)$coefficients

odds_ratios <- data.frame(
  term = rownames(coef_table),
  estimate = coef_table[, "Estimate"],
  std_error = coef_table[, "Std. Error"],
  z_value = coef_table[, "z value"],
  p_value = coef_table[, "Pr(>|z|)"],
  odds_ratio = exp(coef_table[, "Estimate"])
)

write.csv(odds_ratios, "reports/output/logit_odds_ratios.csv", row.names = FALSE)

# 5) predicted probability curve vs steps_completed
# ==============================
# 5) Predicted probability curve vs steps_completed
# ==============================

steps_seq <- seq(0, max(df$steps_completed, na.rm = TRUE), by = 1)
baseline_run <- levels(df$run)[1]

pred_grid <- data.frame(
  steps_completed = steps_seq,
  steps_visited = median(df$steps_visited, na.rm = TRUE),
  run = factor(baseline_run, levels = levels(df$run))
)

pred_grid$pred_prob <- predict(m2, newdata = pred_grid, type = "response")

# Plot: predicted probability vs steps_completed
p1 <- ggplot(pred_grid, aes(x = steps_completed, y = pred_prob)) +
  geom_line(color = "#1f78b4", size = 1.5) +        # blue line
  geom_point(color = "#e31a1c", size = 2) +         # red points for emphasis
  labs(
    title = paste0("Predicted Completion Probability vs Steps Completed (Run = ", baseline_run, ")"),
    x = "Steps Completed",
    y = "Predicted Probability of Completion"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12)
  )

ggsave("graphs/logit_predicted_prob_by_steps_completed.png", p1, width = 8, height = 5)


# ==============================
# 6) ROC-like threshold table and plot
# ==============================

df$pred_prob <- predict(m2, type = "response")
thresholds <- seq(0, 1, by = 0.02)

roc_table <- lapply(thresholds, function(t) {
  pred_class <- ifelse(df$pred_prob >= t, 1, 0)
  
  tp <- sum(pred_class == 1 & df$completed == 1, na.rm = TRUE)
  fp <- sum(pred_class == 1 & df$completed == 0, na.rm = TRUE)
  tn <- sum(pred_class == 0 & df$completed == 0, na.rm = TRUE)
  fn <- sum(pred_class == 0 & df$completed == 1, na.rm = TRUE)
  
  tpr <- ifelse((tp + fn) == 0, NA, tp / (tp + fn))
  fpr <- ifelse((fp + tn) == 0, NA, fp / (fp + tn))
  precision <- ifelse((tp + fp) == 0, NA, tp / (tp + fp))
  accuracy <- (tp + tn) / (tp + tn + fp + fn)
  
  data.frame(
    threshold = t,
    tp = tp, fp = fp, tn = tn, fn = fn,
    tpr = tpr,
    fpr = fpr,
    precision = precision,
    accuracy = accuracy
  )
}) %>% bind_rows()

write.csv(roc_table, "reports/output/logit_roc_table.csv", row.names = FALSE)

# Plot: ROC-like curve
p2 <- ggplot(roc_table, aes(x = fpr, y = tpr)) +
  geom_line(color = "#33a02c", size = 1.5) +        # green line
  geom_abline(slope = 1, intercept = 0,             # diagonal reference
              linetype = "dashed", color = "gray") +
  labs(
    title = "ROC-like Curve (Logistic Model)",
    x = "False Positive Rate (FPR)",
    y = "True Positive Rate (TPR)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 12)
  )

ggsave("graphs/roc_like_curve.png", p2, width = 7, height = 5)
