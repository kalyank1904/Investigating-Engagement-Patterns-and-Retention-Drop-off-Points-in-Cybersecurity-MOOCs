# ============================================================
# 05_enrolments_step_stats.R
# Stats analysis: Enrolments + Step Activity
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

# 1) learner-level engagement from step activity
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

# 2) merge with enrolments completion
engagement_with_completion <- enrolments_clean %>%
  select(run, learner_id, completed) %>%
  left_join(engagement_summary, by = c("run", "learner_id")) %>%
  mutate(
    steps_visited = ifelse(is.na(steps_visited), 0, steps_visited),
    steps_completed = ifelse(is.na(steps_completed), 0, steps_completed),
    completed = as.integer(completed)
  )

write.csv(
  engagement_with_completion,
  "reports/output/engagement_with_completion.csv",
  row.names = FALSE
)

# 3) descriptive stats by completion
engagement_stats <- engagement_with_completion %>%
  group_by(completed) %>%
  summarise(
    learners = n(),
    avg_steps_visited = mean(steps_visited),
    avg_steps_completed = mean(steps_completed),
    median_steps_visited = median(steps_visited),
    median_steps_completed = median(steps_completed),
    sd_steps_visited = sd(steps_visited),
    sd_steps_completed = sd(steps_completed),
    .groups = "drop"
  )

print(engagement_stats)

write.csv(
  engagement_stats,
  "reports/output/engagement_stats_by_completion.csv",
  row.names = FALSE
)

# 4) quantiles by completion
quantile_summary <- engagement_with_completion %>%
  group_by(completed) %>%
  summarise(
    learners = n(),
    q25_visited = quantile(steps_visited, 0.25),
    q50_visited = quantile(steps_visited, 0.50),
    q75_visited = quantile(steps_visited, 0.75),
    q90_visited = quantile(steps_visited, 0.90),
    q25_completed = quantile(steps_completed, 0.25),
    q50_completed = quantile(steps_completed, 0.50),
    q75_completed = quantile(steps_completed, 0.75),
    q90_completed = quantile(steps_completed, 0.90),
    .groups = "drop"
  )

print(quantile_summary)

write.csv(
  quantile_summary,
  "reports/output/engagement_quantiles_by_completion.csv",
  row.names = FALSE
)

# 5) statistical tests (non-parametric)
w_completed <- wilcox.test(steps_completed ~ completed, data = engagement_with_completion)
w_visited <- wilcox.test(steps_visited ~ completed, data = engagement_with_completion)

tests_out <- data.frame(
  test = c("Wilcoxon: steps_completed by completed", "Wilcoxon: steps_visited by completed"),
  statistic = c(unname(w_completed$statistic), unname(w_visited$statistic)),
  p_value = c(w_completed$p.value, w_visited$p.value)
)

print(tests_out)

write.csv(
  tests_out,
  "reports/output/engagement_stats_tests.csv",
  row.names = FALSE
)

# 6) graphs
# -----------------------------
# 6) Graphs with modern colors
# -----------------------------

# Boxplot: Steps Visited
# Boxplot: Steps Visited
p1 <- ggplot(engagement_with_completion, aes(x = factor(completed), y = steps_visited, fill = factor(completed))) +
  geom_boxplot(alpha = 0.7, color = "black") +  # alpha makes it slightly transparent, black outline
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e")) +  # Blue = 0, Orange = 1
  labs(
    title = "Steps Visited by Completion Status",
    x = "Completed (0 = No, 1 = Yes)",
    y = "Steps Visited",
    fill = "Completed"
  ) +
  theme_minimal()

ggsave("graphs/steps_visited_boxplot_by_completion.png", p1, width = 7, height = 5)


# Boxplot: Steps Completed
p2 <- ggplot(engagement_with_completion, aes(x = factor(completed), y = steps_completed, fill = factor(completed))) +
  geom_boxplot(alpha = 0.7, color = "black") +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e")) +
  labs(
    title = "Steps Completed by Completion Status",
    x = "Completed (0 = No, 1 = Yes)",
    y = "Steps Completed",
    fill = "Completed"
  ) +
  theme_minimal()

ggsave("graphs/steps_completed_boxplot_by_completion.png", p2, width = 7, height = 5)

# Density: Steps Visited
p3 <- ggplot(engagement_with_completion, aes(x = steps_visited, fill = factor(completed))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e")) +
  labs(
    title = "Density: Steps Visited (Completed vs Not Completed)",
    x = "Steps Visited",
    y = "Density",
    fill = "Completed"
  ) +
  theme_minimal()
ggsave("graphs/steps_visited_density_by_completion.png", p3, width = 8, height = 5)

# Density: Steps Completed
p4 <- ggplot(engagement_with_completion, aes(x = steps_completed, fill = factor(completed))) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(values = c("0" = "#1f77b4", "1" = "#ff7f0e")) +
  labs(
    title = "Density: Steps Completed (Completed vs Not Completed)",
    x = "Steps Completed",
    y = "Density",
    fill = "Completed"
  ) +
  theme_minimal()
ggsave("graphs/steps_completed_density_by_completion.png", p4, width = 8, height = 5)
