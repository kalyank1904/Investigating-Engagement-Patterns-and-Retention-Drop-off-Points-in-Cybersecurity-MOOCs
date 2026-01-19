library(dplyr)
library(ggplot2)

if (!dir.exists("reports/output")) dir.create("reports/output", recursive = TRUE)

# Combine only summaries (not raw) across runs
step_summary_by_run <- bind_rows(
  step_activity_clean_1 %>% mutate(run = "1"),
  step_activity_clean_3 %>% mutate(run = "3"),
  step_activity_clean_5 %>% mutate(run = "5"),
  step_activity_clean_7 %>% mutate(run = "7")
) %>%
  group_by(run) %>%
  summarise(
    unique_learners = n_distinct(learner_id),
    total_visits = sum(visited, na.rm = TRUE),
    total_completions = sum(completed_step, na.rm = TRUE),
    avg_steps_visited_per_learner = total_visits / unique_learners,
    avg_steps_completed_per_learner = total_completions / unique_learners
  ) %>%
  arrange(run)

print(step_summary_by_run)
write.csv(step_summary_by_run, "reports/output/step_summary_by_run.csv", row.names = FALSE)

# Plot avg visited/completed per learner
p1 <- ggplot(step_summary_by_run, aes(x = run, y = avg_steps_visited_per_learner)) +
  geom_col(fill = "#59A14F") +
  labs(
    title = "Average Steps Visited per Learner (by Run)",
    x = "Run",
    y = "Avg Steps Visited"
  )

ggsave("graphs/step_avg_steps_visited.png", p1, width = 7, height = 4)

p2 <- ggplot(step_summary_by_run, aes(x = run, y = avg_steps_completed_per_learner)) +
  geom_col(fill = "#F28E2B") +
  theme_minimal(base_size = 13) +
  labs(
    title = "Average Steps Completed per Learner (by Run)",
    x = "Run",
    y = "Avg Steps Completed"
  )


ggsave("graphs/step_avg_steps_completed.png", p2, width = 7, height = 4)

# Engagement per learner (already created in your environment sometimes)
# If not available, recreate from ALL runs quickly:
engagement_by_learner <- bind_rows(
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

write.csv(engagement_by_learner, "reports/output/engagement_by_learner.csv", row.names = FALSE)

# Distribution plot: steps visited
p3 <- ggplot(engagement_by_learner, aes(x = steps_visited)) +
  geom_histogram(bins = 40) +
  facet_wrap(~run, scales = "free_y") +
  labs(
    title = "Distribution of Steps Visited per Learner",
    x = "Steps Visited",
    y = "Learner Count"
  )
p2 <- ggplot(top_countries, aes(x = reorder(country, n), y = n, fill = country)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 10 Countries (All Runs)",
    x = "Country",
    y = "Learners"
  ) +
  scale_fill_manual(values = c(
    "USA" = "#4E79A7",
    "India" = "#F28E2B",
    "UK" = "#59A14F"
  ))


ggsave("graphs/engagement_steps_visited_distribution.png", p3, width = 9, height = 5)
