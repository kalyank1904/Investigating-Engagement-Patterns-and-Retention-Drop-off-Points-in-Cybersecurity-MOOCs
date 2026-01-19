library(dplyr)
library(ggplot2)

# ----------------------------------------------------
# Ensure output folders exist
# ----------------------------------------------------
if (!dir.exists("reports/output")) dir.create("reports/output", recursive = TRUE)
if (!dir.exists("graphs")) dir.create("graphs")

# ----------------------------------------------------
# STEP 1: Combine step activity (learner-level metrics)
# ----------------------------------------------------
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

# ----------------------------------------------------
# STEP 2: Merge with enrolments
# ----------------------------------------------------
engagement_with_completion <- enrolments_clean %>%
  select(run, learner_id, completed) %>%
  left_join(engagement_summary, by = c("run", "learner_id")) %>%
  mutate(
    steps_visited = ifelse(is.na(steps_visited), 0, steps_visited),
    steps_completed = ifelse(is.na(steps_completed), 0, steps_completed)
  )

write.csv(
  engagement_with_completion,
  "reports/output/engagement_with_completion.csv",
  row.names = FALSE
)

# ----------------------------------------------------
# STEP 3: Summary stats by completion status
# ----------------------------------------------------
engagement_stats <- engagement_with_completion %>%
  group_by(completed) %>%
  summarise(
    learners = n(),
    avg_steps_visited = mean(steps_visited),
    avg_steps_completed = mean(steps_completed),
    median_steps_visited = median(steps_visited),
    median_steps_completed = median(steps_completed),
    .groups = "drop"
  )

print(engagement_stats)

write.csv(
  engagement_stats,
  "reports/output/engagement_stats_by_completion.csv",
  row.names = FALSE
)

# ----------------------------------------------------
# STEP 4: Visualizations
# ----------------------------------------------------

# Avg steps visited: completers vs non-completers
p1 <- ggplot(engagement_with_completion,
             aes(x = factor(completed), y = steps_visited)) +
  geom_boxplot() +
  labs(
    title = "Steps Visited by Completion Status",
    x = "Completed Course (0 = No, 1 = Yes)",
    y = "Steps Visited"
  )

ggsave("graphs/steps_visited_by_completion.png", p1, width = 7, height = 5)

# Avg steps completed: completers vs non-completers
p2 <- ggplot(engagement_with_completion,
             aes(x = factor(completed), y = steps_completed)) +
  geom_boxplot() +
  labs(
    title = "Steps Completed by Completion Status",
    x = "Completed Course (0 = No, 1 = Yes)",
    y = "Steps Completed"
  )

ggsave("graphs/steps_completed_by_completion.png", p2, width = 7, height = 5)

# ----------------------------------------------------
# STEP 5: Engagement threshold insight
# ----------------------------------------------------
engagement_threshold <- engagement_with_completion %>%
  mutate(engagement_level = case_when(
    steps_completed == 0 ~ "No engagement",
    steps_completed <= 10 ~ "Low engagement",
    steps_completed <= 30 ~ "Medium engagement",
    TRUE ~ "High engagement"
  )) %>%
  group_by(engagement_level) %>%
  summarise(
    learners = n(),
    completion_rate = mean(completed),
    .groups = "drop"
  )

print(engagement_threshold)

write.csv(
  engagement_threshold,
  "reports/output/completion_by_engagement_level.csv",
  row.names = FALSE
)

p3 <- ggplot(engagement_threshold,
             aes(x = engagement_level, y = completion_rate)) +
  geom_col() +
  labs(
    title = "Completion Rate by Engagement Level",
    x = "Engagement Level",
    y = "Completion Rate"
  )

library(ggplot2)

# Example: Bar plot
ggplot(data, aes(x=Category, y=Value, fill=Category)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c("Category1"="blue", "Category2"="red", "Category3"="green"))

# Example: Line plot
ggplot(data, aes(x=Time, y=Value, color=Group)) +
  geom_line(size=1.2) +  # size of line
  scale_color_manual(values=c("Group1"="orange", "Group2"="purple"))


ggsave("graphs/completion_rate_by_engagement_level.png", p3, width = 7, height = 5)
