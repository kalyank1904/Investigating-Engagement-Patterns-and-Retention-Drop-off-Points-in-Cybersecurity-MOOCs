library(dplyr)
library(ggplot2)
library(tidyr)

# Create output folder
if (!dir.exists("reports/output")) dir.create("reports/output", recursive = TRUE)

# 1) Completion rate by run
completion_by_run <- enrolments_clean %>%
  group_by(run) %>%
  summarise(
    learners = n_distinct(learner_id),
    completed_learners = sum(completed == 1, na.rm = TRUE),
    completion_rate = completed_learners / learners
  ) %>%
  arrange(run)

print(completion_by_run)
write.csv(completion_by_run, "reports/output/enrolments_completion_by_run.csv", row.names = FALSE)

# Plot completion rate
p1 <- ggplot(completion_by_run, aes(x = run, y = completion_rate)) +
  geom_col() +
  labs(
    title = "Completion Rate by Run",
    x = "Run",
    y = "Completion Rate"
  )

ggsave("graphs/enrolments_completion_rate_by_run.png", p1, width = 7, height = 4)

# 2) Demographics summary (missingness)
demographics_missing <- enrolments_clean %>%
  summarise(
    gender_missing = mean(is.na(gender)),
    age_range_missing = mean(is.na(age_range)),
    country_missing = mean(is.na(country)),
    education_missing = mean(is.na(highest_education_level)),
    employment_missing = mean(is.na(employment_status))
  )

print(demographics_missing)
write.csv(demographics_missing, "reports/output/enrolments_demographics_missingness.csv", row.names = FALSE)

# 3) Country top 10 (overall)
top_countries <- enrolments_clean %>%
  filter(!is.na(country)) %>%
  count(country, sort = TRUE) %>%
  slice_head(n = 10)

print(top_countries)
write.csv(top_countries, "reports/output/enrolments_top10_countries.csv", row.names = FALSE)

p2 <- ggplot(top_countries, aes(x = reorder(country, n), y = n)) +
  geom_col(color = "#E15759", linewidth = 1.2) +
  coord_flip() +
  labs(
    title = "Top 10 Countries (All Runs)",
    x = "Country",
    y = "Learners"
  )
library(ggplot2)

# -----------------------------
# 1) Completion rate by run
# -----------------------------
p1 <- ggplot(completion_by_run, aes(x = run, y = completion_rate, fill = run)) +
  geom_col(color = "#4E79A7", linewidth = 1.2) +
  scale_fill_manual(values = c(
    "Run1" = "#1f77b4", "Run2" = "#ff7f0e", "Run3" = "#2ca02c", "Run4" = "#d62728"
  )) +  # customize as per your run names
  labs(
    title = "Completion Rate by Run",
    x = "Run",
    y = "Completion Rate"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # remove legend if unnecessary

ggsave("graphs/enrolments_completion_rate_by_run.png", p1, width = 7, height = 4)


# -----------------------------
# 2) Top 10 Countries (Overall)
# -----------------------------
p2 <- ggplot(top_countries, aes(x = reorder(country, n), y = n, fill = country)) +
  geom_col(color = "#4E79A7", linewidth = 1.2) +
  coord_flip() +
  scale_fill_manual(values = c(
    # Example colors for top 10 countries, adjust as needed
    "USA" = "#1f77b4",
    "India" = "#ff7f0e",
    "UK" = "#2ca02c",
    "Canada" = "#d62728",
    "Australia" = "#9467bd",
    "Germany" = "#8c564b",
    "France" = "#e377c2",
    "Brazil" = "#7f7f7f",
    "China" = "#bcbd22",
    "Japan" = "#17becf"
  )) +
  labs(
    title = "Top 10 Countries (All Runs)",
    x = "Country",
    y = "Learners"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # optional

ggsave("graphs/enrolments_top10_countries.png", p2, width = 7, height = 5)
