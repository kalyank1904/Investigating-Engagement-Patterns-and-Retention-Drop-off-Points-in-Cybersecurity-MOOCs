library(dplyr)
library(ggplot2)

# helper: make sure key columns are correct types
fix_leaving_types <- function(df) {
  df %>%
    mutate(
      learner_id = as.character(learner_id),
      leaving_reason = as.character(leaving_reason),
      left_at = as.character(left_at),
      last_completed_step_at = as.character(last_completed_step_at),
      id = as.character(id)
    )
}

leaving_clean_1 <- fix_leaving_types(leaving_clean_1)
leaving_clean_3 <- fix_leaving_types(leaving_clean_3)
leaving_clean_5 <- fix_leaving_types(leaving_clean_5)
leaving_clean_7 <- fix_leaving_types(leaving_clean_7)

# ---- Now your original analysis will work ----

if (!dir.exists("reports/output")) dir.create("reports/output", recursive = TRUE)

leaving_summary <- bind_rows(
  leaving_clean_1 %>% mutate(run = "1"),
  leaving_clean_3 %>% mutate(run = "3"),
  leaving_clean_5 %>% mutate(run = "5"),
  leaving_clean_7 %>% mutate(run = "7")
) %>%
  group_by(run) %>%
  summarise(
    responses = n(),
    unique_learners = n_distinct(learner_id),
    .groups = "drop"
  ) %>%
  right_join(tibble(run = c("1","3","5","7")), by = "run") %>%
  mutate(
    responses = tidyr::replace_na(responses, 0L),
    unique_learners = tidyr::replace_na(unique_learners, 0L)
  ) %>%
  arrange(run)

print(leaving_summary)

write.csv(leaving_summary, "reports/output/leaving_summary_by_run.csv", row.names = FALSE)

leaving_reasons <- bind_rows(
  leaving_clean_5 %>% mutate(run = "5"),
  leaving_clean_7 %>% mutate(run = "7")
) %>%
  filter(!is.na(leaving_reason)) %>%
  count(run, leaving_reason, sort = TRUE)

write.csv(leaving_reasons, "reports/output/leaving_reasons_by_run.csv", row.names = FALSE)

top_reasons <- leaving_reasons %>%
  group_by(run) %>%
  slice_head(n = 8) %>%
  ungroup()

p1 <- ggplot(top_reasons, aes(x = reorder(leaving_reason, n), y = n)) +
  geom_col(fill = "#4E79A7") +
  coord_flip() +
  facet_wrap(~run, scales = "free_y") +
  labs(
    title = "Top Leaving Reasons (Runs 5 and 7)",
    x = "Leaving Reason",
    y = "Count"
  )

library(ggplot2)





ggsave("graphs/leaving_top_reasons.png", plot = p1, width = 10, height = 6)
