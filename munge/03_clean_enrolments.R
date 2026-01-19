library(dplyr)
library(lubridate)

# trim spaces from column names
names(enrolments_all) <- trimws(names(enrolments_all))

# ---- Required columns ----
if (!("learner_id" %in% names(enrolments_all))) stop("learner_id not found in enrolments_all")
if (!("run" %in% names(enrolments_all))) stop("run not found in enrolments_all")

# ---- Parse dates safely (only if columns exist) ----
date_cols <- c("enrolled_at", "unenrolled_at", "fully_participated_at", "purchased_statement_at")
for (cname in date_cols) {
  if (cname %in% names(enrolments_all)) {
    enrolments_all[[cname]] <- ymd_hms(enrolments_all[[cname]], quiet = TRUE)
  }
}

# ---- Create completion variable from fully_participated_at ----
# completed = 1 if fully_participated_at exists and is not NA, else 0
if (!("fully_participated_at" %in% names(enrolments_all))) {
  stop("fully_participated_at not found. Cannot derive completion.")
}

enrolments_clean <- enrolments_all %>%
  mutate(
    learner_id = as.character(learner_id),
    run = as.character(run),
    completed = ifelse(!is.na(fully_participated_at), 1L, 0L)
  ) %>%
  select(run, learner_id, completed, everything())

print(dim(enrolments_clean))
print(head(enrolments_clean, 2))
table(enrolments_clean$completed, useNA = "ifany")
