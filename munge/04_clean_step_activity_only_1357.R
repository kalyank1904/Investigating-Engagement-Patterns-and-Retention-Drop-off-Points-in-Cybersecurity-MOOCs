library(dplyr)
library(lubridate)

runs_keep <- c("1","3","5","7")

# Get only the step-activity datasets for runs 1,3,5,7
step_list <- mget(ls(pattern = "^cyber\\.security\\.(1|3|5|7)_step\\.activity$"))

# Clean each run separately and store in a list
step_activity_clean_list <- lapply(names(step_list), function(nm) {
  
  df <- step_list[[nm]]
  
  # extract run number from object name
  run_id <- sub("^cyber\\.security\\.([0-9]+)_step\\.activity$", "\\1", nm)
  
  df %>%
    mutate(
      run = run_id,
      first_visited_at  = ymd_hms(first_visited_at, tz = "UTC", quiet = TRUE),
      last_completed_at = ymd_hms(last_completed_at, tz = "UTC", quiet = TRUE),
      visited = !is.na(first_visited_at),
      completed_step = !is.na(last_completed_at)
    ) %>%
    # IMPORTANT: the column is `step` (not step_id)
    select(run, learner_id, step, week_number, step_number, visited, completed_step)
})

# Give the list readable names: "1","3","5","7"
names(step_activity_clean_list) <- sub("^cyber\\.security\\.([0-9]+)_step\\.activity$", "\\1", names(step_list))

# Optional: create individual objects too (easy to view)
step_activity_clean_1 <- step_activity_clean_list[["1"]]
step_activity_clean_3 <- step_activity_clean_list[["3"]]
step_activity_clean_5 <- step_activity_clean_list[["5"]]
step_activity_clean_7 <- step_activity_clean_list[["7"]]

library(dplyr)
library(lubridate)

runs_keep <- c("1","3","5","7")

step_list <- mget(ls(pattern = "^cyber\\.security\\.[0-9]+_step\\.activity$"))

# Keep only 1,3,5,7
step_list <- step_list[names(step_list) %in% paste0("cyber.security.", runs_keep, "_step.activity")]

# Stop early if empty
stopifnot(length(step_list) > 0)

