library(dplyr)

# Runs to keep
runs_keep <- c("1", "3", "5", "7")

# Get leaving survey datasets for runs 1,3,5,7
leaving_list <- mget(ls(pattern = "^cyber\\.security\\.(1|3|5|7)_leaving\\.survey\\.responses$"))

# Clean each dataset separately
leaving_clean_list <- lapply(names(leaving_list), function(nm) {
  
  df <- leaving_list[[nm]]
  
  # Extract run number
  run_id <- sub("^cyber\\.security\\.([0-9]+)_leaving\\.survey\\.responses$", "\\1", nm)
  
  df %>%
    mutate(
      run = run_id
    ) %>%
    mutate(across(where(is.character), ~na_if(.x, "")))
})

# Name list clearly: "1", "3", "5", "7"
names(leaving_clean_list) <- sub(
  "^cyber\\.security\\.([0-9]+)_leaving\\.survey\\.responses$", 
  "\\1", 
  names(leaving_list)
)

# Create individual cleaned datasets
leaving_clean_1 <- leaving_clean_list[["1"]]
leaving_clean_3 <- leaving_clean_list[["3"]]
leaving_clean_5 <- leaving_clean_list[["5"]]
leaving_clean_7 <- leaving_clean_list[["7"]]

# Sanity check
sapply(leaving_clean_list, nrow)

library(dplyr)

# helper: ensure consistent column types
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

# Apply to all runs safely
leaving_clean_list <- lapply(leaving_clean_list, fix_leaving_types)

# Recreate individual datasets (overwrite safely)
leaving_clean_1 <- leaving_clean_list[["1"]]
leaving_clean_3 <- leaving_clean_list[["3"]]
leaving_clean_5 <- leaving_clean_list[["5"]]
leaving_clean_7 <- leaving_clean_list[["7"]]

