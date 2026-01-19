library(dplyr)

# Get all step activity datasets loaded by ProjectTemplate
step_list <- mget(ls(pattern = "^cyber\\.security\\.[0-9]+_step\\.activity$"))

# Combine all runs into one data frame
step_activity_all <- bind_rows(lapply(names(step_list), function(nm) {
  df <- step_list[[nm]]
  df$run <- sub("^cyber\\.security\\.([0-9]+)_step\\.activity$", "\\1", nm)
  df
}))

# sanity check
count(step_activity_all, run)
glimpse(step_activity_all)
