library(dplyr)

pick_col <- function(df, candidates) {
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}

make_step_clean <- function(df, run_id) {
  
  # detect columns safely
  col_visited <- pick_col(df, c(
    "visited", "step_visited", "is_visited", "viewed"
  ))
  
  col_completed <- pick_col(df, c(
    "completed_step", "step_completed", "completed"
  ))
  
  df %>%
    mutate(
      run = as.character(run_id),
      learner_id = as.character(learner_id),
      
      visited = if (!is.na(col_visited))
        as.integer(.data[[col_visited]] %in% c(1, "1", TRUE, "TRUE", "true"))
      else 0,
      
      completed_step = if (!is.na(col_completed))
        as.integer(.data[[col_completed]] %in% c(1, "1", TRUE, "TRUE", "true"))
      else 0
    )
}
