library(dplyr)
library(tibble)

enrol_obj_names <- ls(pattern = "^cyber\\.security\\.[0-9]+_enrolments$")
enrol_obj_names <- sort(enrol_obj_names)

enrol_list <- lapply(enrol_obj_names, function(nm) {
  df <- get(nm) %>% as_tibble()
  run <- as.integer(sub("^cyber\\.security\\.([0-9]+)_enrolments$", "\\1", nm))
  df %>% mutate(run = run)
})

enrolments_all <- bind_rows(enrol_list)

runs_keep <- c(1, 3, 5, 7)
enrolments_all <- enrolments_all %>% filter(run %in% runs_keep)

print(dim(enrolments_all))
print(names(enrolments_all))

