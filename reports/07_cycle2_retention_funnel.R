# ============================================================
# 07_cycle2_retention_funnel.R
# Cycle-2: Retention funnel (drop-off) by run
# Requires in environment:
#   enrolments_clean
#   step_activity_clean_1, step_activity_clean_3, step_activity_clean_5, step_activity_clean_7
# Outputs:
#   reports/output/retention_funnel_by_run.csv
#   graphs/retention_funnel_by_run.png
# ============================================================

library(dplyr)
library(ggplot2)

# 1️⃣ Define custom colors for each run
# Change hex codes to any colors you like
run_colors <- c(
  "1" = "#1f77b4",  # blue
  "3" = "#ff7f0e",  # orange
  "5" = "#2ca02c",  # green
  "7" = "#d62728"   # red
)

# 2️⃣ Create the plot with custom colors
p <- ggplot(funnel_long, aes(x = stage, y = rate, color = as.factor(run), group = run)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 3.5) +
  scale_color_manual(values = run_colors) +  # assign custom colors
  coord_flip() +
  labs(
    title = "Retention Funnel by Run",
    x = "Funnel Stage",
    y = "Rate (share of enrolled learners)",
    color = "Run"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

# 3️⃣ Save the plot
ggsave("graphs/retention_funnel_by_run.png", p, width = 10, height = 6)

# 4️⃣ Display the plot
print(p)
