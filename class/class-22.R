set.seed(123)

# Generate dummy dataset
size <- 50
treatment_group <- data.frame(
  treatment = sample(1:1, size, replace=TRUE),
  score = sample(50:100, size, replace=TRUE),
  accepted = sample(0:1, size, replace=TRUE)
)
control_group <- data.frame(
  treatment = sample(0:0, size, replace=TRUE),
  score = sample(20:70, size, replace=TRUE),
  accepted = sample(0:1, size, replace=TRUE)
)
treatment_group
control_group

# accepted
accepted_treatment <- nrow(treatment_group[treatment_group$accepted == 1,]) #27
accepted_control <- nrow(control_group[control_group$accepted == 1,]) #23

# ITT
itt <- mean(treatment_group$score) - mean(control_group$score)
itt #31.86

# Compliance rate
compriance_rate <- accepted_treatment / size - accepted_control / size
compriance_rate # 0.08

# LATE
late <- itt / compriance_rate
late #398.25