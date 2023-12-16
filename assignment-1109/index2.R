# Setup
install.packages("dplyr")
install.packages("readxl")
install.packages("openxlsx")
install.packages("MatchIt")
install.packages("cobalt")
install.packages("ggplot2")
library(dplyr)
library(readxl)
library(openxlsx)
library(MatchIt)
library(cobalt)
data("lalonde", package = "cobalt")
library(ggplot2)

# load data
data <- read_excel("/Users/seito/Documents/develop/cs312/assignment-1109/DataSet para RUO.xlsx")
head(data)

# clean up data
data <- data %>% mutate(treatment = ifelse(row_number() <= 100, 1, 0))
data_clean <- na.omit(data)

# matching
m.out_rf <- matchit(treatment ~ `Race Effectiveness` + `Exercise1 Effectiveness` + `Exercise2 Effectiveness` + `Exercise3 Effectiveness` + `Exercise4 Effectiveness`, 
                 #`Race Effectiveness Increase` + `Exercise1 Effectiveness Increase`	+ `Exercise 2 Effectiveness Increase` + `Exercise3 Effectiveness Increase` + `Exercise4 Effectiveness Increase`,
                 data = data_clean, method = "nearest",
                 caliper = 0.25, ratio = 1, replace = FALSE)
m.out_rfi <- matchit(treatment ~ `Race Effectiveness Increase` + `Exercise1 Effectiveness Increase`	+ `Exercise 2 Effectiveness Increase` + `Exercise3 Effectiveness Increase` + `Exercise4 Effectiveness Increase`,
                 data = data_clean, method = "nearest",
                 caliper = 0.25, ratio = 1, replace = FALSE)
summary(m.out_rf)
summary(m.out_rfi)

# plot matching
love.plot(m.out_rf, stats = "m", thresholds = 0.1, abs = TRUE, 
          var.order = "unadjusted", line = FALSE, stars = "none",
          limits = list(m = c(0,1)),
          colors = c("#191970", "#db7093"),
          shapes = c("triangle", "circle"),
          alpha = .8, grid = TRUE, title = "") + labs(x = "")

love.plot(m.out_rfi, stats = "m", thresholds = 0.1, abs = TRUE, 
          var.order = "unadjusted", line = FALSE, stars = "none",
          limits = list(m = c(0,1)),
          colors = c("#191970", "#db7093"),
          shapes = c("triangle", "circle"),
          alpha = .8, grid = TRUE, title = "") + labs(x = "")

# Analyze by matched data
data_match_rf <- match.data(m.out_rf)
data_match_rfi <- match.data(m.out_rfi)
nrow(data_match_rf) #160
nrow(data_match_rfi) #158

rf_control_group <- data_match_rf[data_match_rf$treatment == 0,]
rf_treatment_group <- data_match_rf[data_match_rf$treatment == 1,]
rfi_control_group <- data_match_rfi[data_match_rfi$treatment == 0,]
rfi_treatment_group <- data_match_rfi[data_match_rfi$treatment == 1,]

## U-test
print_data <- function(exercise_number, control_group, treatment_group, mode = NULL) {
  
  # To get the effect size (r)
  calculate_effect_size_r <- function(U, n1, n2) {
    r <- U / (n1 * n2)
    return(r)
  }
  
  effectiveness_col_name <- if (is.null(mode)) {
    if(exercise_number == "race"){
      paste("Race Effectiveness", sep="")
    } else {
      paste("Exercise", exercise_number, " Effectiveness", sep="")      
    }
  } else {
    if(exercise_number == 2){
      paste("Exercise ", exercise_number, " Effectiveness ", mode, sep="")
    } else if(exercise_number == "race"){
      paste("Race Effectiveness Increase", sep="")      
    } else {
      paste("Exercise", exercise_number, " Effectiveness ", mode, sep="")
    }
  }
  
  control_data <- as.numeric(control_group[[effectiveness_col_name]])
  treatment_data <- as.numeric(treatment_group[[effectiveness_col_name]])
  
  # Perform the Wilcoxon test
  test_result <- wilcox.test(control_data, treatment_data)
  
  title <- if (exercise_number == "race") "Race" else paste('Exercise', exercise_number)
  
  # Output the results
  cat(
    title,
    '\nControl mean:', mean(control_data, na.rm = TRUE),
    '\nControl median: ', median(control_data, na.rm = TRUE),
    '\nExperimental mean:', mean(treatment_data, na.rm = TRUE),
    '\nExperimental median: ', median(treatment_data, na.rm = TRUE),
    '\np-value: ', test_result$p.value,
    '\nU: ', test_result$statistic,
    '\nr: ', calculate_effect_size_r(test_result$statistic, 100, 135),
    '\n'
  )
}
print_data("race", rf_control_group, rf_treatment_group)
print_data(1, rf_control_group, rf_treatment_group)
print_data(2, rf_control_group, rf_treatment_group)
print_data(3, rf_control_group, rf_treatment_group)
print_data(4, rf_control_group, rf_treatment_group)
print_data("race", rfi_control_group, rfi_treatment_group, "Increase")
print_data(1, rfi_control_group, rfi_treatment_group, "Increase")
print_data(2, rfi_control_group, rfi_treatment_group, "Increase")
print_data(3, rfi_control_group, rfi_treatment_group, "Increase")
print_data(4, rfi_control_group, rfi_treatment_group, "Increase")
