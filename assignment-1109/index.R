# Setup
#install.packages("coin")
#install.packages("tidyverse")
install.packages("readxl")
library(readxl)
data <- read_excel("/Users/seito/Documents/develop/cs312/assignment-1109/DataSet para RUO.xlsx")
head(data)

# Made groups
control_group <- data[1:100, ]
treatment_group <- data[101:235, ]

## U-test
print_data <- function(exercise_number, mode = NULL) {
  
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
print_data("race")
print_data(1)
print_data(2)
print_data(3)
print_data(4)
print_data("race", "Increase")
print_data(1, "Increase")
print_data(2, "Increase")
print_data(3, "Increase")
print_data(4, "Increase")