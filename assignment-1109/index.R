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

# Matcing
##install.packages("MatchIt")
##library(MatchIt)

##control_group$treatment <- 0
##treatment_group$treatment <- 1

##combined_data <- rbind(control_group, treatment_group)
##arramged_dataset <- na.omit(combined_data)

##ps_model <- matchit(
##  arramged_dataset$treatment ~ 
##    ##arramged_dataset$`Race Effectiveness` + 
    #arramged_dataset$`Exercise1 Effectiveness` +
    #arramged_dataset$`Exercise2 Effectiveness` +
    #arramged_dataset$`Exercise3 Effectiveness` + 
    #arramged_dataset$`Exercise4 Effectiveness` + 
    #arramged_dataset$`Race Effectiveness Increase` + 
    #arramged_dataset$`Exercise1 Effectiveness Increase` +
    #arramged_dataset$`Exercise 2 Effectiveness Increase` +
    #arramged_dataset$`Exercise3 Effectiveness Increase` + 
    #arramged_dataset$`Exercise4 Effectiveness Increase` + 
    #arramged_dataset$`Race Active Time`,
    #arramged_dataset$`Exercise1 Active Time` +
    #arramged_dataset$`Exercise2 Active Time` +
    #arramged_dataset$`Exercise3 Active Time` +
    #arramged_dataset$`Exercise4 Active Time`,
##  data = arramged_dataset, method = "nearest")

##summary(ps_model)
##matched_data <- match.data(ps_model)
##matched_data
