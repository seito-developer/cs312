# Part 2(b)
install.packages("readr")
install.packages("Matching")
install.packages("rgenoud")
library(readr)
library(Matching)
library(rgenoud)

data=read_csv("/Users/seito/Documents/develop/cs312/assignment-1125/greenbuildings.csv")

# Convert the data to the binary format
data$class_binary <- ifelse(data$class == "A", 0, 
                            ifelse(data$class == "B", 1, 2))
X <- cbind(data$size, data$stories, data$age, data$class_binary, data$renovated)

# Set caliper
caliper_value <- 0.2
caliper <- rep(NA, ncol(X))
names(caliper) <- names(X)
caliper["size"] <- caliper_value
caliper["stories"] <- caliper_value
caliper["age"] <- caliper_value

# Try the genetic matching
genetic_match <- GenMatch(Tr = data$green_rating, X = X, M = 1, caliper = caliper, pop.size = 200, max.generations = 100, wait.generations = 10)
matched_data <- Match(Y = NULL, Tr = data$green_rating, X = X, Weight.matrix = genetic_match)

matched_treated <- data[matched_data$index.treated, ]
matched_control <- data[matched_data$index.control, ]
matched_dataset <- rbind(matched_treated, matched_control)

average_outcome_treated <- mean(matched_dataset$leasing_rate[matched_dataset$green_rating == 1])
average_outcome_control <- mean(matched_dataset$leasing_rate[matched_dataset$green_rating == 0])

# Try T-test
treatment_effect <- average_outcome_treated - average_outcome_control
t_test_result <- t.test(leasing_rate ~ green_rating, data = matched_dataset)

# Dropped units by matching
dropped_units <- nrow(data) - nrow(matched_dataset)

# Result
treatment_effect
t_test_result
dropped_units


# Part 2(c)
x_labels <- c(0, 0.25, 0.5, 0.75, 1.0)
x_pos <- 1:length(x_labels)  # X軸の位置

quantiles_result <- quantile(matched_dataset$leasing_rate)
sorted_result <- sort(quantiles_result)
plot(x_pos, sorted_result,
     xlab = "Quantiles", ylab = "Leasing Rate", 
     main = "Leasing Rate Quantiles",
     type = "b", pch = 19, xaxt = "n" 
     )
axis(1, at = x_pos, labels = x_labels)
