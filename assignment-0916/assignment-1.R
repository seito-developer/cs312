# (1)Install the medicaldata package and ...
library(medicaldata)
data(covid_testing)


# (2) The number of observations (rows) in the data set
num_observations <- nrow(covid_testing)
#num_observations

# (3) The number and names of the columns
num_columns <- ncol(covid_testing)
column_names <- names(covid_testing)

#cat("Number of columns:", num_columns, "\n")
#cat("Column names:", paste(column_names, collapse = ", "), "\n")

# (4) The types of genders, and ...
gender_counts <- table(covid_testing$gender)
#gender_counts

# (5) The number of females younger than 12 years old in the dataset
females_under_12 <- subset(covid_testing, gender == "female" & age < 12)
num_females_under_12 <- nrow(females_under_12)
#num_females_under_12

# (6) Use the “which” command to identify which rows ...
which(
  covid_testing$age <= 11 &
    covid_testing$patient_class == "outpatient" & 
    covid_testing$patient_class == "outpatient" &
    covid_testing$result == "positive"
  )

#(7) Use the quantile function to identify the median, ...
man <- subset(covid_testing, gender == "male")
cat("median: ", quantile(man$age, 0.5),
    "\n10% quantile: ", quantile(man$age, 0.1),
    "\n90% quantile: ", quantile(man$age, 0.9))

#(8) Write a short paragraph (probably 5-10 sentences) ...

# single linear regression
model <- lm(covid_testing$pan_day ~ covid_testing$age, data = covid_testing)
summary(model) #R-squared:  0.09, p-val: <2e-16

## rmse
predictions <- predict(model, covid_testing)
mse <- mean((covid_testing$pan_day - predictions)^2)
rmse <- sqrt(mse)
print(rmse) #25.95354

# Improve model
new_covid_testing <- covid_testing[covid_testing$age < 20, ]
new_covid_testing$age

new_model <- lm(new_covid_testing$pan_day ~ new_covid_testing$age, data = new_covid_testing)
summary(new_model) #R-squared:  0.1967, p-val: <2e-16

## rmse
new_predictions <- predict(new_model, new_covid_testing)
new_mse <- mean((new_covid_testing$pan_day - new_predictions)^2)
new_rmse <- sqrt(new_mse)
print(new_rmse) #24.45253
