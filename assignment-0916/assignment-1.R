# (1)Install the medicaldata package and ...
library(medicaldata)
data(covid_testing)
write.csv(covid_testing, "covid_testing.csv", row.names = FALSE)


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

## single lenear regression
model <- lm(covid_testing$pan_day ~ covid_testing$age, data = covid_testing)
summary(model)

lm(result ~ covid_testing$age, data=covid_testing)


median_panday <- median(covid_testing$pan_day)
pan_day <- ifelse(covid_testing$pan_day <= median_panday, 0, 1)
arranged_data <- data.frame(covid_testing$age, pan_day)
arranged_data


model <- glm(arranged_data$pan_day ~ arranged_data$age, data=arranged_data, family=binomial)





age <- ifelse(covid_testing$age <= 18, 0, 1)
result <- ifelse(covid_testing$result == "negative", 0, 1)

arranged_data <- data.frame(age, result)
arranged_data
model <- glm(result ~ age, data=arranged_data, family=binomial)
summary(model)
predicted_probabilities <- predict(model, newdata=newdata, type="response")
