#PART I: logistic regression for prediction
## 1) When you download the data, read the “readme” page to understand it better.
##.  Next, we are going to reorganize the data a little, to make it easier to work with.

library(readxl)
data <- read_excel("/Users/seito/Documents/develop/cs312/assignment-1014/Data\ PRESAJE\ Dryad\ eng.xlsx")
head(data)

#Regression Assignment

## 2) Delete any rows in which the treatment “Med” was administered, leaving only “SW” (social work) and “control”. Here, treatment is denoted by the column titled “Cohort”.
##.  How many rows do you have in your data set now, after these deletions?

filtered_data <- data[ifelse(data$Cohort != "Med", TRUE, FALSE), ]
nrow(filtered_data) #1453 -> 949

## 3) Rename “SW” as 1 in your data set, and rename “control” as 0. 
filtered_data$Cohort <- ifelse(filtered_data$Cohort == "SW", 1, 0)

## 4) In the “Level.of.Education” variable, rename “postsecondary” as 1 and all other values as 0. 
### Replace " " to "_"
colnames(filtered_data)[colnames(filtered_data) == "Level of Education"] <- "Level_of_Education"
filtered_data$Level_of_Education <- ifelse(filtered_data$Level_of_Education == "postsecondary", 1, 0)

## 5) Rename “follow up” in the Follow.up variable as a 1, and “lost to follow up” as a 0.
### Replace " " to "_"
colnames(filtered_data)[colnames(filtered_data) == "Follow up"] <- "Follow_up"
filtered_data$Follow_up <- ifelse(filtered_data$Follow_up == "follow up", 1, 0)

## 6) Rename “F” as a 1 and “M” as a 0 in the “Sex” variable.
filtered_data$Sex <- ifelse(filtered_data$Sex == "F", 1, 0)

## 7) Fit a logistic regression model to ...
model1 <- glm(Follow_up ~ Cohort + Sex + Level_of_Education,
              family = binomial, 
              data = filtered_data)
summary(model1)

## 8) Because the cohorts were randomly assigned, ...
install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

### Create a new data frame to calculate the predicted probabilities for all combinations
new_data <- expand.grid(
  Cohort = c(0, 1),
  Sex = c(0, 1),
  Level_of_Education = c(0, 1)
)

### Calculate the probability with the model
new_data$predicted_probability <- predict(model1, newdata = new_data, type = "response")

### Calculate the treatment effects
grouped_data <- group_by(new_data, Sex, Level_of_Education)
summarised_data <- summarise(grouped_data, Treatment_Effect = diff(predicted_probability))
treatment_effects <- ungroup(summarised_data)

### Plot the treatment effects
ggplot(treatment_effects, aes(x = factor(Level_of_Education), y = Treatment_Effect, fill = factor(Sex))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_x_discrete(labels = c("Not Postsecondary", "Postsecondary")) +
  scale_fill_manual(values = c("blue", "pink"), labels = c("Male", "Female")) +
  labs(
    x = "Level of Education",
    y = "Treatment Effect (SW - Control)",
    title = "Treatment Effect of Social Work (SW) vs. Control by Sex and Education Level",
    fill = "Sex"
  ) +
  theme_minimal()
colnames(treatment_effects)
