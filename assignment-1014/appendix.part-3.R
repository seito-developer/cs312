#PART III: Incorporating interaction terms

## (1) Begin by eliminating any rows in the data for which

### Install library and dataset
install.packages("medicaldata")
install.packages("caret") 
library(medicaldata)
library(caret)
#library(ggplot2)
data(blood_storage)
head(blood_storage)

### Remove the row with NA
clean_blood_storage <- subset(blood_storage, 
                              !is.na(TimeToRecurrence) & bGS != "N" & PVol != "N")

### Split randomly the data to 80% and 20%
set.seed(123)
train_indices <- sample(seq_len(nrow(clean_blood_storage)), size = 0.8 * nrow(clean_blood_storage))
train_data <- clean_blood_storage[train_indices, ]
test_data <- clean_blood_storage[-train_indices, ]

### Create the model
lm_model <- lm(TimeToRecurrence ~ bGS, data = train_data)
summary(lm_model)

### Show predictions and validations
### function for evaluating all models
evaluate_model <- function(fitted_model) {
  predictions <- predict(fitted_model, newdata = test_data)
  RMSE <- sqrt(mean((test_data$TimeToRecurrence - predictions)^2))
  MAE <- mean(abs(test_data$TimeToRecurrence - predictions))
  sum <- summary(fitted_model)
  cat(
    "RMSE:", RMSE, 
    "\nMAE:", MAE, 
    "\nP-val:", sum$coefficients[, 4])
}
evaluate_model(lm_model)

### Try the coross validation
results <- train(
  x = train_data[, c("bGS")], # select variables
  y = train_data$TimeToRecurrence,
  method = "lm", # select lm model
  trControl = trainControl(method = "cv", number = 10),  # 10-fold cross validation
  preProcess = c("center", "scale")  # normalize
)
cv_results

## (2) Produce a data visualization that shows ...
plot(TimeToRecurrence ~ bGS,data=train_data,
     main="Regression of TimeToRecurrence ~ bGS",
     xlab = "bGS", 
     ylab = "TimeToRecurrence"
     )
abline(lm_model,col = "red", lwd = 2)

## *Create other models for comparing

### Model: PreopTherapy + AA + FamHx
### Model: PreopTherapy + AA + FamHx
lm_model_2 <- lm(TimeToRecurrence ~ Age + PVol, data = train_data)
glm_model_1 <- glm(TimeToRecurrence ~ PreopTherapy + AA + FamHx, data = train_data)
evaluate_model(lm_model_2)
evaluate_model(glm_model_1)
