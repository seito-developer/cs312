#PART II: Non-linear local regression (loess), confidence intervals, prediction intervals, and leave-one-out cross-validation (LOCV)

## (1) Devise a non-linear local regression (loess) that ...

### Install library and dataset
install.packages("medicaldata")
library(medicaldata)
library(ggplot2)
data(laryngoscope)
head(laryngoscope)

### Remove the row with NA
filtered_laryngoscope <- laryngoscope[!is.na(laryngoscope$BMI), ]

### Plot
ggplot(filtered_laryngoscope, aes(x = BMI, y = ease)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Ease of Intubation vs. BMI",
       x = "BMI (Body Mass Index)",
       y = "EOI (Ease of Intubation)") +
  theme_minimal()



### Estimating Test Set Errors with LOOCV
loocv_errors <- sapply(1:nrow(filtered_laryngoscope), function(i) {
  loess_model_i <- loess(ease ~ BMI, data = filtered_laryngoscope[-i,])
  predict(loess_model_i, newdata = filtered_laryngoscope[i,]) - filtered_laryngoscope$ease[i]
})

### Estimate MSE
mse_loocv <- mean(loocv_errors^2, na.rm = TRUE)
mse_loocv
mean(filtered_laryngoscope$ease)
sd(filtered_laryngoscope$ease)


## (2) Run a traditional linear regression to predict ease of intubation.
### Create 3 models
model1 <- lm(ease ~ BMI, data = filtered_laryngoscope)
model2 <- lm(ease ~ age, data = filtered_laryngoscope)
model3 <- lm(ease ~ BMI + age, data = filtered_laryngoscope)

### Create a vector to store LOOCV errors
loocv_errors1 <- rep(NA, nrow(filtered_laryngoscope))
loocv_errors2 <- rep(NA, nrow(filtered_laryngoscope))
loocv_errors3 <- rep(NA, nrow(filtered_laryngoscope))

### Do LOOCV for each models
for (i in 1:nrow(filtered_laryngoscope)) {
  model1_loocv <- lm(ease ~ BMI, data = filtered_laryngoscope[-i, ])
  model2_loocv <- lm(ease ~ age, data = filtered_laryngoscope[-i, ])
  model3_loocv <- lm(ease ~ BMI + age, data = filtered_laryngoscope[-i, ])
  
  pred1 <- predict(model1_loocv, newdata = filtered_laryngoscope[i, , drop = FALSE])
  pred2 <- predict(model2_loocv, newdata = filtered_laryngoscope[i, , drop = FALSE])
  pred3 <- predict(model3_loocv, newdata = filtered_laryngoscope[i, , drop = FALSE])
  
  loocv_errors1[i] <- pred1 - filtered_laryngoscope$ease[i]
  loocv_errors2[i] <- pred2 - filtered_laryngoscope$ease[i]
  loocv_errors3[i] <- pred3 - filtered_laryngoscope$ease[i]
}

### Visualize confidence and prediction intervals for Model 2
new_data1 <- data.frame(BMI = mean(filtered_laryngoscope$BMI, na.rm = TRUE))
new_data2 <- data.frame(age = mean(filtered_laryngoscope$age, na.rm = TRUE))
new_data3 <- data.frame(BMI = mean(filtered_laryngoscope$BMI, na.rm = TRUE), 
                       age = mean(filtered_laryngoscope$age, na.rm = TRUE))

### Estimate MSE, conf, and pred
mse_loocv1 <- mean(loocv_errors1^2, na.rm = TRUE)
mse_loocv2 <- mean(loocv_errors2^2, na.rm = TRUE)
mse_loocv3 <- mean(loocv_errors3^2, na.rm = TRUE)

conf1 <- predict(model1, newdata = new_data1, interval = "confidence")
conf2 <- predict(model2, newdata = new_data2, interval = "confidence")
conf3 <- predict(model3, newdata = new_data3, interval = "confidence")
pred1 <- predict(model1, newdata = new_data1, interval = "prediction")
pred2 <- predict(model2, newdata = new_data2, interval = "prediction")
pred3 <- predict(model3, newdata = new_data3, interval = "prediction")
cat("mse_loocv1:", mse_loocv1, "\nmse_loocv2:", mse_loocv2, "\nmse_loocv3:", mse_loocv3,
  "\nconf1:", conf1, "\nconf2:", conf2, "\nconf3:", conf3,
    "\npred1:", pred1, "\npred2:", pred2, "\npred3:", pred3)

### Plot
ggplot(filtered_laryngoscope, aes(x = age, y = ease)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_errorbar(aes(x = new_data2$age, ymin = conf2[2], ymax = conf2[3]), width = 2) +
  geom_errorbar(aes(x = new_data2$age, ymin = pred2[2], ymax = pred2[3]), width = 1, linetype = "dashed") +
  labs(title = "Confidence and Prediction Intervals with Model2",
       x = "Age",
       y = "Ease of Intubation") +
  theme_minimal()
