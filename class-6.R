head(mtcars)
model <- lm(mpg ~ disp, data = mtcars)
summary(model)

# Test of independent variables
new_disp <- data.frame(disp= c(150, 200, 250))

# Show predictions between 95%
predict(model, newdata = new_disp, interval = "predict")