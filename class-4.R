#(1) My .csv data is here: https://tinyurl.com/3hwkzetf
#(2) This dataset illustrates an imaginary relationship between student's Enslish skill and programming skill.
#(3) My dependent variable is programming_score and the other variables are the independent variables.

data <- read.csv("https://tinyurl.com/3hwkzetf")

train_df <- data[1:20, ]
test_df <- data[21:nrow(mtcars), ]

lm1 <- lm(english_score ~ programming_score, data = train_df)
y_pred <- predict(lm1, test_df)

english_score <- data$english_score
programming_score <- data$programming_score

model <- lm(english_score ~ programming_score)

#summary(model)

# Estimate RMSE
#rmse <- calc_rmse(test_df$english_score, y_pred)
rmse <- sqrt(mean((programming_score - y_pred)^2))
rmse

# Estimate R-squared
#rsq <- calc_rsquared(test_df$programming_score, y_pred)
sse <- sum((programming_score - y_pred)^2)
sst <- sum((programming_score - mean(programming_score))^2)
r_squared <- 1 - (sse/sst)

rmse
r_squared