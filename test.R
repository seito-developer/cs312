# I am an instructor at a programming school for children who are non-native English speakers.
# The hypothesis that "children with high English proficiency may also learn programming quickly" occurred to me, and I wanted to test it.
# I asked 200 students to take English and programming tests.
# I analyzed their scores to see if there was a correlation between their English and programming test scores.

data_size <- 200
beautifull_dummy_data <- seq(from=1, to=90, length.out=data_size)

english_score <- beautifull_dummy_data
programming_score <- beautifull_dummy_data + rnorm(data_size, mean=0, sd=5)

model <- lm(english_score ~ programming_score)

summary(model)

plot(
  programming_score,
  english_score, 
  main="Correlation between Programming skill and English skill", 
  xlab="English Score", ylab="Programming Score", pch=19, col="blue")

abline(model, col="red", lwd=2)