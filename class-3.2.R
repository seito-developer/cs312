# Class
grams_sugar <- rnorm(200, mean = 40, 12)
exercise_min <- rnorm(200, mean = 45, sd = 15)
age <- runif(200, min = 5, max = 15)
screen_bed <- runif(200, min = 0, max = 60)
noise <- rnorm(200, 0, 70) 

df <- data.frame(Sugar = grams_sugar, 
                 Screentime = screen_bed,
                 Years_old = age,
                 Exercise = exercise_min)

## A good way to create dummy dataset
minutes_to_sleep <- grams_sugar*2 + screen_bed - 3*exercise_min  + age^2 + noise

head(minutes_to_sleep)