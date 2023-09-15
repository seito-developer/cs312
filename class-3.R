# Load libraries
library(ggplot2)

# Make base variables
data_size <- 200
beautifull_dummy_data <- seq(from=1, to=90, length.out=data_size)

# Make columns
id <- 1:data_size
english_score <- beautifull_dummy_data
programming_score <- beautifull_dummy_data + rnorm(data_size, mean=0, sd=5)

# Build dataset with the above columns
data <- data.frame(id, english_score, programming_score)

# Split the dataset to 2
index <- sample(1:data_size, 100)
data_set1 <- data[index, ]
data_set2 <- data[-index, ]

# Visualize
head(data_set1)
plot(data_set1$english_score, data_set1$programming_score, xlab="English Score", ylab="Programming Score", main="English vs. Programming scores", pch=19, col="blue")


#my_data <- read.csv("https://drive.google.com/file/d/1mSe7Ddchpu2R0RwVzG1jPjusUMaMO-Ru")
dim(my_data)