library(Matching)
data("lalonde")
head(lalonde)

# Treatment group
hist(lalonde$age[lalonde$treat == 0], 
     main="Age Distribution", 
     xlab="Age", 
     ylab="Frequency", 
     xlim=c(15, 55), 
     col=rgb(0,0,1,0.5), 
     breaks=15)

# Control group
hist(lalonde$age[lalonde$treat == 1], add=TRUE, col=rgb(1,0,0,0.5), breaks=15)

# Added labels
legend("topright", legend=c("Control", "Treatment"), fill=c(rgb(0,0,1,0.5), rgb(1,0,0,0.5)))

# Try t-test
t_test_result <- t.test(age ~ treat, data=lalonde)
t_test_result
