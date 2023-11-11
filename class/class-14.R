library(Matching)
data("lalonde")
head(lalonde)

# > Question 1 (Use code cell below to analyze your data)
# > Consider lalonde observation #9:
lalonde[9,] 

reg1 <- glm(treat ~ age + educ + black + hisp + married + nodegr + re74 + re75 + u74 + u75, 
            data = lalonde, family = binomial)

predict(reg1)
plot(reg1)
