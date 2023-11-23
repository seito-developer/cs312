# Importing the data
library(readr)
d=read_csv("https://bit.ly/2SKjUn2")

# Number of observations
nrow(d)

# Number of registered voters
sum(d$reg_voters)

#Get treatment vector as a dummy
treatment=numeric(length(d$treat))
treatment[d$treat=="pub.pol"]=1

# ATE
lm_basic=lm(d$vote_pop~treatment)
ATE_basic=lm_basic$coefficients[2]

# which is the same as
mean(d$vote_pop[treatment == 1]) - mean(d$vote_pop[treatment == 0])

## (b) What is the treatment effect when you apply the Fisher Exact Test (ignoring blocking)?
### Median of the vote rate
median_vote_pop <- median(d$vote_pop)

### Binarised by whether voter turnout is above or below the median
vote_pop_high <- as.numeric(d$vote_pop > median_vote_pop)

### Make the 2x2 table
cross_table <- table(treatment, vote_pop_high)
cross_table
### Try the fisher's test
test_result <- fisher.test(cross_table)
test_result

## (c) Extra Credit: Wantchekon created 8 blocks of 2 villages each (described above.)

### Process each block individually
results2 <- lapply(split(d, d$block), function(block_data) {
  treatment <- as.numeric(block_data$treat == "pub.pol")
  median_vote_pop <- median(block_data$vote_pop)
  vote_pop_high <- as.numeric(block_data$vote_pop > median_vote_pop)
  
  cross_table2 <- table(treatment, vote_pop_high)
  
  fisher.test(cross_table2)
})
results2
block_data$vote_pop