ground_truth <- c(1,1,1,1,0,0,0,0)
n_sim = 1000 				

# this will be our storage vector
how_many_guessed_correct <- NA

for (i in 1:n_sim){
  # THIS LINE OF CODE SHOULD RANDOMLY SELECT 
  # 4 MILK & 4 TEA IN ANY ORDER 
  random_selection <- sample(ground_truth, 1, replace = FALSE)
  #print(random_selection)
  # count how many of the 8 elements in 'random selection'
  # above match the ground truth. e.g., if random selection
  # is 10101010, then 4 were guessed correctly.
  
  # You can use the function sum()...
  how_many_guessed_correct[i] <- i
}

how_many_guessed_correct