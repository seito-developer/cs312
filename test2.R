babies <- read.csv("https://tinyurl.com/dx9vk4yn")

# then a quick look at the top of the data set would be
head(babies)

# To create a histogram, type: hist() and inside the parenthesis, put the variable name.
#hist(babies$birth_weights)

# To filter for babies of smoking moms: 
smoked_baby <- babies$birth_weights[babies$smoking_status == "Smoking_Mom"]
non_smoked_baby <- babies$birth_weights[babies$smoking_status == "Non_smoking_Mom"]

median(smoked_baby)
median(non_smoked_baby)
#underweights[underweights$smoking_status == "Smoking_Mom",]

#26192
#11912