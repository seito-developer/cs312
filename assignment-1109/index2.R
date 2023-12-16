# Setup
install.packages("dplyr")
library(dplyr)
install.packages("readxl")
library(readxl)
install.packages("openxlsx")
library(openxlsx)
install.packages("MatchIt")
library(MatchIt)
install.packages("cobalt")
library(cobalt)
data("lalonde", package = "cobalt")

install.packages("ggplot2")
library(ggplot2)
#install.packages("EValue")
#library(EValue)
install.packages('cowplot')
library('cowplot')

data <- read_excel("/Users/seito/Documents/develop/cs312/assignment-1109/DataSet para RUO.xlsx")
head(data)

# 1～100行には1を、それ以外には0を設定
data <- data %>% mutate(treatment = ifelse(row_number() <= 100, 1, 0))

data$treatment


m.out <- matchit(treatment ~ `Exercise1 Effectiveness` + `Exercise2 Effectiveness`, 
        data = data, method = "nearest",
        caliper = 0.25, ratio = 1, replace = FALSE)

summary(m.out)

love.plot(m.out, stats = "m", thresholds = 0.1, abs = TRUE, 
          var.order = "unadjusted", line = FALSE, stars = "none",
          limits = list(m = c(0,1)),
          colors = c("#191970", "#db7093"),
          shapes = c("triangle", "circle"),
          alpha = .8, grid = TRUE, title = "")
  #labs(x = "Absolute standardized mean difference")

# Made groups
#control_group <- data[1:100, ]
#treatment_group <- data[101:235, ]
data[1:235]

mean(control_group$`Exercise1 Effectiveness`)
mean(treatment_group$`Exercise1 Effectiveness`)

# Matcing
##install.packages("MatchIt")
##library(MatchIt)

##control_group$treatment <- 0
##treatment_group$treatment <- 1

##combined_data <- rbind(control_group, treatment_group)
##arramged_dataset <- na.omit(combined_data)

##ps_model <- matchit(
##  arramged_dataset$treatment ~ 
##    ##arramged_dataset$`Race Effectiveness` + 
#arramged_dataset$`Exercise1 Effectiveness` +
#arramged_dataset$`Exercise2 Effectiveness` +
#arramged_dataset$`Exercise3 Effectiveness` + 
#arramged_dataset$`Exercise4 Effectiveness` + 
#arramged_dataset$`Race Effectiveness Increase` + 
#arramged_dataset$`Exercise1 Effectiveness Increase` +
#arramged_dataset$`Exercise 2 Effectiveness Increase` +
#arramged_dataset$`Exercise3 Effectiveness Increase` + 
#arramged_dataset$`Exercise4 Effectiveness Increase` + 
#arramged_dataset$`Race Active Time`,
#arramged_dataset$`Exercise1 Active Time` +
#arramged_dataset$`Exercise2 Active Time` +
#arramged_dataset$`Exercise3 Active Time` +
#arramged_dataset$`Exercise4 Active Time`,
##  data = arramged_dataset, method = "nearest")

##summary(ps_model)
##matched_data <- match.data(ps_model)
##matched_data
