# Set the working directory
setwd("D:/Marketing Analytics")

library("QuantPsyc")

# Read Porto train and test data set
#porto.traindata <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
cart <- read.csv(file = "Cartwheel.csv",  stringsAsFactors = FALSE, header = TRUE)

#Data set summary
str(cart)
summary(cart) 

# Find the correlation between attributes
cor (cart, method = "pearson")

# Apply the OLS Regression model
cart_output <- lm(L_Sales	~ L_Cart + L_Line_Circ + L_Pic_Circ + L_Online + Trend + Season, data = cart)
summary(cart_output)

lm.beta(cart_output)
