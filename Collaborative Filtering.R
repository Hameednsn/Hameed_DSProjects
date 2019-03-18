## Set the working directory
setwd("D:/Marketing Analytics")

## Read the required input data file (both train and test data)
input_data <-read.csv('Ancova.csv')
result <- aov(Sales ~ Campaign + Price + Seasonality + ACV, data = input_data)
summary(result)

## ACV and Seasonality does not influence bcoz they r constant (all same value), hence remvoe these covariates, rerun
result <- aov(Sales ~ Campaign + Price, data = input_data)
summary(result)

## Posthoc test
t.test(Sales ~ Campaign, data = input_data)

TukeyHSD(result, "Campaign", conf.level = 0.95)

Campaign1 <- factor(input_data$Campaign, 2)
pairwise.t.test( Sales, Campaign1, p.adj = "bonferroni", pool.sd = T, data = input_data)
