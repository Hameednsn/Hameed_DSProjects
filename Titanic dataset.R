
# Set the working directory
setwd("C:/Users/Hameed/Downloads/R Datasets")
# Read titanic train and test data set
titanic.traindata <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.testdata  <- read.csv(file = "test.csv",  stringsAsFactors = FALSE, header = TRUE)

# Before proceeding further clean data
# Here, we need to clean both training and test data set, but there is no need to clean seperately
# We can combine both the data set and clean.
str(titanic.traindata)
str(titanic.testdata)
summary(titanic.traindata)

# To differentiate between train and test data set after combining, we will add a flag 
titanic.traindata$IsTrainSet <- TRUE
titanic.testdata$IsTrainSet <- FALSE

# As test data set doesn't contain the Survived column, as this column is reqd to cobmine both dataset
titanic.testdata$Survived <- NA
# Combine both dataset
titanic.combined <- rbind(titanic.traindata, titanic.testdata)

#Now, do the data imputation for missing values
titanic.combined[titanic.combined$Embarked=='', "Embarked"] <- 'S'
# Calculate Age median of combined data set
Age.median <- median(titanic.combined$Age, na.rm = TRUE)
titanic.combined[is.na(titanic.combined$Age), "Age"] <- Age.median

fair.median <- median(titanic.combined$Fare, na.rm = TRUE)
titanic.combined[is.na(titanic.combined$Fare), "Fare"]<-fair.median

# Categorical casting
titanic.combined$Pclass   <- as.factor(titanic.combined$Pclass)
titanic.combined$Sex      <- as.factor(titanic.combined$Sex)
titanic.combined$Embarked <- as.factor(titanic.combined$Embarked)


titanic.traindata <- titanic.combined[titanic.combined$IsTrainSet == TRUE, ]
titanic.testdata <- titanic.combined[!titanic.combined$IsTrainSet == TRUE, ]

str(titanic.combined)

titanic.traindata$Survived <- as.factor(titanic.traindata$Survived)

Survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived.formula <- as.formula(Survived.equation)
library(randomForest)

titanic.model <- randomForest(formula = Survived.formula, data = titanic.traindata, ntree = 500, mtry=3, nodesize = 0.01*nrow(titanic.testdata))

variables <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.testdata)

PassengerId <- titanic.testdata$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$Survived <- Survived
write.csv(output.df, file = "KaggaleSubmission.csv", row.names = FALSE )
