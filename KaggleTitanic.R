setwd("/Users/kimberlybarr/Desktop/titanic")
titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

# I want to combine the two data files together, but first I'll add a column to help identify if the information is from the test set or train set.

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

# Checking the number of columns
ncol(titanic.train)
ncol(titanic.test)

# There are a different number of columns. I'm going to check the names of the columns and make sure they match as well as find the missing column.

names(titanic.test)
names(titanic.train)
 
# I need to add a "Survived" column to the titanic.test file.

titanic.test$Survived <- NA

# Now I am ready to combine the two files with a vertical join.

titanic.full <- rbind(titanic.train, titanic.test)

# I just want to check that everything joined correctly.

table(titanic.full$IsTrainSet)

# I'm going to do some cleaning

titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'

table(is.na(titanic.full$Age))

# There are a lot of missing ages.For now, I'm going to use the median age.

age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

# The column "Fare" is missing one value. I will also replace it with the median for now. 

fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Age"] <- fare.median

# I am going to do categorical casting.

titanic.full$Plass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)

# Now that I took cafe of the missing data I will split the tables back up.

titanic.train <- titanic.full[titanic.full$IsTrainSet== TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet== FALSE,]

# I want to add Survived as a factor.

titanic.train$Survived <- as.factor(titanic.train$Survived)

# I am going to make a perdictive model.

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)

# I am going to install some packages

install.packages("randomForest")
library(randomForest)

# Building the predictive model.

titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.test))

# Adding features

features.equation <- " Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"

# Prediction

survived <- predict(titanic.model, newdata = titanic.test)

# I want to make a csv with only PassangerID and Survived.

PassangerId <- titanic.test$PassengerId

# Convert into a data frame

output.df <- as.data.frame(PassangerId)
output.df$Survived <- survived

write.csv(output.df, file = "Kaggle_Submission.csv", row.names = FALSE)

