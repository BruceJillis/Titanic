library(mice)
library(randomForest)
set.seed(42)

# read training data

train <- read.csv('data/train.csv', sep=',', na.strings=c(''))

# drop some rows
train$PassengerId <- NULL
train$Cabin <- NULL
train$Ticket <- NULL
train$Name <- NULL
train$Fare <- NULL

# mark some rows as nominal
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
train$Survived <- as.factor(train$Survived)

# read test set
test <- read.csv('data/test.csv', sep=',')

test$Cabin <- NULL
test$Ticket <- NULL
test$Name <- NULL
test$Fare <- NULL

test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)

# impute missing values

mi <- mice(train)
train <- complete(mi)

mi <- mice(test)
test <- complete(mi)

summary(train)
summary(test)


model <- randomForest(
	Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Pclass:Sex + Pclass:Age + Age:Sex, 
	data=train,
	ntree=2000
)
test$Survived <- predict(model, newdata=test, type="response")

write.csv(test[,c("PassengerId", "Survived")], file="predictions.csv", row.names=FALSE, quote=FALSE)