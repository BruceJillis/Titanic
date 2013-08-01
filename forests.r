print('\n\n\n\n')
print('===============================================================================')

library(randomForest)
library(Hmisc)

set.seed(42)


read.data <- function(file) {
	data <- read.csv(file, sep=',', na.strings=c(''), stringsAsFactors=FALSE)	
	data <- subset(data, select = -c(Name, Fare, Ticket))
	return (data)
}
train <- read.data('data/train.csv')
train$Survived = factor(train$Survived)

test <- read.data('data/test.csv')

# massage and impute missing data
cabin_to_deck <- function(data) {
	data = as.character(data)
	for(i in seq(along=data)) {
		if (is.na(data[i]))
			next
		data[i] <- substr(data[i], 1, 1)
	}
	return (data)
}

# Cabin
train$Cabin = cabin_to_deck(train$Cabin)
train$Cabin = factor(train$Cabin, levels=c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'T'))
train$Cabin = impute(train$Cabin, median)


test$Cabin = cabin_to_deck(test$Cabin)
test$Cabin = factor(test$Cabin, levels=c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'T'))
test$Cabin = impute(test$Cabin, median)

# Age
train$Age <- impute(train$Age, mean)
test$Age <- impute(test$Age, mean)

# Embarked
train$Embarked <- impute(factor(train$Embarked), median)
test$Embarked <- impute(factor(test$Embarked), median)

# Sex
train$Sex <- factor(train$Sex)
test$Sex <- factor(test$Sex)

# Pclass
train$Pclass <- factor(train$Pclass, levels=c(1,2,3))
test$Pclass <- factor(test$Pclass, levels=c(1,2,3))

str(train)
str(test)

model <- randomForest(
	Survived ~ (Pclass + Sex + Age + SibSp + Parch + Embarked)^6, 
	data=train,
	ntree=5000,
	mtry=3
)
print(model)

test$Survived <- predict(model, newdata=test, type="response")
	
write.csv(test[,c("PassengerId", "Survived")], file="predictions.csv", row.names=FALSE, quote=FALSE)