library(randomForest)
set.seed(42)
library(Hmisc)
clc <- function() cat(rep("\n",100))
clc()

set.seed(42)

read.data <- function(file) {
	# read training data
	data <- read.csv(file, sep=',', na.strings=c(''), stringsAsFactors=FALSE)
	# drop some columns
	data <- subset(data, select = -c(Name, Fare, Ticket))
	# correct some column types
	data$Sex <- factor(data$Sex)
	data$Embarked <- factor(data$Embarked)
	data$Pclass <- factor(data$Pclass)
	return (data)
}

train <- read.data('data/train.csv')
train$Survived <- as.factor(train$Survived)

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
tmp_train = cabin_to_deck(train$Cabin)
tmp_test = cabin_to_deck(test$Cabin)

tmp_train = impute(tmp_train, 'random')
tmp_test = impute(tmp_test, 'random')

print(tmp_test)
unique(tmp_test)

train$Cabin <- factor(tmp_train, levels=c("A","B","C","D","E","F","G","T"))
test$Cabin <- factor(tmp_test, levels=c("A","B","C","D","E","F","G","T"))

# Age
train$Age <- impute(train$Age, mean)
test$Age <- impute(test$Age, mean)

# Embarked
train$Embarked <- impute(train$Embarked, mean)
test$Embarked <- impute(test$Embarked, mean)

summary(train)
summary(test)

model <- randomForest(
	Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Pclass:Sex + Pclass:Age + Age:Sex, 
	data=train,
	ntree=2000
)
print(model)

test <- read.csv('data/test-clean.csv', sep=';')
test$Survived <- predict(model, newdata=test, type="response")
	
write.csv(test[,c("PassengerId", "Survived")], file="predictions.csv", row.names=FALSE, quote=FALSE)