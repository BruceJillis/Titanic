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
	 #data$Survived <- as.factor(data$Survived)
	data$Sex <- as.factor(data$Sex)
	data$Embarked <- as.factor(data$Embarked)
	data$Pclass <- as.factor(data$Pclass)
	if('Survived' %in% colnames(data)) {
		data$Survived <- as.factor(data$Survived)
	}
	return (data)
}

train <- read.data('data/train.csv')
test <- read.data('data/test.csv')

# massage and impute missing data
cabin_to_deck <- function(data) {
	data = as.character(data)
	for(i in seq(along=data)) {
		if (is.na(data[i]))
			next
		data[i] <- substr(data[i], 1, 1)
	}
	return (as.factor(data))
}

# Cabin
train$Cabin <- impute(cabin_to_deck(train$Cabin), 'random')
test$Cabin <- impute(cabin_to_deck(test$Cabin), 'random')

# Age
train$Age <- impute(train$Age, mean)
test$Age <- impute(test$Age, mean)

# Embarked
train$Embarked <- impute(train$Embarked, mean)
test$Embarked <- impute(test$Embarked, mean)

model <- glm(
	Survived ~ Pclass + Sex + Age + Cabin + SibSp + Parch + 
		Pclass : Sex +
		Pclass : Age + 
		Pclass : Cabin + 
		Sex : SibSp + 
		Sex : Parch + 
		Sex : Age + 
		Sex : Cabin + 
		Age : Cabin,
	data=train, 
	family="binomial",
	control=list(maxit = 150)
)
summary(model)

anova(model, test="Chisq")
if(T){
head(test)
test$Survived <- predict(model, newdata=test, type="response")
test$Survived <- round(test$Survived)
head(test)
summary(test)

write.csv(test[,c("PassengerId", "Survived")], file="predictions.csv", row.names=FALSE, quote=FALSE)

}