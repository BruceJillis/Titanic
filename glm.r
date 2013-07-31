set.seed(42)

read.data <- function(file, drop) {
	# read training data
	data <- read.csv(file, sep=',', na.strings=c(''))
	# drop some columns
	data <- subset(data, select = -c(Ticket, Name, Fare))

	# correct some column types
	data$Sex <- as.factor(data$Sex)
	data$Cabin <- as.factor(data$Cabin)
	data$Embarked <- as.factor(data$Embarked)
	if('Survived' %in% colnames(data)) {
		data$Survived <- as.factor(data$Survived)
	}
	return (data)
}

train <- read.data('data/train.csv', drop)
test <- read.data('data/test.csv', drop)

model <- glm(
	Survived ~ Pclass * Sex + Pclass * Age + Age * Sex + SibSp + Parch + Embarked + Cabin,
	data=train, 
	family="binomial"
)
summary(model)

anova(model, test="Chisq")

test$Survived <- predict(model, newdata=test, type="response")
test$Survived <- round(test$Survived)
summary(test)

write.csv(test[,c("PassengerId", "Survived")], file="predictions.csv", row.names=FALSE, quote=FALSE)