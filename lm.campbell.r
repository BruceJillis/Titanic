setwd('D:/Development/RScripts/Titanic/')

data = read.csv('data/train.csv', sep=',', na.strings=c(''))

data$Survived <- factor(data$Survived)
#data$Sex <- factor(data$Sex)
#data$Embarked <- factor(data$Embarked)
#data$Pclass <- factor(data$Pclass)

# extract deck name from Cabin number
cabin_to_deck <- function(data) {
	data = as.character(data)
	for(i in seq(along=data)) {
		if (is.na(data[i]))
			next
		data[i] <- substr(data[i], 1, 1)
	}
	return (data)
}

data$Cabin <- cabin_to_deck(data$Cabin)
data$Cabin <- factor(data$Cabin, levels=c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'T'))

# extract Title from Name
extract_title <- function(data) {
	for(i in seq(along=data)) {
		if (is.na(data[i]))
			next
		a <- unlist(strsplit(data[i], ', '))[2]
		b <- unlist(strsplit(a, '. '))[1]
		data[i] <- b
	}
	return (data)
}

data$Title <- extract_title(as.character(data$Name))
data$Title <- factor(data$Title)

# impute age
models.age <- lm(Age ~ Fare + Title + SibSp + Parch, data=data)
for(i in 1:nrow(data)) {
	if (is.na(data[i, 'Age'])) {
		data[i, 'Age'] <- predict(models.age, newdata=data[i,])
	}
}


models.glm = glm(Survived ~ Pclass + Fare + SibSp + Parch + Sex + Age + Pclass:Age + Age:Sex + SibSp:Sex, family=binomial(link='logit'), data=data)

p = predict(models.glm, newdata=data, type='response')
survived = round(p)

library(caret)
confusionMatrix(factor(survived), data$Survived)

# make prediction

test = read.csv('data/test.csv', sep=',', na.strings=c(''))

# extract deck name from Cabin number
cabin_to_deck <- function(data) {
	data = as.character(data)
	for(i in seq(along=data)) {
		if (is.na(data[i]))
			next
		data[i] <- substr(data[i], 1, 1)
	}
	return (data)
}

test$Cabin <- cabin_to_deck(test$Cabin)
test$Cabin <- factor(test$Cabin, levels=c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'T'))

# extract Title from Name
extract_title <- function(data) {
	for(i in seq(along=data)) {
		if (is.na(data[i]))
			next
		a <- unlist(strsplit(data[i], ', '))[2]
		b <- unlist(strsplit(a, '. '))[1]
		data[i] <- b
	}
	return (data)
}

test$Title <- extract_title(as.character(test$Name))
test$Title <- factor(test$Title)

# impute age
models.age <- lm(Age ~ Fare + Title + SibSp + Parch, data=data)
for(i in 1:nrow(test)) {
	if (is.na(test[i, 'Age'])) {
		test[i, 'Age'] <- predict(models.age, newdata=test[i,])
	}
}

test$Fare[153] <- mean(
	with(test, subset(Fare, Pclass == 3)),
	na.rm=TRUE
)

summary(test)

p = predict(models.glm, newdata=test, type='response')

data = data.frame(PassengerId = test$PassengerId, survived = round(p))
write.csv(data, 'predictions.csv', row.names = FALSE)
