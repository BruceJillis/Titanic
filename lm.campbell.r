setwd('D:/Development/RScripts/Titanic/')

# extract deck name from Cabin number
cabin_to_deck <- function(data) {
	for(i in seq(along=data)) {
		if (is.na(data[i])) {
			data[i] = '?'
			next
		}
		data[i] <- substr(data[i], 1, 1)
	}
	return (data)
}

# extract Title from Name
extract_title <- function(data) {
	for(i in seq(along=data)) {
		if (is.na(data[i]))
			next
		a <- unlist(strsplit(data[i], ', '))[2]
		b <- unlist(strsplit(a, ' '))[1]
		if (b == 'the') {
			b = 'Countess.'
		} 
		data[i] <- b
	}
	return (data)
}

rooms <- function(data) {
	result = numeric()
	for(i in seq(along=data)) {
		if (is.na(data[i])) {
			result[i] = 0 
			next
		}
		parts <- strsplit(data[i], ' ')
		result[i] <- length(unlist(parts))
	}
	return (result)	
}


name = 'train'

filename <- paste('data/', '.csv', sep=name)
data <- read.csv(filename, sep=',', na.strings=c(''))
	
# convert some columns to factors		
data$Sex <- factor(data$Sex)
data$Embarked <- factor(data$Embarked)
data$Pclass <- factor(data$Pclass)
	
# extract deck letter from cabin
cabins <- as.character(data$Cabin)
data$Cabin <- cabin_to_deck(cabins)
data$Cabin <- factor(data$Cabin)

data$Rooms <- rooms(cabins)
	
# extract title from name
data$Title <- extract_title(as.character(data$Name))
data$Title <- factor(data$Title)

# drop some columns
data = subset(data, select = -c(Name, Ticket))

# impute age
models.age <- lm(Age ~ Pclass + SibSp + Parch + Sex, data=data)
for(i in 1:nrow(data)) {
	if (is.na(data[i, 'Age'])) {
		data[i, 'Age'] <- max(0, predict(models.age, newdata=data[i,]))
	}
}

# impute embarked
data$Embarked[which(is.na(data$Embarked))] = 'S'

# convert into factor here because test set wont have this column
data$Survived <- factor(data$Survived)

# train model
models.glm = glm(
	Survived ~ Pclass + SibSp + Parch + 
		     Sex + Age + Cabin + Rooms + Pclass:Age + 
 		     Age:Sex + SibSp:Sex,
	family=binomial(link='logit'), 
	data=data
)

p = predict(models.glm, newdata=data, type='response')
survived = round(p)

library(caret)
confusionMatrix(factor(survived), data$Survived)

# make prediction
name = 'test'

filename <- paste('data/', '.csv', sep=name)
test <- read.csv(filename, sep=',', na.strings=c(''))
	
# convert some columns to factors		
test$Sex <- factor(test$Sex)
test$Embarked <- factor(test$Embarked)
test$Pclass <- factor(test$Pclass)
	
# extract deck letter from cabin
cabins <- as.character(test$Cabin)
test$Cabin <- cabin_to_deck(cabins)
test$Cabin <- factor(test$Cabin)

test$Rooms <- rooms(cabins)
	
# extract title from name
test$Title <- extract_title(as.character(test$Name))
test$Title <- factor(test$Title)

# drop some columns
test = subset(test, select = -c(Name, Ticket))
	
# impute missing fare
test$Fare[153] <- mean(
	with(test, subset(Fare, Pclass == 3)),
	na.rm=TRUE
)

# impute age
models.age2 <- lm(Age ~ Pclass + SibSp + Parch + Sex, data=test)
for(i in 1:nrow(test)) {
	if (is.na(test[i, 'Age'])) {
		test[i, 'Age'] <- max(0, predict(models.age2, newdata=test[i,]))
	}
}

p = predict(models.glm, newdata=test, type='response')
p = data.frame(PassengerId = test$PassengerId, survived = round(p))
write.csv(p, 'predictions.csv', row.names = FALSE)
