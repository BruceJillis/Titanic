set.seed(42)

train <- read.csv('data/train.csv', sep=',')
# select subset of the data to keep it managable
train <- train[1:5,c(1,2,6,10)]
train[1,1] = NA
train[2,3] = NA

summary(train)
print(train$Age)

random.imp <- function (a){
	# simple random imputation, works on vectors
	missing <- is.na(a)
	n.missing <- sum(missing)
	a.obs <- a[!missing]
	imputed <- a
	imputed[missing] <- sample(a.obs, n.missing, replace=TRUE)
	return (imputed)
}

train$Age <- random.imp(train$Age)
train$PassengerId <- random.imp(train$PassengerId)

topcode <- function (a, top) {
	# cutoff values in vector if above top and set to top
	return (ifelse (a>top, top, a))
}

train$Age <- topcode(train$Age, 50)