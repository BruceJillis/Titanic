# See for context: 
#   http://ww2.coastal.edu/kingw/statistics/R-tutorials/logistic.html

# Logistic Regression: Multiple Numerical Predictors

file = "http://ww2.coastal.edu/kingw/statistics/R-tutorials/text/gorilla.csv"
# huh, you can turn it around as well.. could be handy
read.csv(file) -> gorilla
str(gorilla)