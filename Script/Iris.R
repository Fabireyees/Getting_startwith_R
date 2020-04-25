library("readr")
iris <- read.csv("C:\\Respaldo FR\\UBIQUM\\Proyectos\\Getting_Start with_R\\Data\\iris.csv")
#Exploring the Data ####
attributes(iris)
summary(iris) 
str(iris)
names(iris)
iris$Species<- as.numeric(iris$Species) 
hist(iris$Species)
plot(iris$Sepal.Length,iris$Species)
qqnorm(iris$Sepal.Length)
qqnorm(iris$Petal.Width)
boxplot(iris$Sepal.Length)
boxplot(iris$Petal.Width)

# Define training and testing sets ####
options(digits = 3)
set.seed(123)
trainSize<-round(nrow(iris)*0.80) 
testSize<-nrow(iris)-trainSize
trainSize
testSize
training_indices<-sample(seq_len(nrow(iris)),size =trainSize)
trainSet<-iris[training_indices,]
testSet<-iris[-training_indices,] 


# Modeling linear regression model from the training set ####
LinearModeliris<- lm(Petal.Length ~ Petal.Width, trainSet)
summary(LinearModeliris)

# Predict the lengths on the test set with the linear regression model ####
predictions<-predict(LinearModeliris,testSet)
predictions


# Plot the linear regression, the predicted lengths, and the observed lengths ####
plot(testSet$Petal.Width,predictions, col='red', type='line',xlab='Petal Width', ylab = 'Petal Length', main = 'Petal Length Predictions based on Petal Width')
points(testSet$Petal.Width,predictions, col='black')
points(testSet$Petal.Width,testSet$Petal.Length,col='green')
legend("topleft", legend=c("Linear Regression", "Predicted Lengths","Observed Lengths"),
       col=c("red", "black","green"), lty=c(1,NA,NA),pch=c(NA,'o','o'))



# Calculate the errors ####
errors <- predictions - testSet$Petal.Length
errors
length(errors)

RMSE <- sqrt(mean(errors^2))
RMSE

MAE <-  mean(abs(errors))
MAE

MAPE <- mean(abs(errors/testSet$Petal.Length))
MAPE

# Plot the errors ####
plot(testSet$Petal.Width, errors, xlab = 'Petal Width', ylab = 'Prediction error', main = 'Errors in the predictions', col= "blue")
abline(0,0)
legend("topleft", legend=c("Errors"),
       col=c("blue"), pch=c('o'))
hist(errors, xlab = 'Error', ylab = 'Density', main = 'Distribution of errors in the predictions')

boxplot(iris[,!colnames(iris)=='X'])
