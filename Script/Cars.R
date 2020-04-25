library("readr")
cars <- read.csv("C:\\Respaldo FR\\UBIQUM\\Proyectos\\Getting_Start with_R\\Data\\cars.csv")
# Remove outliers ####
OutlierDataSet <- cars
OutlierColumn <- cars$distance.of.car
cars <- OutlierDataSet[OutlierColumn > (quantile(OutlierColumn)[[2]] - 1.5*IQR(OutlierColumn)),]
cars <- OutlierDataSet[OutlierColumn < (quantile(OutlierColumn)[[4]] + 1.5*IQR(OutlierColumn)),]

# Define training and testing sets ####
set.seed(123)
trainSize <- round(nrow(cars)*0.8)
testSize <- nrow(cars)-trainSize

training_indices <- sample(seq_len(nrow(cars)),size=trainSize)
trainSet <- cars[training_indices,]
testSet <- cars[-training_indices,]

# Get the linear regression model from the training set ####
Model_lm <- lm(distance.of.car ~ speed.of.car, trainSet)
summary(Model_lm)

# Predict the distances on the test set with the linear regression model ####
predictedDistance <- predict(Model_lm,testSet)
predictedDistance

# Exponential model
# Get the linear regression model from the training set ####
Model_lm <- lm(log(distance.of.car) ~ speed.of.car, trainSet)
summary(coolModel)
# Predict the distances on the test set with the linear regression model ####
predictedDistance <- exp(predict(Model_lm,testSet))
predictedDistance

# Quadratic model
# Get the linear regression model from the training set ####
Model_lm <- lm(sqrt(distance.of.car) ~ speed.of.car, trainSet)
summary(Model_lm)
# Predict the distances on the test set with the linear regression model ####
predictedDistance <- predict(Model_lm,testSet)^2
predictedDistance

# Plot the linear regression, the predicted distances, and the observed distances ####
plot(testSet$speed.of.car,predictedDistance, col='red', type='line',xlab='Speed of car', ylab = 'Distance of car', main = 'Car distance predictions based on speed')
points(testSet$speed.of.car,predictedDistance, col='black')
points(testSet$speed.of.car,testSet$distance.of.car,col='green')
legend("topleft", legend=c("Linear Regression", "Predicted Distances","Observed Distances"),
       col=c("pink", "purple","blue"), lty=c(1,NA,NA),pch=c(NA,'o','o'))

# Calculate the errors ####
errors <- predictedDistance - testSet$distance.of.car
errors
length(errors)

RMSE <- sqrt(mean(errors^2))
RMSE

MAE <-  mean(abs(errors))
MAE

MAPE <- mean(abs(errors/testSet$distance.of.car))
MAPE

# Plot the errors ####
plot(testSet$speed.of.car, errors, xlab = 'Car speed', ylab = 'Prediction error', main = 'Errors in the predictions', col= "blue")
abline(0,0)
legend("topleft", legend=c("Errors"),
       col=c("blue"), pch=c('o'))
hist(errors, xlab = 'Error', ylab = 'Density', main = 'Distribution of errors in the predictions')
