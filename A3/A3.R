#A3

# ----------------------------------------------------------------------------------
# Question 1
# ----------------------------------------------------------------------------------

source("my.prediction.stats.R")
source("wrappers.R")

# Load the housing data
housing_data <- read.csv("housing.2023.csv")
# Fit a multiple linear model
model <- lm(medv ~ ., data = housing_data)
# Extract the summary of the model
summary(model)

# Extract the p-values for each predictor variable
p_values <- summary(model)$coefficients[, 4]

# Calculate the Bonferroni-corrected significance level by dividing by the amount of regressors we have
bonferroni_level <- 0.05 / length(p_values)

# Compare each p-value to the Bonferroni-corrected significance level
significant_vars <- p_values <= bonferroni_level

# Prune with BIC in both directions
fit.bic = step(model, direction = "both", k = log(nrow(housing_data)))
summary(fit.bic)


# Make a prediction for a new set of data inputs
new_inputs <- data.frame(chas = 0, nox = .573, rm = 6.03, dis=2.505, ptratio = 21, lstat = 7.88)
prediction <- predict(fit.bic, newdata = new_inputs)

# Obtain the 95% confidence interval for the prediction
confidence_interval <- predict(fit.bic, newdata = new_inputs, interval = "confidence")

#Testing interaction between rooms and distance to employment centre
model2 <- lm(medv ~ chas+ nox+ rm+ dis + ptratio + lstat + rm*dis , data = housing_data)
# Extract the summary of the model
summary(model2)

# ----------------------------------------------------------------------------------
# Question 2
# ----------------------------------------------------------------------------------
source("my.prediction.stats.R")
source("wrappers.R")
library(rpart)
library(randomForest)

source("my.prediction.stats.R")
source("wrappers.R")

# Load data and train a tree with 10 folds and 5000 repetitions
heart.train <- read.csv("heart.train.2023.csv", header=T, stringsAsFactors=T)
cv = learn.tree.cv(HD~.,data=heart.train,nfolds=10,m=5000)
plot.tree.cv(cv)

#plot the best tree and add labels
plot(cv$best.tree)
text(cv$best.tree,pretty=12)

#fit logistic regression to the model
heart.train2 <- read.csv("heart.train.2023.csv", header=T, stringsAsFactors=T)
# Convert the HD column to 1 for Y and 0 for N
heart.train2$HD <- ifelse(heart.train2$HD == "Y", 1, 0)
model_glm <- glm(HD ~., data=heart.train2, family=binomial)
#prune with BIC and view results
glm.fit.bic = step(model_glm, direction = "both", k = log(nrow(heart.train2)))
summary(glm.fit.bic)

#predict results of HD with new test data for logistic regression and compute confusion matrix
heart.test <- read.csv("heart.test.2023.csv", header=T, stringsAsFactors=T)
prob_reg = predict(glm.fit.bic, heart.test, type="response")
my.pred.stats(prob_reg, heart.test$HD)

#predict results of HD with new test data for decision tree and compute confusion matrix
prob_tree = predict(cv$best.tree, heart.test)[,2]

classes = levels(heart.test$HD)
# Convert probabilities to best guesses at classes
pred = factor(prob_tree > 1/2, c(F,T), classes)

#Gets summary of performance stats
my.pred.stats(prob_tree, heart.test$HD)

#Load 69th patient data and predict with tree and logistic regression
patient69 <- heart.test[69,]
prediction_tree <- predict(cv$best.tree, patient69)
prediction_glm <- predict(glm.fit.bic, patient69)

# Set the number of bootstrap replications
B <- 5000

# Perform the bootstrap procedure with the bca option
library(boot)
set.seed(123)


# Define a function to fit the logistic regression model and predict the probability of heart disease for the 69th patient
logistic_model <- function(data, indices) {
  # Fit the logistic regression model using the predictors selected by BIC
  model <- glm(HD ~ CP + THALACH + OLDPEAK + CA + THAL, data[indices,c("CP", "THALACH" , "OLDPEAK", "CA", "THAL","HD") ], family = binomial())
  # Predict the probability of heart disease for the 69th patient in the test data
  predict(model, newdata = patient69, type = "response")
}

# Set the number of bootstrap replications
B <- 5000

# Perform the bootstrap procedure with the bca option
library(boot)
set.seed(123)
boot_results <- boot(heart.test, logistic_model, R = B)

#get 95% confidence interval
boot_ci <- boot.ci(boot_results,.95, type = "bca")

# ----------------------------------------------------------------------------------
# Question 3
# ----------------------------------------------------------------------------------
# Load datsets
library(kknn)
ms.measured <- read.csv("ms.measured.2023.csv", header=T)
ms.truth <- read.csv("ms.truth.2023.csv", header=T)

#3.1

# Define the range of k values to test


k_values <- 1:25

mse <- function(y_true, y_pred) {
  mean((y_true - y_pred)^2)
}

mse_values <- sapply(k_values, function(k) {

  # Predict the intensity values for the test data
  ytest.hat = fitted( kknn(ms.measured$intensity ~ ., ms.measured, ms.truth,
                           kernel = "optimal", k = k) )
  # Compute the mean squared error between the predicted and true values
  mse(ms.truth$intensity, ytest.hat)
})

# Plot the mean squared error against the various values of k
plot(k_values, mse_values, type = "l",xlab = "k", ylab = "Mean Squared Error")
title(  "K vs MSE for spectrum intensity predictions")

# Define the range of k values to test
new_k_values <- c(2, 5, 10, 25)


# Loop over each value of k and plot the results
for (k in new_k_values) {
  # Predict the intensity values for the test data
  ytest.hat = fitted( kknn(ms.measured$intensity ~ ., ms.measured, ms.truth,
                           kernel = "optimal", k = k) )
  
  # Compute the mean squared error between the predicted and true values
  mse_value <- mse(ms.truth$intensity, ytest.hat)

  # Create a plot of the results
  dev.new()
  plot(ms.measured$MZ, ms.measured$intensity, col = "blue", pch = 16, xlab = "MZ", ylab = "Intensity", main = paste0("k-NN Regression (k = ", k, ", MSE = ", round(mse_value, 2), ")"))
  lines(ms.truth$MZ, ms.truth$intensity, col = "black", lwd=2)
  lines(ms.truth$MZ, ytest.hat, col = "red", lwd = 2, lty = 2)
  legend("topright", legend = c("Training Data", "True Spectrum", "Estimated Spectrum"), pch = c(16, NA, NA), col = c("blue", "black", "red"), lty = c(NA, 1, 2))
}

#3.6 compute optimal k values from 1 to 25
model = train.kknn(ms.measured$intensity ~ ., data = ms.measured, kmax = 25, kernel="optimal")
optimal_k = model$best.parameters$k

optimal_y_hat = fitted( kknn(ms.measured$intensity ~ ., ms.measured, ms.truth,
                         kernel = "optimal", k = optimal_k) )



# Compute the mean squared error between the predicted and true values
optimal_mse_value <- mse(ms.truth$intensity, optimal_y_hat)


#find the max mz, start with the index of the max in our fitted function
max_index <- which.max(optimal_y_hat)
max_mz <- ms.truth$MZ[max_index]
max_mz
#check this aligns by eye
plot(ms.truth$MZ,optimal_y_hat)


#Get the index to use for the max identified MZ 
index <- which(ms.truth$MZ == 7963.3)

# Define a function to fit the a knn model and get a prediction for the 7963.3 MZ value
knn_model <- function(data, indices) {
  # Fit the predictions for knn
  predictions <-  fitted(kknn(intensity ~ MZ, data[indices, c("MZ", "intensity")], ms.truth,
                                       kernel = "optimal", k = k ) )

  predictions[index]
}

set.seed(123)

k  = 3
#get the results of the sample trials
boot_results <- boot(ms.measured, knn_model, 6000)
#get 95% confidence interval and store in the boot_ci array
k3_ci = boot.ci(boot_results,.95, type = "bca")
k3_ci

k = optimal_k
#get the results of the sample trials
boot_results <- boot(ms.measured, knn_model, 6000)
#get 95% confidence interval and store in the boot_ci array
k6_ci =boot.ci(boot_results,.95, type = "bca")
k6_ci

k = 20
#get the results of the sample trials
boot_results <- boot(ms.measured, knn_model, 6000)
#get 95% confidence interval and store in the boot_ci array
k20_ci= boot.ci(boot_results,.95, type = "bca")
k20_ci


