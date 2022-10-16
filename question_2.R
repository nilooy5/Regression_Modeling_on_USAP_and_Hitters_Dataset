################################################
#               Question 2                     #
################################################

library(tidyverse)
library(glmnet)
library(caret)
library(ISLR)


data(Hitters, package = "ISLR")
# creating a new df to strore library data
hitters <- Hitters
head(hitters, 3)
names(hitters)

sum(is.na(hitters))
sum(is.na(hitters$Salary))
# get type of each variable
sapply(hitters, class)
# hitters$League <- as.numeric(hitters$League)
# hitters$Division <- as.numeric(hitters$Division)
# hitters$NewLeague <- as.numeric(hitters$NewLeague)
# sapply(hitters, class)
hitters$Salary[which(is.na(hitters$Salary))] <- mean(hitters$Salary, na.rm = TRUE)
sum(is.na(hitters$Salary))
max(hitters$Salary)
min(hitters$Salary)
# make boxplot of Salary
boxplot(hitters$Salary, main="Salary", xlab="Salary")
# make histogram of Salary
hist(hitters$Salary, main="Salary", xlab="Salary")

# make train and test set
set.seed(123)
# Make a reasonable train-test split of the data.
# Apply a cross-validation strategy with 5-folds.
hitters_train <- hitters %>% sample_frac(0.7)
hitters_test <- setdiff(hitters, hitters_train)
# check if train and test set are the same
identical(hitters_train, hitters_test)
dim(hitters_train)
dim(hitters_test)

# train set to get dependent variable
y_train <- hitters_train$Salary
# train set to get independent variables
x_train <- model.matrix(Salary ~ ., data = hitters_train)[,-1]
head(x_train,3)

# test set to get dependent variable
y_test <- hitters_test$Salary
# test set to get independent variables
x_test <- model.matrix(Salary ~ ., data = hitters_test)[,-1]
head(x_test,3)

# make linear regression model
lm <- train(Salary ~ .,
            data = hitters_train,
            method = "lm",
            trControl = trainControl(method = "cv", number = 5),
            preProcess = c("center", "scale"),
            tuneLength = 10
)
# predict
lm_pred <- predict(lm, newdata = as.data.frame(hitters_test))
metrics_lm <- data.frame(
  rmse = RMSE(y_test, as.numeric(lm_pred)),
  mae = MAE(y_test, as.numeric(lm_pred)),
  r2 = caret::R2(y_test, as.numeric(lm_pred))
)
metrics_lm
# checck lab 7 solution here
# set the range of lambda values, to find best lambda
# lambda_seq <- 10^seq(-2, 2, by = 0.1)
# lambda_seq <- 10^seq(-2, 2, length = 100)
ridge_cv <- cv.glmnet(x_train, y_train, alpha = 0, nfolds = 5)
ridge_cv

# best lambda value, chosen from 2 options
best_lambda <- ridge_cv$lambda.min
best_lambda

# use glmnet function to train the ridge regression
ridge_fit <- glmnet(x_train, y_train, alpha = 0, lambda = best_lambda)
# check the model
summary(ridge_fit)
# find slope estimates
coef(ridge_fit)

# make prediction
ridge_pred <- predict(ridge_fit, s = best_lambda, newx = x_test)
head(ridge_pred)

metrics_ridge <- data.frame(
  rmse = RMSE(y_test, ridge_pred),
  mae = MAE(y_test, ridge_pred),
  r2 = caret::R2(y_test, ridge_pred)
)
metrics_ridge
################################################

# lasso: find best lamda
set.seed(123)
cv_output <- cv.glmnet(x_train, y_train, alpha = 1, nfolds = 5)
cv_output
best_lam <- cv_output$lambda.min
best_lam

# build the lasso model with best lamda value found
best_lasso <- glmnet(x_train, y_train, alpha = 1, lambda = best_lam)
coef(best_lasso)

# predict
lasso_pred <- predict(best_lasso, s = best_lam, newx = x_test)
head(lasso_pred)

#metrics
metrics_lasso <- data.frame(
  rmse = RMSE(y_test, lasso_pred),
  mae = MAE(y_test, lasso_pred),
  r2 = caret::R2(y_test, lasso_pred)
)
metrics_lasso

# elastic net: find best lamda
set.seed(123)
model_elastic <- train(
    x = x_train,
    y = y_train,
    method = "glmnet",
    trControl = trainControl(method = "cv", number = 10),
    tuneLength = 10
    )
model_elastic$bestTune
coef(model_elastic$finalModel, model_elastic$bestTune$alpha)

y_pred <- predict(model_elastic, x_test)

metrics_elastic <- data.frame(
  rmse = RMSE(y_test, y_pred),
  mae = MAE(y_test, y_pred),
  r2 = caret::R2(y_test, y_pred)
)
metrics_elastic

# KNN
set.seed(123)
model_knn <- train(
    x = x_train,
    y = y_train,
    method = "knn",
    trControl = trainControl(method = "cv", number = 10),
    tuneLength = 10
    )
model_knn$bestTune
plot(model_knn)
coef(model_knn$finalModel, model_knn$bestTune$k)

y_pred <- predict(model_knn, x_test)

matrics_knn <- data.frame(
  rmse = RMSE(y_test, y_pred),
  mae = MAE(y_test, y_pred),
  r2 = caret::R2(y_test, y_pred)
)
metrics_lm
metrics_ridge
metrics_lasso
metrics_elastic
matrics_knn
# make data frame of all metrics
metrics <- data.frame()
# add cols with names
metrics <- cbind("rmse", "mae", "r2")
names(metrics) <- c("rmse", "mae", "r2")
metrics <- rbind(metrics_lm[1,])
metrics <- rbind(metrics, metrics_ridge[1,])
metrics <- rbind(metrics, metrics_lasso[1,])
metrics <- rbind(metrics, metrics_elastic[1,])
metrics <- rbind(metrics, matrics_knn[1,])
# add new col with names of models
metrics <- cbind(c("lm","ridge","lasso","elastic","knn"), metrics)
names(metrics) <- c("model", "rmse", "mae", "r2")


metrics
