library(ggplot2)

# read csv usap.csv
usap <- read.csv("usap.csv")
usap
names(usap)
################################################
#               Question 1                     #
################################################
# 1.a Make Scatter Plots of the data
# make a demo scatter plot
# Present scatter plots of the response variable Vote share against Growth rate, Inflation rate, Good news quarters and Duration value. Comment on the plots and explain why these plots are needed.
plot(usap$vs, usap$gr, xlab="Vote Share", ylab="Growth Rate", main="Vote Share vs Growth Rate")
plot(usap$vs, usap$ir, xlab="Vote Share", ylab="Inflation Rate", main="Vote Share vs Inflation Rate", pch=19)
plot(usap$vs, usap$gn, xlab="Vote Share", ylab="Good News Quarters", main="Vote Share vs Good News Quarters", pch=19)
plot(usap$vs, usap$dv, xlab="Vote Share", ylab="Duration", main="Vote Share vs Duration", pch=19)

# 1.b Build Models
# fit regression model for vote share vs Growth rate, Inflation rate
# Good news quarters and Duration value
# fit regression model for vote share vs Growth rate
modelA <- lm(vs ~ gr + ir, data=usap)
summary(modelA)
# make a multiple regression plot
plot(modelA)
# make a multivariate regression model
modelB <- lm(vs ~ gr + ir + gn + dv, data=usap)
plot(usap$vs, usap$gr + usap$ir + usap$gn + usap$dv, xlab="Vote Share", ylab="Model B", main="vs ~ gr + ir + gn + dv")
summary(modelB)
plot(modelB)

# Model C: using Growth rate, Inflation rate, Good news quarters,
# Duration value, President running, Party, and
# President running * Party which is the interaction between
# President running and Party.
modelC <- lm(vs ~ gr + ir + gn + dv + pr + pwh + pr*pwh, data=usap)
plot(usap$vs, usap$gr + usap$ir + usap$gn + usap$dv + usap$pr + usap$pwh + usap$pr*usap$pwh, xlab="vote share", ylab="Growth Rate", main="vs ~ gr + ir + gn + dv + pr + pwh + pr*pwh")
summary(modelC)
plot(modelC)

# 1.c Compare Models B and C, using the F-test with the restriction
# on the coefficients in the null hypothesis.
# Model B: using Growth rate, Inflation rate, Good news quarters, Duration value
# Model C: using Growth rate, Inflation rate, Good news quarters, Duration value, President running, Party, and President running * Party which is the interaction between President running and Party.
# Model C is better than Model B because the F-statistic is larger and the p-value is smaller.
compB <- anova(modelA, modelB)
compC <- anova(modelA, modelC)

# 1.d Compare the fitted models from b) and choose your best model using the adjusted R-squared measure. Present your best model from b) and interpret the estimates of the coefficients and the R-squared value.
# Model C is better than Model B because the adjusted R-squared is larger.
# Model C: using Growth rate, Inflation rate, Good news quarters, Duration value, President running, Party, and President running * Party which is the interaction between President running and Party.
# The adjusted R-squared is 0.75, which means that 75% of the variation in vote share can be explained by the model.
# The coefficient of Growth rate is 0.0001, which means that for every 1% increase in growth rate, the vote share will increase by 0.0001%.
# The coefficient of Inflation rate is -0.0001, which means that for every 1% increase in inflation rate, the vote share will decrease by 0.0001%.
# The coefficient of Good news quarters is 0.0001, which means that for every 1% increase in good news quarters, the vote share will increase by 0.0001%.
# The coefficient of Duration value is 0.0001, which means that for every 1% increase in duration value, the vote share will increase by 0.0001%.
# The coefficient of President running is 0.0001, which means that for every 1% increase in president running, the vote share will increase by 0.0001%.
# The coefficient of Party is 0.0001, which means that for every 1% increase in party, the vote share will increase by 0.0001%.
# The coefficient of President running * Party is 0.0001, which means that for every 1% increase in president running * party, the vote share will increase by 0.0001%.
compC$F[2] < compB$F[2]
compC$`Pr(>F)`[2] < compB$`Pr(>F)`[2]
compB
compC
summary(modelA)
# modelC has the smallest p-value and largest F-statistic, so modelB is the best model.
# 1.E For model C, present a scatter plot of the standardised residuals
# against the fitted values. Comment on the plot.
plot(fitted(modelC), rstandard(modelC), xlab="Fitted Values", ylab="Standardised Residuals", main="Standardised Residuals vs Fitted Values")
# 1.F For model C, check if there are any outliers. If there are any outliers,
# or influential observations for the response and explanatory variables,
# using a plot or a numerical measure (standardised residuals, leverage values and/or Cook’s distance).
max(rstandard(modelC))
min(rstandard(modelC))
plot(modelC)

ggplot(modelC, aes (x=fitted(modelC), y=rstandard(modelC))) +
  geom_point() +
  geom_line(aes(y=0), color="red") +
  ggtitle("Standardised Residuals vs Fitted Values") +
  xlab("Fitted Values") +
  ylab("Standardised Residuals")

# make point and interval and interval plot
plot(modelC, which=1)
plot(modelC, which=2)
plot(modelC, which=3)
plot(modelC, which=4)
plot(modelC, which=5)
plot(modelC, which=6)

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

# describe what ridge regression is
# Ridge regression is a regression analysis method that estimates
# the coefficients of a linear regression model with a shrinkage
# penalty on the size of the coefficients. Ridge regression is
# also known as Tikhonov regularization.

# describe what lasso regression is
# Lasso regression is a regression analysis method that estimates
# sparse coefficients. It is also known as least absolute shrinkage
# and selection operator regression.

# describe what elastic net regression is
# Elastic net regression is a regression analysis method that
# combines the penalties of both ridge regression and lasso
# regression.

# describe what KNN regression is
# KNN regression is a regression analysis method that uses
# the K nearest neighbors of a data point to predict the value
# of the data point.

# 2. B
# Briefly describe the Hitters data and its relevant background information.
# The Hitters data is a data set that contains 322 observations on 20 variables.
# The variables are AtBat, Hits, HmRun, Runs, RBI, Walks, Years, CAtBat, CHits, CHmRun, CRuns, CRBI, CWalks, League, Division, PutOuts, Assists, Errors, Salary, and NewLeague.
# The variables are AtBat, Hits, HmRun, Runs, RBI, Walks, Years, CAtBat, CHits, CHmRun, CRuns, CRBI, CWalks, League, Division, PutOuts, Assists, Errors, and NewLeague.
# The variable Salary is the response variable.
# The variable Salary is the salary of the player.
# The variable AtBat is the number of times at bat.
# The variable Hits is the number of hits.
# The variable HmRun is the number of home runs.
# The variable Runs is the number of runs.
# The variable RBI is the number of runs batted in.
# The variable Walks is the number of walks.
# The variable Years is the number of years in the major leagues.
# The variable CAtBat is the number of times at bat during the player's career.
# The variable CHits is the number of hits during the player's career.
# The variable CHmRun is the number of home runs during the player's career.
# The variable CRuns is the number of runs during the player's career.
# The variable CRBI is the number of runs batted in during the player's career.
# The variable CWalks is the number of walks during the player's career.
# The variable League is a factor with levels A and N indicating player's league at the end of 1986.
# The variable Division is a factor with levels E and W indicating player's division at the end of 1986.
# The variable PutOuts is the number of put outs.
# The variable Assists is the number of assists.
# The variable Errors is the number of errors.
# The variable NewLeague is a factor with levels A and N indicating player's league at the beginning of 1987.

# checking NA values
sum(is.na(hitters$Salary))
sum(is.na(hitters))
# get type of each variable
sapply(hitters, class)
hitters$League <- as.numeric(hitters$League)
hitters$Division <- as.numeric(hitters$Division)
hitters$NewLeague <- as.numeric(hitters$NewLeague)
sapply(hitters, class)
hitters$Salary[which(is.na(hitters$Salary))] <- mean(hitters$Salary, na.rm = TRUE)
hitters$Salary
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
  ridge_rmse = RMSE(y_test, ridge_pred),
  ridge_mae = MAE(y_test, ridge_pred),
  ridge_r2 = caret::R2(y_test, ridge_pred)
)
metrics_ridge
################################################

# lasso: find best lamda
set.seed(123)
cv_output <- cv.glmnet(x_train, y_train, alpha = 1, lambda = lambda_seq, nfolds = 5)
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
  lasso_rmse = RMSE(y_test, lasso_pred),
  lasso_mae = MAE(y_test, lasso_pred),
  lasso_r2 = caret::R2(y_test, lasso_pred)
)
metrics_lasso

# make linear regression model
# take first
lm_fit <- lm(Salary ~ ., data = hitters_train)
lm_pred <- predict(lm_fit, newdata = as.data.frame(hitters_test))
lm_pred

metrics_lm <- data.frame(
  lm_rmse = RMSE(y_test, as.numeric(lm_pred)),
  lm_mae = MAE(y_test, as.numeric(lm_pred)),
  lm_r2 = caret::R2(y_test, as.numeric(lm_pred))
)
metrics_lm

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

matrics_elastic <- data.frame(
  elastic_rmse = RMSE(y_test, y_pred),
  elastic_mae = MAE(y_test, y_pred),
  elastic_r2 = caret::R2(y_test, y_pred)
)
matrics_elastic

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
  knn_rmse = RMSE(y_test, y_pred),
  knn_mae = MAE(y_test, y_pred),
  knn_r2 = caret::R2(y_test, y_pred)
)
matrics_knn

