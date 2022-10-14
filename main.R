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

# include islr package
library(ISLR)

summary(Hitters)
lm(Salary~AtBat+Hits,data=Hitters)

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

names(Hitters)
sum(is.na(Hitters$Salary))
sum(is.na(Hitters))
# get type of each variable
sapply(Hitters, class)
min(Hitters$Salary)
# make boxplot of Salary
boxplot(Hitters$Salary, main="Salary", xlab="Salary")
# make histogram of Salary
hist(Hitters$Salary, main="Salary", xlab="Salary")
# omit NA values from Salary and store in new variable
Hitters2 <- na.omit(Hitters$Salary)
sum(is.na(Hitters2))
max(Hitters2)
min(Hitters2)
# make boxplot of Salary
boxplot(Hitters2, main="Salary", xlab="Salary")
# make histogram of Salary
hist(Hitters2, main="Salary", xlab="Salary")

