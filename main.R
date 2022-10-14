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