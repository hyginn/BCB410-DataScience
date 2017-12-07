# EDA-MOD-Linear.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-MOD-Linear unit.
#
# Version:  0.1
#
# Date:     2017  12  05
# Author:   Vicky Lu (vicky.lu@mail.utoronto.ca)
#
# Versions:
#           0.1    Template of Linear Regression
#           0.2    Initial Version
#           0.3    Final Version

#
# License:  GPL-3  https://www.gnu.org/licenses/gpl-3.0.en.html
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================

# We will download ggplot2 that is similar to R's plotting system plot().
# This library will be used to display the models.

if (!require(ggplot2, quietly=TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# We will also download this library gridExtra to perform grids on
# models.
library(gridExtra)


# ====================================================================
# Part 1: Simple Linear Regression
# ====================================================================

# Linear regression is used to see if the values in the response variable y
# can be predicted to change systematically with the predictor variable x.
# The simple linear regression model is in the form Y = B_0 + B_1 X_i + e
# where Y is the response variable, B_0 is the intercept, B_1 is the slope
# coefficient, and e is the error term in wchich every observed value has
# around the predicted regression line.

sampleSize <- 50
set.seed(123)
# Generate random uniformed x values in the interval (min, max)
x <- runif(sampleSize, min=0, max=75)

# Generate hypothetical y values according to a simple linear equation
y <- 1.5 * x + 1
# Add errors to the y value by adding normalized value with a mean and standard deviation
y <- y + rnorm(sampleSize, mean=10, sd=15)

# Let's see the generated data.
# Store in a data frame so we can plot it
dataDf <- data.frame(x=x, y=y)

linearPlot <- ggplot(dataDf, aes(x, y)) +
  geom_point(colour="dodgerblue",alpha=0.75) +
  geom_point(aes(x=x, y=y), size=2, colour="#993399") +
  xlab("speed (mph)") + ylab("distance(ft)") +
  ggtitle("Synthetic Data (Linear Data)")
print(linearPlot)


# Just by looking at the plot, do you think there is a relationship between
# these variables?
# ...
# Yes there seems to be some sort of increase in the Y variables as X increases
# As a whole the data kind of point upwards

# R has a linear model function that constructs the linear model
# Save it to a variable so that we could use in subsequent calculations
# without having to retype the entire lm() function.
fit1<- lm(y~x)
fit1
# Let's plot our regression line in our model

linearPlot + geom_abline(intercept=coef(fit1)[1], slope=coef(fit1)[2], size=1, colour = "#339900")

save(linearPlot, file = "/tmp/simple.rda")


# In R, the X in the summary of coefficients is the slope.
# Here you see the intercept (B_0) is 8.865 and the slope (we call it B_1) is 1.576
# You could see that for each increment of X there is almost a corresponding
# increase in Y.
# The regression line describes the global trend in the data pretty well
# Is this significant?
# We want to first check if the assumptions are satisfied.

# ==========
# Part 1.1 Checking Assumptions
# ===========

# To observe linearity let's plot residuals vs. predictor
# Grabbing the residuals to res
res <- resid(lm(fit1))
res

# Now let's plot the residuals and predictor to help examine whether the
# linear model is appropriate or not
plot(res~x)
cor(res,x) # -3.405849e-16
# The residuals are not correlated with the predicted values, therefore it is homoscedastic.
# It looks like it is scattered randomly. We could say that it is appropriate.

# Let's observe multivariate normality.
# R provides a series of four plots to look at when you plot a variable
# specified by lm().
# Using this command plot(model name)
par(mfrow = c(1,2))
plot(fit1, which=c(1,2))

# The first plot is Residuals vs Fitted. It is randomly scattered and therefore it is
# not heteroscedastic.
# If it was a U shape then there is curvilinear shape and linearity is not met
# If it has a cone shape, then constant variance of the regression analysis is not met
# Here it shows that the residuals are distributed evenly around the 0.

# The second plot is Normal Q-Q (standardized residuals vs theoretical qunatiles lm(y~x))
# It is used to check for normality of the residuals. It shows that the residuals are
# the values of a normal distribution, therefore it is normally distributed.
# It may not be necessary to improve the regression model.

# We could reproduce the first:
lo2 <- loess(resid(fit1) ~ fitted(fit1), degree = 1, span=0.8)
plot(fitted(fit1), resid(fit1))
lines(fitted(fit1), predict(lo2), col='red', lwd=2)
abline(a=0, b=0, lty=2)

# Overall, it shows that it is a good fit.


# ==============
# Part 1.2 Analyze and Interpret Simple Linear Regression
# ==============

# We good take a look at the correlation of our data points.
cor(x,y) # 0.92
# Wow it seems like the two variables have a high correlation with each other.
# We could come up with an equation for our model.
# What is our line of best fit equation?

# To see in more details there is a function that we could use called summary
summary(fit1)

# The intercept (B_0) is 8.8647 and the slope (we call it B_1) is 1.5763
# Therefore, our line of best fit equation that determines our model is
# y = 8.8647 + 1.5763x

# The summary here shows the coefficient of determination is 0.8615. The two variables
# are approximately 86% associated and that we could correctly predict the y value given
# the x value.

# The F-statistic shows a p-value of 2.2e-16 which is less than 0.05. This means
# that the model using the predictors did a good job of predicting
# the outcome variable and that there is a signficiant relationship between
# the set of predictors and the dependent variable.

# Overall it seems like the regression line is significant.



# ===================================================================
# Part 2: Data Transformation
# ===================================================================
# Source(http://sia.webpopix.org/polynomialRegression1.html by March Lavielle)

# If you come across a model such as the example data below
# that violates the assumption of being homoscedasticty, you may want
# to do a data transformation.

sampleSize <- 50
set.seed(123)
x2 <- seq(sampleSize)
y2<- rnorm(sampleSize, 25 + 14*x2^(-0.2), 1)
dataDf <- data.frame(x=x2, y=y2)
linearPlot2 <- ggplot(dataDf, aes(x, y)) +
  geom_point(colour="dodgerblue",alpha=0.75) +
  geom_point(aes(x=x, y=y), size=2, colour="#993399") +
  xlab("speed (mph)") + ylab("distance(ft)") +
  ggtitle("Synthetic Data (Linear Data)")
print(linearPlot2)
fit2 <- lm(y2~x2)
linearPlot2 + geom_abline(intercept=coef(fit2)[1], slope=coef(fit2)[2], size=1, colour = "#339900")
plot(fit2,which=c(1))

# Here in this Residuals vs Fitted plot, you see this U shape, therefore linearity is
# not met.

# We will do a log 10 based transformation
# we will continue using the data points from above.

plot1 <- linearPlot2 + scale_x_log10()
plot2 <- linearPlot2 + scale_y_log10()
grid.arrange(plot1, plot2, nrow=1)

# If it still doesn't show much of a linear trend.
# Try log-log transformation.

print(linearPlot2 + scale_x_log10() + scale_y_log10() )

# This looks much better!

fit2.log <- lm(log(y2) ~ log(x2))
summary(fit2.log)
# The model now explains %73 of the (transformed) data.






# =====================================================================
# Part 3: Multiple Linear Regression
# =====================================================================
# Let's look at multiple variates.

# There could be other factors predictor variables that could explain
# the response variable
# Multiple Regression has a linear model with the form
# y_i = B_0 + B_1X_1 + B_2X_2 + ... + B_kX_k + e_l
# where there is k different predictor variables, each of which contributing
# to the observed value in y.

testMarks <- c(4, 30, 15, 80, 75, 85, 55, 58, 63, 72)
studyHours <- 1:10
noiseLevel <- c(100, 80, 85, 20, 25, 15, 50, 50, 30, 90)
plot(testMarks~studyHours, xlab="Studying time (hrs)", ylab="Test mark (%)",
     bty="n", col="red", pch=19,ylim=c(0,100), xlim=c(0,15))
plot(testMarks~noiseLevel, xlab="Noise Level (%)", ylab="Test mark (%)",
     bty="n", col="red", pch=19,ylim=c(0,100), xlim=c(0,100))
cbind(testMarks, studyHours, noiseLevel)

# Creating our linear model using the notation lm(Y~X1+X2)
fit3 <- lm(testMarks ~ studyHours + noiseLevel)
summary(fit3)

# Here we see an R squared of 0.8139, which says approximately
# 81% of variation in test marks can be explained by our model (study hours and noise level)
# It says that each correlation coefficient gives a measure that describes
# the association between the two variables say study hours and testmarks
# without taking into account other predictors.

# The F-statistic shows that it is 15.31 and p value of 0.002779.
# This tests the null hypothesis that all model coefficients, study hours, and noise level
# are 0.
# The residual standard error is 13.73. This shows how much the test marks deviate from the
# predicted or fitted test marks.
# The intercept of 62.5067 is the estimated mean Y value when all Xs are 0. That means
# this is the estimated mean test mark for someone with study hours and noise level of 0.
# This doesn't seem meaningful at all realistically.
# Let's try centering study hours and noise level.


# We see that the slope for study hours is 4.1397 which is the effect of study hours on
# our test marks adjusting or controlling for noise level. We associate an increase of
# one hour of studying time with an increase of 4.1397 in our marks on the test adjusting
# or controlling noise level.
# Same reasoning for the noise level.


# We want to calculate Pearson's correlation between study hours and noise level.
cor(studyHours, noiseLevel, method="pearson")
# The collinearity between study hours and noise elvel means that we should not directly
# interpet the slope, as the effect of its feature on test marks adjusting the other feature.

# The correlation is very low suggesting that the two predictors are not bounded together.


# Create the confidence intervals for the model coeifficients
confint(fit3, conf.level=0.95)
# Estimated slope of studyHours is 4.1 , we're 95% cofnfident the true slope is between
# 0.38 and 7.89.

plot(fit3)
# In the residuals vs fitted plot, it looks like the relationship between study hours, noise level
# and test marks is not linear.


# ============
# Part 3.1 Checking Assumptions
# =============

# Let's plot a scatterplot for studyhours and testmarks
plot(studyHours, testMarks)
model1 <- lm(testMarks ~ studyHours)
summary(model1)
abline(model1)
# regression line is the predicted or fitted y value or a mean of y given x
# In our summary we see that our size error is 22.66.


plot(model1, which=c(1))
# it shows a hat indicating that it is violating the linearity assumption.
# There is curviture.

# Let's look at noise levels and test marks
plot(noiseLevel, testMarks)
model2 <- lm(noiseLevel ~ studyHours)
summary(model2)

plot(model1, which=c(1))

###########################################################




#
# [END]

