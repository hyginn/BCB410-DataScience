# ___ID___.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-MOD-Generalized unit.
#
# Version:  0.2
#
# Date:     2017  10    11
# Author:   Walter Nelson <walterj.nelson@mail.utoronto.ca>
#
# Versions:
#           0.1    Template copy
#           0.2    Initial version

#
# TODO:
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================

###   NOTE  ###########################################
### Do not forget to read the accompanying wiki page! #
#######################################################

# = 1 Poisson regression: data synthesis
#
#       We're going to work with synthetic data for this unit. Our data will
#       highlight the various types of generalized models available to us.
#       Some of the data will be biologically focused, some will not. Our first
#       dataset will highlight the need for generalized models over simple
#       general linear models.
#
#       Suppose we're investigating 25 cancer drugs. Specifically, we're
#       interested in the role they play in cell death. We have several
#       independent variables:
#           * Initial average cell size
#           * Patient age
#           * Expression data for 15 genes
#           * Drug concentration
#       We have this data for 30 patients for each of the drugs (i.e. 750
#       patients total). Note that these are all continuous variables. Some of
#       these variables may turn out to be insignificant; not all the drugs will
#       be affected by the same genes' expression levels, for example.
#
#       In this simple mock experiment, each condition begins with 100
#       'identical' (across conditions) cells. We're interested in predicting
#       the number of cells remaining after treatment. A possible application
#       Would be determining the appropriate amount of drug given these
#       measurements for a new patient.

# Assign each drug a letter
drugs <- 0
for (i in 1:25) {
    drugs[i] = LETTERS[i]
}

# Concentrations
concentrations <- c(0.0, 0.10, 0.25, 0.50, 0.75, 0.90, 1.0)

# Generate the patient data
patientMatrix <- matrix(nrow=750, ncol=18)
for (i in 1:750) {
    d <- c()
    # Average cell size in micrometers
    patientMatrix[i, 1] = rnorm(1, mean=45, sd=5)
    # Patient age
    patientMatrix[i, 2] = rnorm(1, mean=50, sd=20)
    # Expression data
    patientMatrix[i, 3:17] = rnorm(15, mean=2, sd=1.25)
    # Drug concentration
    patientMatrix[i, 18] = sample(concentrations, 1)
}

# Assign 30 patients to each drug. Each drug has its own properties: varying
# degrees of dependence on each of the independent variables we specified in
# the patient matrix.
startIndex <- 1
results <- list()
for (drug in drugs) {
    results[[drug]] <- matrix(nrow=30, ncol=19)
    patients <- patientMatrix[startIndex:(startIndex+29), 1:18]
    cellSizeEffect = rnorm(1, mean=.25, sd=0.05)
    patientAgeEffect = rnorm(1, mean=.25, sd=0.05)
    expressionEffects = rnorm(15, mean=0, sd=0.1)
    drugConcentrationEffect = rnorm(1, mean=1, sd=0.25)

    # Addd an error term - this could represent unknown independent variables
    # and/or measurement errors
    errors = rnorm(30, mean=0, sd=4)

    cellsRemaining = floor(100 - cellSizeEffect * patients[, 1] -
      patientAgeEffect * patients[, 2] - patients[, 3:17] %*% expressionEffects -
      - drugConcentrationEffect * patients[, 18] * 5 - errors)
    results[[drug]][, 1] <- cellsRemaining
    results[[drug]][, 2:19] <- patients
}

dataFrames <- list()
for (drug in drugs) {
    dataFrames[[drug]] <- data.frame(Y=results[[drug]][, 1], avgCellSize=results[[drug]][, 2],
             patientAge=results[[drug]][, 3], exprData=results[[drug]][, 4:18],
             drugConcentration=results[[drug]][, 18])
}

# = 2 Poisson regression: fitting the models
#
#     Now we have our data for our 25 drugs. Let's see why the generalized
#     linear model works best as a predictor for our data.

# Okay, our data frames are created. Have a peek at one of them to get an
# idea of what we're working with.
dataFrames$A

# Now, we're interested in predicting Y from the other variables in the
# data frame. We might be able to do this with a simple linear model. Let's
# try it on the first drug.
formula <- as.formula(paste("Y ~ avgCellSize + patientAge + drugConcentration +",
                            paste(xnam, collapse="+")))
lmFit <- lm(formula, data=dataFrames$A)

# Since we're modeling Y as counts, it would make sense to model this as a
# Poisson regression, a specific type of generalized linear model. Let's try
# it and compare the results.
glmFit <- glm(formula, data=dataFrames$A, family=poisson())

# Compare the ANOVA outputs of the two models.
summary(lmFit)
summary(glmFit)
# Notice that we've cut the deviance by nearly 60% by using the proper
# distribution. OK, but this could be one-off. Let's compare the rest
# of the 25 fits.

lmFits <- list()
lmDeviances <- c()
glmFits <- list()
glmDeviances <- c()
i <- 1
for (drug in drugs) {
    lmFits[[drug]] = lm(formula, data=dataFrames[[drug]])
    lmDeviances[i] <- deviance(lmFits[[drug]])
    glmFits[[drug]] = glm(formula, data=dataFrames[[drug]], family=poisson())
    glmDeviances[i] <- deviance(glmFits[[drug]])
    i <- i + 1
}

improvements <- lmDeviances - glmDeviances
resultsDF <- data.frame(lmRes=lmDeviances, glmRes=glmDeviances,
                        improvements=improvements)

# As we can see, by using the proper distribution for our GLM we improve the
# fit by an order of magnitude (as measured by the relatively simple statistic
# of deviance).
resultsDF

# = 3 Mixed-effects models: data synthesis
#
#       Mixed-effects models are a powerful tool which combine known
#       observations (fixed effects) with our knowledge of how the data is
#       affected by random processes (random effects). By combining these two,
#       we can perform more accurate predictions.
#
#       In this example, we'll synthesize fairly simple data. Our goal will
#       be to predict height based on age. A mixed-effects model is a good
#       choice for this data set because much of what determines height is
#       best modeled as a random effect (everyone grows at a different rate).

ages <- c(12, 14, 16, 18, 20)
averageMaleGrowths <- c(12.5, 10, 4.5, 0.5)
averageFemaleGrowths <- c(10, 3.8, 0.1, 0.1)

heightMatrix <- matrix(nrow=1000, ncol=3)
# Generate 5 timepoints for each of 100 males
for (i in 1:100) {
    growthRate = rnorm(1, mean=1, sd=.2)
    for (m in 1:5) {
        j = (i - 1) * 5 + m
        # Individual id
        heightMatrix[j, 1] = i
        # Age
        heightMatrix[j, 2] <- ages[m]
        # Height (cm)
        if (m == 1) {
            heightMatrix[j, 3] <- rnorm(1, mean=149, sd=7.6)
        }
        else {
            jitter = rnorm(1, mean=0.75, sd=0.05)
            heightMatrix[j, 3] <- heightMatrix[j - 1, 3] + jitter
                                    growthRate * averageMaleGrowths[m - 1]
        }
    }
}
# Females
for (i in 1:100) {
    growthRate = rnorm(1, mean=1, sd=.2)
    for (m in 1:5) {
        j = 500 + (i - 1) * 5 + m
        # Individual id
        heightMatrix[j, 1] = 100 + i
        # Age
        heightMatrix[j, 2] <- ages[m]
        # Height (cm)
        if (m == 1) {
            heightMatrix[j, 3] <- rnorm(1, mean=149, sd=7)
        }
        else {
            jitter = rnorm(1, mean=0.5, sd=0.05)
            heightMatrix[j, 3] <- heightMatrix[j - 1, 3] + jitter +
                                    growthRate * averageFemaleGrowths[m - 1]
        }
    }
}

# Now we'll put this data matrix into a data frame, and include the sex
# data
heightDF <- data.frame(id=heightMatrix[, 1], age=heightMatrix[, 2],
                       sex=c(rep("m", 500), rep("f", 500)),
                       height=heightMatrix[, 3])

# = 4 Mixed-effects models: fitting the models
#
#       We'll first investigate a very simple model, and add complexity
#       (and hopefully accuracy) from there.

# We'll use the lme4 package, which is the standard R package for fitting
# mixed-effects models.
install.packages("lme4")
library(lme4)

# Let's fit a basic model with a single random intercept term (for sex).
basicModel = lmer(height ~ age + (1|sex), data=heightDF)

# What faulty assumptions does this make? It assumes a few things that we
# can correct in our next model:
#     * The growth rate of males and females is the same
#     * The growth of any 2 males or any 2 females has the same intercept
# These are obviously faulty assumptions. Let's correct them now:
betterModel = lmer(height ~ age + (1 + age|sex) + (1|id), data=heightDF)

# Let's compare the summaries of the two models
summary(basicModel)
summary(betterModel)

# A few key points:
#   * In the first model, the (1|sex) term specifies that each gender will
#     have its own intercept when fitting the model.
#   * In the second model, the (1|id) term means that each individual will
#     have its own intercept when fitting the model.
#   * In the second model, the (1 + age|sex) term specifies that each sex
#     will have its own intercept, but that the slope of the prediction
#     as a function of age will be determined by the sex of each individual.
#
# Some of these points will become more clear when we look at the coefficients
# of the models. In the basic model, we see that each gender has it's own
# intercept, but that the prediction for both sexes has the same slope.
coef(basicModel)
# In the better model, we see that each individual does indeed have its own
# intercept. Additionally, each gender has its own intercept which represents
# the mean of the intercepts as determined by the individuals in that class.
# We can also see that the prediction as a function of age differs based on
# gender (i.e., a random slope).
coef(betterModel)

# [END]
