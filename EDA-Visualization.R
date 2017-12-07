# EDA-Visualization.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the ___ID___ unit.
#
# Version:  0.1
#
# Date:     2017  12  06
# Author:   Zhifan (roger.wu@mail.utoronto.ca)
#
# Versions:
#           0.2    (Final version)

#
# TODO: Finish Task 1,2 and 3.
#       Try out the exercies 1, 2, 3.
#       Have fun!
#
#
# License:  GPL-3  https://www.gnu.org/licenses/gpl-3.0.en.html
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works. Feel free to edit code to refresh your thoughts
# or to experiment with options, or just play (Have fun is very very important).
#
#
# ==============================================================================


# = 1 EDA visualization with basic R functions
# This is a very basic tutorial of EDA visualization.


# This tutorial is inspired by the following two online source tutorials.
# http://www.hcbravo.org/IntroDataSci/lecture-note/eda/visualization_eda/
# and http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html#introduction
# Remeber check them out, both are very very informatic.
# I changed and combined to make this tutorial more related to our porject.


# [Start]



# == 1.1 Load all the library.==============================
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require(ggrepel)) {
  install.packages("ggrepel")
  library(ggrepel)
}
if (!require(scales)) {
  install.packages("scales")
  library(scales)
}

# == 1.2 Visualization on single data=======================
#### Start ####
# First we load the data, note that the row 2 has a blank ID_Ref
# For easy use, I will delete row 2.
# Load the filepath
filePath <- "./data/GSM112133.csv" # change to your own address
# Read the head
headers <- read.csv(filePath, header = F, nrows = 1,stringsAsFactors = FALSE)
GSMdata <- read.csv(filePath, skip = 2, header = F)
colnames(GSMdata) <- headers
rownames(GSMdata) <- GSMdata[,1]
GSMdata$ID_REF <- as.character(GSMdata$ID_REF)
GSMdata$VALUE <- as.numeric(as.character(GSMdata$VALUE))
# Nota that NA is introduced, let's remove NAs
GSMdata <- na.omit(GSMdata)
# check
head(GSMdata)
str(GSMdata)
# This is a little bit of clean the data to get a better visualization view
# Remember, IN EDA VISULIZATION, we assume all input data are clean.
# If you are interested in Data clean, here are one great source
# http://www.hcbravo.org/IntroDataSci/lecture-note/datamodels/tidy_data/



# Ok now let's do some basic visualization

# Check the default settings first:
?plot.default

#Plot
plot(GSMdata$VALUE)
# This is a very basic visualization of the data.
# Though it is not particular useful, it didn't have a structure and users cannot
# get many information about the datasets from the graph.
# We can try to do some changes on the orders of the points and on the graphical representation.
plot(sort(GSMdata$Ratio), ylab="Raio")
# Hmm, at least provide some sort of information, quite concentrated again.
# While thinking of central tendency, spread and skew of the datasets,
# the result we got was still not useful.
# And how about the histogram? Histogram divides the range of the variable into equal-sized bins
# and then plots the number of observations within each bin. Try the following:
length(GSMdata$VALUE) # Get the length
range(GSMdata$VALUE) # Get the range

#Set the breaks
br <- seq(-2, 2, length.out = 6219)
hist(GSMdata$VALUE, xlab="Value", freq = FALSE, breaks = br,
     main = "Value of GSA datasets", col = "red")

# Note that when deal with large amount of data, for example,
# we have 6219 here, it is useful to set the freq=FALSE, and thus the
# y-axis would show the density of the data.


# Feel free to test with other variables in the dataset!
# Always play around to get yourself more familiar with visualization functions.
# Task1:
#     Try to histogram out value in the datasets.



# One useful funtion to use in histgram is rug.
# check ?rug for information
# Basicly, rug will put all the appearance of the data apperance under the x-axis
# It can help you better locate which range is the data most located
# in the case that the histogram didn't provide much useful information.
rug(GSMdata$VALUE)
# We can use summary to check for the statistical information of the dataset
summary(GSMdata$VALUE)
# add a normail distribution curve onto our hitogram.
curve(dnorm(x, mean=mean(GSMdata$VALUE), sd=sd(GSMdata$VALUE)), add=TRUE, col="white", lwd=2)
# Another useful function is abline.
# We can use this to set a line of particular value of interested.
# see ?abline for details
abline(v = mean(GSMdata$VALUE), col = "dark green", lwd = 2)
# We can simply interpert with other useful value we are interested
# For examply here, I want to see how many VALUE are beyond the mean of Ration
abline(v = mean(GSMdata$Ratio), col = "yellow", lwd = 2)

# Task2:
#      Add abline and rug to your histogram and play around
#      Have fun.

# Here is a very simple histogram generated from rnorm(50)
# It may give you some more ideas of histogram, like we can put the text on histogram etc
# cf. https://github.com/hyginn/R_EDA-Introduction
# check out at our course main page, link name is R-EDA_Introduction
set.seed(950921)
x <- rnorm(50)
hist(x, breaks = 5)
# add a stripchart() of the actual values
stripchart(x, pch="|", add=TRUE, col="red3", xlim=c(-3, 3), at=-0.5)
# Note that rug provide similar information!

# we can explicitly set breakpoints in a vector:
# here we set them at 0.5 sigma intervals from
# -3 to 3

s <- 1.0
hist(x, breaks=seq(-3*s, 3*s, by=0.5*s))

# we can color the bars individually...
# My personally don't like to set the color in this way,
# It would take times to search for what exactly the color is
# I would prefer to set with color names...
# Anyway you can use colors() to find a color you like
colors()

hcol <- c("#4F47FD", "#6982FC", "#8AA6EF", "#AFBBDB", "#BEBEBE", "#A9A9A9",
          "#A9A9A9", "#BEBEBE", "#DBBBAF", "#EFA68A", "#FC8269", "#FD474F")

# Most parameters of a generic plot apply.
h <- hist(x, breaks=seq(-3*s, 3*s, by=0.5*s),
          col=hcol,
          main="",
          xlab=expression(sigma),
          ylab="Counts")

# ... and we can add the individual counts to the plot.
text(h$mids, h$counts, h$counts, adj = c(0.5, -0.5), col = hcol)

# Pretty good examply, very much thanks to our professor


# Now let's try boxplot, shall we?

?boxplot # In case you want see more details
boxplot(GSMdata$VALUE, main = "boxplot")
# That’s not very clear to see, so let’s do a transformation of this data to see this better:
boxplot(log(abs(GSMdata$VALUE)), col = c("sienna"), main = "boxplot")

# Yet another nice simple example of boxplot from our professor
# cf. https://github.com/hyginn/R_EDA-Introduction
# check out at our course main page, link name is R-EDA_Introduction
x <- rnorm(200)
boxplot(x)

m <- x
m <- cbind(m, x^2)
m <- cbind(m, x^3)
m <- cbind(m, x^4)
m <- cbind(m, x^5)
boxplot(m)
boxplot(log(abs(m)), col = c("red","sienna","palevioletred1","royalblue2","blue"))



# == 1.3 Visualizatio of pairs of variables============================

# Crated datasets
data <- GSMdata[1:100, c("VALUE", "Ratio")]
boxplot(data, main = "boxplot", las = 2)

# That’s not very clear to see, so let’s do a transformation of this data to see this better:
# Note that las is use to set the name of x-axis to be vertical shown, try to delete and see what happened
# It is useful when you have say 10 boxplot, names always overlap with each other.

boxplot(log(abs(data)), col = c("sienna","palevioletred1"), main = "boxplot", las = 2)

# For pairs of continuous variables, the most useful visualization is the scatter plot.
# This gives an idea of how one variable varies conditioned on another variable.
plot(GSMdata$VALUE, GSMdata$Ratio, xlab="Value", ylab="Ratio")
# Let's move the default axis
plot(GSMdata$VALUE, GSMdata$Ratio, xlab="Value", ylab="Ratio", axes = FALSE)
# add back
axis(1)
axis(2)

# do the reverse version for how to use ylim
# Also show the usage of par and opar
?par
# set window background and plotting axes via par
opar <- par(bg="steelblue", fg="lightyellow")
plot(GSMdata$VALUE, GSMdata$Ratio, xlab="Value", ylab="Ratio",
     xlim = c(-2, 2), ylim = c(100, 0), axes = FALSE)
axis(1)
axis(2)
# Reset the background
par(opar)
# Let check it.
plot(GSMdata$VALUE, GSMdata$Ratio, xlab="Value", ylab="Ratio",
     xlim = c(-2, 2), ylim = c(100, 0), axes = FALSE)
axis(1)
axis(2)
# Note we can change the default axis
# Check out ?axis
plot(GSMdata$VALUE, GSMdata$Ratio,
     xlab="Value", ylab="Ratio",
     xlim = c(-2, 2), ylim = c(100, 0),
     cex.main=1.3,
     main = "Value and Ration/n relationship",
     cex.sub=0.75,
     col.sub="grey",
     sub = "scatter plot")
# Add the grid
?grid
grid()

# ==1.4 Plot of X-Y-Z diagram
# This part is cf. https://github.com/hyginn/R_EDA-Introduction
# check out at our course main page, link name is R-EDA_Introduction


# Very very very cool demo
?persp
demo(persp)


# see contour
?contour
# Example (mostly taken from the help page)
x <- -6:16
op <- par(mfrow = c(2, 2))
contour(outer(x, x), method = "edge", vfont = c("sans serif", "plain"))
z <- outer(x, sqrt(abs(x)), FUN = "/")
image(x, x, z, col=colorRampPalette(c("#301C25", "#F53A43", "#FFC754", "#FEFDDE"))(60))
contour(x, x, z, col = "pink", add = TRUE, method = "edge",
        vfont = c("sans serif", "plain"))
contour(x, x, z, ylim = c(1, 6), method = "simple", labcex = 1)
contour(x, x, z, ylim = c(-6, 6), nlev = 20, lty = 2, method = "simple")
par(op)


# Here is a good example again provided by our professor,
# cf. https://github.com/hyginn/R_EDA-Introduction
# could not produce a better one...

# Load the data
# GvHD flow cytometry data is a sample dataset provided in the project.
gvhd <- read.table("./data/GvHD.txt", header=TRUE)
head(gvhd)

# Only extract the CD3 positive cells
gvhdCD3p <- as.data.frame(gvhd[gvhd[, 5]>280, 3:6])

# Scatter plots are extremely useful, but learning
# a bit about R's plotting capabilities will help
# tremendously to create INFORMATIVE plots.

# Some special packages
# The overlap in the GvHD data can obscure
# data relationships. Here are some alternatives
# for dense plots:

# load "hexbin" package from CRAN
if (!require(hexbin, quietly=TRUE)) {
  install.packages("hexbin")
  library(hexbin)
}

# variant one: hexbin
hb <- hexbin(gvhdCD3p[, 1:2], xbins = 20)
plot(hb, colramp = colorRampPalette(c("#FFFFDD",
                                      "#77EE99",
                                      "#3377AA",
                                      "#0033BB")))

# === variant two: smoothScatter
smoothScatter(gvhdCD3p[, 1:2],
              nrpoints = 0,
              pch=20,
              cex=0.5,
              col="#6633BB55")

# === variant three: colors vary by density
plot(gvhdCD3p[, c(1,2)],
     col=densCols(gvhdCD3p[, 1], gvhdCD3p[, 2]),
     pch=16,
     cex=2)

# === variant four: analysis with specialized package
# load "prada" package from BioConductor
if (!require(prada, quietly=TRUE)) {
  source("http://www.bioconductor.org/biocLite.R")
  biocLite("prada")
}

# using functions ?fitNorm2 and plotNorm2 from the prada package
nfit <- fitNorm2(gvhdCD3p[, 1], gvhdCD3p[, 2])
plotNorm2(nfit, selection=TRUE, ellipse=TRUE)






# = 2 Using ggplot2 to get better visualization======================
# NOTE !!!!!!!!!!!!!!!!!!
# The following code is impired and cf. by http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html
# GO BACK TO THE ORIGINAL TUTORIAL if you are not in BIOINFORMATIC related usage of ggplot2
# Because I change the original to satisfied my projective, to more related to
# BIOINFORMATICS and COMPUTATIONAL BIOLOGY




# While we have seen a basic repertoire of graphics,
# it’s easier to proceed if we have a bit more formal way of thinking about graphics and plots.
# Here I will use the grammar of graphics implemented in R by the package ggplot2.


# Let's look at housing prices and GSM112133.csv data.
# I will mainly using housing data as the example, and I do try the GSM data and it works.
# So feel free to test your own data and if your have any questions, contact me by mail.
housing <- read.csv("data/landdata-states.csv") # change to your own address
# Load for further usage
head(housing[1:5])
hp2001Q1 <- subset(housing, Date == 2001.25)

##   `ggplot2' histogram example:
ggplot(housing, aes(x = Home.Value)) +
  geom_histogram()
ggplot(GSMdata, aes(x = VALUE)) +
  geom_histogram()
# Recall what you have done in Task 1
# Task 3:
#      Same, try histogram with other value in GSMdata, compare
#      the difference, have fun.

##   `ggplot2' colored scatter plot example:
ggplot(subset(housing, State %in% c("MA", "TX")),
       aes(x=Date,
           y=Home.Value,
           color=State))+
  geom_point()
ggplot(GSMdata[GSMdata$`Flagged?` == TRUE,],
       aes(x=Ratio,
           y = VALUE))+
  geom_point()


# 2.1 == Geometric Objects And Aesthetics====================
# This is the most charming quality of ggplot2.
# We can add the particular request (position, color, fill, shape etc)
# in Aesthetic mappings function, `aes()'.
# Also note that color is the outside color, fill to make the inside color!

#  Check the following
# You can get a list of available geometric objects using the code...
help.search("geom_", package = "ggplot2")

# Let try the Points (Scatterplot) and Prediction line
# One very cool thing is in ggplot2, we can make a plot with more than one geom.
# Our plot could use a regression line:

# Always run ?geom_point for more details.

trueID <- GSMdata[GSMdata$`Flagged?` == TRUE,]
head(trueID)
ggplot(trueID,
       aes(x=VALUE,
           y = Ratio))+
  geom_point()
ggplot(trueID,
       aes(x=VALUE,
           y = log(Ratio)))+
  geom_point()



# Make a prediction line
trueID$pred.SC <- predict(lm(trueID$Ratio ~ trueID$VALUE, data = trueID))

plot1 <- ggplot(trueID,
             aes(x=VALUE,
                 y = Ratio))+
  geom_point()
plot1 + geom_line(aes(y = pred.SC))



# Let check out the Smoothers
# Note that not all geometric objects are simple shapes--the smooth geom includes
##  a line and a ribbon.
plot1 +
  geom_smooth(method = "gam")
# Note that the default method is "auto", it choose method based on the size of the
# largest group (across all panels), loess is used for than 1,000 observations
# method is the function to use, eg. "lm", "glm", "gam", "loess", "rlm".
# gam is used with formula = y ~ s(x, bs = "cs").
# Somewhat anecdotally, loess gives a better appearance, but is O(n^2) in memory,
# so does not work for larger datasets.
# The above is cited from the help menu, check out
?geom_smooth



# We can also add Text (Label Points)
# Note `geom_text()' accepts a `labels' mapping.
plot1 +
  geom_text(aes(label=substring(trueID$ID_REF, 1, 4)), size = 3)
# This is so so so massed up. Ok lets illustrate this with the housing data
# Housing data shows a pretty good example here.
p1 <- ggplot(hp2001Q1, aes(x = log(Land.Value), y = Structure.Cost))

p1 +
  geom_point() +
  geom_text_repel(aes(label=State), size = 3)



# Exercise I cf.http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html
# I used their original exercises, since they don't provide a solution
# I worked it out and put the solution in the end. Try them first, really
# not that hard. Have fun

##   The data for the exercises is available in the
##   `data/EconomistData.csv' file. Read it in with
dat <- read.csv("data/EconomistData.csv")
head(dat)
ggplot(dat, aes(x = CPI, y = HDI, size = HDI.Rank)) + geom_point(color="blue")
##   Original sources for these data are
##     [http://www.transparency.org/content/download/64476/1031428]
##     [http://hdrstats.undp.org/en/indicators/display_cf_xls_indicator.cfm?indicator_id=103106&lang=en]
##   These data consist of /Human Development Index/ and /Corruption
##   Perception Index/ scores for several countries.
##   1. Create a scatter plot with CPI on the x axis and HDI on the y axis.


##   2. Color the points blue.


##   3. Map the color of the the points to Region.


##   4. Make the points bigger by setting size to 2


##   5. Map the size of the points to HDI.Rank



# ==  2.2 Statistical Transformations=======================
# In ggplot2, some plot types (such as scatterplots) do not require
# transformations (just put each point at x and y coordinates equal to
# the original value).
# But in other plots, such as boxplots, histograms require statistical transformations
# Again, the purpose for this is not copy and paste the knowledge, if you are
# interested, go their website and check it out.
# http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

# check the default statistic
args(geom_histogram)
args(stat_bin)

# Let's try with histogram
plot2 <- ggplot(GSMdata, aes(x = VALUE))
plot2 + geom_histogram()
#   The binwidth looks reasonable by default, but we can change it by
#   passing the `binwidth' argument to the `stat_bin' function, I have
# tried with 0.01, 0.001, 10, 0.1 seems the best.
plot2 + geom_histogram(stat = "bin", binwidth=0.1)


## Exercise II cf.http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

##   1. Re-create a scatter plot with CPI on the x axis and HDI on the y
##      axis (as you did in the previous exercise).

##   2. Overlay a smoothing line on top of the scatter plot using
##      `geom_smooth'.

##   3. Overlay a smoothing line on top of the scatter plot using
##      `geom_smooth', but use a linear model for the predictions. Hint:
##      see `?stat_smooth'.

##   4. Overlay a smoothing line on top of the scatter plot using
##      `geom_line'. Hint: change the statistical transformation.

##   5. BONUS: Overlay a smoothing line on top of the scatter plot using
##      the default /loess/ method, but make it less smooth. Hint: see
##      `?loess'.

## Exercise III cf.http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html

##   1. Create a scatter plot with CPI on the x axis and HDI on the y axis.
##      Color the points to indicate region.
##   2. Modify the x, y, and color scales so that they have more
##      easily-understood names (e.g., spell out "Human development Index"
##      instead of "HDI").
##   3. Modify the color scale to use specific values of your choosing.
##      Hint: see `?scale_color_manual'.


# = 3  Task solutions============================
# = 3.1  Task 1: ...
##   1. Create a scatter plot with CPI on the x axis and HDI on the y axis.
g1 <- ggplot(dat, aes(x = CPI, y = HDI))
g1 + geom_point()
##   2. Color the points blue.
g1 + geom_point(color = "blue")
##   3. Map the color of the the points to Region.
g1 + geom_point(aes(color = Region))
##   4. Make the points bigger by setting size to 2
g1 + geom_point(aes(color=Region), size = 2)
##   5. Map the size of the points to HDI.Rank
g1 + geom_point(aes(color=Region, size = HDI.Rank))
# = 3.2  Task 2: ...
##   1. Re-create a scatter plot with CPI on the x axis and HDI on the y
##      axis (as you did in the previous exercise).
g1 <- ggplot(dat, aes(x = CPI, y = HDI, size = HDI.Rank))
g1 + geom_point()
##   2. Overlay a smoothing line on top of the scatter plot using
##      `geom_smooth'.
g1 + geom_point()+geom_smooth()
##   3. Overlay a smoothing line on top of the scatter plot using
##      `geom_smooth', but use a linear model for the predictions. Hint:
##      see `?stat_smooth'.
?stat_smooth
g1 + geom_point() + geom_smooth(method = "lm")
##   4. Overlay a smoothing line on top of the scatter plot using
##      `geom_line'. Hint: change the statistical transformation.
?geom_line
g1 + geom_point() + geom_line(stat = "identity")
##   5. BONUS: Overlay a smoothing line on top of the scatter plot using
##      the default /loess/ method, but make it less smooth. Hint: see
##      `?loess'.
?stat_smooth
?loess
g1 + geom_point() + geom_smooth(method = "loess")
# = 3.3 task 3
##   1. Create a scatter plot with CPI on the x axis and HDI on the y axis.
##      Color the points to indicate region.
g3 <- ggplot(dat, aes(x = CPI, y = HDI)) + geom_point(aes(color=Region))
g3
##   2. Modify the x, y, and color scales so that they have more
##      easily-understood names (e.g., spell out "Human development Index"
##      instead of "HDI").
g3 + scale_x_continuous(name="Human development Index") +
  scale_y_continuous(name = "Corruption Perception Index")+
  scale_color_discrete(l = 40, c = 200, na.value = "black")
##   3. Modify the color scale to use specific values of your choosing.
##      Hint: see `?scale_color_manual'.
?scale_color_manual
g3 + scale_x_continuous(name="Human development Index") +
  scale_y_continuous(name = "Corruption Perception Index")+
  scale_colour_manual(values=c("white", "coral", "chocolate",
                               "cornsilk", "papayawhip", "blanchedalmond"))

# [END]
