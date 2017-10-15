# ___ID___.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the ___ID___ unit.
#
# Version:  0.1
#
# Date:     2017  10  14
# Author:   Zhifan (roger.wu@mail.utoronto.ca)
#
# Versions:
#           0.1    (Describe ...)

#
# TODO: Finish exercise 1,2 and 3. The solution is on the EDA-Visualization unit.
#
#
# == DO NOT SIMPLY  source()  THIS FILE! =======================================
#
# If there are portions you don't understand, use R's help system, Google for an
# answer, or ask your instructor. Don't continue if you don't understand what's
# going on. That's not how it works ...
#
# ==============================================================================


# = 1 EDA visualization with basic R functions
# This is a very basic tutorial of EDA visualization.
# I will use the flight data from the nycflights13 package that you can download
# directly from the Rstudio.
# This tutorial is credited
# to the http://www.hcbravo.org/IntroDataSci/lecture-note/eda/visualization_eda/
# and to the http://tutorials.iq.harvard.edu/R/Rgraphics/Rgraphics.html#introduction
# I did some changes in it to make it more related to our porject.

# = 1.1 Load all the library.
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
if (!require(nycflights13)) {
  install.packages("nycflights13")
  library(nycflights13)
}
# = 1.2 Visualization on single data
#### Start ####
# Call head() function to get a overview how the data looks like.
head(flights)
# Basic visualizaiton of the dep_delay variable in the flights dataset.
(plot(flights$dep_delay))
# This is not particular useful.
# Although we can change the order of the points to depend on
# departure delay and change the graphical representation to make easier to see.
plot(sort(flights$dep_delay), type="h", ylab="Departure Delay")
# Now let's try to use the histgram.
hist(flights$dep_delay, xlab="Departure Delay")
# One useful funtion to use in histgram is rug.
?rug
rug(flights$dep_delay)
# Another useful function is abline.
?abline
abline(v = 400, col = "red", lwd = 4)
# Now lets take a look at the boxplot.
boxplot(flights$dep_delay, ylab="Departure Delay")
# That’s not very clear to see, so let’s do a transformation of this data to see this better:
boxplot(log(flights$dep_delay -
              min(flights$dep_delay,na.rm=TRUE)
            +1), ylab="Departure Delay")
# = 1.2 Visualizatio of pairs of variables
# Suppose we want to see the relationship between dep_delay (a numeric variable)
# and origin (a categorical variable).
# Here is how we can see a plot of the distribution of departure delays
# conditioned on origin airport.
boxplot(log(flights$dep_delay - min(flights$dep_delay, na.rm=TRUE) + 1) ~ flights$origin,
        ylab="Departure Delay", xlab="Airport of origin")
# For pairs of continuous variables, the most useful visualization is the scatter plot.
# This gives an idea of how one variable varies conditioned on another variable.
plot(flights$dep_delay, flights$arr_delay, xlab="Departure Delay", ylab="Arrival Delay")

# = 2 Using ggplot2 to get better visualization
# While we have seen a basic repertoire of graphics it’s easier to proceed
# if we have a bit more formal way of thinking about graphics and plots.
# Here is where we will use the grammar of graphics implemented in R by the package ggplot2.
# Load the library.
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
# Let's look at housing prices and GSM112133.csv data.
# I will mainly using housing data as the example, and I do try the GSM data and it works.
# So feel free to test your own data and if your have any questions, contact me on slack or mail.
housing <- read.csv("data/landdata-states.csv") # change to your own address
GSE112 <- read.csv("data/GSM112133.csv") # change to your own address
head(GSE112)
head(housing[1:5])
##   `ggplot2' histogram example:
library(ggplot2)
ggplot(housing, aes(x = Home.Value)) +
  geom_histogram()
ggplot(GSE112, aes(x = INV_VALUE)) +
  geom_histogram()
##   `ggplot2' colored scatter plot example:
ggplot(subset(housing, State %in% c("MA", "TX")),
       aes(x=Date,
           y=Home.Value,
           color=State))+
  geom_point()
ggplot(subset(GSE112, Flagged. %in% "TRUE"),
       aes(x=ID_REF,
           y = Ratio))+
  geom_point()
# 2.1 = Geometric Objects And Aesthetics
##   In ggplot land /aesthetic/ means "something you can see". Examples
##   include:
##   - position (i.e., on the x and y axes)
##   - color ("outside" color)
##   - fill ("inside" color)
##   - shape (of points)
##   - linetype
##   - size
##   Each type of geom accepts only a subset of all aesthetics--refer to
##   the geom help pages to see what mappings each geom accepts. Aesthetic
##   mappings are set with the `aes()' function.
## Geometic Objects (`geom')
##   Geometric objects are the actual marks we put on a plot. Examples
##   include:
##   - points (`geom_point', for scatter plots, dot plots, etc)
##   - lines (`geom_line', for time series, trend lines, etc)
##   - boxplot (`geom_boxplot', for, well, boxplots!)
##   A plot must have at least one geom; there is no upper limit. You can
##   add a geom to a plot using the `+' operator

##   You can get a list of available geometric objects using the code
##   below:
help.search("geom_", package = "ggplot2")

# = 2.2 Points (Scatterplot)

## Always run ?geom_point for more details.
trueID <- subset(GSE112, Flagged. = "TRUE")
head(trueID)
ggplot(trueID,
       aes(x=ID_REF,
           y = Ratio))+
  geom_point()
hp2001Q1 <- subset(housing, Date == 2001.25)
ggplot(hp2001Q1,
       aes(y = Structure.Cost, x = Land.Value)) +
  geom_point()

ggplot(hp2001Q1,
       aes(y = Structure.Cost, x = log(Land.Value))) +
  geom_point()

# = 2.3 Lines (Prediction Line)
##   A plot constructed with `ggplot' can have more than one geom. In that
##   case the mappings established in the `ggplot()' call are plot defaults
##   that can be added to or overridden. Our plot could use a regression
##   line:
hp2001Q1$pred.SC <- predict(lm(Structure.Cost ~ log(Land.Value), data = hp2001Q1))
p1 <- ggplot(hp2001Q1, aes(x = log(Land.Value), y = Structure.Cost))
p1 + geom_point(aes(color = Home.Value)) +
  geom_line(aes(y = pred.SC))
# = 2.4 Smoothers
##   Not all geometric objects are simple shapes--the smooth geom includes
##   a line and a ribbon.
p1 +
  geom_point(aes(color = Home.Value)) +
  geom_smooth()
# = 2.5 Text (Label Points)
##   Each `geom' accepts a particualar set of mappings--for example
##   `geom_text()' accepts a `labels' mapping.

p1 +
  geom_text(aes(label=State), size = 3)

library(ggrepel)
p1 +
  geom_point() +
  geom_text_repel(aes(label=State), size = 3)
# = 2.6 Aesthetic Mapping VS Assignment
##   Note that variables are mapped to aesthetics with the `aes()'
##   function, while fixed aesthetics are set outside the `aes()' call.
##   This sometimes leads to confusion, as in this example:

p1 +
  geom_point(aes(size = 2),# incorrect! 2 is not a variable
             color="red") # this is fine -- all points red
# = 2.7 Mapping Variables To Other Aesthetics
##   Other aesthetics are mapped in the same way as x and y in the previous
##   example.

p1 +
  geom_point(aes(color=Home.Value, shape = region))

## Exercise I
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

# = 3 Statistical Transformations
# = 3.1 Statistical Transformations
##   Some plot types (such as scatterplots) do not require
##   transformations--each point is plotted at x and y coordinates equal to
##   the original value. Other plots, such as boxplots, histograms,
##   prediction lines etc. require statistical transformations:
##   - for a boxplot the y values must be transformed to the median and
##     1.5(IQR)
##   - for a smoother smother the y values must be transformed into
##     predicted values

##   Each `geom' has a default statistic, but these can be changed. For
##   example, the default statistic for `geom_bar' is `stat_bin':
args(geom_histogram)
args(stat_bin)

# = 3.2 Setting Statistical Transformation Arguments
##   Arguments to `stat_' functions can be passed through `geom_'
##   functions. This can be slightly annoying because in order to change it
##   you have to first determine which stat the geom uses, then determine
##   the arguments to that stat.
##   For example, here is the default histogram of Home.Value:
p2 <- ggplot(housing, aes(x = Home.Value))
p2 + geom_histogram()
##   The binwidth looks reasonable by default, but we can change it by
##   passing the `binwidth' argument to the `stat_bin' function:
p2 + geom_histogram(stat = "bin", binwidth=4000)

# = 3.3 Changing The Statistical Transformation
##   Sometimes the default statistical transformation is not what you need.
##   This is often the case with pre-summarized data:
housing.sum <- aggregate(housing["Home.Value"], housing["State"], FUN=mean)
rbind(head(housing.sum), tail(housing.sum))

ggplot(housing.sum, aes(x=State, y=Home.Value)) +
  geom_bar()

ggplot(housing.sum, aes(x=State, y=Home.Value)) +
  geom_bar()
# Error: stat_count() must not be used with a y aesthetic.

##   What is the problem with the previous plot? Basically we take binned
##   and summarized data and ask ggplot to bin and summarize it again
##   (remember, `geom_bar' defaults to `stat = stat_count'); obviously this
##   will not work. We can fix it by telling `geom_bar' to use a different
##   statistical transformation function:
ggplot(housing.sum, aes(x=State, y=Home.Value)) +
  geom_bar(stat="identity")

## Exercise II
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

# = 4 Scales
# = 4.1 Scales: Controlling Aesthetic Mapping
##   Aesthetic mapping (i.e., with `aes()') only says that a variable
##   should be mapped to an aesthetic. It doesn't say /how/ that should
##   happen. For example, when mapping a variable to /shape/ with
##   `aes(shape = x)' you don't say /what/ shapes should be used.
##   Describing what colors/shapes/sizes etc. to use is done by modifying
##   the corresponding /scale/. In `ggplot2' scales include
##   - position
##   - color and fill
##   - size
##   - shape
##   - line type
##   Scales are modified with a series of functions using a
##   `scale_<aesthetic>_<type>' naming scheme. Try typing `scale_<tab>' to
##   see a list of scale modification functions.

# = 4.2 Common Scale Arguments
##   The following arguments are common to most scales in ggplot2:
##   name: the first argument gives the axis or legend title
##   limits: the minimum and maximum of the scale
##   breaks: the points along the scale where labels should appear
##   labels: the labels that appear at each break

##   Specific scale functions may have additional arguments; for example,
##   the `scale_color_continuous' function has arguments `low' and `high'
##   for setting the colors at the low and high end of the scale.

# = 4.3 Scale Modification Examples
##   Start by constructing a dotplot showing the distribution of home
##   values by Date and State.

p3 <- ggplot(housing,
             aes(x = State,
                 y = Home.Price.Index)) +
  theme(legend.position="top",
        axis.text=element_text(size = 6))
(p4 <- p3 + geom_point(aes(color = Date),
                       alpha = 0.5,
                       size = 1.5,
                       position = position_jitter(width = 0.25, height = 0)))

##   Now modify the breaks for the x axis and color scales

p4 + scale_x_discrete(name="State Abbreviation") +
  scale_color_continuous(name="",
                         breaks = c(1976, 1994, 2013),
                         labels = c("'76", "'94", "'13"))

##   Next change the low and high values to blue and red:
p4 +
  scale_x_discrete(name="State Abbreviation") +
  scale_color_continuous(name="",
                         breaks = c(1976, 1994, 2013),
                         labels = c("'76", "'94", "'13"),
                         low = "blue", high = "red")

if (!require(scales)) {
  install.packages("scales")
  library(scales)
}
p4 +
  scale_color_continuous(name="",
                         breaks = c(1976, 1994, 2013),
                         labels = c("'76", "'94", "'13"),
                         low = muted("blue"), high = muted("red"))

# = 4.4 Using different color scales
##   ggplot2 has a wide variety of color scales; here is an example using
##   `scale_color_gradient2' to interpolate between three different colors.
p4 +
  scale_color_gradient2(name="",
                        breaks = c(1976, 1994, 2013),
                        labels = c("'76", "'94", "'13"),
                        low = muted("blue"),
                        high = muted("red"),
                        mid = "gray60",
                        midpoint = 1994)

## Available Scales
##   Note that in RStudio you can type `scale_' followed by TAB to get the
##   whole list of available scales.

## Exercise III

##   1. Create a scatter plot with CPI on the x axis and HDI on the y axis.
##      Color the points to indicate region.
##   2. Modify the x, y, and color scales so that they have more
##      easily-understood names (e.g., spell out "Human development Index"
##      instead of "HDI").
##   3. Modify the color scale to use specific values of your choosing.
##      Hint: see `?scale_color_manual'.

# = 5 Faceting
# = 5.1 Faceting
##   - Faceting is `ggplot2' parlance for *small multiples*
##   - The idea is to create separate graphs for subsets of data
##   - `ggplot2' offers two functions for creating small multiples:
##     1. `facet_wrap()': define subsets as the levels of a single grouping
##        variable
##     2. `facet_grid()': define subsets as the crossing of two grouping
##        variables
##   - Facilitates comparison among plots, not just of geoms within a plot

##   - Start by using a technique we already know--map State to color:
p5 <- ggplot(housing, aes(x = Date, y = Home.Value))
p5 + geom_line(aes(color = State))
##   There are two problems here--there are too many states to distinguish
##   each one by color, and the lines obscure one another.

# = 5.2 Faceting to the rescue
##   We can remedy the deficiencies of the previous plot by faceting by
##   state rather than mapping state to color.

(p5 <- p5 + geom_line() +
    facet_wrap(~State, ncol = 10))

# = 6  Task solutions
# = 6.1  Task 1: ...
##   1. Create a scatter plot with CPI on the x axis and HDI on the y axis.
g1 <- ggplot(dat, aes(x = CPI, y = HDI, size = HDI.Rank))
g1 + geom_point()
##   2. Color the points blue.
g1 + geom_point(color = "blue")
##   3. Map the color of the the points to Region.
g1 + geom_point(aes(color = Region))
##   4. Make the points bigger by setting size to 2
g1 + geom_point(aes(color=Region), size = 2)
##   5. Map the size of the points to HDI.Rank
g1 + geom_point(aes(color=Region, size = HDI.Rank))
# = 6.2  Task 2: ...
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
# = 6.3 task 3
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
