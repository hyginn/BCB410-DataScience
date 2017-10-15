

#===================== example from online source =================================
#based on https://cran.r-project.org/web/packages/xgboost/vignettes/discoverYourData.html
#purpose: familiar with transforming raw data into features

if(!require(xgboost))install.packages("xgboost")
require(xgboost)
if(!require(Matrix))install.packages("Matrix")
require(Matrix)
if(!require(data.table))install.packages("data.table")
require(data.table)
if(!require('vcd'))install.packages('vcd')

# Xgboost manages only numeric vectors.
# In R, a categorical variable is called factor.


#Conversion from categorical to numeric variables
# step 1:load Arthritis dataset in memory and wrap it with data.table package.
data(Arthritis)
df <- data.table(Arthritis, keep.rownames = F)
head(df) # look to the first lines of the data.tables.
str(df) #check the format of each column
# 2 columns have factors types, one has ordinal types.

#cleaning data
df[, ID:=NULL]

#list different values for the column "Treatment"
levels(df[,Treatment])

#Random split into two groups
head(df[,AgeCat:= as.factor(ifelse(Age > 30, "Old", "Young"))])

#One-hot encoding
# transform the categorical data to dummy variables.
#The purpose is to transform each value of each categorical feature into a binary feature{0,1}
sparse_matrix <- sparse.model.matrix(Improved~.-1, data=df)
head(sparse_matrix)
#Improved ~.-1 used above means transform all categorical features but column Improved to binary values.
#-1 is to remove first column which is full of 1.
output_vector = df[, Improved] == "Marked"





#==================== Another example dataset ===========================
# purpose: look at different dataset and play around different ways of transformation

data(CO2)
str(CO2)
View(CO2)

df2 <- data.table(CO2, keep.rownames = F)
head(df2)
str(df2)

#by looking at dataset, "Plant","Type" and "Treatment" are Factor(categorial data)
levels(df2[,Plant])
# return  [1] "Qn1" "Qn2" "Qn3" "Qc1" "Qc3" "Qc2" "Mn3" "Mn2" "Mn1" "Mc2" "Mc3" "Mc1"
# 12 types of categorical data
levels(df2[,Type])
#return [1] "Quebec"      "Mississippi"
# two types of categorical data
levels(df2[,Treatment])
#return [1] "nonchilled" "chilled"
# 2 types of categorical data
sparse_matrix <- sparse.model.matrix(Plant~.-1,data=df2)
head(sparse_matrix)
