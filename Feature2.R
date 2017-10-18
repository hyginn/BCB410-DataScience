# Copyright 2017 Dave Langer
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#



#
# This R source code file corresponds to video 6 of the YouTube series
# "Data Wrangling & Feature Engineering with dplyr" located at the following URL:
#     https://youtu.be/E4Gx6iZ1siA
#



#======================================================================
# Load libraries and take a look at the help files
#
library(dplyr)
library(stringr)

help(package = "dplyr")
help(package = "stringr")



#======================================================================
# Load the data and take a first look
#
train <- read.csv("titanic_train.csv", stringsAsFactors = FALSE)
str(train)

View(train)



#======================================================================
# What about those zero fares?
#
zero.fare <- train %>%
  filter(Fare == 0.0)
View(zero.fare)

# Let's get the totals by Pclass
zero.fare.pclass <- zero.fare %>%
  group_by(Pclass) %>%
  summarize(Total = n()) %>%
  arrange(desc(Total))
zero.fare.pclass



#======================================================================
# Add the new feature for the Title of each passenger
#
train <- train %>%
  mutate(Title = str_extract(Name, "[a-zA-Z]+\\."))

table(train$Title)



#======================================================================
# Condense titles down to small subset
#
titles.lookup <- data.frame(Title = c("Mr.", "Capt.", "Col.", "Don.", "Dr.",
                                      "Jonkheer.", "Major.", "Rev.", "Sir.",
                                      "Mrs.", "Dona.", "Lady.", "Mme.",
                                      "Countess.",
                                      "Miss.", "Mlle.", "Ms.",
                                      "Master."),
                            New.Title = c(rep("Mr.", 9),
                                          rep("Mrs.", 5),
                                          rep("Miss.", 3),
                                          "Master."),
                            stringsAsFactors = FALSE)
View(titles.lookup)

# Replace Titles using lookup table
train <- train %>%
  left_join(titles.lookup, by = "Title")
View(train)

train <- train %>%
  mutate(Title = New.Title) %>%
  select(-New.Title)
View(train)

#
# Alternatively, you could do the above in one dplyr pipeline
#
# train <- train %>%
#   left_join(titles.lookup, by = "Title") %>%
#   mutate(Title = New.Title) %>%
#   select(-New.Title)
#



#======================================================================
# Double-check our work
#
table(train$Title)

train %>%
  filter((Sex == "female" & (Title == "Mr." | Title == "Master.")) |
           (Sex == "male" & (Title == "Mrs." | Title == "Miss.")))

train$Title[train$PassengerId == 797] <- "Mrs."



#======================================================================
# Create summary stats for Fares for those passengers with a Title
# of "Mr." by Pclass
#
mr.fare.stats <- train %>%
  filter(Title == "Mr.") %>%
  group_by(Pclass) %>%
  summarize(Fare.Min = min(Fare),
            Fare.Max = max(Fare),
            Fare.Mean = mean(Fare),
            Fare.Median = median(Fare),
            Fare.Var = var(Fare),
            Fare.SD = sd(Fare),
            Fare.IQR = IQR(Fare))
View(mr.fare.stats)

summary(train$Fare[train$Title == "Mr." & train$Pclass == 1])
summary(train$Fare[train$Title == "Mr." & train$Pclass == 2])
summary(train$Fare[train$Title == "Mr." & train$Pclass == 3])



#======================================================================
# Create "tracking feature" for those records that were originally
# had 0.00 for the Fare variable
#
train$Fare.Zero <- ifelse(train$Fare == 0.0, "Y", "N")
View(train)



#======================================================================
# Create lookup table for zero fare values
#
zero.fare.lookup <- train %>%
  filter(Title == "Mr.") %>%
  group_by(Pclass, Title) %>%
  summarize(New.Fare = median(Fare))
View(zero.fare.lookup)



#======================================================================
# Impute the zero fares using the lookup table
#
train <- train %>%
  left_join(zero.fare.lookup, by = c("Pclass", "Title")) %>%
  mutate(Fare = ifelse(Fare == 0.0, New.Fare, Fare)) %>%
  select(-New.Fare)
View(train)



#======================================================================
# Take a closer look at the Age variable all-up
#
age.stats <- train %>%
  group_by(Pclass, Title) %>%
  summarize(Age.Min = min(Age, na.rm = TRUE),
            Age.Max = max(Age, na.rm = TRUE),
            Age.Mean = mean(Age, na.rm = TRUE),
            Age.Median = median(Age, na.rm = TRUE),
            Age.NA.Count = sum(is.na(Age)),
            Age.Var = var(Age, na.rm = TRUE),
            Age.SD = sd(Age, na.rm = TRUE),
            Age.IQR = IQR(Age, na.rm = TRUE)) %>%
  arrange(Title, Pclass)
View(age.stats)

table(train$Pclass)



#======================================================================
# Create "tracking feature" for those records that originally had
# missing ages.
#
train$Age.Missing <- ifelse(is.na(train$Age), "Y", "N")
View(train)



#======================================================================
# Create lookup table
#
age.lookup <- age.stats %>%
  select(Pclass, Title, Age.Mean, Age.Median)
View(age.lookup)



#======================================================================
# Impute missing ages using lookup table
#

# NOTE - The following code incorporates a bug fix not shown
#        in the video!
train <- train %>%
  left_join(age.lookup, by = c("Pclass", "Title")) %>%
  mutate(Age = ifelse(Age.Missing == "Y",
                      ifelse(Title == "Miss." & Pclass == "3",
                             Age.Median, Age.Mean),
                      Age)) %>%
  select(-Age.Median, -Age.Mean)
View(train)



#======================================================================
# Look at imputed Age distribution all up
#
quantile(train$Age, probs = seq(0.05, 1, 0.05))



#======================================================================
# Create Ticket-based features
#
ticket.lookup <- train %>%
  group_by(Ticket) %>%
  summarize(Group.Count = n(),
            Avg.Fare = max(Fare) / n(),
            Female.Count = sum(Sex == "female"),
            Male.Count = sum(Sex == "male"),
            Child.Count = sum(Age < 18),
            Elderly.Count = sum(Age > 54.0),
            Female.Ratio = sum(Sex == "female") / n(),
            Male.Ratio = sum(Sex == "male") / n(),
            Child.Ratio = sum(Age < 18) / n(),
            Elderly.Ratio = sum(Age > 54.0) / n(),
            Female.Child.Ratio = (sum(Age < 18) +
                                    sum(Sex == "female" & Age >= 18)) / n(),
            Min.Age = min(Age),
            Max.Age = max(Age))
View(ticket.lookup)
