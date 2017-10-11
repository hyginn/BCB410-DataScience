# EDA-CLU-Mutual_information.R
#
# Purpose:  A Bioinformatics Course:
#              R code accompanying the EDA-CLU-Mutual_information unit.
#
# Version:  0.2
#
# Date:     2017  10  10
# Author:   Dominick Han (dominick.han@mail.utoronto.ca)
#
# Versions:
#           0.2    (Added MI calculation script)

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

# = 1 Clustering by Mutual Information
# = 1.1 Calculating Mutual Information
Calculate_MI <- function(v1, v2) {
  # Purpose:
  #     Calculate Mutual Information between two vectors
  #
  # Parameters:
  #     v1:   vector of int
  #     v2:   vector of int
  #
  # Value:
  #     Return the Mutual Information value between two vectors

  v1 = sample(1:5, 10, replace=T);
  v2 = sample(1:5, 10, replace=T);

  min1 = min(v1);
  min2 = min(v2);
  matrix = matrix(0, max(v1)-min1+1, max(v2)-min2+1);

  for (i in 1:length(v1)) {
    matrix[v1[i]-min1+1, v2[i]-min2+1] = matrix[v1[i]-min1+1, v2[i]-min2+1] + 1;
  };

  matrix = matrix/length(v1);
  colS = colSums(matrix);
  rowS = rowSums(matrix);

  for (i in 1:length(colS)) {
    colS[i] = colS[i]*log2(colS[i]+(colS[i]==0));
  };
  for (i in 1:length(rowS)) {
    rowS[i] = rowS[i]*log2(rowS[i]+(rowS[i]==0));
  };
  for (i in 1:length(rowS)) {
    for (j in 1:length(colS)) {
      matrix[i,j] = matrix[i,j]*log2(matrix[i,j]+(matrix[i,j]==0));
    };
  };

  MI = -(sum(colS)+sum(rowS))/sum(matrix);

  return(MI);
};

# = 1.2 Example
v1=sample(1:5, 10, replace=T);
v2=sample(1:5, 10, replace=T);
Calculate_MI(v1, v1)
Calculate_MI(v2, v2)
Calculate_MI(v1, v2)

# [END]
