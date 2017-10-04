# .utilities.R
#
# Purpose:  Miscellaneous R code to suppport the project
# Version:  1.0
# Date:     2017 10 00
# Author:   Boris Steipe (boris.steipe@utoronto.ca)
#
# Dependencies:
#           NA
#
# License: GPL-3 (https://www.gnu.org/licenses/gpl-3.0.en.html)
#
# Version history:
#    1.0  First version
#
# ToDo:
#    NA
#
#
# ==============================================================================

biCode <- function(s) {
  # make a 5 character "biCode" from a binomial name by concatening
  # the uppercased first three letter of the first word and the first
  # two letters of the second word.
  # s: character vector of binomial species names
  # value: character vector of biCodes, same length as s

  b <- character(length(s))
  s <- gsub("[^a-zA-Z ]", "", s) # remove all non-alphabetic characters
                                 # except space
  s <- toupper(s)

  for (i in seq_along(s)) {
    b[i] <- sprintf("%s%s",
                    unlist(substr(s[i], 1, 3)),
                    unlist(substr(strsplit(s[i], "\\s+")[[1]][2], 1, 2)))
  }
  return(b)
}


pBar <- function(i, l, nCh = 50) {
  # Draw a progress bar in the console
  # i: the current iteration
  # l: the total number of iterations
  # nCh: width of the progress bar
  ticks <- round(seq(1, l-1, length.out = nCh))
  if (i < l) {
    if (any(i == ticks)) {
      p <- which(i == ticks)
      p1 <- paste(rep("#", p), collapse = "")
      p2 <- paste(rep("-", nCh - p), collapse = "")
      cat(sprintf("\r|%s%s|", p1, p2))
      flush.console()
    }
  }
  else { # done
    cat("\n")
  }
}


waitTimer <- function(t, nIntervals = 50) {
  # pause and wait for t seconds and display a progress bar as
  # you are waiting
  t <- as.numeric(t)

  if (t < 0.1) {return(invisible())}

  increment <- t / nIntervals

  bar <- "----:----|"  # One module for the progress bar:
  bar <- rep(bar, ceiling(nIntervals / 10))  # repeat,
  bar <- unlist(strsplit(bar, "")) # split into single characters,
  bar <- bar[1:nIntervals]  # truncate,
  bar <- paste(bar, collapse="") # and collapse.

  cat(sprintf("\nWaiting: |%s\n         |", bar))
  for (i in 1:(nIntervals - 1)) {
    Sys.sleep(increment)
    cat("=")
  }
  Sys.sleep(increment)
  cat("|\n\n")

  return(invisible())
}



# [END]
