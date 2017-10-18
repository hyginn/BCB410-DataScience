# BCB410-DataScience_utilities.R
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


message("BCB410-DataScience: loading function pBar().")
pBar <- function(i, l, nCh = 50) {
  # Draw a progress bar in the console. This is useful if you are running a
  # large number of iterations of a slow function, to provide feedback on
  # how the task is progressing.
  # Parameters:
  #      i     int   the current iteration
  #      l     int   the total number of iterations
  #      nCh   int   width of the progress bar
  # Value:
  #      NA          invoked for the side-effect of printing a progress bar.
  # Note: execute the function on every loop iteration, passing the value
  #         of the current loop counter into this function.

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


# [END]
