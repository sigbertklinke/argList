#' diff
#' @description
#' Shows the differences and similarities between two argument lists in the style of Unix diff:
#'   * `< ...` means that `...` is only `x`,
#'   * `> ...` means that `...` is only `y`, and
#'   * '  ...` means that `...` is in `x` and `y`.
#'
#' @param x,y argList: the list to compare
#' @param xtitle,ytitle character: titles for the argLists (default: `NULL` = `deparse1(substitute(.))`)
#' @param ... unused
#'
#' @return a character vector
#' @importFrom utils capture.output str
#' @export
#'
#' @examples
#' 1+1
diff.argList <- function(x, y, xtitle=NULL, ytitle=NULL, ...) {
  diff_matrix <- function(vector1, vector2) {
    m <- length(vector1)
    n <- length(vector2)
    # Compute LCS using dynamic programming
    dp <- matrix(0, nrow = m + 1, ncol = n + 1)
    # Fill dp matrix
    for (i in 1:m) {
      for (j in 1:n) {
        dp[i + 1, j + 1] <- ifelse(vector1[i] == vector2[j], dp[i, j] + 1, max(dp[i + 1, j], dp[i, j + 1]))
      }
    }
    # Backtrack to fill the diff_matrix
    diff_matrix <- matrix("", nrow = m + n, ncol = 2)
    k <- 1
    i <- m
    j <- n
    while (i > 0 && j > 0) {
      if (vector1[i] == vector2[j]) {
        diff_matrix[k, ] <- c(vector1[i], vector2[j])
        k <- k + 1
        i <- i - 1
        j <- j - 1
      } else if (dp[i + 1, j] >= dp[i, j + 1]) {
        diff_matrix[k, 2] <- vector2[j]
        k <- k + 1
        j <- j - 1
      } else {
        diff_matrix[k, 1] <- vector1[i]
        k <- k + 1
        i <- i - 1
      }
    }
    # Fill remaining elements if any
    if (i > 0) {
      diff_matrix[k:(k + i - 1), 1] <- vector1[1:i]
    } else if (j > 0) {
      diff_matrix[k:(k + j - 1), 2] <- vector2[1:j]
    }
    # Reverse the rows to maintain original order
    diff_matrix <- diff_matrix[rev(seq_len(k - 1)), ]

    return(diff_matrix)
  }
  #
  stopifnot(inherits(y, "argList"))
  if (is.null(xtitle)) xtitle <- deparse1(substitute(x))
  if (is.null(ytitle)) xtitle <- deparse1(substitute(y))
  lx    <- capture.output(str(x, give.attr = FALSE))
  ly    <- capture.output(str(y, give.attr = FALSE))
  res   <- diff_matrix(lx, ly)
  ret   <- rep(NA_character_, nrow(res))
  for (i in 1:nrow(res)) {
    if(res[i,1]==res[i,2]) {
      ret[i] <- sprintf('  %s\n', res[i,1])
    } else if(res[i,1]=="") {
      ret[i] <- sprintf('< %s\n', res[i,2])
    } else {
      ret[i] <- sprintf('> %s\n', res[i,1])
    }
  }
  c(sprintf('\n < %s\n', xtitle), sprintf('> %s\n', ytitle), '-------\n', ret)
}
