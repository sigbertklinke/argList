#' @rdname getEllipsis
#' @title getEllipsis
#' @aliases gE
#' @description #' Returns from the internal package environment the ellipsis arguments. If `rm.used` is `TRUE` then
#' parameters by intermediate operations are deleted.
#'
#' @param rm.used logical:
#'
#' @return a list with (unused) ellipsis parameters or `NULL`
#' @export
#'
#' @examples
#' test <- function(x, ...) { setEllipsis(list(...)); getEllipsis() }
#' #
#' test(x=1, y=2)
getEllipsis <- function(rm.used=TRUE) {
  if (isTRUE(rm.used)) do[['...']] <- do[['...']][setdiff(names(do[['...']]), do[['...used']])]
  do[['...']]
}

#' @rdname getEllipsis
#' @export
gE <- getEllipsis
