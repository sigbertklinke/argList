#' @rdname updateEllipsis
#' @aliases uE
#' @title updateEllipsis
#' @description Updates the ellipsis with a local parameter `listname.elemname=value` if a global parameter `elemname` is given
#' and `value` is not a list.
#'
#' @param margs specified arguments of the calling function
#' @param ... further arguments
# @param listname character: name parameter list
# @param elemname character: name of parameter
# @param value R object
#'
#' @return `TRUE` if something has been added to the ellipsis, otherwise `FALSE`
#' @export
#'
#' @examples
#' 1+1
# test <- function(axis=list(side=1), ...) {
#   setEllipsis(list(...))
#   updateEllipsis(axis.side='axis')
#   getEllipsis()
# }
# #
# test()       # empty ellipsis
# test(axis=3) # added axis.side=3
updateEllipsis <- function(margs, ...) {
  args <- list(...)
  nargs <- names(args)
  if (!is.null(nargs)) {
    nmc <- names(margs)
    for (i in seq_along(args)) {
      namei  <- nargs[i]
      parami <- as.character(args[[i]])
      if ((nchar(namei)>0) && (parami %in% nmc) && !is.list(margs[[parami]])) {
        if (is.null(do[['...']][[namei]])) do[['...']][[namei]] <- margs[[parami]]
      }
    }
  }
}

#' @rdname updateEllipsis
#' @export
uE <- updateEllipsis
