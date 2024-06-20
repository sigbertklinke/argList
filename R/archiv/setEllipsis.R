#' setEllipsis
#'
#' Sets the ellipsis arguments to the package environment.
#'
#' @param ellipsis an ellipsis (`...`)
#' @param ... further named parameters added to the ellipsis
#'
#' @return nothing
#' @export
#'
#' @examples
#' test <- function(x, ...) { setEllipsis(list(...)); getEllipsis() }
#' #
#' test(x=1, y=2)
setEllipsis <- function(ellipsis, ...) {
  args  <- list(...)
  nargs <- names(args)
  if (is.null(nargs)) nargs <- rep('', length(args))
  for (i in seq_along(nargs)) {
    if(nchar(nargs[i])) ellipsis[[nargs[i]]] <- args[[i]]
  }
  do[['...']] <- ellipsis
  do[['...used']] <- NULL
}
