#' @rdname paramlist
#' @aliases setParamList
#' @aliases getParamList
#' @title Parameter lists
#' @description
#' `setParamList` sets the global parameter for a function, `getParameterList` returns the currently parameter list(s).
#' `setParamList` could be used in functions like `.onLoad` for a package or locally in a function.
#'
#' @param funname character: function name
#' @param fun character/function: function to extract parameters by [base::formals()] (default: `NULL` = no formals are extracted)
#' @param deny character: parameter names which should not be modified by global parameters
#' @param allow character: additional parameter names which could be modified by global parameters
#'
#' @return returns invisibly the currently parameter list(s)
#' @export
#'
#' @examples
#' # allow all parameters from [graphics::axis()] and `par(no.readonly=TRUE)`,
#' # but deny `at`and `labels`
#' setParamList('axis', graphics::axis, deny=c('at', 'labels'), allow=names(par(no.readonly=TRUE)))
#' getParamList()
setParamList <- function(funname, fun=NULL, deny=NULL, allow=NULL) {
  funname <- as.character(funname)
  do[[funname]] <- NULL
  if (!is.null(fun)) {
    fun <- if (is.character(fun)) eval(parse(text=fun)) else match.fun(fun)
    do[[funname]] <- unique(c(do[[funname]], names(formals(fun))))
  }
  do[[funname]] <- unique(c(do[[funname]], allow))
  do[[funname]] <- sort(setdiff(do[[funname]], deny))
  invisible(do[[funname]])
}

#' @rdname paramlist
#' @export
getParamList <- function(funname=NULL) {
  if (is.null(funname)) as.list(do) else do[[funname]]
}
