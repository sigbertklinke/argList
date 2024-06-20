#' @rdname argList
#' @title argList
#' @description Creates an argument list and propagates parameter, if available.
#' @param margs call: a call where all specified arguments are provided using their full names, typically achieved with [match.call()]
#' @param fargs list: the formal parameters of a function, typically obtained using [formals()]
#' @param ... named parameter to propagate, provided they are not lists
#'
#' @return a modified argument list
#' @export
#'
#' @examples
#' test <- function(x, y, plot=list(type="n", axes=FALSE, xlab="xlab", ylab="ylab"),
#'                  axis1=list(side=1), axis2=list(side=2), ...) {
#'    update(argList(match.call(), formals(), axis1.side='axis1', axis2.side='axis2'),
#'           plot='plotparams', axis1='axisparams', axis2='axisparams')
#' }
#'
#' # set global parameters
#' setParamList('plotparams', graphics::plot.default, deny=c('...', 'x', 'y', 'axes'))
#' setParamList('axisparams', graphics::axis, deny=c('...', 'at', 'labels'))
#' # propagate parameter `main` to `plot`
#' str(test(iris[,1], iris[,2], main="plain"))
#' # and propagate `axis2`
#' str(test(iris[,1], iris[,2], axis2=4, main="axis2==4"))
#' # and set local parameter `axis2.side`
#' str(test(iris[,1], iris[,2], axis2=3, axis2.side=4, main="axis2=3 & axis2.side==4"))
argList <- function(margs, fargs, ...) { UseMethod('argList') }

#' @rdname argList
#' @export
argList.default <- function (margs, fargs, ...) {
#  browser()
  al <- fargs[!names(fargs) %in% "..."]
  nm <- names(margs)
  for (i in seq_along(margs)) {
    if (nchar(nm[i])) al[[nm[i]]] <- margs[[i]]
  }
  al <- lapply(al, env=parent.frame(),
               function(e, env) { if (is.language(e)) eval(e, envir=env) else e})
  nal <- names(al)
  args  <- list(...)
  nargs <- names(args)
  if (is.null(nargs)) nargs <- rep('', length(args))
  prop <- NULL
#  browser()
  for (i in seq_along(args)) {
    namei  <- nargs[i]
    parami <- as.character(args[[i]])[1]
    if ((parami %in% names(al)) && (!is.list(al[[parami]]))) {
      if (!namei %in% names(al)) al[[namei]] <- al[[parami]]
      prop <- c(prop, parami)
    }
  }
#  browser()
  prop <- intersect(prop, names(fargs))
  if (length(prop)) {
    for (i in seq_along(prop)) {
      al[[prop[i]]] <- fargs[[prop[i]]]
      if (is.language(al[[prop[i]]])) al[[prop[i]]] <- eval(al[[prop[i]]])
    }
  }
#  browser()
  structure(al, class=c('argList', class(al)))
}

#' update
#'
#' Updates a argument list with global and local parameters
#'
#' @param object argument list: list of arguments
#' @param ... named parameter for managing global and local parameters
#' @return a modified argument list
#' @export
#'
#' @examples
#' 1+1
update.argList <- function(object, ...) {
#  browser()
  args <- list(...)
  nargs <- names(args)
  if (is.null(nargs)) return(object)
  nobject <- names(object)
  # global parameters
  for (i in seq_along(nargs)) {
    namei  <- nargs[i]
    parami <- do[[as.character(args[[i]])[1]]]
    if (is.list(object[[namei]]) && (!is.null(parami))) {
      pj <- intersect(names(object), parami)
      object[[namei]][pj] <- object[pj]
    }
  }
#  browser()
  # local params
  for (i in seq_along(nargs)) {
    namei  <- nargs[i]
    posi   <- which(startsWith(nobject, paste0(namei, '.')))
    parami <- substring(nobject, 2+nchar(namei))[posi]
    object[[namei]][parami] <- object[nobject[posi]]
  }
#  browser()
  object
}
