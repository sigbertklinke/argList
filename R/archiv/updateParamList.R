#' @rdname updateParamList
#' @aliases uPL
#' @title updateParamList
#' @description Updates a given parameter list by global and local parameters.
#'
#' @param fargs formal arguments of the calling function
#' @param margs specified arguments of the calling function
#' @param ... further arguments
# @param funname character: function name with allowed parameter
# @param paramname character: name of the parameter (for local parameters)
# @param paramlist list: list of default parameters (usually from [formals()])
#'
#' @return the modified parameter list
#' @export
#'
#' @examples
#' 1+1
# test <- function(x, axis=list(side=1, at=5), ...) {
#   setEllipsis(list(...))
#   updateParamList('axis', 'axis', formals()$axis)
# }
# #
# setParamList('axis', graphics::axis, deny=c('at', 'labels'), allow=names(par(no.readonly=TRUE)))
# test(1, at=0, side=3) # at will not be modified
#updateParamList <- function(funname, paramname, paramlist) {
#  paramlist <- as.list(paramlist)
#  if (paramlist[[1]]=="list") paramlist <- paramlist[-1]
#  # global params
#  nell <- names(do[['...']])
#  if (length(nell)) {
#    args <- intersect(nell, do[[funname]])
#    do[['...used']] <- c(do[['...used']], args)
#    paramlist[args] <- do[['...']][args]
#    # local params
#    prefix <- paste0(paramname, '.')
#    args <-  nell[startsWith(nell, prefix)]
#    do[['...used']] <- c(do[['...used']], args)
#    paramlist[substring(args, nchar(prefix)+1)] <- do[['...']][args]
#  }
#  paramlist
#}
updateParamList <- function(fargs, margs, ...) {
  browser()
  eargs  <- list(...)
  nargs <- names(eargs)
  largs <- list()
  if (is.null(nargs)) return(list())
  for (i in seq_along(nargs)) {
    namei <- nargs[i]
    if (nchar(namei)>0) {
      largs[[namei]] <- if (is.null(fargs[[namei]])) list() else fargs[[namei]]
      if (is.language(largs[[namei]])) largs[[namei]] <- as.list(largs[[namei]])[-1]
      if (namei %in% names(margs)) {
        if (is.list(margs[[namei]])) largs[[namei]] <- margs[[namei]]
      }
    }
  }
  nell  <- names(do[['...']])
  if (length(nell)) {
    for (i in seq_along(nargs)) {
      namei  <- nargs[i]
      parami <- as.character(eargs[[i]])
      # global parameters
      if (isTRUE((nchar(namei)>0) && (nchar(parami)>0))) {
        args <- intersect(nell, do[[parami]])
        do[['...used']] <- c(do[['...used']], args)
        largs[[namei]][args] <- do[['...']][args]
      }
      # local params
      prefix <- paste0(namei, '.')
      args <-  nell[startsWith(nell, prefix)]
      do[['...used']] <- c(do[['...used']], args)
      largs[[namei]][substring(args, nchar(prefix)+1)] <- do[['...']][args]
    }
  }
  browser()
  #  if (length(nargs)) {
  #    # init
  #    pos1 <- which(nchar(nargs)>0)
  #    for (i in pos1) largs[[nargs[i]]] <- fargs[[nargs[i]]]
  #    # run updateEllipsis
  #    pos0 <- which(nchar(nargs)==0)
  #    for (i in pos0) {
  #      ni <- args[[i]][1]
  #      vi <- if (length(args[[i]][1])==2) ni else args[[i]][3]
  #      if ((vi %in% names(margs)) && !updateEllipsis(ni, args[[i]][2], margs[[vi]])) largs[[ni]] <- margs[[vi]]
  #    }
  #    # run updateParamLists
  #    browser()
  #    for (i in pos1) {
  #      print(ni)
  #      ni <- nargs[i]
  #      largs[[ni]] <- updateParamList(ni, args[[i]], largs[[ni]])
  #    }
  #  }
  largs
}

#' @rdname updateParamList
#' @export
uPL <- updateParamList
