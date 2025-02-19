% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/paramlist.R
\name{setParamList}
\alias{setParamList}
\alias{getParamList}
\title{Parameter lists}
\usage{
setParamList(funname, fun = NULL, deny = NULL, allow = NULL)

getParamList(funname = NULL)
}
\arguments{
\item{funname}{character: function name}

\item{fun}{character/function: function to extract parameters by \code{\link[base:formals]{base::formals()}} (default: \code{NULL} = no formals are extracted)}

\item{deny}{character: parameter names which should not be modified by global parameters}

\item{allow}{character: additional parameter names which could be modified by global parameters}
}
\value{
returns invisibly the currently parameter list(s)
}
\description{
\code{setParamList} sets the global parameter for a function, \code{getParameterList} returns the currently parameter list(s).
\code{setParamList} could be used in functions like \code{.onLoad} for a package or locally in a function.
}
\examples{
# allow all parameters from [graphics::axis()] and `par(no.readonly=TRUE)`,
# but deny `at`and `labels`
setParamList('axis', graphics::axis, deny=c('at', 'labels'), allow=names(par(no.readonly=TRUE)))
getParamList()
}
