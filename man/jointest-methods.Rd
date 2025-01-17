% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jointest-methods.R
\docType{methods}
\name{jointest-methods}
\alias{jointest-methods}
\alias{print.jointest}
\alias{summary.jointest}
\alias{p.adjust}
\alias{plot.jointest}
\title{Methods for \code{jointest} objects}
\usage{
\method{print}{jointest}(x, ...)

\method{summary}{jointest}(object, ...)

p.adjust(object, method = "maxT", tail = 0, ...)

\method{plot}{jointest}(x, ...)
}
\arguments{
\item{x}{an object of class \code{jointest}.}

\item{...}{additional arguments to be passed, i.e., \code{mark_signif} and
\code{p.values=c("raw","adjusted")}. See details.}

\item{object}{an object of class \code{jointest}.}

\item{method}{any method implemented in \code{flip::flip.adjust} or 
a custom function. In the last case it must be a function that uses a matrix 
as input and returns a vector of adjusted p-values equal to the number of columns of the inputed matrix.}

\item{tail}{argument: expresses the tail direction of the alternative hypothesis. 
It can be "two.sided" (or 0, the default), "less" (or -1) or "greater" (or +1).}
}
\description{
Methods to extract and manipulate relevant information from
a \code{jointest} object.

\code{print} method for class "\code{jointest}".

\code{summary} method for class "\code{jointest}"

\code{p.adjust} method for class "\code{jointest}". 
Add adjusted p-values into the \code{jointest} object.

\code{plot} method for class "\code{jointest}"
This \code{plot} function visualizes p-values from multiverse models, with
different markers to indicate statistical significance levels as defined by
the \code{mark_signif} argument (default is 0.05). Points are plotted with 
varying shapes based on whether the p-value is below the significance threshold,
and colors are used to distinguish between different coefficients.
}
\details{
\code{mark_signif} argument: numeric value representing the significance threshold 
for marking p-values. Any p-value below this threshold will be marked 
with a dot. The default is \code{0.05}.
\code{p.values} argument: a character vector specifying which p-values to display.
It can be either \code{"raw"} for raw p-values or \code{"adjusted"} for 
adjusted p-values. The default is \code{"raw"}.
}
\seealso{
\code{\link[flip]{flip.adjust}}
}
