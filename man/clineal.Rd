\name{clineal}
\alias{clineal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Contrast linear expansion algorithm for AVIRIS images}
\description{
  Applies the contrast linear expantion algorithm to de input data.
}
\usage{
clineal(Z, A, B)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Z}{Input data as a matrix or vector}
  \item{A}{Minimum value of range (0)}
  \item{B}{Maximum value of range (1)}
}

\value{
  Returns the expansion result
}

\author{Marcelo Almiron and Adrian Muract}

\seealso{See Also \code{\link{cgauss}}}

\keyword{methods}% at least one, from doc/KEYWORDS

