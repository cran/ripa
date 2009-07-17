\name{writeBand}
\alias{writeBand}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to save an AVIRIS image band}
\description{
  Saves an image band using \code{wband} interface.
}
\usage{
writeBand(I, Z, X = NA, C = 614, F = 512, B = 224)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{I}{Name of the image file}
  \item{Z}{Data to be saved}
  \item{X}{Number of the band}
  \item{C}{Number of columns of each band}
  \item{F}{Number of lines of each band}
  \item{B}{Total number of bands}
}

\author{Marcelo Almiron and Adrian Muract}

\seealso{See Also \code{\link{loadBand}}}

\keyword{methods}% at least one, from doc/KEYWORDS

