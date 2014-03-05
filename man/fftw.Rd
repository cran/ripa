\name{fftw}
\alias{fftw}
\title{Apply FFT to 2-Dimensional Data}
\description{
  This function applies FFT to 2-dimensional data (i.e. image) using
  fftw library.
}
\usage{
fftw(img, dir = -1, debug=FALSE)
}
\arguments{
  \item{img}{target image}
  \item{dir}{set -1 for normal transformation and 
    1 for inverse transformation}
  \item{debug}{set TRUE if you want to output debug message}
}
\value{
  a matrix of complex number
}
\references{
  FFTW (Fastest Fourier Transform in the West) \url{http://www.fftw.org}
}
\seealso{\code{\link{fftw}}}
\keyword{misc}
