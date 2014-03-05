\name{rgb2grey}
\alias{rgb2grey}
\title{Convert color imagematrix to grey imagematrix}
\description{
  This function convert color imagematrix to grey imagematrix.
}
\usage{rgb2grey(img, coefs=c(0.30, 0.59, 0.11))}
\arguments{
  \item{img}{target image}
  \item{coefs}{coefficients for red plane, green plane, and blue plane.}
}
\value{
  grey imagematrix
}
\examples{
\dontrun{
  x <- read.jpeg(system.file("extdata", "Rlogo.jpg", package="ripa"))
  plot(rgb2grey(x))
  }
}
\keyword{misc}


