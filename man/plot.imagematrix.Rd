\name{plot.imagematrix}
\alias{plot.imagematrix}
\title{Plotting an imagematrix object}
\description{
  This function outputs an imagematrix object as an image.
}
\usage{
  \method{plot}{imagematrix}(x, ...)}
\arguments{
  \item{x}{target image}
  \item{...}{plotting options}
}
\examples{
  op <- par(mfrow=c(1,2))

  data(logo)
  plot(logo, main="plot(logo)")
  plot(logo^2, main="plot(logo^2)")

  par(op)
}
\seealso{\code{\link{imagematrix}}}
\keyword{misc}


