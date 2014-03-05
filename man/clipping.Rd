\name{clipping}
\alias{clipping}
\title{Clipping image}
\description{
  This function returns the image which restricts pixel value from the
  specified lowest value to the specified highest value
  in the original image. This means that the pixels which have lower
  value than the given lowest (default: 0) are replaced to
  the lowest and the pixels have greater value than
  the given highest (default: 1) are replaced to the highest.
}
\usage{clipping(img, low=0, high=1)}
\arguments{
  \item{img}{target image}
  \item{low}{lowest value}
  \item{high}{highest value}
}
\value{
  Data of the same mode as 'img'
}
\examples{
  data(logo)
  op <- par(mfrow=c(2,2))
  plot(logo, main="Source Image")

  # the appearance of next one doesn't change because of normalization.
  plot(normalize(2*logo), main="Doubled pixel value with normalization")

  # the next one is saturated as expected
  plot(clipping(2*logo), main="Doubled pixel value with clipping")
}
\keyword{misc}


