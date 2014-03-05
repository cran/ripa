\name{highpass}
\alias{highpass}
\title{High pass filter for image}
\description{
  Computes a high-pass filtered image with dimensions of the given
  input image. the hp pasing distance is given via radius.
}
\usage{
highpass(img,radius)
}
\arguments{
  \item{img}{pixmap or image}
  \item{radius}{gives the blocking radius}
}
\value{
  an imagematrix
}
\seealso{\code{\link{lowpass}}}
\examples{
  \dontrun{
  data(logo)
  plot(normalize(highpass(logo)))
  }
}
\keyword{misc}
