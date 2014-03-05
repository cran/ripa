\name{lowpass}
\alias{lowpass}
\title{Low Pass Filter for Image}
\description{
  Computes a low-pass filtered image with dimensions of the given
  input image. the lp pasing distance is given via radius.
}
\usage{
lowpass(img,radius)
}
\arguments{
  \item{img}{pixmap or matrix}
  \item{radius}{gives the pass radius}
}
\value{
  an imagematrix
}
\seealso{\code{\link{highpass}}}
\examples{
  \dontrun{
  data(logo)
  plot(normalize(lowpass(logo)))
  }
}
\keyword{misc}
