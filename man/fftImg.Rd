\name{fftImg}
\alias{fftImg}
\title{Compute FFT image}
\description{
  This function computates the power spectrum of a given image by FFT.
}
\usage{
fftImg(img)
}
\arguments{
  \item{img}{target image}
}
\value{
  an imagematrix
}
\references{
  FFTW (Fastest Fourier Transform in the West) \url{http://www.fftw.org}
}
\seealso{\code{\link{fftw}},\code{\link{imagematrix}}}
\examples{
  \dontrun{
  data(logo)
  plot(normalize(fftImg(logo)))
  }
}
\keyword{misc}
