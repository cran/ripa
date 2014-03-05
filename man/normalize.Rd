\name{normalize}
\alias{normalize}
\title{Normalization for vector and matrix}
\description{
  This function normalizes image so that the minimum value is 0 and
  the maximum value is 1.
}
\usage{normalize(img)}
\arguments{
  \item{img}{target image}
}
\value{
  Data of the same mode as 'img', in which minimum value is 0 and maximum
  value is 1.
}
\examples{
  data(logo)
  plot(normalize(logo))
}
\keyword{misc}


