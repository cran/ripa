\name{aviris_scene-class}
\Rdversion{1.1}
\docType{class}
\alias{aviris_scene-class}
\alias{aviris_scene-method}

\title{Class "aviris_scene"}
\description{Class that represents a scene of an AVIRIS image}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("aviris_scene", name, numberOfLines, samples, bands, imageName, type, path)}.
}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{Object of class \code{"character"}.}
    \item{\code{numberOfLines}:}{Object of class \code{"numeric"}.}
    \item{\code{samples}:}{Object of class \code{"numeric"}.}
    \item{\code{bands}:}{Object of class \code{"numeric"}.}
    \item{\code{imageName}:}{Object of class \code{"character"}.}
    \item{\code{type}:}{Object of class \code{"character"}.}
    \item{\code{path}:}{Object of class \code{"character"}.}
  }
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "aviris_scene")}: ... }
	 }
}

\author{Marcelo Almiron and Adrian Muract}

\seealso{See Also \code{\link{aviris_image-class}}, \code{\link{aviris_band-class}}, \code{\link{aviris_training-class}}}

\examples{
showClass("aviris_scene")
}
\keyword{classes}
