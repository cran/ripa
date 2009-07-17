\name{aviris_image-class}
\Rdversion{1.1}
\docType{class}
\alias{aviris_image-class}
\alias{aviris_image-method}

\title{Class "aviris_image"}
\description{Class that represents an AVIRIS image}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("aviris_image", name, numberOfScenes, linesInLastScene, type, path)}.
}
\section{Slots}{
	 \describe{
    \item{\code{name}:}{Object of class \code{"character"}.}
    \item{\code{numberOfScenes}:}{Object of class \code{"numeric"}.}
    \item{\code{linesInLastScene}:}{Object of class \code{"numeric"}.}
    \item{\code{type}:}{Object of class \code{"character"}.}
    \item{\code{path}:}{Object of class \code{"character"}.}
  }
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "aviris_image")}: ... }
	 }
}

\author{Marcelo Almiron and Adrian Muract}

\seealso{See Also \code{\link{aviris_band-class}}, \code{\link{aviris_scene-class}}, \code{\link{aviris_training-class}}}

\examples{
showClass("aviris_image")
}
\keyword{classes}
