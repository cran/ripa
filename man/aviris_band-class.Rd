\name{aviris_band-class}
\Rdversion{1.1}
\docType{class}
\alias{aviris_band-class}
\alias{aviris_band-method}

\title{Class "aviris_band"}
\description{Class that represents a band of an AVIRIS image}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("aviris_band", scene, band, type, numberOfLines, samples, data)}.
}
\section{Slots}{
	 \describe{
    \item{\code{scene}:}{Object of class \code{"character"}. Name of the image scene.}
    \item{\code{band}:}{Object of class \code{"numeric"}. Number of the band.}
    \item{\code{type}:}{Object of class \code{"character"}. Type of the band, like "reflectance" for instance.}
    \item{\code{numberOfLines}:}{Object of class \code{"numeric"}.}
    \item{\code{samples}:}{Object of class \code{"numeric"}.}
    \item{\code{data}:}{Object of class \code{"matrix"}.}
    \item{\code{min}:}{Object of class \code{"numeric"}.}
    \item{\code{max}:}{Object of class \code{"numeric"}.}
    \item{\code{mean}:}{Object of class \code{"numeric"}.}
    \item{\code{sd}:}{Object of class \code{"numeric"}.}
  }
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "aviris_band")}: ... }
	 }
}

\author{Marcelo Almiron and Adrian Muract}

\seealso{See Also \code{\link{aviris_image-class}}, \code{\link{aviris_scene-class}}, \code{\link{aviris_training-class}}}

\examples{
showClass("aviris_band")
}

\keyword{classes}

