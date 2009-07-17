\name{aviris_training-class}
\Rdversion{1.1}
\docType{class}
\alias{aviris_training-class}
\alias{aviris_training-method}

\title{Class "aviris_training"}
\description{Class to create a training category.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("aviris_training", category, color, scene, band, posX, posY)}.
}
\section{Slots}{
	 \describe{
    \item{\code{category}:}{Object of class \code{"character"}. Name of the new category.}
    \item{\code{color}:}{Object of class \code{"character"}. Color that will represent the category.}
    \item{\code{scene}:}{Object of class \code{"aviris_scene"}.}
    \item{\code{bands}:}{Object of class \code{"list"}.}
    \item{\code{posX}:}{Object of class \code{"numeric"}.}
    \item{\code{posY}:}{Object of class \code{"numeric"}.}
  }
}
\section{Methods}{
  \describe{
    \item{initialize}{\code{signature(.Object = "aviris_training")}: ... }
	 }
}

\author{Marcelo Almiron and Adrian Muract}

\seealso{See Also \code{\link{aviris_image-class}}, \code{\link{aviris_scene-class}}, \code{\link{aviris_training-class}}}

\examples{
showClass("aviris_training")
}
\keyword{classes}
