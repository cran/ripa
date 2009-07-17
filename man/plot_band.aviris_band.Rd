\name{plot_band.aviris_band}
\alias{plot_band.aviris_band}

\title{Function to plot an AVIRIS band}
\description{
  Plots an AVIRIS band defined as a composition of three bands (R, G and B)
}
\usage{
	\method{plot_band}{aviris_band}(R = NULL, G = NULL, B = NULL, type = NULL, x0 = 1, y0 = 1, \dots)
}

\arguments{
  \item{R}{The R band}
  \item{G}{The G band}
  \item{B}{The B band}
  \item{type}{Type of the image: "grey" or "rgb".}
  \item{x0}{1}
  \item{y0}{1}
  \item{\dots}{Other possible arguments}
}

\author{Marcelo Almiron and Adrian Muract}

\keyword{methods}% at least one, from doc/KEYWORDS

