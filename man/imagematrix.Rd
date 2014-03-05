\name{imagematrix}
\alias{imagematrix}
\title{Generate an imagematrix, i.e. primary data structure of rimage}
\description{
  This function makes an imagematrix object from a matrix. This data
  structure is primary data structure to represent image in rimage package.
}
\usage{imagematrix(mat, type=NULL,
         ncol=dim(mat)[1], nrow=dim(mat)[2], noclipping=FALSE)}
\arguments{
  \item{mat}{array, matrix or vector}
  \item{type}{"rgb" or "grey"}
  \item{ncol}{width of image}
  \item{nrow}{height of image}
  \item{noclipping}{TRUE if you disable automatic clipping. See details.}
}
\details{
  For grey scale image, matrix should be given in the form of 2
  dimensional matrix. First dimension is row, and second dimension is
  column.

  For rgb image, matrix should be given in the form of 3 dimensional
  array (row, column, channel). mat[,,1], mat[,,2], mat[,,3] are
  red plane, green plane and blue plane, respectively.

  You can omit 'type' specification if you give a proper array or
  matrix.
  Also, if you give a rgb image matrix and specify "grey" as type, the rgb
  image matrix is automatically converted to a grey scale image.

  This function automatically clips the pixel values which are
  less than 0 or greater than 1. If you want to disable this
  behavior, give 'noclipiing=TRUE'.

  The major difference between imagematrix and pixmap is 
  representation method. pixmap (>0.3) uses OOP class.
  On the other hand, rimage uses traditional S class.
  The advantage of traditional S class in representing image is that
  one can deal with the data structure as an ordinary matrix.

  The minor difference between imagematrix and pixmap is
  automatic data conversion behavior.
  pixmap normalizes a given matrix automatically if
  any element of the matrix is out of range between 0 and 1.
  On the other hand, imagematrix clips the matrix, which means
  that the pixels which have lower
  value than 0 are replaced to 0 and the pixels have
  greater value than 1 are replaced to 1.
}
\value{
  return an imagematrix object
}
\examples{
  p <- q <- seq(-1, 1, length=20)
  r <- 1 - outer(p^2, q^2, "+") / 2
  plot(imagematrix(r))
}
\seealso{\code{\link{plot.imagematrix}},\code{\link{print.imagematrix}}}
\keyword{misc}


