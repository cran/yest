\name{ind.model}
\alias{ind.model}
\title{Independence Model Corresponding to Variance Matrix}
\description{
}
\usage{
ind.model(V,tol=0.000001,inv=TRUE) 
}
\arguments{
  \item{V}{(inverse) variance matrix}
  \item{tol}{tolerance constant}
  \item{inv}{is inverse variance (TRUE) or variance (FALSE) matrix in \code{V}}
}
\value{
A character string of all independence relations that occur in a matrix \code{V}.
}
\seealso{\code{\link{ind.identification}}}
\examples{
V<-ind.rgauss(type=12)
ind.plot(ind.model(V))
}
\keyword{misc}
