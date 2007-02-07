\name{ind.rgauss}
\alias{ind.rgauss}
\title{Random Variance Matrix Generator}
\description{Generate a radnom variance matrix corresponding to the given independence model.}
\usage{
ind.rgauss(ind=NA,model=NA,type=NA)
}
\arguments{
  \item{ind}{independence string of 24 characters (0/1)}
  \item{model}{model number (1-629)}
  \item{type}{type number (1-53)}
}
\seealso{\code{\link{ind.model}}}
\examples{
V<-ind.rgauss(type=12)
ind.plot(ind.model(V))
}
\keyword{datagen}
