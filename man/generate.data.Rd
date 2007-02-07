\name{generate.data}
\alias{generate.data}
\title{Simulate from a Multivariate Gaussian Distribution}
\description{Produces samples from the specified Gaussian distribution.
}
\usage{
generate.data(V=NA,n=1000,Sigma=NA,mu=c(0,0,0,0))
}
\arguments{
  \item{V}{inverse variance matrix of Gaussian distribution}
  \item{Sigma}{variance matrix of Gaussian distribution}
  \item{n}{number of rows to be generated}
  \item{mu}{expected value of Gaussian distribution}
}
\details{
Either \code{Sigma} or \code{V} must be entered. 
}
\value{
A matrix of \code{n} rows and \code{4} columns. 
Each row is generated from a multivariate Gaussian distribution 
with parameters \code{mu} and \code{Sigma}.
}
\seealso{
\code{\link{mvrnorm}}
}
\examples{
V<-ind.rgauss(type=12)
data<-generate.data(V)
summary(data)
}
\keyword{datagen}
