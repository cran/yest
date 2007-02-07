\name{kl.div}
\alias{kl.div}
\title{Kullback-Leibler Divergence}
\description{Compute Kullback-Leibler divergence between two multivariate normal distributions.
}
\usage{
kl.div(V0,mu0,V1,mu1=NA,inv=TRUE)
}
\arguments{
  \item{V0}{(inverse) variance matrix of the first distribution}
  \item{mu0}{expected value of the first distribution}
  \item{V1}{(inverse) variance matrix of the second distribution}
  \item{mu1}{expected value of the second distribution}
  \item{inv}{whether \code{V0} and \code{V1} are inverse varience (TRUE) or variance (FALSE) matrices}
}
\details{
If \code{mu1} is \code{NA} then a zero vector is taken as this parameter.
}
\examples{
v1<-ind.rgauss(type=12)
v2<-ind.rgauss(type=50)
kl.div(v1,rep(0,4),v2)
}
\keyword{math}
