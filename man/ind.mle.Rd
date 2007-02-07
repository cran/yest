\name{ind.mle}
\alias{ind.mle}
\title{Parameters' Estimation for Given Independence Model}
\description{Find parameters of Gaussian distribution under conditional independence contraints given by an independence model.
}
\usage{
ind.mle(data=NA,V=NA,Sigma=NA,n=NA,ind=NA,model=NA,type=NA,tol =
1e-06,nb.trials=10)
}
\arguments{
  \item{data}{dataset}
  \item{V}{inverse variance matrix}
  \item{Sigma}{variance matrix}
  \item{n}{number of lines in dataset}
  \item{ind}{independence string}
  \item{model}{model number (1-629)}
  \item{type}{type number (1-53)}
  \item{tol}{tolerance for numerical lack of positive-definiteness}
  \item{nb.trials}{how many starting values should be tried in optimization}
}
\details{
Either \code{data} or \code{V}/\code{Sigma} and \code{n} must be entered.
}
\value{
A list of expected value, inverse variance matrix, variance matrix, AIC and BIC.
}
\seealso{\code{\link{model.selection}}}
\examples{
i<-ind.identification(type=12)$model
V<-ind.rgauss(model=i)
data<-generate.data(V,10000)
ind.mle(data,model=i)$model
}
\keyword{multivariate}
