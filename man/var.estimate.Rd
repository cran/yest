\name{var.estimate}
\alias{var.estimate}
\title{Estimation of a Variance Matrix}
\description{ 
}
\usage{
var.estimate(data=NA,inv=TRUE,tol = 1e-06,nb.trials=10,trim=0.1)
}
\arguments{
  \item{data}{dataset}
  \item{inv}{whether variance (inv=FALSE) or inverse variance (inv=TRUE) matrix should be estimated}
  \item{tol}{tolerance for numerical lack of positive-definiteness}
  \item{nb.trials}{how many starting values should be tried in optimization}
  \item{trim}{trimming constant used for averaging}
}
\details{
Considers all groups of four variables and estimates variance matrix for each group by BIC model selection process.  
}
\value{
A list of a variance and inverse variance matrix.
}
\seealso{\code{\link{model.selection}}}
\keyword{multivariate}
