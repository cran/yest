\name{model.selection}
\alias{model.selection}
\title{Model Selection Over Independence and Graphical Models by BIC
Optimization}
\description{Fits all 629 independence models and calculate BIC. Selects the
minimizer. 
}
\usage{
model.selection(data=NA,V=NA,Sigma=NA,n=NA,tol = 1e-06,nb.trials=10,search.table=TRUE)
}
\arguments{
  \item{data}{dataset}
  \item{V}{inverse variance matrix}
  \item{Sigma}{variance matrix}
  \item{n}{number of lines in dataset}
  \item{tol}{tolerance for numerical lack of positive-definiteness}
  \item{nb.trials}{how many starting values should be tried in optimization}
  \item{search.table}{should be a table with AIC/BIC values a part of output or not}
}
\details{
Either \code{data} or \code{V}/\code{Sigma} and \code{n} must be entered.

Both independence model and graphical model maximizing BIC are found.
}
\value{
A list of model/type number, best BIC value, variance and invarse variance matrix, model number of best graphical model, variance and invarse variance matrix of this graphical model.
}
\seealso{\code{\link{ind.mle}}}
\examples{
i<-ind.identification(type=12)$model
V<-ind.rgauss(model=i)
data<-generate.data(V,10000)
# it takes 10 minus to do this!
# model.selection(data)

data(bulls)
set.seed(123)
control<-sample(nrow(bulls),round(nrow(bulls)/10))
data1<-bulls[-control,]
data2<-bulls[control,]
S1<-var(data1)*(nrow(data1)-1)/nrow(data1)
S2<-var(data2)*(nrow(data2)-1)/nrow(data2)
#This takes 20 minutes
#m1<-model.selection(Sigma=S1,n=nrow(data1))
#m2<-model.selection(Sigma=S2,n=nrow(data2))
#kl.div(S1,S2,inv=FALSE)
#kl.div(S1,m2$Sigma,inv=FALSE)
#kl.div(m1$Sigma,S2,inv=FALSE)
#kl.div(m1$Sigma,m2$Sigma,inv=FALSE)

}
\keyword{multivariate}
