\name{ind.print}
\alias{ind.print}
\title{List Triplets in a Model}
\description{Give a list of all elementary independencies in the given independence model.
}
\usage{
ind.print(ind=NA,model=NA,type=NA)
}
\arguments{
  \item{ind}{independence string of 24 characters (0/1)}
  \item{model}{model number (1-629)}
  \item{type}{type number (1-53)}
}
\seealso{
\code{\link{ind.plot}}
}
\examples{
i<-ind.identification(type=12)
ind.print(i$ind)
print(i)
}
\keyword{print}
