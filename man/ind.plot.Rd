\name{ind.plot}
\alias{ind.plot}
\title{Draw Diagram}
\description{Draw Lnenicka's diagram corresponding to the given independence model.
}
\usage{
ind.plot(ind=NA,model=NA,type=NA)
}
\arguments{
  \item{ind}{independence string of 24 characters (0/1)}
  \item{model}{model number (1-629)}
  \item{type}{type number (1-53)}
}
\seealso{
\code{\link{ind.print}}
}
\examples{
i<-ind.identification(type=12)
ind.plot(i$ind)
print(i)
}
\keyword{hplot}
