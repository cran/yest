\name{ind.identification}
\alias{ind.identification}
\title{Identification of Type, Model and List of Conditional Independences}
\description{Translate one kind of a description of an independence model to the others.
}
\usage{
ind.identification(ind=NA,model=NA,type=NA)
}
\arguments{
  \item{ind}{independence string of 24 characters (0/1)}
  \item{model}{model number (1-629)}
  \item{type}{type number (1-53)}
}
\details{
You may enter either a type or a model or a character string  
and the other two ways of description are added. If more parameters entered
then just the first one is used and the rest is ignored.
}
\value{
A list with type/model number and independence character string.
}
\examples{
i<-ind.identification(type=12)
ind.plot(i$ind)
print(i)
}
\keyword{utilities}
