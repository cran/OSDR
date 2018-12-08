\name{listmat}
\alias{listmat}
\title{
From cost matrix to list of feasible assignments. 
}
\description{
Transforms the cost matrix of the assignment problem in the corresponding list of suitable applicants for each job. 
}
\usage{
listmat(x)
}
\arguments{
  \item{x}{
A cost matrix where entry \eqn{(i,j)} is the cost of assigning job \emph{i} to applicant \emph{j}.  
}
}
\details{
In statistical matching problems the input is usually the cost matrix while in assignment problems is the list of assignable elements. Functions \code{matlist} and \code{listmat} go back and forth these two representations of the input.
}
\value{
A list of suitable applicants. The \eqn{i^{th}} element of the list contains the suitable applicants for job \emph{i}. These are all applicants with \emph{finite} assignment cost. Note that if the cost matrix contains finite non zero values there is a loss of information in the transformation.
}
\author{
\packageMaintainer{OSDR}
}

\seealso{
See \code{\link{matlist}}, the reverse function.
}
\examples{
# a list of feasible applicants for five jobs	
M1<-c("A","B","C")
M2<-c("A","C")
M3<-c("B")
M4<-c("A","C")
M5<-c("A","D")
M  <-list(M1,M2,M3,M4,M5)
M

# list --> cost matrix
m <- listmat(M)

# cost matrix --> list
l <- matlist(m)
}
\keyword{ ~list of feasible assignments  }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~assignment problem }% __ONLY ONE__ keyword per line
