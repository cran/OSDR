\name{matlist}
\alias{matlist}
\title{
From list of feasible assignments to cost matrix.
}
\description{
Transforms the list of subsets of suitable applicants into the corresponding cost matrix. 
}
\usage{
matlist(x)
}
\arguments{
  \item{x}{
An ordered list of applicants for a set of jobs. The \eqn{i^{th}} element of the list contains applicants suitable for job \emph{i}. Equivalently, an ordered list of controls where the \eqn{i^{th}} element of the list contains controls matchable with the \eqn{i^{th}} treated unit.
}
}
\details{
In statistical matching problems the input is usually the cost matrix while in assignment problems is usually the list of assignable elements. Functions \code{matlist} and \code{listmat} go back and forth these two representations of the input.
}
\value{
The cost matrix of the assignment problem. The \eqn{(i,j)} entry of the matrix is zero if the \eqn{j^{th}} applicant is assignable to the \eqn{i^{th}} job and \eqn{\infty} otherwise. Equivalently, the \eqn{(i,j)} entry of the matrix is zero if the \eqn{j^{th}} control unit can be matched to the \eqn{i^{th}} treated unit and \eqn{\infty} otherwise.
}
\author{
\packageMaintainer{OSDR}
}
\seealso{
See also the reverse function \code{\link{listmat}}.
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

# the corresponding cost matrix
m<-listmat(M)

# back to the list
l<-matlist(m)

}

\keyword{ ~ assignment cost matrix }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~ assignment problem }% __ONLY ONE__ keyword per line
