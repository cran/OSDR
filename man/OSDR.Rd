\name{OSDR}
\alias{OSDR}
\title{
Finds an Optimal System of Distinct Representatives from a family of subsets. 
}
\description{
The function finds an Optimal Set of Distinct Representatives (OSDR) from an ordered family of subsets. Optimality is order-wise in the sense specified by Gale: any other SDR cannot be larger and its elements cannot be chosen from more important sets (see details). 
}
\usage{
OSDR(M)
}
\arguments{
  \item{M}{
An ordered family of subsets. In assignment problems the list specifying the feasible applicants for each job. In matching problems the list specifying the controls matchable with each treated unit. The list is assumed \emph{ordered} by decreasing assignment/matching priority. 
}
}
\details{
Consider a set of jobs and a set of suitable applicants for each job. It was shown by D.Gale that there exists an \emph{optimally assignable} subset of the jobs in the sense that if \eqn{{a_1, \ldots a_k}} is optimal and \eqn{{b_1, \ldots b_h}} is another assignable set of jobs then: a) \eqn{k \ge h}  and b)  \eqn{a_i \le b_i}  for  all \eqn{i}. The latter means that job \eqn{a_i} has as higher a priority as job \eqn{b_i}. From a graph perspective, if \eqn{J} is the set of jobs and \eqn{A} is the set of applicants the OSDR is an \emph{order wise} maximum size matching of the bipartite graph with vertex set \eqn{J union A}. 

Function \code{OSDR} finds the optimal matching using an algorithmic proof of Hall's theorem due to logician D.J. Shoesmith. The algorithm assigns greedily until possible and correct backward when necessary. A message is given when a correction is attempted (when it fails another message warns that Hall's condition is not satisfied).
}
\value{
A list containing three elements: the OSDR (a zero in position \emph{i} indicates impossibility of assigning job \emph{i}), the index of optimally assignable jobs and the index of unassignable jobs. If the latter is not empty \code{M} does not meet Hall's condition, i.e., a complete SDR is not possible.

%A list containing three elements: the optimally assignable subset of jobs, the optimal assignment (i.e. the OSDR) and a list of unmatched jobs. 
%the osdr, i.e. an ordered vector containing in position \emph{i} the representative of the \eqn{i^{th}} set. A zero indicates impossibility of finding a representative for that set (i.e. Hall's condition is not met and thus a complete matching is not possible).
}
\references{
Gale, D. (1968) Optimal matching in an ordered set: an application of matroid theory. \emph{Journal of Combinatorial Theory 4: 176-180}.

Anderson, I. (1989) A first course in Combinatorial Mathematics. \emph{Oxford University Press}.

}
\author{
\packageMaintainer{OSDR}
}
\note{
In statistical matching problems the sets \eqn{J} and \eqn{A} are the sets of treated and control units and it is usually required to find the minimum covariate distance matching of treated versus control units. When a complete matching does not exist it can be convenient to find either an order optimal matching or a minimum cost matching on the \code{OSDR} subset (see the gender gap example). 
}

\seealso{
See also \code{\link{matlist}}, \code{ \link{listmat}} 
}
\examples{
### example 1
# M is the list of suitable applicants for five jobs
M1 <- c("A" , "B")
M2 <- c("B" , "C")
M3 <- c("B")
M4 <- c("A" , "C")
M5 <- c("B" , "C" , "D")
M  <- list(M1 , M2 , M3 , M4 , M5)

OSDR(M)

# $OSDR
# [1] "A" "C" "B" "0" "D"
# $matched
# [1] 1 2 3 5
# $unmatched
# [1] 4

# job 4 unmatched so Hall's condition is not satisfied: it's impossible to fill all the jobs
# note that there are (order-\emph{suboptimal}) assignments of the same length of the optimal: 
# eg: 0CBAD , BC0AD

#### example 2: sligthly modified: more than one order optimal matching
M1<-c("A","B","C")
M2<-c("A","C")
M3<-c("B")
M4<-c("A","C")
M5<-c("A","D")
M  <-list(M1,M2,M3,M4,M5)

OSDR(M)

# note there are other order optimal matchings: ACB0D or CAB0D 
# note there are also other maximum size matchings (not order optimal):
# e.g. 0CBAD or BC0AD


}
\keyword{ ~order optimal matching }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~incomplete matching }% __ONLY ONE__ keyword per line
