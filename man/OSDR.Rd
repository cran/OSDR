\name{OSDR}
\alias{OSDR}
\title{
Finds an Optimal System of Distinct Representatives for a family of subsets. 
}
\description{
The function finds an Optimal Set of Distinct Representatives (OSDR) from the ordered family of feasible representatives. Optimality is order-wise in the sense specified by Gale: any other SDR either is shorter or its elements are partly chosen from less important sets (see details). 
}
\usage{
OSDR(M)
}
\arguments{
  \item{M}{
A list specifying the feasible applicants (representatives) for each set. The list is assumed \emph{ordered} by decreasing matching priority. 
}
}
\details{
Consider a set of jobs and screen the jobs in decreasing order of priority until finding for each job the subset of suitable applicants. It was shown by D.Gale that there exists an \emph{optimally assignable} subset of the jobs in the sense that if \eqn{\{a_1, \cdots a_k\}} is optimal and \eqn{\{b_1, \cdots b_h\}} is another assignable set then either \eqn{k > h} or there exists \eqn{i} such that \eqn{a_i < b_i} (the latter meaning that job \eqn{a_i} has priority over job \eqn{b_i}). From a graph perspective, OSDR is a \emph{order wise} maximum size matching of the bipartite graph with vertex set \eqn{J\cup A} where \eqn{J} is the set of jobs and \eqn{A} is the set of applicants. In statistical matching problems the sets \eqn{J} and \eqn{A} are replaced by the sets of treated and control units and it is usually required to find the minimum covariate distance matching of treated versus control units. When a complete matching does not exist it can be convenient to find an order optimal matching or a minimum cost matching on the optimally matchable subset \code{OSDR}. 
}
\value{

The OSDR, an ordered vector containing in position \emph{i} the representative of the \eqn{i^{th}} set. A zero indicates impossibility of finding a representative for that set (i.e. Hall's condition is not met and thus a complete matching is not possible).
}
\references{
Gale, D. (1968) Optimal matching in an ordered set: an application of matroid theory. \emph{Journal of Combinatorial Theory 4: 176-180}.

Anderson, I. (1989) A first course in Combinatorial Mathematics. \emph{Oxford University Press}.

}
\author{
\packageMaintainer{OSDR}
}
\note{
Function \code{OSDR} finds the optimal matching using an algorithmic 
proof of Hall's theorem due to logician D.J. Shoesmith.
}

\seealso{
See also \code{\link{matlist}}, \code{ \link{listmat}} 
}
\examples{
# example 1
# M is a family of feasible representatives (suitable applicants) for five jobs
M1 <- c("A" , "B")
M2 <- c("B" , "C")
M3 <- c("B")
M4 <- c("A" , "C")
M5 <- c("B" , "C" , "D")
M  <- list(M1 , M2 , M3 , M4 , M5)
M

OSDR(M)

# job 4 unmatched so Hall's condition is not satisfied: it's impossible to fill all the jobs
# note that it is possible an order-suboptimal assignment of the same length of the optimal, 
# eg: 0CBAD , BC0AD

# example 2: sligthly modified: more than one order optimal matching
M1<-c("A","B","C")
M2<-c("A","C")
M3<-c("B")
M4<-c("A","C")
M5<-c("A","D")
M  <-list(M1,M2,M3,M4,M5)

OSDR(M)
# note that there are other maximum size matchings 
# e.g. opt = ACB0D or CAB0D  and subopt = 0CBAD or BC0AD

# case study: matching men and women executives
# load executive data  
data(exdata)

# descriptives on:
# sex(0=M; 1=F) ;
# position (4=top manager, 3=medium/first line manager, 2 =supervisor);
table(exdata$sex)# there are more women
table(exdata$position,exdata$sex)# and more in apical position

# order by matching priority (high-rank women first)
# see e.g. Lynn and Thompson(1997), J. of Appl. Psych. 82(3))
data <- exdata[order(-exdata$sex,-exdata$position, -exdata$seniority),] 

# covariate distance matrix
require(optmatch)
dist <- match_on(sex ~ position+education+year_born+contract+part_fulltime+seniority ,
data=exdata) 
# use broad caliper to avoid very bad matches
dist <- caliper(dist,4,values=TRUE)

# minimum distance pair matching (package optmatch)
copt <- pairmatch(dist,data=exdata)
summary(copt)
sum(matched.distances(copt,dist)) # total cost 19

### find osdr
#order dist by priority order (i.e by decreasing position)
dist <- as.matrix(dist)[order(match(rownames(dist),rownames(exdata))),]
mylist <- matlist(dist)
res <- OSDR(mylist)

# index and labels of treated and untreated in OSDR
ord_dist<-as.matrix(dist)[order(match(names(mylist),rownames(exdata))),]
index_t<-res$matching; names_t<-rownames(ord_dist)[index_t]
index_ut<-res$unmatched;names_ut<-rownames(ord_dist)[index_ut]

# compare matched treated: optmatch vs ordmatch 
 #matched treated optmatch
   matched(copt)[which(exdata$sex==1)]     
 #matched treated ordmatch
   rownames(data)[which(exdata$sex==1)] \%in\% names_t
   
   
 # compare total matching cost: optmatch vs ordmatch
 
 # case 1: distance matrix is zero infinity: same cost (0)
 # case 2: distance matrix is not zero infinity
 #         find minimum cost matching on osdr:

    data2<-exdata[-match(names_ut,rownames(exdata)),]
    dist2<-as.matrix(dist)[-match(names_ut,rownames(dist)),]
    copt2 <- pairmatch(dist2,data=data2)
    summary(copt2);copt2
    sum(matched.distances(copt2,dist2)) # 22
    sum(matched.distances(copt,dist)) # 19


}
\keyword{ ~order optimal matching }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~incomplete matching }% __ONLY ONE__ keyword per line
