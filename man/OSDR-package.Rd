\name{OSDR-package}
\alias{OSDR-package}
\docType{package}
\title{
\packageTitle{OSDR}
}
\description{
\packageDescription{OSDR}
}
\details{
%The DESCRIPTION file:
\packageDESCRIPTION{OSDR}
\packageIndices{OSDR}

The aim of this small package is to provide a function solving the optimal matching problem on an \emph{ordered} set. The main function \code{OSDR} exploits an algorithm suggested by I.Anderson to find the order optimal matching, as defined by D.Gale (see references). This matching problem can be viewed in two ways. First, as the original ordered assignment problem, where jobs must be filled by suitable applicants in order of importance. Second, as the problem of choosing for each treated unit a control to match on, unit within a pre-specified caliper having zero matching cost and units outside the caliper having infinity cost. Routines for classic combinatorial optimization problems are available on R via the \pkg{\link[optrees]{optrees}} package and the \pkg{\link[maxmatching]{maxmatching}} package. A package for statistically oriented optimal matching is the \pkg{\link[optmatch]{optmatch}} package which can be used in observational studies to find a minimum covariate distance matching of control to treated units prior to outcome analysis (Rosenbaum). This package includes some examples (both combinatorial and statistically oriented) to illustrate the use of \code{\link{OSDR}}.
}
\author{
\packageAuthor{OSDR}

Maintainer: \packageMaintainer{OSDR}
}
\references{

Gale, D. (1968) Optimal matching in an ordered set: an application of matroid theory. \emph{Journal of Combinatorial Theory 4, pp. 176-180}.

Anderson, I. (1989) A first course in Combinatorial Mathematics. \emph{Oxford University Press}.

Rosenbaum, P. R. (1989). Optimal matching for observational studies. \emph{Journal of the American Statistical Association 84(408): pp. 1024-1032}. 

Cannas, M. Order optimal matching for statistical application: a gender gap case study (available on \emph{arXiv}).

}

\keyword{ ordered assignment problem, order optimal matching, observational studies. }
\seealso{

\code{\link[optrees]{optrees}},
\code{\link[maxmatching]{maxmatching}}
\code{\link[optmatch]{optmatch}}. 
}
\examples{
#See OSDR help for the examples. 
}
