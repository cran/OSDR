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

The aim of this small package is to provide a function solving the assignment problem on an \emph{ordered} set. This matching problem can be viewed in two ways. First, as the original ordered assignment problem, where a set of jobs, ordered by importance, must be filled by suitable applicants in the best possible way. Second, as an ordered matching problem where treated unit should be matched in order of importance with suitable controls. Suitability can be obtained by a ''caliper'': units within the caliper having zero matching cost and units outside the caliper having infinity cost. The main function \code{OSDR} exploits an algorithm suggested by I.Anderson to find an \emph{order optimal} matching, as defined by D.Gale (see \code{\link{OSDR}} for details). The package includes some utilities and examples (both combinatorial and statistically oriented) to illustrate the use of \code{OSDR}.
}
\author{
\packageAuthor{OSDR}

Maintainer: \packageMaintainer{OSDR}
}
\references{

Gale, D. (1968) Optimal matching in an ordered set: an application of matroid theory. \emph{Journal of Combinatorial Theory 4, pp. 176-180}.

Anderson, I. (1989) A first course in Combinatorial Mathematics. \emph{Oxford University Press}.

Rosenbaum, P. R. (1989). Optimal matching for observational studies. \emph{Journal of the American Statistical Association 84(408): pp. 1024-1032}. 
}

\keyword{ ordered assignment problem}
\examples{
#See OSDR help for the examples. 
}
