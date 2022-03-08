\name{exdata}
\alias{exdata}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
Executive dataset (subset of)
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
Contains executives data for an italian firm operating in the energy sector (year 2015). The complete dataset contains 302 rows (10 firms) and 180 columns (it will be released in next package version).
}
\usage{data("exdata")}
\format{
  A data frame with 28 observations on the following 8 variables.
  \describe{
    \item{\code{firm}}{a character vector}
    \item{\code{position}}{a numeric vector}
    \item{\code{education}}{a numeric vector}
    \item{\code{year_born}}{a numeric vector}
    \item{\code{contract}}{a numeric vector}
    \item{\code{part_fulltime}}{a numeric vector}
    \item{\code{seniority}}{a numeric vector}
    \item{\code{sex}}{a numeric vector}
  }
}
\details{
	  \describe{
	\item{\code{position}}{ is coded: 4 = top manager, 3 = medium/first line manager, 2 = supervisor}
	\item{\code{sex}}{ is coded: 0 = M, 1 = F}
	\item{\code{education}}{is coded: Post-graduate = 5, Graduate = 4, High school = 3}
	\item{\code{contract type}}{ is coded: fixed term = 4; permanent = 3}
	\item{\code{seniority}}{ is measured by number of years in the current position}
}
	
%%  ~~ If necessary, more details than the __description__ above ~~
}
% \source{
% %%  ~~ reference to a publication or URL from which the data were obtained ~~

% }
\examples{
# load executive data
data(exdata)

# case study: matched samples for comparing women and men executives

table(exdata$sex)
table(exdata$position,exdata$sex)
# There are more women and more in apical position. 

# A complete matching is not possible for several choices of the caliper.
# Gap differences tend to be higher for higher ranks

# e.g. Lynn and Thompson(1997), Above the glass ceiling? A comparison of
# matched Samples of Men and Women Executives. J. of Appl. Psych. 82(3)

# so we would give higher matching priority to women in higher position.
# We can use OSDR to find a minimum cost matching 
# performing matching by decreasing hierarchical position. 
}
\keyword{ordered assignment problem}
