\name{corcoef}
\alias{corcoef}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{corcoef
%%  ~~function to do ... ~~
}
\description{unction to random select, identify hk and nhk, calculate cor coef
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
corcoef(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{countmatrix}{countmatrix containing both housekeeping and non-housekeeping genes.No header. Each row should represent a gene. Each column should represent a single cell.
%%     ~~Describe \code{x} here~~
}\item{rep}{Randomly sampling times to calculate correlation coefficient}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{a matrix containing correlation coefficients calculated using HK genes and NHK genes respectively
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
corcoef_identify(countmatrix,rep)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ correlation_coefficient }% use one of  RShowDoc("KEYWORDS")

