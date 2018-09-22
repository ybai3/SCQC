\name{suggest_cutoff}
\alias{suggest_cutoff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Suggest cutoff of library size
%%  ~~function to do ... ~~
}
\description{This function suggests cutoff of library size for library-wise screening by an empirical criteria. The cutoff is found by seeking first three bins with wilcoxon P-value lower than multiple test corrected P-value at 0.05. Although the library-wise cutoff can be suggested by this function, we still recommend users to look at the boxplot made by corcoefplot() function for double check.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
suggest_cutoff(rcleaned_matrix,wilcox_p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rcleaned_matrix}{Gene-wise cleaned countmatrix, containing both of housekeeping and non-housekeeping data
%%     ~~Describe \code{x} here~~
}\item{wilcox_p}{Wilcoxon P-value in gene-wise screening. Can be found in the output list of genewise function.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Suggested library size with its quantile across libraries.
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
suggest_cutoff(rcleaned_matrix,wilcox_p)

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~library-wise cutoff }% use one of  RShowDoc("KEYWORDS")

