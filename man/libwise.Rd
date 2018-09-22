\name{libwise}
\alias{libwise}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{library-wise screening
%%  ~~function to do ... ~~
}
\description{The analysis was performed by randomly selecting a pair of libraries, then conducting Spearman correlation for HK genes, followed by Spearman correlation for NHK genes. Keep the correlation coefficient for HK and for NHK genes. Repeat this procedure for a large number of times, e.g., n = 100.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
libwise(countmatrix,repitation=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rcleaned_matrix}{Gene-wise cleaned countmatrix, containing both of housekeeping and non-housekeeping data
%%     ~~Describe \code{x} here~~
}\item{repitation}{Repeat times,defaults to 100}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{a list containing
\item{cor_coef}{The correlation coefficients calculated from HK and NHK groups}
\item{wilcox}{Wilcox test p-value}
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
\note{Please make sure ensebl_id is loaded
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
libwise(countmatrix,repitation=100)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ library-wise }% use one of  RShowDoc("KEYWORDS")
