\name{genewise}
\alias{genewise}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Genewise screening
%%  ~~function to do ... ~~
}
\description{Gene-wise screening is based on the expectation that for a gene with a low technical noise, a gene’s count in a library will tend to increase with the increase of library size, which was tested using negative binomial regression of gene count (as dependent variable) against library size (as independent variable). We kept only those genes achieving a significant (FDR>0.10), positive regression coefficient.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
genewise(x,presetfdr=0.1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{submatrix}{housekeeping or non-housekeeping countmatrix from previous step
%%     ~~Describe \code{x} here~~
}\item{presetfdr}{preset false discovery rate}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{a list contaitning
\item{beta_nb}{the beta values of negative binomial regression}
\item{beta_cleaned}{beta values controlled by FDR}
\item{fdr}{Flase discovery rate for each gene}
\item{countmatrix_rwcl}{genewise cleaned countmatrix}
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
genewise(matrix,presetfdr=0.15)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ genewise }% use one of  RShowDoc("KEYWORDS")

