\name{plotbeta}
\alias{plotbeta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot beta from NB regression
%%  ~~function to do ... ~~
}
\description{Plot the distributions of beta values from negative binomial regression of HK and NHK groups in one figure.View the difference in Beta values between HK and NHK groups.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotbeta(beta_oth_fdr, beta_hk_fdr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{beta_oth_fdr}{The beta values from previous Negative Binomial regression using non-housekeeping genes.
%%     ~~Describe \code{x} here~~
}\item{beta_hk_fdr}{The beta values from previous Negative Binomial regression using housekeeping genes.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Return a distribution plot
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
plotbeta(beta_oth_fdr,beta_hk_fdr)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ distribution }% use one of  RShowDoc("KEYWORDS")
\keyword{ mode }% __ONLY ONE__ keyword per line
