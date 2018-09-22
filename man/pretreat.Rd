\name{pretreat}
\alias{pretreat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{pretreat countmatrix
%%  ~~function to do ... ~~
}
\description{This function removes all-0-genes from the countmatrix and distinguish housekeeping genes from non-housekeeping genes.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pretreat(countmatrix, report = T, )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{countmatrix}{Input countmatrix. No header. Each row should represent a gene. Each column should represent a single cell.
%%     ~~Describe \code{countmatrix} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{It will return a list containing two matrices. One for housekeeping genes. The other one for non-housekeeping genes.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Human housekeeping genes, revisited. Eisenberg E, Levanon EY.
https://www.ncbi.nlm.nih.gov/pubmed/23810203
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{Please remember to load the ensebl_id.RData in the example dataset first.
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(snu)
pretreat(snu_human_unnormalized)
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ pretreat }% use one of  RShowDoc("KEYWORDS")
