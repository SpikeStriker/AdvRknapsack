\name{brute_force_knapsack}
\alias{brute_force_knapsack}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
brute_force_knapsack(x, W)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{W}{
%%     ~~Describe \code{W} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (x, W) 
{
    stopifnot(is.data.frame(x))
    stopifnot(is.numeric(W) && (W > 0))
    stopifnot(all(colnames(x) == c("w", "v")))
    stopifnot(all(sapply(x, function(x) x > 0)) && all(sapply(x, 
        function(x) is.numeric(x))))
    n <- nrow(x)
    value <- 0
    elements <- NULL
    for (i in 0:(2^n - 1)) {
        combination <- as.logical(intToBits(i)[1:n])
        totalWeight <- sum(x$w[combination])
        totalValue <- sum(x$v[combination])
        if (totalWeight <= W && totalValue > value) {
            value <- totalValue
            elements <- which(combination)
        }
    }
    return(list(value = value, elements = elements))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
