\name{knapsack_dynamic}
\alias{knapsack_dynamic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
knapsack_dynamic(x, W)
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
    m <- matrix(0, nrow = n + 1, ncol = W + 1)
    for (i in 1:n) {
        for (j in 1:W) {
            if (x$w[i] > j) {
                m[i + 1, j + 1] <- m[i, j + 1]
            }
            else {
                m[i + 1, j + 1] <- max(m[i, j + 1], m[i, j + 
                  1 - x$w[i]] + x$v[i])
            }
        }
    }
    value <- m[n + 1, W + 1]
    elements <- integer(0)
    j <- W
    for (i in n:1) {
        if (m[i + 1, j + 1] != m[i, j + 1]) {
            elements <- c(elements, i)
            j <- j - x$w[i]
        }
    }
    return(list(value = value, elements = sort(elements)))
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
