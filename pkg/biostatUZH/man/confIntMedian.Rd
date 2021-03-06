\name{confIntMedian}
\alias{confIntMedian}
\title{Compute exact confidence interval for median of a sample based on order statistics}
\description{
Given a sample \eqn{X_1, ..., X_n}, this function computes an exact confidence interval for the median of the sample
based on the binomial distribution.
}
\usage{confIntMedian(x, conf.level = 0.95)}

\arguments{
  \item{x}{Vector of observations.}
  \item{conf.level}{Confidence level for confidence interval.}
}

\value{Table containing the median and the confidence interval.}

\references{
Lehmann, E. (1975).
Nonparametrics: Statistical Methods Based on Ranks.
\emph{Holden-Day}, 182--183.

A worked out example (in Section 5, p. 38) can be found in 

Altman, D.G., Machin, D., Bryant, T.N., Gardner, M.J.(2000).
Statistics with confidence.
University Press Belfast.
}

\author{Kaspar Rufibach \cr \email{kaspar.rufibach@gmail.com} }

\examples{
# generate random sample
set.seed(1977)
n <- 40
x <- rnorm(n)

# compute CI for median
confIntMedian(x, conf.level = 0.95)

# data from Altman (2000), p. 38
x <- c(66, 71.2, 83, 83.6, 101, 107.6, 122, 143, 160, 177, 414)
confIntMedian(x, conf.level = 0.95)
}

\keyword{nonparametric}

