\name{sampleSizeWilcoxTwoSample}
\alias{sampleSizeWilcoxTwoSample}
\title{Compute sample size for two sample Wilcoxon (Mann-Whitney) test}
\description{Compute sample size to test the hypothesis that two samples come from
the same population against that \eqn{Y}'s tend to be larger than \eqn{X}'s.}
\usage{sampleSizeWilcoxTwoSample(a = 0.05, b = 0.2, c = 0.5, 
    pxy = 0.75, two.sided = TRUE)}
\arguments{
  \item{a}{Significance level of test.}
  \item{b}{Desired power of test.}
  \item{c}{Proportion of observations in group 1: \eqn{c = m / (m + n)} (\eqn{c = 0.5} means equally sized groups).}
  \item{pxy}{A value for the probability \eqn{P(Y > X)}.}
  \item{two.sided}{If \code{TRUE} a two-sided test is assumed, otherwise one-sided.}
}
\details{Given two independent samples \eqn{X_1, ..., X_m} and \eqn{Y_1, ..., Y_n}, 
we want to test the hypothesis that the two samples come from the same population against that 
\eqn{Y}'s tend to be larger than \eqn{X}'s.
}
\value{
  \item{m}{Sample size of the first group.}
  \item{n}{Sample size of the second group.}
}
\references{
Nother, G.E. (1987).
Sample Size Determination for Some Common Nonparametric Tests
\emph{JASA}, \bold{82}, 644--647.
}
\author{Kaspar Rufibach \cr \email{kaspar.rufibach@gmail.com}}

\examples{
# compute sample size for some pxy's
pxys <- c(0.65, 0.7, 0.75)
dat1 <- matrix(ncol = 3, nrow = length(pxys))
colnames(dat1) <- c("P(Y > X)", "m", "n")
for (j in 1:length(pxys)){dat1[j, ] <- c(pxys[j], 
    sampleSizeWilcoxTwoSample(a = 0.05, b = 0.1, c = 0.5, 
    pxy = pxys[j], two.sided = TRUE))}
dat1
}

\keyword{htest}

