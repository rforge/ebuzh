\name{confIntKappa}
\alias{confIntKappa}
\title{Confidence intervals for weighted kappa and m >= 2 raters}
\description{
  Compute confidence intervals for the coefficient of
  agreement for two nominal or ordered variables and two or more raters.
}

\usage{
confIntKappa(dat, type = "not Cohen", weights = c("absolute", "squared")[1],
             M = 1000, conf.level = 0.95)
}

\arguments{
    \item{dat}{Data frame that contains the ratings as columns.}  
    \item{type}{Defines the type of confidence interval that is computed. If equal to "Cohen", then Cohen's 
                unweighted \eqn{kappa} is computed, i.e. ratings are assumed to be nominal. If not equal to 
                "Cohen", the weighted version for ordered ratings is computed.}
    \item{weights}{Define weights to be used if ordered ratings are compared. Only used if \code{type != "Cohen"}.}                
    \item{M}{Number of bootstrap samples to be generated.}
    \item{conf.level}{Confidence level for confidence interval.}
}

\details{
  This function computes bootstrap confidence intervals for an
  unweighted or weight \eqn{kappa} coefficient, 
  based on all pairwise complete observations in \code{dat}.
}

\value{
  A list containing:
  \item{n}{Number of observations used to compute confidence intervals.}
  \item{kappa}{Computed \code{kappa}.}
  \item{boot.quant}{Confidence interval based on quantiles of the bootstrap distribution.}
  %\item{adj.boot}{Adjusted boostrap percentile interval (BCa).}
}

\references{
Conger, A.J. (1980), Integration and generalisation of Kappas for multiple raters, Psychological Bulletin, 88, 322-328.
}

\seealso{
  This function basically implements the example given for
  \code{lkappa} in package \pkg{psy}.
}

\examples{
if (requireNamespace("psy")) {
    ## example comparable to that when called ?lkappa
    data("expsy", package = "psy")
    set.seed(1)
    confIntKappa(dat = expsy[,c(11,13,15)], type = "not Cohen", weights = "absolute", 
                 M = 200, conf.level = 0.95)
}
}

\keyword{htest}
\keyword{models}
