\name{sampleSizePhase2BinaryTest}
\alias{sampleSizePhase2BinaryTest}
\alias{sampleSizePhase2BinaryTestSimulated}
\title{Compute sample size for a Phase 2 study of a binary test}
\description{Compute sample size for a Phase 2 study of a binary test using either normal approximation or via simulation.
In simulations, the function goes through a grid of sample sizes and computes empirically the power for the given alternatives \code{FPF1} and \code{TPF1}.
The test decision is based on a rectangular confidence region using one-sided confidence limits for these two binomial parameters where the confidence intervals 
are either computed according 
to Wilson's method or exact as Clopper-Pearson intervals. The chosen sample sizes are the smallest numbers of cases and controls where the power is above \code{power} for all subsequent
combinations. It may happen that the desired power is reached for even smaller numbers of cases and controls but that for larger numbers we find power below
the desired limit. This phenomena can be attributed to the discrete nature of the problem and is well-known from the similar setup for one binomial proportion.}
\usage{
sampleSizePhase2BinaryTest(FPF0 = 0.2, TPF0 = 0.75, FPF1 = 0.05, TPF1 = 0.9,
    alpha = 0.05, power = 0.9)
sampleSizePhase2BinaryTestSimulated(FPF0, TPF0, FPF1, TPF1,
    alpha = 0.05, power = 0.9, M = 5000, range = 15,
    print = TRUE, type = c("wilson", "exact")[2])
}
\arguments{
  \item{FPF0}{Minimally acceptable value for the false positive fraction.}
  \item{TPF0}{Minimally acceptable value for the true positive fraction.}
  \item{FPF1}{Desirable value for the false positive fraction.}
  \item{TPF1}{Desirable value for the true positive fraction.}
  \item{alpha}{Significance level.}
  \item{power}{Desired power, i.e. 1 - probability of type II error.}
  \item{M}{Number of diagnostic tests to be simulated for the alternative.}
  \item{range}{Starting from the normally approximated sample size \code{wald.n} a range of \code{wald.n}\eqn{\pm}\code{range} is went through to compute power.}
  \item{print}{If \code{TRUE} progress is indicated through simulations.}
  \item{type}{Confidence interval to be used: either Wilson or the exact by Clopper and Pearson.}
}
\value{A list with the entries
  \item{n.cases}{Number of cases necessary to achieve prescribed power based on wald test.}
  \item{n.controls}{Number of cases necessary to achieve prescribed power based on wald test.}
  for \code{sampleSizePhase2BinaryTest} and entries
  \item{wald.n.cases}{Number of cases necessary to achieve prescribed power based on wald test.}
  \item{wald.n.controls}{Number of cases necessary to achieve prescribed power based on wald test.}
  \item{simul.n.cases}{Number of cases necessary to achieve prescribed power based on simulations.}
  \item{simul.n.controls}{Number of controls necessary to achieve prescribed power based on simulations.}
  \item{simul.power}{Actual power that is achieved in simulations.}
  \item{alphastar}{Confidence level for the two univariate confidence intervals for FPF and TPF, equals \eqn{1 - \sqrt{1 - \alpha}}.}
  \item{simul.powers}{Computed powers for all the number of cases/controls combinations in simulations.}
}
\references{
Pepe, M.S. (2003) \emph{The statistical evaluation of medical tests for classification and prediction}.
Oxford: Oxford University Press.
}
\author{Kaspar Rufibach \cr \email{kaspar.rufibach@gmail.com}}

\examples{
## Example 8.1 in Pepe (2001):
## "The Statistical Evaluation Medical Tests for Classification and Prediction"
sampleSizePhase2BinaryTest(FPF0 = 0.2, TPF0 = 0.75, FPF1 = 0.05, TPF1 = 0.9, 
    alpha = 0.1, power = 0.9)
sampleSizePhase2BinaryTestSimulated(FPF0 = 0.2, TPF0 = 0.75, FPF1 = 0.05, 
    TPF1 = 0.9, alpha = 0.1, power = 0.9, M = 1000, range = 15, print = TRUE)
}
\seealso{The function \code{\link{sampleSizePhase2BinaryTestSimulated}} depends on \code{\link{sampleSizePhase2BinaryTest}}, \code{\link{wilson}}, and \cite{\link{clopperPearson}}.}
\keyword{htest}

