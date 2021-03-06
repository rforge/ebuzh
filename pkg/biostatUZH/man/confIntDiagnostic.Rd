\name{confIntDiagnostic}
\alias{confIntDiagnostic}

\title{Compute confidence intervals for operating characteristics of a diagnostic test}
\description{
Compute confidence intervals for sensitivity, specificity, positive and
negative likelihood ratio of a diagnostic test.
}
\usage{confIntDiagnostic(tp, fp, tn, fn, conf.level = 0.95)}
\arguments{
  \item{tp}{Number of true positives.}
  \item{fp}{Number of false positives.}
  \item{tn}{Number of true negatives.}
  \item{fn}{Number of false negatives.}
  \item{conf.level}{Confidence level for confidence intervals.}
}

\value{
A dataframe containing the estimated confidence intervals for
sensitivity, specificity, positive and
negative likelihood ratio.}

\references{
Pepe, M.S. (2003) \emph{The statistical evaluation of medical tests for classification and prediction}.
Oxford: Oxford University Press.
}

\author{Leonhard Held}

\examples{
## Calculate confidence intervals for data from the Million Women Study

confIntDiagnostic(tp=629, fp=3885, tn=117744, fn=97)

}


