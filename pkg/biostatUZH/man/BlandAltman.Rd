\name{BlandAltman}
\alias{BlandAltman}
\title{Bland-Altman plot to assess agreement between two continuous variables}
\description{Agreement between two continuous measurements is usually assessed by plotting the difference
vs. the mean of the two measurements, the so-called Bland-Altman plot. Methods can be used interchangeably
if all observations in the Bland-Altman lie between the mean +/- two standard deviations, provided
that differences in this range were not clinically important. Note that the function automatically only
uses pairwise complete observations.}
\usage{BlandAltman(x, y, conf.level = 0.95, group = NA, labx = "", laby = "", 
    maintit = "", limy = NA, plot = TRUE)}
\arguments{
  \item{x}{Measurements of first method.}
  \item{y}{Measurements of second method.}
  \item{conf.level}{Confidence level for confidence intervals around limitis of agreement.}
  \item{group}{Factor that enables different colors for points (e.g. to mark observations at different timepoints).}
  \item{labx}{Character string, label for \eqn{x}-axis.}
  \item{laby}{Character string, label for \eqn{y}-axis.}
  \item{maintit}{Main title of plot.}
  \item{limy}{2-d vector containing upper and lower limit for scaling of \eqn{y}-axis.}
  \item{plot}{If \code{TRUE} Bland-Altman plot is generated.}
}
\value{A list containing the following elements:
\item{difference.mean}{Mean difference.}
\item{difference.sd}{Standard deviation of differences.}
\item{difference.se}{Standard error of mean differences.}
\item{upper.agreement.limit}{Upper limit of agreement.}
\item{lower.agreement.limit}{Lower limit of agreement.}
\item{agreement.limit.se}{Standard error of limits of agreement.}
\item{t.value}{\eqn{t}-quantile used for computation of limits of agreement.}
}
\references{
Bland, J.M. and Altman, D.G. (1986).
Statistical methods for assessing agreement between two methods of clinical measurement.
\emph{Lancet}, \bold{1}, 307--310.
}
\author{Kaspar Rufibach \cr \email{kaspar.rufibach@gmail.com}}

\examples{
n <- 50
meas1 <- rnorm(n, 20, 4)
meas2 <- meas1 + rnorm(n, 0.5, 1)
time <- sample(c(rep(1, n / 2), rep(2, n / 2)))

BlandAltman(x = meas1, y = meas2, conf.level = 0.95, group = NA, labx = 
    "mean of measurements", laby = "difference of measurements", 
    maintit = "Assess agreement between measurement 1 and measurement 2", 
    limy = NA)

BlandAltman(x = meas1, y = meas2, conf.level = 0.95, group = time, labx = 
    "mean of measurements", laby = "difference of measurements",
    maintit = "Assess agreement between measurement 1 and measurement 2", 
    limy = NA)
}

\keyword{dplot}
\keyword{aplot}
\keyword{htest}
