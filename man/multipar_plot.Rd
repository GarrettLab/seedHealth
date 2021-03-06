% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multipar_plot.R
\name{multipar_plot}
\alias{multipar_plot}
\title{Plots of output for multiple parameter combinations}
\usage{
multipar_plot(pHSinit = 0.2, Kx = 100, betax = 0.02, wxtnormm = seq(0,
  1, 0.2), wxtnormsd = 0.3, hx = 1, mxtnormm = 1, mxtnormsd = 0,
  axtnormm = 1, axtnormsd = 0, rx = 0.1, zxtnormm = seq(0, 1, 0.2),
  zxtnormsd = 0.3, gx = 4, cx = 0.9, phix = 0, nseasons = 10,
  nsim = 100, HPcut = 0.5, pHScut = 0.5, maY = 100, miY = 0,
  thetax = 0.2, Ex = 0)
}
\arguments{
\item{pHSinit}{the initial proportion of healthy seed, numeric or numeric vector.}

\item{Kx}{the total number of plants, positive interger, numeric or numeric vector.}

\item{betax}{the maximum seasonal transmission rate, numeric or numeric vector.}

\item{wxtnormm}{the environmental effect on transmission rate (mean of underlying normal distribution prior to truncation), numeric or numeric vector.}

\item{wxtnormsd}{the environmental effect on transmission rate (standard deviation of underlying normal distribution prior to truncation), numeric or numeric vector.}

\item{hx}{the host effect on transmission rate, numeric or numeric vector.}

\item{mxtnormm}{the vector management effect on transmission rate (mean of underlying normal distribution prior to truncation), numeric or numeric vector.}

\item{mxtnormsd}{the vector management effect on transmission rate (standard deviation of underlying normal distribution prior to truncation), numeric or numeric vector.}

\item{axtnormm}{the roguing effect in terms of decreased DP (mean of underlying normal distribution prior to truncation), numeric or numeric vector.}

\item{axtnormsd}{the roguing effect in terms of decreased DP (standard deviation of underlying normal distribution prior to truncation), numeric or numeric vector.}

\item{rx}{the reversion rate, numeric or numeric vector.}

\item{zxtnormm}{the proportional selection against diseased plants (mean of underlying normal distribution prior to truncation), numeric or numeric vector.}

\item{zxtnormsd}{the proportional selection against diseased plants (standard deviation of underlying normal distribution prior to truncation), numeric or numeric vector.}

\item{gx}{the seed production rate in healthy plants, numeric or numeric vector.}

\item{cx}{the proportional seed production rate in diseased plants, numeric or numeric vector.}

\item{phix}{the proportion clean seed purchased, numeric or numeric vector.}

\item{nseasons}{the number of seasons, numeric or numeric vector.}

\item{nsim}{vector of number of simulations}

\item{HPcut}{the proportion healthy plant number cutoff, numeric or numeric vector.}

\item{pHScut}{the proportion healthy seed cutoff, numeric or numeric vector.}

\item{maY}{the maximum attainable yield, end of season, in the absence of disease, numeric or numeric vector.}

\item{miY}{the minimum yield when all plants are diseased (useable yield despite disease), numeric or numeric vector.}

\item{thetax}{the rate of decline of Yld with increasing disease incidence, numeric or numeric vector.}

\item{Ex}{the amount of external inoculum around field, numeric or numeric vector.}
}
\description{
This function plots a heatmap and a 3-D graph of output for multiple parameter combinations across nseasons
}
\details{
The truncated random normal variables were generated by \code{\link{altruncnorm}}. 
Refer to \code{\link{onesim}}, \code{\link{multisim}}, and \code{\link{multipar}} for relevant information.
}
\examples{
multipar_plot() # more examples to be added
}
\keyword{health}
\keyword{seed}
