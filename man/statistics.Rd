% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{statistics}
\alias{statistics}
\title{Example set of performance statistics}
\format{
An object of class list.
}
\description{
A set of performance statistics is provided, coded in the format that
\code{performance()}requires. The statistics included in this list are as
follows:
}
\details{
\itemize{
\item SB0: Mean spawner biomass relative to unfished.
\item SBMSY: Mean spawnwer biomass relative to SBMSY.
\item Ftarget: Mean fishing mortality relative to target.
\item FMSY: Mean fishing mortality relative to FMSY.
\item green: Probability of being in Kobe green quadrant.
\item red: Probability of being in Kobe red quadrant.
\item PSBMSY: Probability of SB greater or equal to SBMSY.
\item PBlim: Probability that spawner biomass is above Blim.
\item risk1: ICES Risk 1, mean probability that spawner biomass is below Blim.
\item risk2: ICES Risk 2, probability that spawner biomass is above Blim once.
\item risk3: ICES Risk 3, max probability that spawner biomass is above Blim.
\item C: Mean catch over years.
\item VarC: Catch variability.
\item PC0: Probability of fishery shutdown.
}

Each indicator is itself a list object, with three elements, the first two
of them compulsory:
\itemize{
\item An unnamed element of class \emph{formula}, e.g. \code{yearMeans(SB/SB0)}.
\item name: A short name to be output on tables and plots, of class character,
e.g. "SB/SB\link{0}".
\item desc: A longer description of the indicator, of class character, e.g. "Mean
spawner biomass relative to unfished"
}
}
\keyword{datasets}
