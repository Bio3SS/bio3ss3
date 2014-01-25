\name{bd}
\alias{bd}
\title{basic one-species continuous-time population model}
\usage{
bd(N0 = NULL, MaxTime = 20, steps = 100, popMax = 100, popSteps = 100,
  b0 = 1, bDD = NULL, bAllee = NULL, d0 = 0.5, dDD = NULL,
  dAllee = NULL, reportPcTotal = "b", reportDiff = FALSE, title = "",
  tlab = "Time (years)", plab = "Population size (number)",
  logScale = FALSE, fontSize = 1, legendSize = 1, printPlots = TRUE,
  plotType = "ggplot")
}
\arguments{
  \item{MaxTime}{maximum time for dynamics plot [time]}

  \item{steps}{number of steps for dynamics plot}

  \item{popMax}{maximum population for demographic
  parameter plot [number]}

  \item{popSteps}{number of steps for demographic parameter
  plot}

  \item{b0}{\emph{per capita} birth rate at zero density
  [1/time]}

  \item{bDD}{characteristic density for exponential
  decrease in per capita birth rate with increasing
  population density [number]}

  \item{bAllee}{characteristic scale for Allee effect in
  birth rate [number]}

  \item{d0}{\emph{per capita} death rate at zero density
  [1/time]}

  \item{dDD}{characteristic density for exponential
  increase in \emph{per capita} death rate with increasing
  population density [number]}

  \item{dAllee}{characteristic scale for Allee effect in
  death rate [number]}

  \item{N0}{initial population size for dynamics plot
  [number]: if \code{N0}=0, simulations of time dynamics
  will not be run nor plotted}

  \item{reportPcTotal}{whether to plot \emph{per capita}
  rates ("p"), total rates ("t"), both ("b"), or neither
  ("n")}

  \item{reportDiff}{whether to plot the overall growth rate
  (birth-death) rather than birth and death separately}

  \item{fontSize}{scaled font size}

  \item{legendSize}{scaled legend size (base plots only)}

  \item{title}{plot title}

  \item{tlab}{label for time axis}

  \item{plab}{label for population size axis}

  \item{printPlots}{print plots (alternatively, return a
  list of plots)?}

  \item{plotType}{"ggplot2" or "base"}

  \item{logScale}{make y-axis logarithmic (for time
  dynamics only)?}
}
\description{
show plots of demographic parameters or time dynamics
}

