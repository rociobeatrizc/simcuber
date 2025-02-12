% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_occurrences.R
\name{simulate_occurrences}
\alias{simulate_occurrences}
\title{Simulate occurrences within spatiotemporal scope}
\usage{
simulate_occurrences(
  polygon,
  initial_average_abundance = 50,
  spatial_autocorr = c("random", "clustered", "regular"),
  n_time_points = 10,
  temporal_autocorr = ifelse(n_time_points == 1, NA, "random_walk"),
  spatiotemporal_autocorr = NA,
  seed = NA
)
}
\arguments{
\item{polygon}{An sf object with POLYGON geometry indicating the spatial
extend to simulate occurrences.}

\item{initial_average_abundance}{A positive integer value indicating the
average number of occurrences to be simulated within the extend of \code{polygon}
at time point 1. This value will be used as mean of a Poisson distribution
(lambda parameter).}

\item{spatial_autocorr}{\code{"random"}, \code{"clustered"}, \code{"regular"} or a numeric
value between -1 and 1 representing Moran's I. \code{"random"} corresponds to 0,
\code{"clustered"} to 0.9 and \code{"regular"} to -0.9.}

\item{n_time_points}{A positive integer value indicating the number of time
points to simulate.}

\item{temporal_autocorr}{\code{NA}, \code{"random_walk"} or a function which generates
a trend in abundance over time. Only used if \code{time_points > 1}. When there
are multiple time points the function will by default use a \code{"random_walk"}
function.}

\item{spatiotemporal_autocorr}{A numeric value between indicating the
strength of spatiotemporal autocorrelation.}

\item{seed}{A positive numeric value. The seed for random number generation
to make results reproducible. If \code{NA} (the default), no seed is used.}
}
\value{
An sf object with POINT geometry containing the locations of the
simulated occurrences and a \code{time_point} column containing the time point
associated with each occurrence.
}
\description{
The function simulates occurrences of a species within a given spatial
and/or temporal extend.
}
\examples{

# add example here
}
