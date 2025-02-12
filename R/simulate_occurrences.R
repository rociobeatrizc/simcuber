#' Simulate occurrences within spatiotemporal scope
#'
#' The function simulates occurrences of a species within a given spatial
#' and/or temporal extend.
#'
#' @param polygon An sf object with POLYGON geometry indicating the spatial
#' extend to simulate occurrences.
#' @param initial_average_abundance A positive integer value indicating the
#' average number of occurrences to be simulated within the extend of `polygon`
#' at time point 1. This value will be used as mean of a Poisson distribution
#' (lambda parameter).
#' @param spatial_autocorr `"random"`, `"clustered"`, `"regular"` or a numeric
#' value between -1 and 1 representing Moran's I. `"random"` corresponds to 0,
#' `"clustered"` to 0.9 and `"regular"` to -0.9.
#' @param n_time_points A positive integer value indicating the number of time
#' points to simulate.
#' @param temporal_autocorr `NA`, `"random_walk"` or a function which generates
#' a trend in abundance over time. Only used if `time_points > 1`. When there
#' are multiple time points the function will by default use a `"random_walk"`
#' function.
#' @param spatiotemporal_autocorr A numeric value between indicating the
#' strength of spatiotemporal autocorrelation.
#' @param seed A positive numeric value. The seed for random number generation
#' to make results reproducible. If `NA` (the default), no seed is used.
#'
#' @returns An sf object with POINT geometry containing the locations of the
#' simulated occurrences and a `time_point` column containing the time point
#' associated with each occurrence.
#'
#' @export
#'
#' @examples
#'
#' # add example here ...
#'
#' # This is just to create an example polygon for input
#' library(sf)
#'
#' # Define the coordinates of the polygon vertices
#' polygon <- st_polygon(list(rbind(c(0, 0),
#'                                  c(1, 0),
#'                                  c(1, 1),
#'                                  c(0, 1),
#'                                  c(0, 0))))
#' # Convert the polygon to an sf object
#' # Can be used as polygon input argument
#' polygon_sf <- st_sfc(polygon)

simulate_occurrences <- function(
    polygon,
    initial_average_abundance = 50,
    spatial_autocorr = c("random", "clustered", "regular"),
    n_time_points = 10,
    temporal_autocorr = ifelse(n_time_points ==  1, NA, "random_walk"),
    spatiotemporal_autocorr = NA,
    seed = NA) {

  # ...
}
