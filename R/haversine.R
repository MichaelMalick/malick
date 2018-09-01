#' @title Compute great circle distance between two points
#'
#' @description
#'  Computes the great circle distance between two points given their latitude
#'  and longitude (in decimal degrees) using the haversine formula. The output
#'  is the distance between the two points in the same units as the input `r`
#'  (default is meters)
#'
#' @param lon1
#'  longitude of first point
#' @param lat1
#'  latitude of first point
#' @param lon2
#'  longitude of second point
#' @param lat2
#'  latitude of second point
#' @param r
#'  radius of earth (default is in meters)
#'
#' @return
#'  Outputs a scalar giving the great circle distance in the same units as the
#'  input radius.
#'
#' @author Michael Malick
#'
#' @export
#'
#' @examples
#'  haversine(-147, 48, -154, 49)
#'
haversine <- function(lon1, lat1, lon2, lat2, r = 6378137) {

    if(!is.numeric(c(lon1, lat1, lon2, lat2)))
        stop("Inputs are not numeric")

    # Convert degrees to radians
    lon1 <- lon1 * pi / 180
    lat1 <- lat1 * pi / 180
    lon2 <- lon2 * pi / 180
    lat2 <- lat2 * pi / 180

    delta.lon <- (lon2 - lon1)
    delta.lat <- (lat2 - lat1)
    a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) *
         sin(delta.lon/2)^2
    c <- 2 * asin(min(1,sqrt(a)))
    d <- r * c

    return(d) # Distance
}
