#' @title Compute great circle distance between two points
#'
#' @description
#'     This function computes the great circle distance between
#'     two points given their latitiude and longitude (in decimal
#'     degrees) using the haversine formula. The output is the distance
#'     between the two points in km.
#'
#' @param lat1
#'     latitude of first point
#' @param lon1
#'     longitude of first point
#' @param lat2
#'     latitude of second point
#' @param lon2
#'     longitude of second point
#'
#' @return
#'     The function outputs a scalar giving the great circle distance in
#'     kilometers.
#'
#' @author Michael Malick
#'
#' @export
#'
#' @examples
#' haversine(48, -147, 49, -154)
#'
haversine <- function(lat1, lon1, lat2, lon2) {

    # Convert degrees to radians
    lat1 <- lat1 * pi / 180
    lon1 <- lon1 * pi / 180
    lat2 <- lat2 * pi / 180
    lon2 <- lon2 * pi / 180

    R <- 6371 # Earth mean radius [km]
    delta.lon <- (lon2 - lon1)
    delta.lat <- (lat2 - lat1)
    a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) *
         sin(delta.lon/2)^2
    c <- 2 * asin(min(1,sqrt(a)))
    d = R * c

    return(d) # Distance in km
}
