haversine <- function(lat1, lon1, lat2, lon2) {

    # Michael Malick
    # 10 Jul 2013

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


