#' @title Load netCDF file
#' 
#' @description
#'     This function loads a netCDF file
#' 
#' @param datafile
#'     netCDF file to load
#' @param var
#'     name of the variable in the netCDF file
#' @param lat.name
#'     name of the latitude variable in the netCDF file
#' @param lon.name
#'     name of the longitude variable in the netCDF file
#' @param time.name
#'     name of the time variable in the netCDF file
#' 
#' @return Returns a list:
#'     \itemize{
#'         \item{datLat: latitude vector}
#'         \item{datLon: longitude vector}
#'         \item{datTime: time vector}
#'         \item{dim: dimensions of dat}
#'         \item{dat: data arrays}}
#' 
#' @author Michael Malick
#' 
#' @export
#' 
#' @examples
#' \dontrun{load_ncdf("./data/sst.nc", var = "sst")}
#' 
load_ncdf <- function(datafile, var, lat.name = "lat", lon.name = "lon",
    time.name = "time") {
    
    dat.file <- ncdf::open.ncdf(datafile)
    datLat   <- ncdf::get.var.ncdf(dat.file, varid = lat.name)  
    datLon   <- ncdf::get.var.ncdf(dat.file, varid = lon.name)  
    datTime  <- ncdf::get.var.ncdf(dat.file, varid = time.name) 
    dat      <- ncdf::get.var.ncdf(dat.file, varid = var)    
    dim      <- dim(dat)    
    
    invisible(ncdf::close.ncdf(dat.file)) 
    
    return(list(datLat = datLat, datLon = datLon, datTime = datTime,
        dim = dim, dat = dat))
}
