load.ncdf <- function(datafile, var, lat.name = "lat", lon.name = "lon",
    time.name = "time") {
    
    # datafile = netCDF file
    # var = variable name in netCDF file
    #
    # Michael Malick
    # 09 Jun 2014

    require(ncdf)

    dat.file <- open.ncdf(datafile)
    datLat   <- get.var.ncdf(dat.file, varid = lat.name)  
    datLon   <- get.var.ncdf(dat.file, varid = lon.name)  
    datTime  <- get.var.ncdf(dat.file, varid = time.name) 
    dat      <- get.var.ncdf(dat.file, varid = var)    
    dim      <- dim(dat)    
    
    invisible(close.ncdf(dat.file)) 
    
    return(list(datLat = datLat, datLon = datLon, datTime = datTime,
        dim = dim, dat = dat))
}







