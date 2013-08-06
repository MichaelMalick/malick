load.ncdf <- function(datafile, var) {
    
    # datafile = netCDF file
    # var = variable name in netCDF file
    #
    # Michael Malick
    # 10 Jul 2013

    require(ncdf)

    dat.file <- open.ncdf(datafile)
    datLat   <- get.var.ncdf(dat.file, varid = "lat")  
    datLon   <- get.var.ncdf(dat.file, varid = "lon")  
    datTime  <- get.var.ncdf(dat.file, varid = "time") 
    dat      <- get.var.ncdf(dat.file, varid = var)    
    dim      <- dim(dat)    
    
    invisible(close.ncdf(dat.file)) 
    
    return(list(datLat = datLat, datLon = datLon, datTime = datTime,
        dim = dim, dat = dat))
}







