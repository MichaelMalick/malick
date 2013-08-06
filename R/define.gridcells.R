define.gridcells <- function(
    sub.cells = data.frame(row = 1, column = 1),
    locations = data.frame(long = 1, lat = 1),
    cell.size = 2,
    lat.min = 46,
    lat.max = 68,
    lon.min = -168,
    lon.max = -122,
    plot = TRUE,
    all.cells = FALSE) {

    # sub.cells = dataframe defining which grid cells to subset
    # locations = optional coordinates to plot on map
    # cell.size = cell size in degrees
    # lat.min = min lat of study region
    # lat.max = max lat of study region
    # lon.min = min lon of study region
    # lon.max = max lon of study region
    # plot = should a plot of the grid cells be printed
    # all.cells = if TRUE all the grid cells within the region bounds
    #             are included; overrides sub.cells when TRUE
    #
    # Michael Malick
    # 10 Jul 2013

    require(lattice)
    require(maps)
    require(reshape2)
    require(plyr)

    sz <- cell.size * 0.5

    # Find center of all 1x1 grid cells in study region
    y                <- seq(lat.min, lat.max, cell.size)
    x                <- seq(lon.min, lon.max, cell.size)
    center           <- expand.grid(x, rev(y))
    names(center)    <- c("lon", "lat")
    center$matrix.id <- 1:length(center[, 1])


    # Create a matrix of grid cells with indices that relate back
    # to the dataframe giving the center of each grid cell in 
    # lat and lon
    n.col <- length(x)
    n.row <- length(y)

    matrix.id <- matrix(center$matrix.id, ncol = n.col, nrow = n.row,
        byrow = TRUE, dimnames = 
        list(as.character(1:n.row), as.character(1:n.col)))

    # Melt matrix
    mat.melt        <- melt(matrix.id)
    names(mat.melt) <- c("row", "column", "matrix.id")
    
    # Combine mat.melt and center
    suppressMessages(mat.melt <- join(mat.melt, center))
    mat.melt <- with(mat.melt, data.frame(matrix.id, row, column,
        lat, lon))

    if(all.cells == TRUE) {
        sub.cells <- expand.grid(1:n.row, 1:n.col)
        names(sub.cells) <- c("row", "column")
    }

    suppressMessages(use <- join(sub.cells, mat.melt))


    # Define max and min lat and lon
    use$min.lat <- use$lat - sz
    use$max.lat <- use$lat + sz
    use$min.lon <- use$lon - sz
    use$max.lon <- use$lon + sz


    if(plot == TRUE) {
        
        # Create seq for grid lines
        yy <- seq(lat.min - sz, lat.max + sz, cell.size)
        xx <- seq(lon.min - sz, lon.max + sz, cell.size)
        
        # Create padding for xlim and ylim
        pad <- cell.size * 1.5

        p <- xyplot(lat ~ -long, data = locations, 
            xlim = c(lon.min - pad, lon.max + pad),
            ylim = c(lat.min - pad, lat.max + pad), 
            main = "Study Region Grid",
            pch = 20, cex = 1, col = "#FF00FF",
            xlab = expression(paste("Longitude (", degree, "W)")),
            ylab = expression(paste("Latitude (", degree, "N)")),
            panel = function(...) { 
                
                # Redrawn points
                panel.xyplot(...) 

                # Draw map
                mp <- map('world', fill = T, plot = FALSE)
                lpolygon(mp$x, mp$y, col = "grey70", 
                    border = "grey60")

                # Add center grid cell dots
                panel.xyplot(x = mat.melt$lon, y = mat.melt$lat, 
                    col = "grey40", pch = 20, cex = 0.4) 

                # Add subsetted center grid cell dots
                panel.xyplot(x = use$lon, y = use$lat, 
                    col = "black", pch = 20, cex = 0.9) 
                # Add grid lines
                panel.abline(v = xx)
                panel.abline(h = yy)

                # Add cell indices
                panel.text(x = x, y = lat.max + cell.size, 
                    cex = 0.5, labels = as.character(1:length(x)))
                panel.text(y = y, x = lon.min - cell.size, 
                    cex = 0.5, labels = as.character(length(y):1))

             }) 
         print(p)

    }

    # Order cells 
    use <- use[order(use$lat, use$lon + 180), ]

    # Remove duplicates
    use <- use[!duplicated(use$matrix.id), ]

    # Reset row names
    row.names(use) <- NULL

    # Create cell.id column
    use$cell.id <- seq(1, length(use[ ,1]), 1)

    # Rearrange dataframe
    use <- with(use, data.frame(
        matrix.id, cell.id, row, column, lat, lon, 
        min.lat, max.lat, min.lon, max.lon))

    return(use)

}


# ----------------------------
# TESTING
# ----------------------------
if(FALSE) {

    define.gridcells()
    define.gridcells(all.cells = TRUE)

}



