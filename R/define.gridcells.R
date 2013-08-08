define.gridcells <- function(
    sub.cells = NULL,
    cell.size = 2,
    lat.min = -90,
    lat.max = 90,
    lon.min = -180,
    lon.max = 180,
    plot = TRUE,
    locations = NULL) {

    # sub.cells = dataframe defining which grid cells to subset
    #             if NULL all cells in region are returned
    # cell.size = cell size in degrees
    # lat.min = min lat of study region
    # lat.max = max lat of study region
    # lon.min = min lon of study region
    # lon.max = max lon of study region
    # plot = should a plot of the grid cells be printed
    # locations = optional coordinates to plot on map
    #
    # Michael Malick
    # 07 Aug 2013

    require(lattice)
    require(maps)
    require(reshape2)
    require(plyr)

    sz <- cell.size * 0.5

    
    # Find center of all 1x1 grid cells in study region
    y.seq <- seq(lat.min, lat.max, cell.size)
    x.seq <- seq(lon.min, lon.max, cell.size)
    n.lat <- length(y.seq) - 1
    n.lon <- length(x.seq) - 1
    y     <- rep(NA, n.lat)
    x     <- rep(NA, n.lon)

    for(i in 1:n.lat)
        y[i] <- mean(c(y.seq[i], y.seq[i + 1]))
        
    for(i in 1:n.lon)
        x[i] <- mean(c(x.seq[i], x.seq[i + 1]))

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


    # Define max and min lat and lon
    mat.melt$min.lat <- mat.melt$lat - sz
    mat.melt$max.lat <- mat.melt$lat + sz
    mat.melt$min.lon <- mat.melt$lon - sz
    mat.melt$max.lon <- mat.melt$lon + sz

    if(is.null(sub.cells) == TRUE) {
        sub.cells <- expand.grid(1:n.row, 1:n.col)
        names(sub.cells) <- c("row", "column")
    }

    suppressMessages(use <- join(sub.cells, mat.melt))


    if(plot == TRUE) {
        
        # Create seq for grid lines
        yy <- sort(unique(c(mat.melt$min.lat, mat.melt$max.lat)))
        xx <- sort(unique(c(mat.melt$min.lon, mat.melt$max.lon)))
        
        if(is.null(locations)) {
            locations <- data.frame(long = 1, lat = 1)
            loc.color <- "white"
        } else
            loc.color <- "#FF00FF"
        

        p <- xyplot(lat ~ -long, data = locations, 
            xlim = c(lon.min - cell.size, lon.max),
            ylim = c(lat.min, lat.max + cell.size), 
            main = "Study Region Grid",
            pch = 20, cex = 1, col = loc.color,
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
                    col = "black", pch = 20, cex = 0.7) 
                # Add grid lines
                panel.abline(v = xx)
                panel.abline(h = yy)

                # Add cell indices
                panel.text(x = x, y = lat.max + (cell.size / 2), 
                    cex = 0.5, labels = as.character(1:length(x)))
                panel.text(y = y, x = lon.min - (cell.size / 2), 
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



