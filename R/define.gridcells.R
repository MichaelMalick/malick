#' @title Define gridcells within a study region
#' 
#' @description
#'     This function creates a grid over a defined study region and
#'     returns the coordinates for the center of the gridcells. It can
#'     also subset these gridcells according to predefined grid cell
#'     coordinates.
#' 
#' @param sub.cells
#'     optional dataframe giving the indices of the grid cells to subset within the
#'     study region. It's expected that that the grid coordinates have 1,1 in the
#'     top lefthand corner. If NULL all grid cells are used
#' @param cell.size
#'     grid cell size in degrees 
#' @param lat.min
#'     minimum latitude of study region
#' @param lat.max
#'     maximum latitude of study region
#' @param lon.min
#'     minimum longitude of study region
#' @param lon.max
#'     maximum longitude of study region
#' @param plot
#'     logical, should a plot of the grid cells be created
#' @param locations
#'     optional coordinates to plot on map
#'      
#' @return
#'     The output of the function is a dataframe that gives the unique
#'     row and column identifiers for each grid cell, the unique grid
#'     cell id for all grid cells (matrix.id) and the subsetted grid
#'     cells (cell.id), the center coordinates of the grid cell (lat and
#'     lon), and the max and min coordinates for each grid cell.
#' 
#' @details
#'     This functions takes as input the boundaries of the study region
#'     and the size of the desired grid cells in degrees. The function
#'     creates a grid of cells over the entire region giving each grid
#'     cell a unique id (matrix.id). The function treats this grid like
#'     a matrix with each cell having a unique column and row
#'     identifier. The sub.cells argument takes as input a dataframe
#'     that gives the row and column indices of specific grid cells to
#'     subset. The first index (1,1) should be in the upper
#'     lefthand corner and filling by row moving to the right. The last
#'     cell should be in the lower righthand corner. If sub.cells is
#'     supplied only data for the given cells are returned. If sub.cells
#'     is NULL data for all cells in the region are returned. 
#' 
#'     The function expects the study region to be defined within the
#'     coordinates: Lat: -90:90 Lon: -180:180.
#' 
#'     Requires the reshape2 and plyr packages.
#' 
#' @author Michael Malick
#' 
#' @export
#' 
#' @examples
#' define.gridcells(plot = FALSE)
#' 
define.gridcells <- function(
    sub.cells = NULL,
    cell.size = 2,
    lat.min   = -90,
    lat.max   = 90,
    lon.min   = -180,
    lon.max   = 180,
    plot      = TRUE,
    locations = NULL) {


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
