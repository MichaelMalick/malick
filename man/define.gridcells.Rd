% Michael Malick
% 08 Jul 2013



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Header
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{define.gridcells}
\alias{define.gridcells}
\title{Define gridcells within a study region}


\description{
    This function creates a grid over a defined study region and
    gives the center of the gridcells. It can also subset these
    gridcells according to predefined grid cell coordinates.
}


\usage{
    define.gridcells(
        sub.cells = data.frame(row = 1, column = 1),
        locations = data.frame(long = 1, lat = 1),
        cell.size = 2,
        lat.min = 46,
        lat.max = 68,
        lon.min = -168,
        lon.max = -122,
        plot = TRUE,
        all.cells = FALSE) 
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Body
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{

    \item{sub.cells}{
        optional dataframe giving the coordinates of the grid cells
        to subset within the study region. It's expected that that
        the grid coordinates have 1,1 in the top lefthand corner.
    }
    \item{locations}{
        optional coordinates to plot on map.
    } 
    \item{cell.size}{
       grid cell size in degrees 
    } 
    \item{lat.min}{
       minimum latitude of study region
    } 
    \item{lat.max}{
       maximum latitude of study region
    } 
    \item{lon.min}{
       minimum longitude of study region
    } 
    \item{lon.max}{
       maximum longitude of study region
    } 
    \item{plot}{
       logical, should a plot of the grid cells be created
    } 
    \item{all.cells}{
       logical, if true all the grid cells within the study region
       bounds are included; overrides sub.cells when true
    } 
}


\value{
    The output of the function is a dataframe specifying the lon and
    lat of the center of the subsetted grid cells along with their
    indices and unique id.

}


\details{
    This functions takes as input the boundaries of the study region
    and the size of the desired grid cells in degrees. The function
    creates a grid of cells over the entire region giving each grid
    cell a unique id (matrix.id). It also takes as input a dataframe 
    that identifies the indices of the grid cells that
    should be subsetted. The dataframe should identify the 
    row and columns indices of the cells to use with the first index 
    (1,1) being in the upper lefthand corner and filling by row moving
    to the right. The last cell should be in the lower lefthand
    corner.

    Requires the reshape2 and plyr packages.
}

\author{Michael Malick}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{

define.gridcells(plot = FALSE)
define.gridcells(all.cells = TRUE, plot = TRUE)

}

