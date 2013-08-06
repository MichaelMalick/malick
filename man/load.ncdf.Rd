% Michael Malick
% 06 Jul 2013


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HEADER 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{load.ncdf}

\alias{load.ncdf}

\title{Load a netCDF File}

\description{
    This function loads a netCDF file
}

\usage{
    load.ncdf(datafile, var)
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BODY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
    \item{datafile}{netCDF file to load}
    \item{var}{name of the variable in the netCDF file}
        
}


\value{
    Returns a list:
        \item{datLat}{latitude vector}
        \item{datLon}{longitude vector}
        \item{datTime}{time vector}
        \item{dim}{dimensions of dat}
        \item{dat}{data arrays}
}

\author{Michael Malick}

\note{Requires the netcdf package}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{

    # load.ncdf("./data/sst.nc", var = "sst")

}

