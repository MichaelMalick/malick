% Michael Malick
% 10 Jul 2013



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Header
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{haversine}
\alias{haversine}
\title{Compute the Great Circle Distance between Two Points}


\description{
    This function computes the great circle distance between 
    two points given their latitiude and longitude (in decimal
    degrees) using the haversine formula. The output is the distance
    between the two points in km.
}


\usage{
    haversine(lat1, lon1, lat2, lon2)
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Body
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{

    \item{lat1}{
        latitude of first point
    }
    \item{lon1}{
        longitude of firs point
    }
    \item{lat2}{
        latitude of second point
    }
    \item{lon2}{
        longitude of second point
    }
}


\value{The function outputs a scalar giving the great circle
    distance in kilometers.
}


\author{Michael Malick}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{

haversine(48, -147, 49, -154)

}

