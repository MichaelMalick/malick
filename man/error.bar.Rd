% Michael Malick
% 11 May 2012



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Header
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{error.bar}
\alias{error.bar}
\title{Error Bars for a Barplot}

\description{
    Draws error bars on a barplot.
}

\usage{
    error.bar(x, y, ebl, ebu = ebl, length = 0.08, ...) 
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Body
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
    \item{x}{
        barplot() object
    }

    \item{y}{
        vector of bar heights
    }

    \item{ebl}{
        vector of lower error bar limit
    }

    \item{ebu}{
        vector of upper error bar limit
    }

    \item{length}{
        length of the actual horizontal bars
    }

    \item{\dots}{
        additional parameters controlling the error bar
    }
}

\details{This function currently only works on barplots.}

\value{Returns a barplot with error bars.}

\author{Michael Malick}

\seealso{
    \code{\link{barplot}}
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{
## Bar Heights
x <- c(10,8,13,7)

## Error Bars
up <- c(1,1,1,1)
lo <- c(1,1,1,1)

## Barplot
plt <- barplot(x, ylim = c(0, 15), las = 1)
error.bar(plt, x, up, lo)
}
