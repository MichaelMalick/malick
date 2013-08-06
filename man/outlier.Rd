% Michael Malick
% 12 May 2012


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HEADER 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{outlier}

\alias{outlier}

\title{Identify Outliers in a Series of Residuals}

\description{Identifies outliers in a series of residuals according
their index number in the vector.}

\usage{
    outlier(x, type = "outlier", p = 0.05, plot.it = TRUE)
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BODY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
    \item{x}{
        vector of residuals
	}

    \item{type}{
        One of \code{"outlier"}, \code{"max.abs"}, or
        \code{"max.min"}. 
    }

    \item{p}{
        p-value
    }

    \item{plot.it}{
        logical, should the results be plotted
    }
}


\value{
    Returns a list with two components
        \item{positive}{positive outliers}
        \item{negative}{negative outliers}

    There are three types:
    \item{outlier}{
        returns the positions of all residuals that are 
        larger than (smaller than) the (1-p/2)th (p/2th) percentile 
        of the distribution of the residuals assuming they follow a 
        normal distribution.
    }

    \item{max.abs}{
        returns the position and sign of the maximum absolute
        residual
    }

    \item{max.min}{
        returns the positions of the maximum and minimum residual 
    }

}

\author{Michael Malick}

\seealso{
    \code{\link{qqnorm}}
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{

    set.seed(29)
    x <- rnorm(25)
    y <- rnorm(25)
    fit <- lm(y ~ x)
    outlier(resid(fit))

}


