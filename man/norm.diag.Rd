% Michael Malick
% 15 Mar 2013


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HEADER 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{norm.diag}

\alias{norm.diag}

\title{Explore the Normality Assumption of a vector}

\description{
    Examine the distribution of data and check against normal
    distribution. 
}

\usage{
    norm.diag(x, shapiro.test = FALSE, na.rm = TRUE, main.title = "")
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BODY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
    \item{x}{
        vector of numeric data
	}
    \item{shapiro.test}{
        logical, should a Shaprio-Wilks test be run
    }
    \item{na.rm}{
        logical, should NA values be removed before plotting
    }
    \item{main.title}{
        main title of plot
    }

}


\value{
    Returns a plotting device with four diagnostic plots
        \item{histogram}{includes an empirical density estimate}
        \item{boxplot}{ }
        \item{normal qq plot}{ }
        \item{empirical cdf}{includes the corresponding normal
            cumulative density function}
}

\author{Michael Malick}

\seealso{
    \code{\link{shapiro.test}},
    \code{\link{plot.ecdf}},
    \code{\link{boxplot}},
    \code{\link{hist}},
    \code{\link{density}},
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{

    x <- rnorm(100)
    norm.diag(x)

}

