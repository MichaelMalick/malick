% Michael Malick
% 12 May 2012


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HEADER 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{boot.median}

\alias{boot.median}

\title{Non-Parametric Bootstrap of the Median}

\description{
    This function performs a non-parametric bootstrap of a vector
    with replacement. 
}

\usage{
    boot.median(x, rep = 999) 
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BODY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
    \item{x}{
        a vector
	}

    \item{rep}{
        number of iterations
    }
}

\value{
    Returns a list:
        \item{boot.mean}{mean of the bootstrap samples}
        \item{boot.sd}{standard deviation of the bootstrap samples}
        \item{bias}{the mean of the bootstrap samples minus the
            median of the original data}
        \item{distribution}{vector of the bootstrap samples}
}


\author{Michael Malick}



\seealso{
    \code{\link{median}},
    \code{\link{sample}}
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{
    x <- 1:10
    boots <- boot.median(x)

    hist(boots$distribution, col = "grey20")
    abline(v = boots$boot.mean, lwd = 2, col = 2)
}

