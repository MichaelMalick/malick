% Michael Malick
% 12 May 2012


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HEADER 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{conf.int}

\alias{conf.int}

\title{Compute Confidence Intervals}

\description{
    Function to calculate confidence intervals (two-tailed) based on
    the t-distribution.
}

\usage{
    conf.int(x, alpha = 0.975, na.rm = FALSE)
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BODY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
    \item{x}{
        numeric vector
	}

    \item{alpha}{
        significance level
    }

    \item{na.rm}{
        logical, should NA's be removed. Function fails if NA's are
        present and na.rm = FALSE.
    }
}


\value{
    Returns a list:
        \item{Mean}{mean of the vector}
        \item{SD}{standard deviation of the vector}
        \item{CI}{confidence interval}
        \item{Upper}{upper confidence bound}
        \item{Lower}{lower confidence bound}
}


\author{Michael Malick}


\seealso{
    \code{\link{qt}}
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{

    x <- 1:10
    conf.int(x)

    y <- c(1, 2, 3, NA, 4, 5:10)
    conf.int(y, na.rm = TRUE)

}

