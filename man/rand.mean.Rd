% Michael Malick
% 12 May 2012


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HEADER 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{rand.mean}

\alias{rand.mean}

\title{Randomization Test for Two Means}

\description{
    Function to test the difference between means of two samples
    using randomization.
}

\usage{
    rand.mean(x, y, R = 1000, one.sided = FALSE)
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BODY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
    \item{x}{
        numeric vector, sample 1
    }

    \item{y}{
        numeric vector, sample 2
    }

    \item{R}{
        number of iterations
    }

    \item{one.sided}{
        logical, should the test be one-tailed
    }
}


\value{
    Returns a histogram of the null distribution indicating the
    observed difference with a red vertical line and a list with
    components:
        \item{null.distr}{null distribution}
        \item{data}{a list giving the two vectors}
        \item{T}{absolute value of the differences between the
            means of the samples}
        \item{p.value}{the p-value of the test}
        \item{test}{tesll if its a one or two sided test}
}


\author{Michael Malick}


\seealso{
    \code{\link{mean}}
    \code{\link{sample}}

}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{
    x <- 1:100
    y <- 11:110
    rand.mean(x, y)
}


