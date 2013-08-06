% Michael Malick
% 12 May 2012


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HEADER 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{cross.validate}

\alias{cross.validate}

\title{Cross Validation Routine for GLM's}

\description{
    Function to compute prediction standard errors for general linear
    models using leave-one-out cross validation techniques
}

\usage{
    cross.validate(fit, observed, data, K = 1)
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BODY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
    \item{fit}{fitted glm object}
    \item{observed}{observed (x) values, need to specify columns
        (e.g. dat[1])} 
    \item{data}{data used to fit glm model}
    \item{K}{number of data points to leave out}
        
}


\value{
    Returns a list:
        \item{MSE}{mean squared prediction error}
        \item{comp1 }{prediction standard deviation}
}

\author{Michael Malick}

\note{Modified code from cv.glm in boot package}


\seealso{
    \code{\link{glm}},
    \code{\link{predict}}
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{

    set.seed(29)
    x <- rnorm(25)
    y <- rnorm(25)
    dat <- data.frame(x = x, y = y)

    fit <- glm(y ~ x) # same result as lm()
    cross.validate(fit = fit, observed = dat[1], data = dat)

}

