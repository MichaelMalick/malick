% Michael Malick
% 11 May 2012



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Header
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{aic.c}
\alias{aic.c}
\title{Compute AICc}


\description{
    This function computes the AIC corrected for small samples sizes
    (AICc) and includes the additive constant. Currently only one
    object can be analyzed at a time.
}


\usage{
    aic.c(model, k = 2)
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Body
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
    \item{model}{
        linear model object produced using the lm() function
    }
    \item{k}{
        penalty term for additional parameters. k = 2 is standard
        usage.
    } 
}


\value{The function outputs a list.
    \item{edf}{Effective degrees of freedom}
    \item{AIC}{AIC value}
    \item{AICc}{AICc value}
}


\references{
    Burnham, K. P., and D. R. Anderson. 2002. Model selection and
    multimodel inference: a practical information-theoretical
    approach, 2nd edition. Springer-Verlag, New York.
}


\author{Michael Malick}


\seealso{
    \code{\link{AIC}}
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{
## Create some data
x <- rnorm(10)
y <- rnorm(10)

## Fit linear model
fit <- lm(y ~ x)

## Compute AICc
aic.c(fit)
}

