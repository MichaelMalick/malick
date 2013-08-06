% Michael Malick
% 16 Mar 2013



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Header
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{lm.diag}
\alias{lm.diag}
\title{Linear Model Diagnostics}

\description{
    This function produces eight standard diagnostic plots used to
    assess the assumptions of a linear regression. The plots analyze
    the residuals of the linear model. See also 
    \code{link{norm.diag}} for assessing the normality of the
    residuals.
}

\usage{
    lm.diag(x, main.title = "Residual Diagnostic Plots") 
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Body
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
    \item{x}{
        linear model object
    }

    \item{main.title}{
        global title for plotting device
    }
}

\details{
    Standardized and studentized residuals are calculated according
    to the functions in the MASS library. Cooks distances give the
    influence of a point on all fitted values (4/(n-p-1) =
    potentially influential). Leverages give influence of a point on
    the model fit. Leverages over 2p/n are considered influential.
    ACF values +/- 2/sqrt(n) are condidered significant. 
}

\value{
    Returns a plotting device with eight standard diagnostic plots
    for linear models:
        \item{histogram of the residuals}{ } 
        \item{normal qq plot of residuals}{ } 
        \item{residuals vs fitted values}{ } 
        \item{standardized residuals vs fitted values}{ } 
        \item{studentized residuals vs fitted values}{ } 
        \item{autocorrelation function}{ } 
        \item{cooks distance vs fitted}{ }
        \item{leverages vs fitted}{ }
}

\references{
    Kutner, M. H., C. J. Nachtsheim, J. Neter, and W. Li. 2005.
    Applied linear statistical models, 5th edition.
    McGraw-Hill/Irwin, New York.
}

\author{Michael Malick}

\note{This function requires library MASS.}


\seealso{
    \code{\link{lm}}, 
    \code{\link{cooks.distance}}, 
    \code{\link{stdres}}, 
    \code{\link{studres}}, 
    \code{\link{norm.diag}}, 
    \code{\link{influence}}
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{
## Create some data
x <- rnorm(100)
y <- 1:100
 
## Fit linear model
fitted <- lm(x ~ y)

## Run diagnostics
lm.diag(fitted)


}
