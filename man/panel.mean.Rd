% Michael Malick
% 12 May 2012


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HEADER 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{panel.mean}

\alias{panel.mean}

\title{Add Factor Means to a Lattice Plot}

\description{
    Panel function for lattice plots to add a mean bar onto an xyplot
    that plots a numeric ~ factor, where the the mean represents the
    mean of the factor.
}

\usage{
    panel.mean(x, y, 
        fun = mean, 
        horizontal = TRUE,
        lwd = reference.line$lwd,
        lty = reference.line$lty,
        col,
        col.line = reference.line$col,
        type = "l", ...) 
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BODY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
    \item{x}{x}
    \item{y}{y}
    \item{fun}{\code{mean} function is used for each panel}
    \item{horizontal}{logical}
    \item{lwd}{line width (defaults to trellis.par.get setting)}
    \item{lty}{line type (defaults to trellis.par.get setting)}
    \item{col}{color}
    \item{col.line}{line color (defaults to trellis.par.get
        setting)}
    \item{type}{points or line}
    \item{\dots}{other plotting parameters (e.g., col, lwd)}
}

\value{
   Used as part of an xyplot call. 
}

\author{Michael Malick}

\seealso{
    \code{\link{lattice}},
    \code{\link{xyplot}},
    \code{\link{panel.abline}},
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{

   require(lattice)

    xyplot(Sepal.Length ~ Species, data = iris,
        panel = function(x, y, col, ...)		{
		panel.grid(h=4, v=0, col = "grey80", ...)
		panel.xyplot(x, y, col = "grey40", ...)
		panel.mean(x, y, type = "p", horizontal = F, col = 1, 
		pch = "-", cex = 10, ...) })

}


