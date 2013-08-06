% Michael Malick
% 09 Mar 2013



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Header
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{mm.par}
\alias{mm.par}
\title{Par Settings for Lattice Graphics}

\description{
    Returns a list of par settings for lattice that can be passed
    to the \code{par.settings} argument.    
}

\usage{
    mm.par(\dots) 
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Body
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
    \item{\dots}{
        A list of parameters to pass to \code{par.settings}. This
        allows the user to change additional parameters not set by
        \code{mm.par} or to change the settings in \code{mm.par}.
    }

}

\details{
    A list of parameters supplied to the function should be in the
    same format as for \code{par.settings}.
}

\value{
    Returns a list of visually pleasing par setting for lattice
    graphics.
}

\author{Michael Malick}

\seealso{
    \code{\link{trellis.par.get}}, 
    \code{\link{trellis.par.set}}, 
    \code{\link{lattice.options}},
    \code{\link{xyplot}}
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{

# require(lattice)
# 
# xyplot(Petal.Width ~ Petal.Length | Species, data = iris,
#     par.settings = mm.par())
# 
# xyplot(Petal.Width ~ Petal.Length | Species, data = iris,
#     par.settings = mm.par(), grid = TRUE)
# 
# xyplot(Petal.Width ~ Petal.Length | Species, data = iris,
#     par.settings = mm.par(list(fontsize = list(text = 16),
#     par.main.text = list(col = "grey50"), 
#     plot.symbol = list(pch="l"))), 
#     main = "Iris")
# 
# barchart(Species ~ Petal.Length, data = iris, 
#     par.settings = mm.par())
# 
# barchart( ~ Petal.Length, data = iris, groups = Species,
#     par.settings = mm.par(), auto.key = TRUE,
#     panel = function(x, y, ...) {
#         panel.grid(-1, -1)
#         panel.barchart(x, y, ...)
#     })
# 
# dotplot(Species ~ Petal.Length, data = iris,
#     par.settings = mm.par())


}


