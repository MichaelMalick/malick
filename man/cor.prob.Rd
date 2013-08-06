% Michael Malick
% 12 May 2012


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HEADER 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{cor.prob}

\alias{cor.prob}

\title{Pairwise Correlation Matrix}

\description{
    The function produces a matrix of all pairwise correlations with
    the correlations below the main diagonal and the p-values from
    pairwise deletion significant tests above the main diagonal.
}

\usage{
    cor.prob(X)
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BODY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
    \item{X}{
	    A matrix or dataframe
    }
}

\details{
    Correlations are performed among the columns of the matrix or
    dataframe.
}

\value{Returns a matrix}


\author{
    Michael Malick
}

\note{
    Most of the code was found online
}

\seealso{
    \code{\link{cor}},
    \code{\link{cor.test}}
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{

    x <- rnorm(25)
    mat <- matrix(data = x, nrow = 5, ncol = 5, dimnames = list(
        c("A", "B","C", "D", "E"), c("M", "N","O", "P", "Q"))) 

    cor.prob(mat)

    # Iterative Version of cor Function 
    cor(mat, use = "pairwise.complete.obs")

}



