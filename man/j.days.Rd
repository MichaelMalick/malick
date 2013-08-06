% Michael Malick
% 11 May 2012



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Header
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\name{j.days}
\alias{j.days}
\title{Compute Daily Time Intervals}

\description{
    Returns daily values of year, month, day of the month, week of
    the year, and Julian days for the input year. This function
    accounts for leap years.  February will have 29 days in leap
    years.  
}

\usage{
    j.days(year)
}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Body
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\arguments{
	\item{year}{
        year for which to compute values.
    }
}

\value{
    Returns a dataframe that contains a record for each day of the
    year, 365 for non-leap years and 366 for leap years. The
    dataframe contains seven columns: year, jday, month, week, day,
    day5, day8.

    For the weeks columns, the last week of the year contains an
    extra day (2 days in leap years). For the day8 column the last
    period only has 5 days.
    }

\author{Michael Malick}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Examples
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\examples{
j.days(1999)
}

