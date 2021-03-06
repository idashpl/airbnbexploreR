% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_available_flat.R
\name{find_available_flat}
\alias{find_available_flat}
\title{Find available flat}
\usage{
find_available_flat(data, calendar, checkin, num_days)
}
\arguments{
\item{data}{A data.frame with listings. If file was downloaded by
\code{download_airbnb_data} the name is "*listings.csv".}

\item{calendar}{A data.frame with calendar. If file was downloaded by
\code{download_airbnb_data} the name is "*calendar.csv".}

\item{checkin}{The checkin date in the following format (YYYYMMDD, "YYYY/MM/DD", "YYYY-MM-DD").}

\item{num_days}{The number of nights to stay.}
}
\value{
A data.frame with flat which meet the available criterion.
}
\description{
Find only those flats which are available in a given period of time.
}
