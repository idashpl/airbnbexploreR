% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_your_place.R
\name{find_your_place}
\alias{find_your_place}
\title{Find you place}
\usage{
find_your_place(data, calendar, checkin, num_days, distance = NULL,
  num_guests = NULL, rooms = NULL, price = NULL, certain = 30,
  prior = 90)
}
\arguments{
\item{data}{A data.frame with listings. If file was downloaded by
\code{download_airbnb_data} the name is "*listings.csv".}

\item{calendar}{A data.frame with calendar. If file was downloaded by
\code{download_airbnb_data} the name is "*calendar.csv".}

\item{checkin}{The checkin date in the following format (YYYYMMDD, "YYYY/MM/DD", "YYYY-MM-DD").}

\item{num_days}{The number of nights to stay.}

\item{distance}{The distance from the city center. A numeric scalar will define
maximum distance , a numeric vector of length two will create a range.}

\item{num_guests}{The number of guests.}

\item{rooms}{A room type the user is looking for. Possible options:
\code{"Private room"}, \code{"Entire home/apt"}, \code{"Shared room"}. By default,
it takes all of them into account. A character scalar will filter one room type,
but by using vector, it's possible to choose more room types at once.}

\item{price}{The flat price for one night. A numeric scalar will define
maximum price the user is ready to pay, a numeric vector of length two will create a range.}

\item{certain}{A numeric value which defines the level of certainty.
It represents how confident the model is. It is equivalent to a number of observations.}

\item{prior}{A numeric value which defines mean of prior.}
}
\value{
A data.frame with all flats which meet the criteria.
}
\description{
Find the optimal flat for your travel. The dataset is sorted by
review column (calculated by \code{\link{find_reviewed_flat}}.)
}
\examples{
\dontrun{
find_your_place(listings,
                calendar,
                checkin = "2019-01-30",
                num_days = 2,
                distance = c(1,6),
                num_guests = 2,
                price = c(20, 100))
}
}
\seealso{
\code{\link{find_available_flat}}, \code{\link{find_located_flat}},
\code{\link{find_priced_flat}},\code{\link{find_reviewed_flat}}
}
