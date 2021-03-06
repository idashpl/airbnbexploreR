% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_priced_flat.R
\name{find_priced_flat}
\alias{find_priced_flat}
\title{Find priced flat}
\usage{
find_priced_flat(data, num_guests = NULL, rooms = NULL, price = NULL)
}
\arguments{
\item{data}{A data.frame with listings. If file was downloaded by
\code{download_airbnb_data} the name is "*listings.csv".}

\item{num_guests}{The number of guests.}

\item{rooms}{A room type the user is looking for. Possible options:
\code{"Private room"}, \code{"Entire home/apt"}, \code{"Shared room"}. By default,
it takes all of them into account. A character scalar will filter one room type,
but by using vector, it's possible to choose more room types at once.}

\item{price}{The flat price for one night. A numeric scalar will define
maximum price the user is ready to pay, a numeric vector of length two will create a range.}
}
\value{
A data.frame with flat which meet all criteria.
}
\description{
Filter a \code{listings} data.frame in terms of some criteria.
}
\examples{
\dontrun{
find_priced_flat(listings,
                 num_guests = 3,
                 price = c(100, 200)) \%>\%
 ggplot(aes(price)) +
 geom_histogram()
}
}
