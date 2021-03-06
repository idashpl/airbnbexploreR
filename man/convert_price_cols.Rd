% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_priced_flat.R
\name{convert_price_cols}
\alias{convert_price_cols}
\title{Convert  \code{$} columns}
\usage{
convert_price_cols(data)
}
\arguments{
\item{data}{A data.frame with listings. If file was downloaded by
\code{download_airbnb_data} the name is "*listings.csv".}
}
\value{
A data.frame with converted columns.
}
\description{
Convert all columns with \code{$} sign into numeric column.
}
