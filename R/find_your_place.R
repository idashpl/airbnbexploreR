#' Find you place
#'
#' @description Find the optimal flat for your travel. The dataset is sorted by
#' review column (calculated by \code{\link{find_reviewed_flat}}.)
#' @export
#' @param data A data.frame with listings. If file was downloaded by
#' \code{download_airbnb_data} the name is "*listings.csv".
#' @param calendar A data.frame with calendar. If file was downloaded by
#' \code{download_airbnb_data} the name is "*calendar.csv".
#' @param checkin The checkin date in the following format (YYYYMMDD, "YYYY/MM/DD", "YYYY-MM-DD").
#' @param num_days The number of nights to stay.
#' @param distance The distance from the city center. A numeric scalar will define
#' maximum distance , a numeric vector of length two will create a range.
#' @param num_guests The number of guests.
#' @param rooms A room type the user is looking for. Possible options:
#' \code{"Private room"}, \code{"Entire home/apt"}, \code{"Shared room"}. By default,
#' it takes all of them into account. A character scalar will filter one room type,
#' but by using vector, it's possible to choose more room types at once.
#' @param price The flat price for one night. A numeric scalar will define
#' maximum price the user is ready to pay, a numeric vector of length two will create a range.
#' @param certain A numeric value which defines the level of certainty.
#' It represents how confident the model is. It is equivalent to a number of observations.
#' @param prior A numeric value which defines mean of prior.
#' @return A data.frame with all flats which meet the criteria.
#' @seealso \code{\link{find_available_flat}}, \code{\link{find_located_flat}},
#' \code{\link{find_priced_flat}},\code{\link{find_reviewed_flat}}
#' @examples
#' \dontrun{
#' find_your_place(listings,
#'                 calendar,
#'                 checkin = "2019-01-30",
#'                 num_days = 2,
#'                 distance = c(1,6),
#'                 num_guests = 2,
#'                 price = c(20, 100))
#'}

find_your_place <- function(data, calendar, checkin, num_days, distance = NULL,
                            num_guests = NULL, rooms = NULL, price = NULL,
                            certain = 30, prior = 90){

    df <- data %>%
        find_located_flat(distance_center, distance) %>%
        find_reviewed_flat(bayes_review, certain, prior) %>%
        find_priced_flat(num_guests, rooms, price) %>%
        select(id, host_name, bayes_review, number_of_reviews, review_scores_rating, price, distance_center,
               accommodates, room_type, latitude, longitude) %>%
        find_available_flat(calendar, checkin, num_days)

    if (nrow(df) == 0) {
        warning("There's no flat which meets the criteria. Try again.", call. = F)
        return(invisible(df))
    }

    return(df)
}
