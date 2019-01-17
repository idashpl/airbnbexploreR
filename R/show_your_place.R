#' Show your potential flats
#'
#' @description Produce a leaflet map to show top selected flats.
#' @export
#' @param data A data.frame. Output of \code{\link{find_your_place}} function.
#' @param top Define top n flats to show on a map. The function will not show a map
#' if \code{top} is greater than 30.
#' @return A leaflet map with your top n flats.
#' @examples
#' \dontrun{
#' data_df <- find_your_place(listings, calendar, checkin = "2019-01-30",
#'                           num_days = 2, distance = c(1,3), num_guests = 2,
#'                           price = c(20, 100))
#' show_your_place(data_df)
#' }

show_your_place <- function(data, top = 3) {

    if (top >= 30)
        stop("The `top` argument is bigger than 30; the map do not show up. Try again.", call. = F)

    df <- data %>%
        utils::head(top) %>%
        mutate(info = paste0(host_name, ", price: ", price, '$, review: ', round(bayes_review, 2)))

    leaflet::leaflet(df) %>%
        leaflet::addTiles() %>%
        leaflet::addMarkers(lng = ~longitude, lat = ~latitude)
}
