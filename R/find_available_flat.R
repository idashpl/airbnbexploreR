#' Find available flat
#'
#' @description Find only those flats which are available in a given period of time.
#' @export
#' @inheritParams find_your_place
#' @return  A data.frame with flat which meet the available criterion.

find_available_flat <- function(data, calendar, checkin, num_days) {

    key_cols <- c("available", "date")
    if (columns_exist(calendar, key_cols))
        stop("It's not a calendar dataset. Try again.", call. = F)

    checkin <- lubridate::ymd(checkin)
    checkout <- lubridate::ymd(checkin) + lubridate::ddays(num_days)

    ava_listings <- calendar %>%
        filter(listing_id %in% data$id) %>%
        select(-price) %>%
        filter(date >= checkin, date <= checkout) %>%
        group_by(listing_id) %>%
        summarise(available_all = all(available == "t")) %>%
        filter(available_all) %>%
        pull(listing_id)

    df <- data %>% filter(id %in% ava_listings)

    return(df)
}
