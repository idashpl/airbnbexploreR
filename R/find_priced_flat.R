#' Find priced flat
#'
#' @description Filter a \code{listings} data.frame in terms of some criteria.
#' @export
#' @inheritParams find_your_place
#' @return A data.frame with flat which meet all criteria.
#' @examples
#' \dontrun{
#' find_priced_flat(listings,
#'                  num_guests = 3,
#'                  price = c(100, 200)) %>%
#'  ggplot(aes(price)) +
#'  geom_histogram()
#' }

find_priced_flat <- function(data, num_guests = NULL, rooms = NULL, price = NULL) {

    p_threshold <- rev(price)

    df <- data %>%
        convert_price_cols() %>%
        defined_filter(num_guests, accommodates >= num_guests) %>%
        defined_filter(rooms, room_type %in% rooms) %>%
        defined_filter(p_threshold, price <= p_threshold[1]) %>%
        mutate(weekly_price = ifelse(weekly_price == 0, price * 7, weekly_price))

    if (length(p_threshold) > 1)
        df <- filter(df, price >= p_threshold[2])

    return(df)

}

#' Convert  \code{$} columns
#'
#' @description Convert all columns with \code{$} sign into numeric column.
#' @export
#' @inheritParams find_your_place
#' @return A data.frame with converted columns.

convert_price_cols <- function(data) {

    # Detect columns with `$`` sign
    columns <- data %>%
        utils::head(200) %>%
        ungroup() %>%
        mutate_all(funs(find_dolars)) %>%
        colSums(., na.rm = T) %>%
        purrr::keep(~.x > 5) %>%
        names()

    if (length(columns) == 0)
        return(data)

    replaced_cols <- create_replace_na(columns)

    df <- data %>%
        mutate_at(vars(columns), funs(readr::parse_number)) %>%
        tidyr::replace_na(replaced_cols)

    return(df)
}

create_replace_na <- function(cols) {
    rep(0, length(cols)) %>%
        list() %>%
        purrr::map(stats::setNames, cols) %>%
        purrr::flatten()
}


find_dolars <- function(x) {
    stringr::str_detect(x, pattern = "[$][0-9]+")
}
