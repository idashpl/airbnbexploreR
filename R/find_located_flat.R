#' Find located flat
#'
#' @description Modify a \code{listings} data.frame by defining column with distance
#' from city center. Additionally, filter out in terms of the distance from city center.
#' @export
#' @inheritParams find_your_place
#' @param name An assigned name for the distance column.
#' @return A data.frame with flat which meet the distance criterion.
#' @examples
#' \dontrun{
#'  find_located_flat(listings, distance_center, c(1,3)) %>%
#'     ggplot(., aes(xxx)) +
#'     geom_density()
#' }

find_located_flat <- function(data, name, distance = NULL) {

    expr_name <- rlang::enquo(name)

    # Necessary for correct filtation with vector
    d_threshold <- rev(distance)

    # Necessary test
    key_cols <- c("latitude", "longitude")
    if (columns_exist(data, key_cols))
        stop("The data does not have `longitute` and `latitude` columns. Try again.", call. = F)

    the_city_coord <- get_latlong(data)

    df <- data %>%
        group_by(id) %>%
        mutate(!!expr_name := calc_distance(the_city_coord, latitude, longitude)) %>%
        defined_filter(d_threshold, !!expr_name <= d_threshold[1])

    if (length(d_threshold) > 1)
        df <- filter(df, !!expr_name >= d_threshold[2])

    return(df)
}

calc_distance <- function(center, lat, long) {

    mat <- matrix(c(center$lng, long, center$lat, lat), ncol = 2)
    sp::spDistsN1(mat, mat[2,], longlat = T)[1]

}

get_latlong <- function(data) {
    the_city <- data %>%
        count(city) %>%
        arrange(-n) %>%
        pull(city) %>%
        utils::head(5)

    df <- filter(cities_coord, city_ascii %in% the_city)

    if (nrow(df) == 0)
        stop(paste0("Couldn't find a center coordinates for ", the_city[1],"."), call. = F)

    return(df)
}

