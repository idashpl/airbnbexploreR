#' Show a list with cities
#'
#' @description Show all available cities to scrape from
#' the \url{http://insideairbnb.com} webpage.
#' @param what Category you want to return. Available options: \code{"city"}
#' or \code{"country"}.
#' @export
#' @return A vector with all available cities.
#' @examples extract_metadata("city")

extract_metadata <- function(what) {

    if (!any(what %in% c("country", "city"))) {
        stop("You have to choose either `country` or `city`. Try again.")
    }

    what_fun <- ifelse(what == 'city', utils::head, utils::tail)

    xml2::read_html(base_url) %>%
        rvest::html_nodes(".contentContainer h2") %>%
        rvest::html_text() %>%
        stringr::str_split(", ") %>%
        purrr::map(what_fun ,1) %>%
        unlist() %>%
        unique()

}
