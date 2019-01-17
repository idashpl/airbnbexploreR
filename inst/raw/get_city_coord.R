get_city_coord <- function() {
    require(dplyr)
    require(rvest)

    base_url = "http://insideairbnb.com/get-the-data.html"
    load("worldcities.RData")

    city_names <- xml2::read_html(base_url) %>%
        rvest::html_nodes(".contentContainer h2") %>%
        rvest::html_text() %>%
        stringr::str_split(", ") %>%
        purrr::map(head ,1) %>%
        unlist() %>%
        unique()

    cities_coord <- worldcities %>%
        filter(city_ascii %in% city_names, capital %in% c("admin", "primary")) %>%
        arrange(city) %>%
        filter(iso2 != "VE")

    save(base_url, cities_coord, file = "../../R/sysdata.rda")
}
