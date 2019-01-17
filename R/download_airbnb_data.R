#' Download airbnb many datasets at once
#'
#' @description
#' Scrape the \url{http://insideairbnb.com} webpage to download selected datasets.
#'
#' @export
#' @param dir Directory to store downloaded data. If not specified, it will build
#'  \code{airbnb_data/} directory.
#' @param data_to_load Names of datasets to download. Possible options:
#' `listings`, `calendar`, `reviews`. By default, it takes all of them into account.
#' A character scalar will save one dataset type, but by using vector it's possible
#' to define more dataset types.
#' @param archived Boolean option. Whether to download archived files as well.
#' By defualt,  \code{archived = FALSE} which means, it saves only the latest datasets.
#' @param city City name. The function will download only datasets related to the  \code{city}.
#' It could be character scalar or a vector with more cities. To see all available cities,
#' check \code{\link{extract_metadata}} function.
#' @param format Defined dataset format. Two options available:  \code{raw} which stores
#' raw data and  \code{summary} which have only the most important columns from raw data.
#' By default the function will download both formats.
#' @param confirm_threshold The user will be asked for confirmation when the number of datasets
#'  to download exceeds the  \code{confirm_threshold}.
#' @param unzip Boolean option. Whether to uzip saved datasets.
#' This opiton is only useful in case of raw data..
#' @seealso \code{\link{aux_download}}
#' @examples
#' \dontrun{
#' download_airbnb_data(format = 'raw',
#'                      archived = TRUE,
#'                      data_to_load = c("listings", "calendar"),
#'                      unzip = TRUE,
#'                      city = c("Berlin", "Vienna"))
#'}

download_airbnb_data <- function(dir = NULL,
                                 data_to_load = NULL,
                                 archived = FALSE,
                                 city = NULL,
                                 format = NULL,
                                 confirm_threshold = 20,
                                 unzip = FALSE) {

    # Define helper variables
    confirm <- TRUE
    summary <- ifelse(format == "summary", TRUE, FALSE)
    unzip <- ifelse(format == "summary", FALSE, unzip)

    # Filter out linkbase based on pre-defined criteria
    page_df <- filter_linkbase(data_to_load, archived, city, format)

    # Ask for confirmation (in R console)
    if (nrow(page_df) > confirm_threshold)
        confirm <- askYesNo(paste0("Do you want to download ", nrow(page_df), " datasets?"),
                            prompts = 'y/n/c')

    # Downloading files
    if (confirm & !is.na(confirm))
        page_df$links %>% purrr::walk(aux_download, dir, unzip, summary)
}

#' Filter linkbase
#'
#' @description Modify a scraped dataset based on filtering criteria.
#' @keywords internal
#' @inheritParams download_airbnb_data
#' @return A data.frame with filtered links scraped from \url{http://insideairbnb.com} webpage.

filter_linkbase <- function(data_to_load = NULL,
                            archived = FALSE,
                            city = NULL,
                            format = NULL) {

    # Necessary test
    if (is.null(data_to_load))
        data_to_load <- c("listings", "reviews", "calendar")

    if (!any(c("listings", "reviews", "calendar") %in% data_to_load)) {
        stop("You have to choose either `listings`, `calendar` or `reviews` dataset. Try again.", call. = F)
    }

    df <- build_linkbase() %>%
        mutate(links = extract_links(),
               archived = extract_archived_info()) %>%
        filter(dataset %in% data_to_load) %>%
        defined_filter(format, form == format) %>%
        defined_filter(city, city_name %in% adjust_geo_names(city))

    if (!archived)
        df <- filter(df, archived == "")

    return(df)
}

#' Build linkbase
#'
#' @description Build a dataset based on scraped from \url{http://insideairbnb.com} webpage.
#' @keywords internal
#' @inheritParams download_airbnb_data
#' @return A data.frame with all information scraped from \url{http://insideairbnb.com} webpage.

build_linkbase <- function() {

    col_names <- c("_", "_", "_", 'country', 'region', 'city_name',
                   'release_date', 'form', 'dataset')

    extract_links() %>%
        purrr::map(stringr::str_split, "/") %>%
        purrr::flatten() %>%
        purrr::map(as.list) %>%
        purrr::map(stats::setNames, col_names) %>%
        bind_rows() %>%
        select(-"_") %>%
        mutate(form = ifelse(form == 'data', 'raw', 'summary'),
               dataset = purrr::map_chr(dataset, clean_dataset_col))
}

clean_dataset_col <- function(x) {
    stringr::str_split(x, "\\.") %>% purrr::map(1) %>% unlist()
}

#' Extract all links
#'
#' @description Extract all available links with data from \url{http://insideairbnb.com} webpage.
#' @keywords internal
#' @return A list with all links scraped from \url{http://insideairbnb.com} webpage.

extract_links <- function() {

    xml2::read_html(base_url) %>%
        rvest::html_nodes(".table-hover tbody td a") %>%
        rvest::html_attrs() %>%
        purrr::map(1) %>%
        unlist()

}

#' Extract data status
#'
#' @description Get information whether a given dataset is archived.
#' @keywords internal
#' @return A list with \code{archived} status.

extract_archived_info <- function() {

    xml2::read_html(base_url) %>%
        rvest::html_nodes(".table-hover tbody tr") %>%
        rvest::html_attr("class")
}


adjust_geo_names <- function(x) {
    stringr::str_to_lower(x) %>% stringr::str_replace_all(" ", "-")
}
