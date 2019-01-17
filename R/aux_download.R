#' Download a single airbnb dataset
#'
#' @description
#' Download a specific dataset from \url{http://insideairbnb.com} webpage.
#' @export
#' @param link Link to a given airbnb dataset.
#' @inheritParams download_airbnb_data
#' @param summary Boolean option. Add "summary_" prefix to file name
#' to distinguish raw dataset from summary dataset.
#' @seealso \code{\link{download_airbnb_data}}
#' @examples
#' the_link <- "http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2018-12-06/data/listings.csv.gz"
#' aux_download(the_link, unzip = TRUE)

aux_download <- function(link, dir = NULL, unzip = FALSE, summary = FALSE) {

    if (is.null(dir))
        dir <- "airbnb_data"

    file_name <- extract_file_name(link, summary)
    if (!dir.exists(dir))
        dir.create(dir)

    path <- file.path(dir, file_name)

    if (file.exists(path)) {
        print(paste0(file_name, " already exists. Skipping."))
        return(invisible(file_name))
    }

    utils::download.file(link, destfile = path)

    if (unzip) {
        cmd <- paste0("gunzip -k ", path)
        system(cmd)
    }

}

#' Create file name from a link
#'
#' @description
#' Scrape webpage to create a proper file name for downloaded data.
#' @keywords internal
#' @inheritParams aux_download

extract_file_name <- function(link, summary = F) {
    name <- stringr::str_split(link, "/") %>%
        unlist() %>%
        .[c(6,7,9)] %>%
        paste(collapse = '') %>%
        stringr::str_remove_all(pattern = "-")

    if (summary)
        name <- paste0("summary_", name)

    return(name)
}
